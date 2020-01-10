/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2001-2017. All Rights Reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * %CopyrightEnd%
 */

/* Description: A dynamic lock order checker.
 *              A global locking order is recorded during runtime
 *              and continuously checked against itself.
 *
 * Author: Sverker Eriksson
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "sys.h"
#include "erl_threads.h"
#include "erl_dyn_lock_check.h"

#define DLC_ASSERT(X) ERTS_ASSERT(X)

#define MAX_LOCK_TYPES (64*2)
#define MAX_LOCK_NAME_SZ 64

static erts_atomic_t n_lock_types; 
static erts_mtx_t lock_types_mtx;

struct lock_type
{
    char name[MAX_LOCK_NAME_SZ];
};

static struct lock_type lock_types[MAX_LOCK_TYPES];

static erts_tsd_key_t dlc_thread_key;

typedef struct
{
    UWord locked_now[2];
    UWord locked_before[2];
    unsigned n_locked;
    struct {
        unsigned ix;
        unsigned cnt;
        unsigned trylock;
    } lock_order[MAX_LOCK_TYPES];
}  dlc_thread_t;

static erts_atomic_t locked_before[MAX_LOCK_TYPES][2];

static int check_lock_order(dlc_thread_t*, erts_dlc_t*);
static int lock_order_error(dlc_thread_t*, erts_dlc_t*);

#define DLC_UNIT_TEST
#ifdef DLC_UNIT_TEST
static int dlc_unit_test = 0;
static void erts_dlc_unit_test(void);
#endif

void erts_dlc_init(void)
{
    erts_atomic_init_nob(&n_lock_types, 0);
    erts_tsd_key_create(&dlc_thread_key, "dyn_lock_check");

    erts_mtx_init(&lock_types_mtx, "dyn_lock_check", NIL,
           ERTS_LOCK_FLAGS_PROPERTY_STATIC | ERTS_LOCK_FLAGS_CATEGORY_GENERIC);

#ifdef DLC_UNIT_TEST
    erts_dlc_unit_test();
#endif
}

void erts_dlc_create_lock(erts_dlc_t* dlc, const char* name)
{
    erts_aint_t i, n = erts_atomic_read_nob(&n_lock_types);
    int name_len;

    for (i = 0; name[i]; i++) {
        if (name[i] == '[')
            break;
    }
    name_len = i;

    for (i=0; i < n; i++) {
        if (sys_strncmp(name, lock_types[i].name, name_len) == 0) {
            dlc->ix = i;
            return; /* already exists */
        }
    }

    if (dlc != &lock_types_mtx.dlc)
        erts_mtx_lock(&lock_types_mtx);
    else
        DLC_ASSERT(erts_atomic_read_nob(&n_lock_types) == 0);

    n = erts_atomic_read_nob(&n_lock_types);

    for ( ; i < n; i++) {
        if (sys_strncmp(name, lock_types[i].name, name_len) == 0) {
            dlc->ix = i;
            goto done; /* already exists (race) */
        }
    }

    ERTS_ASSERT(n < MAX_LOCK_TYPES);
    ERTS_ASSERT(name_len < MAX_LOCK_NAME_SZ);
    sys_strncpy(lock_types[n].name, name, name_len);
    lock_types[n].name[name_len] = 0;
    erts_atomic_set_nob(&n_lock_types, n+1);
    dlc->ix = n;

done:
    if (dlc != &lock_types_mtx.dlc)
        erts_mtx_unlock(&lock_types_mtx);
}

#define IX_TO_BIT(IX) ((UWord)1 << (IX))


static dlc_thread_t *get_thr(void)
{
    dlc_thread_t *thr = (dlc_thread_t*) erts_tsd_get(dlc_thread_key);
    
    if (!thr) {
        thr = malloc(sizeof(dlc_thread_t));
        thr->locked_now[0] = 0;
        thr->locked_now[1] = 0;
        thr->locked_before[0] = 0;
        thr->locked_before[1] = 0;
        thr->n_locked = 0;
        erts_tsd_set(dlc_thread_key, thr);
    }
    return thr;
}


static int is_bit_set(unsigned ix, const UWord* words)
{
    DLC_ASSERT(ix < MAX_LOCK_TYPES);
    return (words[ix / 64] & IX_TO_BIT(ix % 64)) != (UWord)0;
}

int erts_dlc_lock(erts_dlc_t* dlc)
{
    dlc_thread_t *thr = get_thr();
    const UWord lock_bit = IX_TO_BIT(dlc->ix % 64);
    const unsigned lock_word = dlc->ix / 64;

        
    if (thr->locked_now[0] | thr->locked_now[1]) {
        UWord before[2];
        UWord new_before[2];

        DLC_ASSERT(thr->n_locked);
        if (is_bit_set(dlc->ix, thr->locked_now)) {
            int i;
            /*
             * Lock of this type already held.
             * Must be other instance of last locked lock
             */
            DLC_ASSERT(is_bit_set(dlc->ix, thr->locked_before));
            i = thr->n_locked-1;
            while (dlc->ix != thr->lock_order[i].ix) {
                if (thr->lock_order[i].trylock) {
                    i--;
                    DLC_ASSERT(i >= 0);
                    continue;
                }
                return lock_order_error(thr, dlc);
            }
            thr->lock_order[i].cnt++;
            thr->lock_order[i].trylock = 0;
            return 1;
        }

        before[0] = erts_atomic_read_nob(&locked_before[dlc->ix][0]);
        before[1] = erts_atomic_read_nob(&locked_before[dlc->ix][1]);

        /*
         * Check if we introduce new lock dependencies
         */
        new_before[0] = thr->locked_before[0] & ~before[0];
        new_before[1] = thr->locked_before[1] & ~before[1];
        if (new_before[0] | new_before[1]) {
            if (!check_lock_order(thr, dlc)) {
                DLC_ASSERT(dlc_unit_test);
                return 0;
            }
            erts_atomic_read_bor_mb(&locked_before[dlc->ix][0],
                                    thr->locked_before[0]);
            erts_atomic_read_bor_mb(&locked_before[dlc->ix][1],
                                    thr->locked_before[1]);
            /* check again to detect race */
            if (!check_lock_order(thr, dlc)) {
                DLC_ASSERT(dlc_unit_test);
                /* can't continue test as 'locked_before' is inconsistent */
                abort();
            }
        }
    }
    else {
        DLC_ASSERT(!(thr->locked_before[0]|thr->locked_before[1]));
        DLC_ASSERT(!thr->n_locked);
    }
    thr->locked_now[lock_word] |= lock_bit;
    thr->locked_before[lock_word] |= lock_bit;
    thr->lock_order[thr->n_locked].ix = dlc->ix;
    thr->lock_order[thr->n_locked].cnt = 1;
    thr->lock_order[thr->n_locked].trylock = 0;
    thr->n_locked++;
    return 1;
}

void erts_dlc_trylock(erts_dlc_t* dlc, int locked)
{
    const UWord lock_bit = IX_TO_BIT(dlc->ix % 64);
    const unsigned lock_word = dlc->ix / 64;
    dlc_thread_t *thr = get_thr();

    if (!locked) {
        /* We have no way to detect trylock of self-locked instance (yet)
           so nothing to do here. */
        return;
    }

    if (is_bit_set(dlc->ix, thr->locked_now)) {
        int i;
        DLC_ASSERT(is_bit_set(dlc->ix, thr->locked_before));
        i = 0;
        while (1) {
            DLC_ASSERT(i < thr->n_locked);
            if (dlc->ix == thr->lock_order[i].ix)
                break;
            i++;
        }
        DLC_ASSERT(thr->lock_order[i].cnt > 0);
        thr->lock_order[i].cnt++;
        /* keep .trylock as is */
    }
    else {
        thr->locked_now[lock_word] |= lock_bit;

        if (!is_bit_set(dlc->ix, thr->locked_before)) {
            thr->locked_before[lock_word] |= lock_bit;
            thr->lock_order[thr->n_locked].ix = dlc->ix;
            thr->lock_order[thr->n_locked].cnt = 1;
            thr->lock_order[thr->n_locked].trylock = 1;
            thr->n_locked++;
        }
        else {
            int i = 0;
            while (1) {
                DLC_ASSERT(i < thr->n_locked);
                if (dlc->ix == thr->lock_order[i].ix)
                    break;
                i++;
            }
            thr->lock_order[i].cnt++;
        }
    }
}

void erts_dlc_unlock(erts_dlc_t* dlc)
{
    const UWord lock_bit = IX_TO_BIT(dlc->ix % 64);
    const unsigned lock_word = dlc->ix / 64;
    dlc_thread_t *thr = (dlc_thread_t*) erts_tsd_get(dlc_thread_key);
    int i;
    
    ERTS_ASSERT(thr);           
    ERTS_ASSERT(is_bit_set(dlc->ix, thr->locked_now));
    DLC_ASSERT(is_bit_set(dlc->ix, thr->locked_before));
    DLC_ASSERT(thr->n_locked > 0);

    i = 0;
    while (1) {
        DLC_ASSERT(i < thr->n_locked);
        if (thr->lock_order[i].ix == dlc->ix)
            break;
        i++;
    }

    DLC_ASSERT(thr->lock_order[i].cnt > 0);
    thr->lock_order[i].cnt--;
    if (thr->lock_order[i].cnt > 0)
        return; /* still locked by other instance */

    thr->locked_now[lock_word] &= ~lock_bit;

    /*
     * Now clear and forget all our unlocked locks (including this one)
     * THAT was not locked *before* any of our still locked locks.
     */
    for (i = thr->n_locked-1; i >= 0; i--) {
        UWord bit = IX_TO_BIT(thr->lock_order[i].ix % 64);
        UWord word = thr->lock_order[i].ix / 64;
        if (bit & thr->locked_now[word]) {
            if (!thr->lock_order[i].trylock) {
                /* A locked lock, must remember it and all locked before it. */
                break;
            }
        }
        else { /* forget this unlocked lock */
            int j;

            DLC_ASSERT(thr->locked_before[word] & bit);
            thr->locked_before[word] &= ~bit;
            thr->n_locked--;

            /* and move up all trylocks that we may have skipped over */
            for (j = i; j < thr->n_locked; j++) {
                DLC_ASSERT(thr->lock_order[j+1].trylock);
                thr->lock_order[j] = thr->lock_order[j+1];
            }
        }
    }
}

static int check_lock_order(dlc_thread_t *thr, erts_dlc_t* dlc)
{
    const UWord lock_bit = IX_TO_BIT(dlc->ix % 64);
    const unsigned lock_word = dlc->ix / 64;
    int i, error = 0;

    for (i = 0; i < thr->n_locked; i++) {
        const unsigned ix = thr->lock_order[i].ix;

        if (lock_bit & erts_atomic_read_nob(&locked_before[ix][lock_word])) {
            if (!error) {
                error = 1;
                erts_fprintf(stderr, "###### DYNAMIC LOCK ORDER VIOLATION ######\n");
                erts_fprintf(stderr, "# Trying to lock '%s'\n", lock_types[dlc->ix].name);
            }
            erts_fprintf(stderr, "# while '%s' is held\n",
                         lock_types[thr->lock_order[i].ix].name);
        }
    }
    if (error) {
        if (dlc_unit_test)
            return 0;
        abort();
    }
    return 1;
}

static int lock_order_error(dlc_thread_t *thr, erts_dlc_t* dlc)
{
    int error = !check_lock_order(thr, dlc);
    ERTS_ASSERT(error);
    return 0;
}


#ifdef DLC_UNIT_TEST
static void erts_dlc_clear_order(void)
{
    int i, n = erts_atomic_read_nob(&n_lock_types);

    for (i = 0; i < n; i++) {
        erts_atomic_set_nob(&locked_before[i][0], 0);
        erts_atomic_set_nob(&locked_before[i][1], 0);
    }
}

static void erts_dlc_unit_test(void)
{
    erts_aint_t save_n_lock_types = erts_atomic_read_nob(&n_lock_types);
    dlc_thread_t* thr = get_thr();
    dlc_thread_t save_thr = *thr;

    erts_dlc_t A,B,C,D,E,F;


    dlc_unit_test = 1;

    erts_dlc_create_lock(&A, "A");
    erts_dlc_create_lock(&B, "B");
    erts_dlc_create_lock(&C, "C");
    erts_dlc_create_lock(&D, "D");
    erts_dlc_create_lock(&E, "E");
    erts_dlc_create_lock(&F, "F");

    ERTS_ASSERT(erts_dlc_lock(&A));
    ERTS_ASSERT(erts_dlc_lock(&C));
    ERTS_ASSERT(!erts_dlc_lock(&A));

    erts_dlc_unlock(&A);
    ERTS_ASSERT(!erts_dlc_lock(&A));
    erts_dlc_unlock(&C);
    ERTS_ASSERT(erts_dlc_lock(&A));
    ERTS_ASSERT(erts_dlc_lock(&B));
    ERTS_ASSERT(erts_dlc_lock(&C));
    erts_dlc_unlock(&A);
    erts_dlc_unlock(&B);
    erts_dlc_unlock(&C);
    ERTS_ASSERT(erts_dlc_lock(&A));
    ERTS_ASSERT(erts_dlc_lock(&C));
    ERTS_ASSERT(!erts_dlc_lock(&B));
    erts_dlc_unlock(&A);
    erts_dlc_unlock(&C);

    erts_dlc_clear_order();

    ERTS_ASSERT(erts_dlc_lock(&A));
    ERTS_ASSERT(erts_dlc_lock(&B));
    erts_dlc_unlock(&A);
    ERTS_ASSERT(erts_dlc_lock(&C));
    erts_dlc_unlock(&B);
    ERTS_ASSERT(erts_dlc_lock(&D));
    erts_dlc_unlock(&C);
    ERTS_ASSERT(erts_dlc_lock(&E));
    erts_dlc_unlock(&D);
    ERTS_ASSERT(erts_dlc_lock(&F));
    erts_dlc_unlock(&E);
    erts_dlc_unlock(&F);
    ERTS_ASSERT(erts_dlc_lock(&F));
    ERTS_ASSERT(!erts_dlc_lock(&A));
    erts_dlc_unlock(&F);

    erts_dlc_clear_order();
    ERTS_ASSERT(erts_dlc_lock(&A));
    erts_dlc_trylock(&B, 1);
    erts_dlc_unlock(&A);
    ERTS_ASSERT(erts_dlc_lock(&A));
    erts_dlc_unlock(&A);
    erts_dlc_unlock(&B);

    erts_dlc_clear_order();
    ERTS_ASSERT(erts_dlc_lock(&A));
    ERTS_ASSERT(erts_dlc_lock(&B));
    ERTS_ASSERT(erts_dlc_lock(&C));
    erts_dlc_trylock(&D, 1);
    erts_dlc_trylock(&E, 1);
    ERTS_ASSERT(erts_dlc_lock(&F));
    erts_dlc_unlock(&C);
    erts_dlc_unlock(&F);
    ERTS_ASSERT(erts_dlc_lock(&B));
    erts_dlc_unlock(&B);
    ERTS_ASSERT(!erts_dlc_lock(&A));
    erts_dlc_unlock(&B);
    ERTS_ASSERT(erts_dlc_lock(&A));
    erts_dlc_unlock(&A);
    erts_dlc_unlock(&A);
    erts_dlc_unlock(&D);
    erts_dlc_unlock(&E);

    /* Restore */
    erts_dlc_clear_order();
    erts_atomic_set_nob(&n_lock_types, save_n_lock_types);
    *thr = save_thr;
}
#endif /* DLC_UNIT_TEST */

