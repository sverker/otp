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
static erts_atomic_t locked_after[MAX_LOCK_TYPES][2];

static int lock_order_error(dlc_thread_t*, erts_dlc_t*);
#ifdef DLC_UNIT_TEST
static void erts_dlc_test(void);
#endif

void erts_dlc_init(void)
{
    erts_atomic_init_nob(&n_lock_types, 0);
    erts_tsd_key_create(&dlc_thread_key, "dyn_lock_check");

    erts_mtx_init(&lock_types_mtx, "dyn_lock_check", NIL,
           ERTS_LOCK_FLAGS_PROPERTY_STATIC | ERTS_LOCK_FLAGS_CATEGORY_GENERIC);

#ifdef DLC_UNIT_TEST
    erts_dlc_test();
#endif
}

void erts_dlc_create_lock(erts_dlc_t* dlc, const char* name)
{
    erts_aint_t i, n = erts_atomic_read_nob(&n_lock_types);
    int len;

    for (i=0; i < n; i++) {
        if (strcmp(name, lock_types[i].name) == 0) {
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
        if (strcmp(name, lock_types[i].name) == 0) {
            dlc->ix = i;
            goto done; /* already exists (race) */
        }
    }

    ERTS_ASSERT(n < MAX_LOCK_TYPES);
    len = strlen(name);
    ERTS_ASSERT(len < MAX_LOCK_NAME_SZ);
    strcpy(lock_types[n].name, name);
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
        UWord before[2], after[2];

        DLC_ASSERT(thr->n_locked);
        if (is_bit_set(dlc->ix, thr->locked_now)) {
            /*
             * Lock of this type already held.
             * Must be other instance of last locked lock
             */
            DLC_ASSERT(is_bit_set(dlc->ix, thr->locked_before));
            if (dlc->ix != thr->lock_order[thr->n_locked-1].ix)
                return lock_order_error(thr, dlc);
            thr->lock_order[thr->n_locked-1].cnt++;
            thr->lock_order[thr->n_locked-1].trylock = 0;
            return 1;
        }

        before[0] = erts_atomic_read_nob(&locked_before[dlc->ix][0]);
        before[1] = erts_atomic_read_nob(&locked_before[dlc->ix][1]);
        after[0] = erts_atomic_read_nob(&locked_after[dlc->ix][0]);
        after[1] = erts_atomic_read_nob(&locked_after[dlc->ix][1]);

        if ((thr->locked_before[0] & after[0]) ||
            (thr->locked_before[1] & after[1]))
            return lock_order_error(thr, dlc);

        ERTS_ASSERT(!is_bit_set(dlc->ix, thr->locked_before));

        /*
         * Check if we introduce new lock dependencies
         */
        if ((thr->locked_before[0] & ~before[0]) ||
            (thr->locked_before[1] & ~before[1])) {
            UWord new_before[2];
            before[0] = erts_atomic_read_bor_nob(&locked_before[dlc->ix][0],
                                                 thr->locked_before[0]);
            before[1] = erts_atomic_read_bor_nob(&locked_before[dlc->ix][1],
                                                 thr->locked_before[1]);
            new_before[0] = thr->locked_before[0] & ~before[0];
            new_before[1] = thr->locked_before[1] & ~before[1];
            if (new_before[0] | new_before[1]) {
                int i;
                for (i = 0; i < thr->n_locked; i++) {
                    const UWord ix = thr->lock_order[i].ix;
                    DLC_ASSERT(is_bit_set(ix,thr->locked_before));
                    if (is_bit_set(ix, new_before)) {
                        erts_atomic_read_bor_nob(&locked_after[ix][lock_word],
                                                 lock_bit);
                        new_before[ix / 64] &= ~IX_TO_BIT(ix % 64);
                    }
                }
                DLC_ASSERT(!(new_before[0]|new_before[1]));
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

    if (thr->lock_order[thr->n_locked-1].ix == dlc->ix) {
        thr->locked_before[lock_word] &= ~lock_bit;
        while (--thr->n_locked) {
            UWord bit = IX_TO_BIT(thr->lock_order[thr->n_locked-1].ix % 64);
            UWord word = thr->lock_order[thr->n_locked-1].ix / 64;
            if (bit & thr->locked_now[word])
                break;
            DLC_ASSERT(thr->locked_before[word] & bit);
            thr->locked_before[word] &= ~bit;
        }
    }
}

static int dlc_test = 0;

static int lock_order_error(dlc_thread_t *thr, erts_dlc_t* dlc)
{
    int i, ok = 0;
    UWord after[2];
    after[0] = erts_atomic_read_nob(&locked_after[dlc->ix][0]);
    after[1] = erts_atomic_read_nob(&locked_after[dlc->ix][1]);

    erts_fprintf(stderr, "###### DYNAMIC LOCK ORDER VIOLATION ######\n");
    erts_fprintf(stderr, "# Trying to lock '%s'\n", lock_types[dlc->ix]);
    for (i = 0; i < thr->n_locked; i++) {
        UWord bit = IX_TO_BIT(thr->lock_order[i].ix % 64);
        UWord word = thr->lock_order[i].ix / 64;
        if (bit & after[word]) {
            erts_fprintf(stderr, "# while '%s' is held\n",
                         lock_types[thr->lock_order[i].ix]);
            ok = 1;
        }
    }
    if (!ok)
        erts_fprintf(stderr, "Huh? Did not find any lock out of order\n");
    if (dlc_test)
        return 0;
    abort();
}


#ifdef DLC_UNIT_TEST
static void erts_dlc_clear_order(void)
{
    int i, n = erts_atomic_read_nob(&n_lock_types);

    for (i = 0; i < n; i++) {
        erts_atomic_set_nob(&locked_before[i], 0);
        erts_atomic_set_nob(&locked_after[i], 0);
    }
}

static void erts_dlc_test(void)
{
    erts_aint_t save_n_lock_types = erts_atomic_read_nob(&n_lock_types);
    dlc_thread_t* thr = get_thr();
    dlc_thread_t save_thr = *thr;

    erts_dlc_t A,B,C,D,E,F;


    dlc_test = 1;

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

    erts_dlc_clear_order();
    erts_atomic_set_nob(&n_lock_types, save_n_lock_types);
    *thr = save_thr;
}
#endif /* DLC_UNIT_TEST */

