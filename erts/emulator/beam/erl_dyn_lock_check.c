#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "sys.h"
#include "erl_threads.h"
#include "erl_dyn_lock_check.h"

#define MAX_LOCK_TYPES 64
#define MAX_LOCK_NAME_SZ 32

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
    UWord locked_now;
    UWord locked_before;
    unsigned n_locked;
    unsigned lock_order[MAX_LOCK_TYPES]; 
}  dlc_thread_t;

static erts_atomic_t locked_before[MAX_LOCK_TYPES];
static erts_atomic_t locked_after[MAX_LOCK_TYPES];

void erts_dlc_init(void)
{
    erts_atomic_init_nob(&n_lock_types, 0);
    erts_mtx_init(&lock_types_mtx, "dyn_lock_check", NIL,
           ERTS_LOCK_FLAGS_PROPERTY_STATIC | ERTS_LOCK_FLAGS_CATEGORY_GENERIC);

    erts_tsd_key_create(&dlc_thread_key, "dyn_lock_check");
}

void erts_dlc_create_lock(erts_dlc_t* dlc, const char* name)
{
    erts_aint_t i, n = erts_atomic_read_nob(&n_lock_types);
    unsigned len;

    for (i=0; i < n; i++) {
        if (strcmp(name, lock_types[i].name) == 0)
            dlc->ix = i;
            return; /* already exists */
    }

    erts_mtx_lock(&lock_types_mtx);
    n = erts_atomic_read_nob(&n_lock_types);

    for ( ; i < n; i++) {
        if (strcmp(name, lock_types[i].name) == 0) {
            erts_mtx_unlock(&lock_types_mtx);
            dlc->ix = i;
            return; /* already exists (race) */
        }
    }

    ERTS_ASSERT(n < MAX_LOCK_TYPES);
    len = strlen(name);
    ERTS_ASSERT(len < MAX_LOCK_NAME_SZ);   
    strcpy(lock_types[n].name, name);
    erts_atomic_set_nob(&n_lock_types, n+1);
    erts_mtx_unlock(&lock_types_mtx);
    dlc->ix = n;
}


void erts_dlc_lock(erts_dlc_t* dlc)
{
    const UWord lock_bit = 1 << dlc->ix;
    dlc_thread_t *thr = (dlc_thread_t*) erts_tsd_get(dlc_thread_key);
    
    if (!thr) {
        thr = malloc(sizeof(dlc_thread_t));
        thr->locked_now = 0;
        thr->locked_before = 0;
        thr->n_locked = 0;
    }
        
    if (thr->locked_now) {
        UWord before, after;
        ERTS_ASSERT(!(lock_bit & thr->locked_now));  /* relocking */
        ERTS_ASSERT(!(lock_bit & thr->locked_before));  /* thread lock order */
        ASSERT(thr->n_locked);

        before = erts_atomic_read_nob(&locked_before[dlc->ix]);
        after = erts_atomic_read_nob(&locked_after[dlc->ix]);

        ERTS_ASSERT(!(thr->locked_before & after)); /* lock order */
        if ((thr->locked_before | before) != before) {
            UWord new_before;
            before = erts_atomic_read_bor_nob(&locked_before[dlc->ix],
                                              thr->locked_before);            
            new_before = (before ^ thr->locked_before) & thr->locked_before;
            while (new_before) {
                int i;
                for (i = 0; i < thr->n_locked; i++) {
                    const UWord ix = thr->lock_order[i];                    
                    const UWord bit = 1 << ix;
                    ASSERT(bit & thr->locked_before);
                    if (bit & new_before) {
                        erts_atomic_read_bor_nob(&locked_after[ix], lock_bit);
                    }
                }
            }
        }       
    }
    thr->locked_now |= lock_bit;
    thr->locked_before |= lock_bit;
    thr->lock_order[thr->n_locked++] = dlc->ix;
}

void erts_dlc_unlock(erts_dlc_t* dlc)
{
    const UWord lock_bit = 1 << dlc->ix;
    dlc_thread_t *thr = (dlc_thread_t*) erts_tsd_get(dlc_thread_key);
    
    ERTS_ASSERT(thr);           
    ERTS_ASSERT(thr->locked_now & lock_bit);
    ERTS_ASSERT(thr->locked_before & lock_bit);

    thr->locked_now ^= lock_bit;

    if (thr->lock_order[thr->n_locked-1] == dlc->ix) {
        while (--thr->n_locked) {
            UWord bit = 1 << thr->lock_order[thr->n_locked-1];
            if (bit & thr->locked_now)
                break;
            ERTS_ASSERT(thr->locked_before & bit);
            thr->locked_before ^= bit;
        }
    }
}


