#define ERTS_DYN_LOCK_CHECK

typedef struct {
    unsigned ix;
} erts_dlc_t;

void erts_dlc_create_lock(erts_dlc_t* dlc, const char* name);
void erts_dlc_lock(erts_dlc_t* dlc);
void erts_dlc_unlock(erts_dlc_t* dlc);
void erts_dlc_init(void);

