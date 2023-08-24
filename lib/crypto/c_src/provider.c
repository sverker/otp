#include <openssl/params.h>
#include <openssl/provider.h>
#include <openssl/err.h>

#include "provider.h"
#include "digest.h"

static ErlNifResourceType* provider_rtype;

struct provider_resource {
    OSSL_PROVIDER *prov;
    struct provider_resource *next;  /* in prov_list */
};
static struct provider_resource *prov_list = NULL;
static ErlNifMutex *prov_list_mtx = NULL;

static void provider_dtor(ErlNifEnv* env, struct provider_resource* ctx)
{
}

int init_provider(ErlNifEnv *env)
{
    provider_rtype = enif_open_resource_type(env, NULL, "PROVIDER",
                                             (ErlNifResourceDtor*) provider_dtor,
                                             ERL_NIF_RT_CREATE|ERL_NIF_RT_TAKEOVER,
                                             NULL);
    if (provider_rtype == NULL) {
        PRINTF_ERR0("CRYPTO: Could not open resource type 'PROVIDER'");
        return 0;
    }
    if (!prov_list_mtx) {
        prov_list_mtx = enif_mutex_create("crypto.provider_list");
        if (!prov_list_mtx) {
            PRINTF_ERR0("CRYPTO: Could not create mutex 'crypto.provider_list'");
            return 0;
        }
    }

    return 1;
}
void cleanup_provider(ErlNifEnv* env)
{
    enif_mutex_destroy(prov_list_mtx);
    prov_list_mtx = NULL;
}
static ERL_NIF_TERM make_algo_list(ErlNifEnv* env, OSSL_PROVIDER *prov,
                                   int operation_id)
{
    const OSSL_ALGORITHM* algos =
        OSSL_PROVIDER_query_operation(prov, operation_id, 0);
    ERL_NIF_TERM ret = enif_make_list(env, 0);
    if (algos) {
        for ( ; algos->algorithm_names; ++algos) {
            ERL_NIF_TERM tpl = enif_make_tuple2(env,
                enif_make_string(env, algos->algorithm_names, ERL_NIF_UTF8),
                enif_make_string(env, algos->property_definition, ERL_NIF_UTF8));
            ret = enif_make_list_cell(env, tpl, ret);
        }
    }
    return ret;
}

ERL_NIF_TERM make_algo_list_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    struct provider_resource* ctx;
    int operation_id;

    if (!enif_get_resource(env, argv[0], provider_rtype, (void**)&ctx)
        || !enif_get_int(env, argv[1], &operation_id))
        return enif_make_badarg(env);

    return make_algo_list(env, ctx->prov, operation_id);
}

static
struct provider_resource* already_loaded(OSSL_PROVIDER* prov)
{
    struct provider_resource* p;

    for (p = prov_list; p; p = p->next) {
        if (p->prov == prov) {
            return p;
        }
    }
    return NULL;
}

ERL_NIF_TERM provider_load_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    OSSL_PROVIDER *prov = NULL;
    ErlNifBinary so_file_bin;
    char* so_file;
    struct provider_resource* resource;
    ERL_NIF_TERM res, res_atom, res_ref;

    if (!enif_inspect_binary(env, argv[0], &so_file_bin)
        || !zero_terminate(&so_file_bin, &so_file))
        return enif_make_badarg(env);

    prov = OSSL_PROVIDER_load(NULL, so_file);
    if (!prov) {
        enif_fprintf(stderr, "OSSL_PROVIDER_load FAILED to load '%s'\n", so_file);
        res = atom_error;
        goto done;
    }

#ifdef DEBUG
    enif_fprintf(stdout, "Provider prov=%p for '%s'\n", prov, so_file);
#endif

    enif_mutex_lock(prov_list_mtx);
    resource = already_loaded(prov);
    if (!resource) {
        resource = enif_alloc_resource(provider_rtype, sizeof(struct provider_resource));
        resource->prov = prov;
        resource->next = prov_list;
        prov_list = resource;
        res_atom = atom_loaded;
    }
    else {
        res_atom = atom_already_loaded;
    }
    enif_mutex_unlock(prov_list_mtx);

    res_ref = enif_make_resource(env, resource);
    res = enif_make_tuple2(env, res_atom, res_ref);

done:
    if (so_file)
        enif_free(so_file);
    return res;
}

ERL_NIF_TERM providers_loaded_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM res = enif_make_list(env, 0);
    struct provider_resource* p;

    enif_mutex_lock(prov_list_mtx);
    for (p = prov_list; p; p = p->next) {
        ERL_NIF_TERM name = enif_make_string(env, OSSL_PROVIDER_get0_name(p->prov),
                                             ERL_NIF_UTF8);
        ERL_NIF_TERM tpl = enif_make_tuple2(env, enif_make_resource(env, p), name);
        res = enif_make_list_cell(env, tpl, res);
    }
    enif_mutex_unlock(prov_list_mtx);
    return res;
}

ERL_NIF_TERM md_fetch_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    struct digest_type_t* get_digest_type(ERL_NIF_TERM type);
    char properties[50];
    struct digest_type_t* dt;
    EVP_MD* md;

    // ToDo: Make thread-safe

    dt = get_digest_type(argv[0]);
    if (!dt || enif_get_string(env, argv[1], properties, sizeof(properties),
                               ERL_NIF_UTF8) <= 0)
        return enif_make_badarg(env);

    md = EVP_MD_fetch(NULL, dt->str_v3, properties);
    if (!md) {
        enif_fprintf(stderr, "EVP_MD_fetch(%s,%s) FAILED\n", dt->str_v3, properties);
        return atom_error;
    }
    dt->md.p = md;
    return atom_ok;
}


int get_provider_pkey(ErlNifEnv* env, ERL_NIF_TERM map,
                      char* key_id, char* passwd, EVP_PKEY **pkey_p, int priv)
{
    ERL_NIF_TERM provider_val, key_type_val;
    ErlNifBinary provider_bin, key_type_bin;
    char* propquery = NULL;
    char* key_type = NULL;
    EVP_PKEY_CTX *ctx = NULL;
    OSSL_PARAM params[3];
    int selection, ix = 0;
    int ret = 0;

    if (!enif_get_map_value(env, map, atom_provider, &provider_val)
        || !enif_inspect_binary(env, provider_val, &provider_bin)
        || !zero_terminate(&provider_bin, &propquery)) {
        goto err;
    }
    if (!enif_get_map_value(env, map, atom_key_type, &key_type_val)
        || !enif_inspect_binary(env, key_type_val, &key_type_bin)
        || !zero_terminate(&key_type_bin, &key_type)) {
        goto err;
    }
   
    ctx = EVP_PKEY_CTX_new_from_name(NULL, key_type, propquery);
    if (!ctx) {
        enif_fprintf(stderr, "GOT NO CTX from name\n");
        goto err;
    }
    //enif_fprintf(stderr, "call fromdata_init\n");
    if (EVP_PKEY_fromdata_init(ctx) <= 0) {
        enif_fprintf(stderr, "fromdata_init FAILED\n");
        goto err;
    }

    params[ix++] = OSSL_PARAM_construct_utf8_string("id", key_id, 0);
    if (passwd)
        params[ix++] = OSSL_PARAM_construct_utf8_string("passwd", passwd, 0);
    params[ix++] = OSSL_PARAM_construct_end();
    selection = priv ? EVP_PKEY_KEYPAIR : EVP_PKEY_PUBLIC_KEY;
    //enif_fprintf(stderr, "call fromdata\n");
    if (EVP_PKEY_fromdata(ctx, pkey_p, selection, params) <= 0) {
        enif_fprintf(stderr, "EVP_PKEY_fromdata FAILED\n");
        goto err;
    }
    ret = 1; /* ok */
err:
    EVP_PKEY_CTX_free(ctx);
    if (propquery)
        enif_free(propquery);
    if (key_type)
        enif_free(key_type);
    return ret;
}

