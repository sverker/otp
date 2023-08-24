/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2017-2022. All Rights Reserved.
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

#ifdef _WIN32
#define OPENSSL_OPT_WINDLL
#endif

#include <stdio.h>
#include <string.h>
#include <limits.h>
#include <stdint.h>
#include <malloc.h>
#include <assert.h>
#include <openssl/core.h>
#include <openssl/core_dispatch.h>
#include <openssl/md5.h>
#include <openssl/params.h>
#include <openssl/evp.h>
#include <openssl/pem.h>

#ifdef DEBUG
#  define DBG_TRACE(FUNC) fprintf(stderr, "DBG_TRACE:%d in %s\r\n", __LINE__, #FUNC)
#else
#  define DBG_TRACE(FUNC)
#endif

/*
 * To ensure we get the function signature right, forward declare
 * them using function types provided by openssl/core_dispatch.h
 */
static OSSL_FUNC_digest_digest_fn     digest_md5_digest;
static OSSL_FUNC_digest_newctx_fn     digest_md5_newctx;
static OSSL_FUNC_digest_freectx_fn    digest_md5_freectx;
static OSSL_FUNC_digest_dupctx_fn     digest_md5_dupctx;
static OSSL_FUNC_digest_init_fn       digest_md5_init;
static OSSL_FUNC_digest_update_fn     digest_md5_update;
static OSSL_FUNC_digest_final_fn      digest_md5_final;
static OSSL_FUNC_digest_get_params_fn digest_md5_get_params;

static void add_test_data(unsigned char *md, unsigned int len)
{
    unsigned int i;

    for (i=0; i<len; i++) {
        md[i] = (unsigned char)(i & 0xff);
    }
}

static int digest_md5_get_params(OSSL_PARAM params[])
{
    int i;
    for (i=0; params[i].key; i++) {
//      if (strcmp(params[i].key, "blocksize") == 0) {
//          if (params[i].data_type != OSSL_PARAM_UNSIGNED_INTEGER ||
//              params[i].data_size < sizeof(size_t))
//              return 0;
//          *(size_t*)params[i].data = MD5_CBLOCK;
//          params[i].return_size = sizeof(size_t);
//      }
        if (strcmp(params[i].key, "size") == 0) {
            if (params[i].data_type != OSSL_PARAM_UNSIGNED_INTEGER ||
                params[i].data_size < sizeof(size_t))
                return 0;
            *(size_t*)params[i].data = MD5_DIGEST_LENGTH;
            params[i].return_size = sizeof(size_t);
        }
    }
    return 1;
}
static void *digest_md5_newctx(void *provctx)
{
    return &digest_md5_newctx; /* a dummy non-null context pointer */
}
static void digest_md5_freectx(void *dctx)
{
    assert(dctx == &digest_md5_newctx);
}
static void *digest_md5_dupctx(void *provctx)
{
    return &digest_md5_newctx;
}
static int digest_md5_init(void *dctx, const OSSL_PARAM params[])
{
    assert(dctx == &digest_md5_newctx);
    return 1;
}
static int digest_md5_update(void *dctx, const unsigned char *in, size_t inl)
{
    assert(dctx == &digest_md5_newctx);
    return 1;
}
static int digest_md5_final(void *dctx, unsigned char *out, size_t *outl,
                            size_t outsz)
{
    return digest_md5_digest(NULL, NULL, 0, out, outl, outsz);
}
static int digest_md5_digest(void *provctx,
                             const unsigned char *in, size_t inl,
                             unsigned char *out, size_t *outl, size_t outsz)
{
    if (outsz < MD5_DIGEST_LENGTH)
        return 0;
    add_test_data(out, MD5_DIGEST_LENGTH);
    *outl = MD5_DIGEST_LENGTH;
    return 1;
}

/* Helper typedef for leaner type casts to generic void function pointer */
typedef void VF(void);

static const OSSL_DISPATCH digest_md5_fns[] = {
    { OSSL_FUNC_DIGEST_GET_PARAMS, (VF*) digest_md5_get_params},
    { OSSL_FUNC_DIGEST_NEWCTX,     (VF*) digest_md5_newctx},
    { OSSL_FUNC_DIGEST_FREECTX,    (VF*) digest_md5_freectx},
    { OSSL_FUNC_DIGEST_DUPCTX,     (VF*) digest_md5_dupctx},
    { OSSL_FUNC_DIGEST_INIT,       (VF*) digest_md5_init},
    { OSSL_FUNC_DIGEST_UPDATE,     (VF*) digest_md5_update},
    { OSSL_FUNC_DIGEST_FINAL,      (VF*) digest_md5_final},
    { OSSL_FUNC_DIGEST_DIGEST,     (VF*) digest_md5_digest},
    { 0, NULL }
};

/***************************** KEYMGMT **************************************/

static OSSL_FUNC_keymgmt_new_fn                 test_km_newdata;
static OSSL_FUNC_keymgmt_free_fn                test_km_freedata;
static OSSL_FUNC_keymgmt_has_fn                 test_km_has;
static OSSL_FUNC_keymgmt_import_fn              test_km_import;
static OSSL_FUNC_keymgmt_import_types_fn        test_km_import_types;

#ifdef DEBUG
static void DBG_PRINT_PARAMS(const OSSL_PARAM params[])
{
    for (int i=0; params[i].key; i++) {
        fprintf(stderr, "params[%d].key = %s\r\n", i, params[i].key);
    }
}
#else
# define DBG_PRINT_PARAMS(P)
#endif


struct my_keydata_t
{
    const char* struct_type;
    EVP_PKEY* pkey;
};

static const char my_keydata_t_string[] = "struct my_keydata_t";

static void *test_km_newdata(void *provctx)
{
    DBG_TRACE(test_km_newdata);
    {
        struct my_keydata_t* keydata = malloc(sizeof(struct my_keydata_t));
        keydata->struct_type = my_keydata_t_string;
        keydata->pkey = NULL;
        return keydata;
    }
}
static void test_km_freedata(void* vkeydata)
{
    DBG_TRACE(test_km_freedata);
    {
        struct my_keydata_t* keydata = (struct my_keydata_t*) vkeydata;
        assert(keydata->struct_type == my_keydata_t_string);
        if (keydata->pkey)
            EVP_PKEY_free(keydata->pkey);
        free(keydata);
    }
}
static int test_km_has(const void *keydata, int selection)
{
    DBG_TRACE(test_km_has);
    return 1;
}
static int pem_passwd_cb_fun(char *buf, int size, int rwflag, void *password)
{
    size_t i;
    DBG_TRACE(pem_passwd_cb_fun);

    if (size < 0)
        return 0;

    if (!password)
        return 0;

    i = strlen(password);
    if (i >= (size_t)size || i > INT_MAX - 1)
        goto err;

    /* whole pwd (incl terminating 0) fits */
    memcpy(buf, (char*)password, i+1);
    return (int)i+1;

 err:
    fprintf(stderr, "Got TO LONG pwd %zu(%d) chars\r\n", i, size);
    /* meaningless with a truncated password */
    return 0;
}

static int test_km_import(void *vkeydata, int selection, const OSSL_PARAM params[])
{
    struct my_keydata_t* keydata = (struct my_keydata_t*) vkeydata;
    const char* id = NULL;
    const char* passwd = NULL;
    FILE *f;

    DBG_TRACE(test_km_import);
    assert(keydata->struct_type == my_keydata_t_string);
    DBG_PRINT_PARAMS(params);

    if (!keydata)
        return 0;

    for (int i=0; params[i].key; i++) {
        if (strcmp(params[i].key, "id") == 0) {
            if (params[i].data_type != OSSL_PARAM_UTF8_STRING)
                return 0;
            id = (char*)params[i].data;
        }
        if (strcmp(params[i].key, "passwd") == 0) {
            if (params[i].data_type != OSSL_PARAM_UTF8_STRING)
                return 0;
            passwd = (char*)params[i].data;
        }
    }
    if (!id)
        return 0;

    f = fopen(id, "r");
    if (!f) {
        fprintf(stderr, "%s:%d fopen(%s) failed\r\n", __FILE__,__LINE__,id);
        return 0;
    }
    if (keydata->pkey)
        EVP_PKEY_free(keydata->pkey);

    keydata->pkey = (selection & OSSL_KEYMGMT_SELECT_PRIVATE_KEY)
        ? PEM_read_PrivateKey(f, NULL, pem_passwd_cb_fun, (void*)passwd)
        : PEM_read_PUBKEY(f, NULL, NULL, NULL);

    fclose(f);

    if (!keydata->pkey) {
        fprintf(stderr, "%s:%d Key read from file %s failed.\r\n", __FILE__,__LINE__,id);
        if (passwd)
            fprintf(stderr, "Pwd = \"%s\".\r\n", passwd);
        return 0;
    }

    return 1;
}

static const OSSL_PARAM *test_km_import_types(int selection)
{
    DBG_TRACE(test_km_import_types);
    return NULL;
}

const OSSL_DISPATCH test_keymgmt_fns[] = {
    { OSSL_FUNC_KEYMGMT_NEW,                 (VF*) test_km_newdata },
    { OSSL_FUNC_KEYMGMT_FREE,                (VF*) test_km_freedata },
    { OSSL_FUNC_KEYMGMT_HAS,                 (VF*) test_km_has },
    { OSSL_FUNC_KEYMGMT_IMPORT,              (VF*) test_km_import },
    { OSSL_FUNC_KEYMGMT_IMPORT_TYPES,        (VF*) test_km_import_types },
    { 0, NULL }
};


/**************************** SIGNATURE **********************************/

static OSSL_FUNC_signature_newctx_fn               test_sign_newctx;
static OSSL_FUNC_signature_freectx_fn              test_sign_freectx;
static OSSL_FUNC_signature_sign_init_fn            test_sign_sign_init;
static OSSL_FUNC_signature_sign_fn                 test_sign_sign;
static OSSL_FUNC_signature_verify_init_fn          test_sign_verify_init;
static OSSL_FUNC_signature_verify_fn               test_sign_verify;
static OSSL_FUNC_signature_verify_recover_init_fn  test_sign_verify_recover_init;
static OSSL_FUNC_signature_verify_recover_fn       test_sign_verify_recover;
static OSSL_FUNC_signature_digest_sign_init_fn     test_sign_digest_sign_init;
static OSSL_FUNC_signature_digest_sign_fn          test_sign_digest_sign;
static OSSL_FUNC_signature_digest_verify_init_fn   test_sign_digest_verify_init;
static OSSL_FUNC_signature_digest_verify_fn        test_sign_digest_verify;
static OSSL_FUNC_signature_set_ctx_params_fn       test_sign_set_ctx_params;
static OSSL_FUNC_signature_settable_ctx_params_fn  test_sign_settable_ctx_params;

struct my_sign_ctx_t
{
    const char* struct_type;
    struct my_keydata_t* keydata;
    EVP_PKEY_CTX* pkey_ctx;
    EVP_MD_CTX* md_ctx;
};

static const char my_sign_ctx_t_string[] = "struct my_sign_ctx_t";

static void *test_sign_newctx(void *provctx, const char *propq)
{
    DBG_TRACE(test_sign_newctx);
    {
        struct my_sign_ctx_t *ctx =
            (struct my_sign_ctx_t*) malloc(sizeof(struct my_sign_ctx_t));
        ctx->struct_type = my_sign_ctx_t_string;
        ctx->keydata = NULL;
        ctx->pkey_ctx = NULL;
        ctx->md_ctx = NULL;
        return ctx;
    }
}

static void test_sign_freectx(void *v_ctx)
{
    DBG_TRACE(test_sign_freectx);
    {
        struct my_sign_ctx_t *ctx = (struct my_sign_ctx_t*) v_ctx;
        assert(ctx->struct_type == my_sign_ctx_t_string);
        EVP_PKEY_CTX_free(ctx->pkey_ctx);
        if (ctx->md_ctx)
            EVP_MD_CTX_free(ctx->md_ctx);
        free(ctx);
    }
}

static int test_sign_sign_init(void *v_ctx, void *provkey,
                               const OSSL_PARAM params[])
{
    DBG_TRACE(test_sign_sign_init);
    {
        struct my_sign_ctx_t *ctx = (struct my_sign_ctx_t*) v_ctx;
        assert(ctx->struct_type == my_sign_ctx_t_string);
        assert(ctx->keydata == NULL);

        ctx->keydata = (struct my_keydata_t*) provkey;
        assert(ctx->keydata->struct_type == my_keydata_t_string);

        ctx->pkey_ctx = EVP_PKEY_CTX_new(ctx->keydata->pkey, NULL);
        return EVP_PKEY_sign_init(ctx->pkey_ctx);
    }
}

static int test_sign_sign(void *v_ctx,
                          unsigned char *sig, size_t *siglen, size_t sigsize,
                          const unsigned char *tbs, size_t tbslen)
{
    DBG_TRACE(test_sign_sign);
    {
        struct my_sign_ctx_t *ctx = (struct my_sign_ctx_t*) v_ctx;
        *siglen = sigsize;
        return EVP_PKEY_sign(ctx->pkey_ctx, sig, siglen, tbs, tbslen);
    }
}

static int test_sign_verify_init(void *v_ctx, void *provkey,
                                 const OSSL_PARAM params[])
{
    DBG_TRACE(test_sign_verify_init);
    {
        struct my_sign_ctx_t *ctx = (struct my_sign_ctx_t*) v_ctx;
        assert(ctx->struct_type == my_sign_ctx_t_string);
        assert(ctx->keydata == NULL);

        ctx->keydata = (struct my_keydata_t*) provkey;
        assert(ctx->keydata->struct_type == my_keydata_t_string);

        ctx->pkey_ctx = EVP_PKEY_CTX_new(ctx->keydata->pkey, NULL);
        return EVP_PKEY_verify_init(ctx->pkey_ctx);
    }
}

static int test_sign_verify(void *v_ctx,
                            const unsigned char *sig, size_t siglen,
                            const unsigned char *tbs, size_t tbslen)
{
    DBG_TRACE(test_sign_verify);
    {
        struct my_sign_ctx_t *ctx = (struct my_sign_ctx_t*) v_ctx;
        return EVP_PKEY_verify(ctx->pkey_ctx, sig, siglen, tbs, tbslen);
    }
}
static int test_sign_verify_recover_init(void *v_ctx, void *provkey,
                                         const OSSL_PARAM params[])
{
    DBG_TRACE(test_sign_verify_recover_init);
    {
        struct my_sign_ctx_t *ctx = (struct my_sign_ctx_t*) v_ctx;
        assert(ctx->struct_type == my_sign_ctx_t_string);

        assert(ctx->keydata == NULL);
        ctx->keydata = (struct my_keydata_t*) provkey;
        assert(ctx->keydata->struct_type == my_keydata_t_string);

        ctx->pkey_ctx = EVP_PKEY_CTX_new(ctx->keydata->pkey, NULL);

        return EVP_PKEY_verify_recover_init(ctx->pkey_ctx);
    }
}
static int test_sign_verify_recover(void *v_ctx, unsigned char *rout,
                                    size_t *routlen, size_t routsize,
                                    const unsigned char *sig, size_t siglen)
{
    DBG_TRACE(test_sign_verify_recover);
    {
        struct my_sign_ctx_t *ctx = (struct my_sign_ctx_t*) v_ctx;
        assert(ctx->struct_type == my_sign_ctx_t_string);
        *routlen = routsize;
        return EVP_PKEY_verify_recover(ctx->pkey_ctx, rout, routlen,
                                       sig, siglen);
    }
}

static int test_sign_digest_sign_init(void *v_ctx, const char *mdname,
                               void *provkey, const OSSL_PARAM params[])
{
    DBG_TRACE(test_sign_digest_sign_init);
    {
        struct my_sign_ctx_t *ctx = (struct my_sign_ctx_t*) v_ctx;
        assert(ctx->struct_type == my_sign_ctx_t_string);
        assert(ctx->keydata == NULL);

        ctx->keydata = (struct my_keydata_t*) provkey;
        assert(ctx->keydata->struct_type == my_keydata_t_string);

        //ctx->pkey_ctx = EVP_PKEY_CTX_new(ctx->keydata->pkey, NULL);
        ctx->md_ctx = EVP_MD_CTX_new();

        return EVP_DigestSignInit_ex(ctx->md_ctx, NULL,
                          mdname, NULL, NULL, ctx->keydata->pkey,
                          params);

    }
}
static int test_sign_digest_sign(void *v_ctx,
                                 unsigned char *sigret, size_t *siglen,
                                 size_t sigsize, const unsigned char *tbs,
                                 size_t tbslen)
{
    DBG_TRACE(test_sign_digest_sign);
    {
        struct my_sign_ctx_t *ctx = (struct my_sign_ctx_t*) v_ctx;
        assert(ctx->struct_type == my_sign_ctx_t_string);

        *siglen = sigsize;
        return EVP_DigestSign(ctx->md_ctx, sigret, siglen, tbs, tbslen);
    }
}

static int test_sign_digest_verify_init(void *v_ctx, const char *mdname,
                                        void *provkey, const OSSL_PARAM params[])
{
    DBG_TRACE(test_sign_digest_verify_init);
    {
        struct my_sign_ctx_t *ctx = (struct my_sign_ctx_t*) v_ctx;
        assert(ctx->struct_type == my_sign_ctx_t_string);
        assert(ctx->keydata == NULL);

        ctx->keydata = (struct my_keydata_t*) provkey;
        assert(ctx->keydata->struct_type == my_keydata_t_string);

        //ctx->pkey_ctx = EVP_PKEY_CTX_new(ctx->keydata->pkey, NULL);
        ctx->md_ctx = EVP_MD_CTX_new();

        return EVP_DigestVerifyInit_ex(ctx->md_ctx, NULL,
                                       mdname, NULL, NULL, ctx->keydata->pkey,
                                       params);

    }
}
static int test_sign_digest_verify(void *v_ctx,
                                   const unsigned char *sig, size_t siglen,
                                   const unsigned char *tbs, size_t tbslen)
{
    DBG_TRACE(test_sign_digest_verify);
    {
        struct my_sign_ctx_t *ctx = (struct my_sign_ctx_t*) v_ctx;
        assert(ctx->struct_type == my_sign_ctx_t_string);

        return EVP_DigestVerify(ctx->md_ctx, sig, siglen, tbs, tbslen);
    }
}

static int test_sign_set_ctx_params(void *v_ctx, const OSSL_PARAM params[])
{
    DBG_TRACE(test_sign_set_ctx_params);
    DBG_PRINT_PARAMS(params);
    {
        struct my_sign_ctx_t *ctx = (struct my_sign_ctx_t*) v_ctx;
        assert(ctx->struct_type == my_sign_ctx_t_string);
        return EVP_PKEY_CTX_set_params(ctx->pkey_ctx, params);
    }
}
static
const OSSL_PARAM* test_sign_settable_ctx_params(void *v_ctx, void* provctx)
{
    DBG_TRACE(test_sign_settable_ctx_params);
    {
        struct my_sign_ctx_t *ctx = (struct my_sign_ctx_t*) v_ctx;
        assert(ctx->struct_type == my_sign_ctx_t_string);
        return EVP_PKEY_CTX_settable_params(ctx->pkey_ctx);
    }
}

static const OSSL_DISPATCH test_signature_fns[] = {
    { OSSL_FUNC_SIGNATURE_NEWCTX,              (VF*) test_sign_newctx },
    { OSSL_FUNC_SIGNATURE_FREECTX,             (VF*) test_sign_freectx },
    { OSSL_FUNC_SIGNATURE_SIGN_INIT,           (VF*) test_sign_sign_init },
    { OSSL_FUNC_SIGNATURE_SIGN,                (VF*) test_sign_sign },
    { OSSL_FUNC_SIGNATURE_VERIFY_INIT,         (VF*) test_sign_verify_init },
    { OSSL_FUNC_SIGNATURE_VERIFY,              (VF*) test_sign_verify },
    { OSSL_FUNC_SIGNATURE_VERIFY_RECOVER_INIT, (VF*) test_sign_verify_recover_init },
    { OSSL_FUNC_SIGNATURE_VERIFY_RECOVER,      (VF*) test_sign_verify_recover },
    { OSSL_FUNC_SIGNATURE_DIGEST_SIGN_INIT,    (VF*) test_sign_digest_sign_init },
    { OSSL_FUNC_SIGNATURE_DIGEST_SIGN,         (VF*) test_sign_digest_sign },
    { OSSL_FUNC_SIGNATURE_DIGEST_VERIFY_INIT,  (VF*) test_sign_digest_verify_init },
    { OSSL_FUNC_SIGNATURE_DIGEST_VERIFY,       (VF*) test_sign_digest_verify },
    { OSSL_FUNC_SIGNATURE_SET_CTX_PARAMS,      (VF*) test_sign_set_ctx_params },
    { OSSL_FUNC_SIGNATURE_SETTABLE_CTX_PARAMS, (VF*) test_sign_settable_ctx_params },
    { 0, NULL}
};

/***************************** ASYM_CIPHER *******************************/

static OSSL_FUNC_asym_cipher_newctx_fn               test_aciph_newctx;
static OSSL_FUNC_asym_cipher_freectx_fn              test_aciph_freectx;
static OSSL_FUNC_asym_cipher_encrypt_init_fn         test_aciph_encrypt_init;
static OSSL_FUNC_asym_cipher_encrypt_fn              test_aciph_encrypt;
static OSSL_FUNC_asym_cipher_decrypt_init_fn         test_aciph_decrypt_init;
static OSSL_FUNC_asym_cipher_decrypt_fn              test_aciph_decrypt;
static OSSL_FUNC_asym_cipher_set_ctx_params_fn       test_aciph_set_ctx_params;
static OSSL_FUNC_asym_cipher_settable_ctx_params_fn  test_aciph_settable_ctx_params;

struct my_aciph_ctx_t
{
    const char* struct_type;
    struct my_keydata_t* keydata;
    EVP_PKEY_CTX* pkey_ctx;
};

static const char my_aciph_ctx_t_string[] = "struct my_aciph_ctx_t";

static void *test_aciph_newctx(void *provctx)
{
    DBG_TRACE(test_aciph_newctx);
    {
        struct my_aciph_ctx_t *ctx =
            (struct my_aciph_ctx_t*) malloc(sizeof(struct my_aciph_ctx_t));
        ctx->struct_type = my_aciph_ctx_t_string;
        ctx->keydata = NULL;
        ctx->pkey_ctx = NULL;
        return ctx;
    }
}

static void test_aciph_freectx(void *v_ctx)
{
    DBG_TRACE(test_aciph_freectx);
    {
        struct my_aciph_ctx_t *ctx = (struct my_aciph_ctx_t*) v_ctx;
        assert(ctx->struct_type == my_aciph_ctx_t_string);
        EVP_PKEY_CTX_free(ctx->pkey_ctx);
        free(ctx);
    }
}

static int test_aciph_encrypt_init(void *v_ctx, void *provkey,
                                   const OSSL_PARAM params[])
{
    DBG_TRACE(test_aciph_encrypt_init);
    {
        struct my_aciph_ctx_t *ctx = (struct my_aciph_ctx_t*) v_ctx;
        assert(ctx->struct_type == my_aciph_ctx_t_string);
        assert(ctx->keydata == NULL);

        ctx->keydata = (struct my_keydata_t*) provkey;
        assert(ctx->keydata->struct_type == my_keydata_t_string);

        ctx->pkey_ctx = EVP_PKEY_CTX_new(ctx->keydata->pkey, NULL);
        return EVP_PKEY_encrypt_init(ctx->pkey_ctx);
    }
}

static int test_aciph_encrypt(void *v_ctx,
                              unsigned char *out, size_t *outlen, size_t outsize,
                              const unsigned char *in, size_t inlen)
{
    DBG_TRACE(test_aciph_encrypt);
    {
        struct my_aciph_ctx_t *ctx = (struct my_aciph_ctx_t*) v_ctx;
        *outlen = outsize;
        return EVP_PKEY_encrypt(ctx->pkey_ctx, out, outlen, in, inlen);
    }
}

static int test_aciph_decrypt_init(void *v_ctx, void *provkey,
                                   const OSSL_PARAM params[])
{
    DBG_TRACE(test_aciph_decrypt_init);
    {
        struct my_aciph_ctx_t *ctx = (struct my_aciph_ctx_t*) v_ctx;
        assert(ctx->struct_type == my_aciph_ctx_t_string);
        assert(ctx->keydata == NULL);

        ctx->keydata = (struct my_keydata_t*) provkey;
        assert(ctx->keydata->struct_type == my_keydata_t_string);

        ctx->pkey_ctx = EVP_PKEY_CTX_new(ctx->keydata->pkey, NULL);
        return EVP_PKEY_decrypt_init(ctx->pkey_ctx);
    }
}

static int test_aciph_decrypt(void *v_ctx,
                              unsigned char *out, size_t *outlen, size_t outsize,
                              const unsigned char *in, size_t inlen)
{
    DBG_TRACE(test_aciph_encrypt);
    {
        struct my_aciph_ctx_t *ctx = (struct my_aciph_ctx_t*) v_ctx;
        *outlen = outsize;
        return EVP_PKEY_decrypt(ctx->pkey_ctx, out, outlen, in, inlen);
    }
}


static int test_aciph_set_ctx_params(void *v_ctx, const OSSL_PARAM params[])
{
    DBG_TRACE(test_aciph_set_ctx_params);
    DBG_PRINT_PARAMS(params);
    {
        struct my_aciph_ctx_t *ctx = (struct my_aciph_ctx_t*) v_ctx;
        assert(ctx->struct_type == my_aciph_ctx_t_string);
        return EVP_PKEY_CTX_set_params(ctx->pkey_ctx, params);
    }
}
static
const OSSL_PARAM* test_aciph_settable_ctx_params(void *v_ctx, void* provctx)
{
    DBG_TRACE(test_aciph_settable_ctx_params);
    {
        struct my_aciph_ctx_t *ctx = (struct my_aciph_ctx_t*) v_ctx;
        assert(ctx->struct_type == my_aciph_ctx_t_string);
        return EVP_PKEY_CTX_settable_params(ctx->pkey_ctx);
    }
}

static const OSSL_DISPATCH test_aciph_fns[] = {
    { OSSL_FUNC_ASYM_CIPHER_NEWCTX,              (VF*) test_aciph_newctx },
    { OSSL_FUNC_ASYM_CIPHER_FREECTX,             (VF*) test_aciph_freectx },
    { OSSL_FUNC_ASYM_CIPHER_ENCRYPT_INIT,        (VF*) test_aciph_encrypt_init },
    { OSSL_FUNC_ASYM_CIPHER_ENCRYPT,             (VF*) test_aciph_encrypt },
    { OSSL_FUNC_ASYM_CIPHER_DECRYPT_INIT,        (VF*) test_aciph_decrypt_init },
    { OSSL_FUNC_ASYM_CIPHER_DECRYPT,             (VF*) test_aciph_decrypt },
    { OSSL_FUNC_ASYM_CIPHER_SET_CTX_PARAMS,      (VF*) test_aciph_set_ctx_params },
    { OSSL_FUNC_ASYM_CIPHER_SETTABLE_CTX_PARAMS, (VF*) test_aciph_settable_ctx_params },
    { 0, NULL}
};


/**************************************************************************/

static OSSL_FUNC_provider_query_operation_fn    p_query;
static OSSL_FUNC_provider_teardown_fn           p_teardown;

OSSL_provider_init_fn OSSL_provider_init;

OSSL_FUNC_core_vset_error_fn *c_vset_error = NULL;

/* Provider context */
struct prov_ctx_st {
    const OSSL_CORE_HANDLE *handle;
};

static const OSSL_ALGORITHM digests[] = {
    { "MD5", "provider=otp_test_provider", digest_md5_fns },
    { NULL, NULL, NULL }
};
static const OSSL_ALGORITHM test_keymgmt[] = {
    { "RSA", "provider=otp_test_provider", test_keymgmt_fns},
    { "DSA", "provider=otp_test_provider", test_keymgmt_fns},
    { NULL, NULL, NULL }
};
static const OSSL_ALGORITHM test_signature[] = {
    { "RSA", "provider=otp_test_provider", test_signature_fns},
    { "DSA", "provider=otp_test_provider", test_signature_fns},
    { NULL, NULL, NULL }
};
static const OSSL_ALGORITHM test_aciph[] = {
    { "RSA", "provider=otp_test_provider", test_aciph_fns},
    { "DSA", "provider=otp_test_provider", test_aciph_fns},
    { NULL, NULL, NULL }
};

static const OSSL_ALGORITHM *p_query(void *provctx,
                                     int operation_id,
                                     int *no_store)
{
#ifdef DEBUG
    static int first_call = 1;
    static const char* op_str[OSSL_OP__HIGHEST+1];
    if (first_call) {
        first_call = 0;
        for (int i=0; i <= OSSL_OP__HIGHEST; i++)
            op_str[i] = "UNKNOWN";
#define DBG_INIT_OP(OP) op_str[OP] = #OP
        DBG_INIT_OP(OSSL_OP_DIGEST);
        DBG_INIT_OP(OSSL_OP_CIPHER);
        DBG_INIT_OP(OSSL_OP_MAC);
        DBG_INIT_OP(OSSL_OP_KDF);
        DBG_INIT_OP(OSSL_OP_RAND);
        DBG_INIT_OP(OSSL_OP_KEYMGMT);
        DBG_INIT_OP(OSSL_OP_KEYEXCH);
        DBG_INIT_OP(OSSL_OP_SIGNATURE);
        DBG_INIT_OP(OSSL_OP_ASYM_CIPHER);
        DBG_INIT_OP(OSSL_OP_KEM);
        DBG_INIT_OP(OSSL_OP_ENCODER);
        DBG_INIT_OP(OSSL_OP_DECODER);
        DBG_INIT_OP(OSSL_OP_STORE);
#undef DBG_INIT_OP
    }
    fprintf(stderr, "DBG_TRACE:%u in p_query(%s(%d))\r\n", __LINE__,
            op_str[operation_id], operation_id);
#endif

    switch (operation_id) {
    case OSSL_OP_DIGEST:
        return digests;
    case OSSL_OP_KEYMGMT:
        return test_keymgmt;
    case OSSL_OP_SIGNATURE:
        return test_signature;
    case OSSL_OP_ASYM_CIPHER:
        return test_aciph;
    }
    return NULL;
}

static void p_teardown(void *provctx)
{
}

static int p_get_params(void *provctx, OSSL_PARAM params[])
{
    int i;
    for (i=0; params[i].key; i++) {
        if (strcmp(params[i].key, "buildinfo") == 0) {
            if (params[i].data_type != OSSL_PARAM_UTF8_PTR)
                return 0;
            *(const char**)params[i].data = __DATE__;
            params[i].return_size = strlen(__DATE__);
        }
    }
    return 1;
}

static const OSSL_DISPATCH prov_fns[] = {
    { OSSL_FUNC_PROVIDER_TEARDOWN,           (VF*) p_teardown },
    { OSSL_FUNC_PROVIDER_QUERY_OPERATION,    (VF*) p_query },
    { OSSL_FUNC_PROVIDER_GET_PARAMS,         (VF*) p_get_params },
    { 0, NULL }
};

int OSSL_provider_init(const OSSL_CORE_HANDLE *handle,
                       const OSSL_DISPATCH *in,
                       const OSSL_DISPATCH **out,
                       void **provctx)
{
    static struct prov_ctx_st pctx;

#ifdef DEBUG
    fprintf(stderr, "OSSL_provider_init at %p,%p called for otp_test_provider\r\n",
            &OSSL_provider_init, &pctx);
#endif

    *out = prov_fns;

    pctx.handle = handle;
    *provctx = &pctx;
    return 1;
}
