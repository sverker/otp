/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2018. All Rights Reserved.
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

/*
 * Purpose:  Dynamically loadable NIF library for high performance counters.
 */

#include "config.h"

#include <stddef.h> /* offsetof */

#include <erl_nif.h>

#include <ethread.h>

#if SIZEOF_VOID_P != 8
#  error "Not supported architecture"
#endif


#ifdef DEBUG
#  include <stdio.h>
#  define ASSERT(e) \
    ((void) ((e) ? 1 : (fprintf(stderr,"Assert '%s' failed at %s:%d\n",\
				#e, __FILE__, __LINE__), abort(), 0)))
#else
#  define ASSERT(e) ((void) 1)
#endif

#ifdef __GNUC__
#  define INLINE __inline__
#elif defined(__WIN32__)
#  define INLINE __forceinline
#else
#  define INLINE
#endif

#if 0
# define PRINTF_ERR0(FMT) enif_fprintf(stderr, FMT "\n")
# define PRINTF_ERR1(FMT, A1) enif_fprintf(stderr, FMT "\n", A1)
# define PRINTF_ERR2(FMT, A1, A2) enif_fprintf(stderr, FMT "\n", A1, A2)
#else
# define PRINTF_ERR0(FMT)
# define PRINTF_ERR1(FMT,A1)
# define PRINTF_ERR2(FMT,A1,A2)
#endif

/* NIF interface declarations */
static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info);
static int upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data, ERL_NIF_TERM load_info);
static void unload(ErlNifEnv* env, void* priv_data);

/* The NIFs: */
static ERL_NIF_TERM new_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM put_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM get_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM add_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM add_get_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

static int library_refc = 0; /* number of users of this dynamic library */

static ErlNifFunc nif_funcs[] = {
    {"new", 2, new_nif},
    {"put", 3, put_nif},
    {"get", 2, get_nif},
    {"add", 3, add_nif},
    {"add_get", 3, add_get_nif}
};

ERL_NIF_INIT(counters,nif_funcs,load,NULL,upgrade,unload)

static ERL_NIF_TERM atom_ok;
static ERL_NIF_TERM atom_enomem;


static ErlNifResourceType* counters_ref_rt;


static int initialize(ErlNifEnv* env, ERL_NIF_TERM load_info)
{
    counters_ref_rt = enif_open_resource_type(env, NULL, "counters_ref",
                                              NULL,
                                              ERL_NIF_RT_CREATE|ERL_NIF_RT_TAKEOVER,
                                              NULL);
    if (!counters_ref_rt) {
	PRINTF_ERR0("CRYPTO: Could not open resource type 'counters_ref'");
	return __LINE__;
    }

    if (library_refc > 0) {
	/* Repeated loading of this library (module upgrade).
	 * Atoms and callbacks are already set, we are done.
	 */
	return 0;
    }

    atom_ok = enif_make_atom(env,"ok");
    atom_enomem = enif_make_atom(env,"enomem");

    return 0;
}

static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    int errline = initialize(env, load_info);
    if (errline) {
	return errline;
    }

    *priv_data = NULL;
    library_refc++;
    return 0;
}

static int upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data,
		   ERL_NIF_TERM load_info)
{
    int errline;
    if (*old_priv_data != NULL) {
	return __LINE__; /* Don't know how to do that */
    }
    if (*priv_data != NULL) {
	return __LINE__; /* Don't know how to do that */
    }
    errline = initialize(env, load_info);
    if (errline) {
	return errline;
    }
    library_refc++;
    return 0;
}

static void unload(ErlNifEnv* env, void* priv_data)
{
    --library_refc;
}


typedef struct
{
    unsigned vlen;
    ethr_atomic_t v[1];
}CountersRef;

static ERL_NIF_TERM new_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CountersRef* p;
    unsigned i, cnt;
    ERL_NIF_TERM ref;

    if (!enif_get_uint(env, argv[0], &cnt)
        || cnt == 0
        || !enif_is_empty_list(env, argv[1])) {
        return enif_make_badarg(env);
    }
    p = enif_alloc_resource(counters_ref_rt,
                            offsetof(CountersRef, v) + cnt*sizeof(p->v[0]));
    if (!p)
        return enif_raise_exception(env, atom_enomem);

    p->vlen = cnt;
    for (i=0; i < cnt; i++) {
        ethr_atomic_init(&p->v[i], 0);
    }
    ref = enif_make_resource(env, p);
    enif_release_resource(p);
    return ref;
}

static INLINE int get_ref_ix(ErlNifEnv* env, const ERL_NIF_TERM argv[],
                             CountersRef** pp, unsigned* ixp)
{
    return enif_get_resource(env, argv[0], counters_ref_rt, (void**)pp)
        && enif_get_uint(env, argv[1], ixp)
        && *ixp < (*pp)->vlen;
}

static ERL_NIF_TERM put_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CountersRef* p;
    unsigned ix;
    ErlNifSInt64 val;

    if (!get_ref_ix(env, argv, &p, &ix)
        || !enif_get_int64(env, argv[2], &val)) {
        return enif_make_badarg(env);
    }

    ethr_atomic_set_mb(&p->v[ix], val);
    return atom_ok;
}

static ERL_NIF_TERM get_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CountersRef* p;
    unsigned ix;

    if (!get_ref_ix(env, argv, &p, &ix)) {
        return enif_make_badarg(env);
    }

    return enif_make_int64(env, ethr_atomic_read_mb(&p->v[ix]));
}

static ERL_NIF_TERM add_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CountersRef* p;
    unsigned ix;
    ErlNifSInt64 incr;

    if (!get_ref_ix(env, argv, &p, &ix)
        || !enif_get_int64(env, argv[2], &incr)) {
        return enif_make_badarg(env);
    }

    ethr_atomic_add_mb(&p->v[ix], incr);

    return atom_ok;
}

static ERL_NIF_TERM add_get_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CountersRef* p;
    unsigned ix;
    ErlNifSInt64 incr;

    if (!get_ref_ix(env, argv, &p, &ix)
        || !enif_get_int64(env, argv[2], &incr)) {
        return enif_make_badarg(env);
    }

    return enif_make_int64(env, ethr_atomic_add_read_mb(&p->v[ix], incr));
}

