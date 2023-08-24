/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2023. All Rights Reserved.
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

#ifndef E_PROVIDER_H__
#define E_PROVIDER_H__ 1

#include "common.h"

int init_provider(ErlNifEnv *env);
void cleanup_provider(ErlNifEnv*);
ERL_NIF_TERM make_algo_list_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM provider_load_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM providers_loaded_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM md_fetch_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

int get_provider_pkey(ErlNifEnv* env, ERL_NIF_TERM map,
                      char* key_id, char* passwd, EVP_PKEY **pkey, int priv);

#endif /* E_PROVIDER_H__ */

