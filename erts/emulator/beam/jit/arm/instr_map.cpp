/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2020-2020. All Rights Reserved.
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
#include <algorithm>
#include "beam_asm.hpp"

using namespace asmjit;

extern "C"
{
#include "erl_map.h"
#include "beam_common.h"
}

void BeamModuleAssembler::emit_ensure_map(const ArgVal &map) {
    emit_nyi("emit_ensure_map");
}

void BeamGlobalAssembler::emit_new_map_shared() {
    ERTS_ASSERT(!"NYI");
}

void BeamModuleAssembler::emit_new_map(const ArgVal &Dst,
                                       const ArgVal &Live,
                                       const ArgVal &Size,
                                       const std::vector<ArgVal> &args) {
    emit_nyi("emit_new_map");
}

void BeamGlobalAssembler::emit_i_new_small_map_lit_shared() {
    ERTS_ASSERT(!"NYI");
}

void BeamModuleAssembler::emit_i_new_small_map_lit(
        const ArgVal &Dst,
        const ArgVal &Live,
        const ArgVal &Keys,
        const ArgVal &Size,
        const std::vector<ArgVal> &args) {
    emit_nyi("emit_i_new_small_map_lit");
}

void BeamModuleAssembler::emit_i_get_map_element(const ArgVal &Fail,
                                                 const ArgVal &Src,
                                                 const ArgVal &Key,
                                                 const ArgVal &Dst) {
    emit_nyi("emit_i_get_map_element");
}

void BeamModuleAssembler::emit_i_get_map_elements(
        const ArgVal &Fail,
        const ArgVal &Src,
        const ArgVal &Size,
        const std::vector<ArgVal> &args) {
    emit_nyi("emit_i_get_map_elements");
}

void BeamModuleAssembler::emit_i_get_map_element_hash(const ArgVal &Fail,
                                                      const ArgVal &Src,
                                                      const ArgVal &Key,
                                                      const ArgVal &Hx,
                                                      const ArgVal &Dst) {
    emit_nyi("emit_i_get_map_element_hash");
}

/* ARG3 = live registers, ARG4 = update vector size, ARG5 = update vector. */
void BeamGlobalAssembler::emit_update_map_assoc_shared() {
    ERTS_ASSERT(!"NYI");
}

void BeamModuleAssembler::emit_update_map_assoc(
        const ArgVal &Src,
        const ArgVal &Dst,
        const ArgVal &Live,
        const ArgVal &Size,
        const std::vector<ArgVal> &args) {
    emit_nyi("emit_update_map_assoc");
}

/* ARG3 = live registers, ARG4 = update vector size, ARG5 = update vector.
 *
 * Result is returned in RET, error is indicated by ZF. */
void BeamGlobalAssembler::emit_update_map_exact_guard_shared() {
    ERTS_ASSERT(!"NYI");
}

/* ARG3 = live registers, ARG4 = update vector size, ARG5 = update vector.
 *
 * Does not return on error. */
void BeamGlobalAssembler::emit_update_map_exact_body_shared() {
    ERTS_ASSERT(!"NYI");
}

void BeamModuleAssembler::emit_update_map_exact(
        const ArgVal &Src,
        const ArgVal &Fail,
        const ArgVal &Dst,
        const ArgVal &Live,
        const ArgVal &Size,
        const std::vector<ArgVal> &args) {
    emit_nyi("emit_update_map_exact");
}
