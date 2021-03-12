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

void BeamModuleAssembler::emit_i_select_tuple_arity(
        const ArgVal &Src,
        const ArgVal &Fail,
        const ArgVal &Size,
        const std::vector<ArgVal> &args) {
    emit_nyi("emit_i_select_tuple_arity");
}

void BeamModuleAssembler::emit_i_select_val_lins(
        const ArgVal &Src,
        const ArgVal &Fail,
        const ArgVal &Size,
        const std::vector<ArgVal> &args) {
    emit_nyi("emit_i_select_val_lins");
}

void BeamModuleAssembler::emit_i_select_val_bins(
        const ArgVal &Src,
        const ArgVal &Fail,
        const ArgVal &Size,
        const std::vector<ArgVal> &args) {
    emit_nyi("emit_i_select_val_bins");
}

/*
 * Emit code for a binary search through an interval Left <= Right of
 * the i_select_val argument vector `args`.
 *
 * ARG2 is the value being looked up.
 */
void BeamModuleAssembler::emit_binsearch_nodes(
        size_t Left,
        size_t Right,
        const ArgVal &Fail,
        const std::vector<ArgVal> &args) {
    emit_nyi("emit_binsearch_nodes");
}

void BeamModuleAssembler::emit_i_jump_on_val(const ArgVal &Src,
                                             const ArgVal &Fail,
                                             const ArgVal &Base,
                                             const ArgVal &Size,
                                             const std::vector<ArgVal> &args) {
    emit_nyi("emit_i_jump_on_val");
}

/*
 * Attempt to optimize the case when a select_val has exactly two
 * values which only differ by one bit and they both branch to the
 * same label.
 *
 * The optimization makes use of the observation that (V == X || V ==
 * Y) is equivalent to (V | (X ^ Y)) == (X | Y) when (X ^ Y) has only
 * one bit set.
 *
 * ARG2 contains the value.
 * Return true if the optimization was possible, in
 * which case ARG1 should be considered trashed.
 */
bool BeamModuleAssembler::emit_optimized_three_way_select(
        const ArgVal &Fail,
        const std::vector<ArgVal> &args) {
    ERTS_ASSERT(!"NYI");
}
