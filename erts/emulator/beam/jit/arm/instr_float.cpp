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

#include "beam_asm.hpp"

extern "C"
{
#include "big.h"
}

void BeamModuleAssembler::emit_fload(const ArgVal &Src, const ArgVal &Dst) {
    emit_nyi("emit_fload");
}

void BeamModuleAssembler::emit_fstore(const ArgVal &Src, const ArgVal &Dst) {
    emit_nyi("emit_fstore");
}

void BeamModuleAssembler::emit_fconv(const ArgVal &Src, const ArgVal &Dst) {
    emit_nyi("emit_fconv");
}

void BeamModuleAssembler::emit_i_fadd(const ArgVal &LHS,
                                      const ArgVal &RHS,
                                      const ArgVal &Dst) {
    emit_nyi("emit_i_fadd");
}

void BeamModuleAssembler::emit_i_fsub(const ArgVal &LHS,
                                      const ArgVal &RHS,
                                      const ArgVal &Dst) {
    emit_nyi("emit_i_fsub");
}

void BeamModuleAssembler::emit_i_fmul(const ArgVal &LHS,
                                      const ArgVal &RHS,
                                      const ArgVal &Dst) {
    emit_nyi("emit_i_fmul");
}

void BeamModuleAssembler::emit_i_fdiv(const ArgVal &LHS,
                                      const ArgVal &RHS,
                                      const ArgVal &Dst) {
    emit_nyi("emit_i_fdiv");
}

void BeamModuleAssembler::emit_i_fnegate(const ArgVal &Src, const ArgVal &Dst) {
    emit_nyi("emit_i_fnegate");
}
