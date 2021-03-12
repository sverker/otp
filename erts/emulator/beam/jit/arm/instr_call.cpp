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
#include "beam_common.h"
}

void BeamGlobalAssembler::emit_dispatch_return() {
    ERTS_ASSERT(!"NYI");
}

void BeamModuleAssembler::emit_return() {
    emit_nyi("return");
}

void BeamModuleAssembler::emit_i_call(const ArgVal &CallDest) {
    emit_nyi("emit_i_call");
}

void BeamModuleAssembler::emit_i_call_last(const ArgVal &CallDest,
                                           const ArgVal &Deallocate) {
    emit_nyi("emit_i_call_last");
}

void BeamModuleAssembler::emit_i_call_only(const ArgVal &CallDest) {
    emit_nyi("emit_i_call_only");
}

/* Handles save_calls. Export entry is in RET.
 *
 * When the active code index is ERTS_SAVE_CALLS_CODE_IX, all remote calls will
 * land here. */
void BeamGlobalAssembler::emit_dispatch_save_calls() {
    ERTS_ASSERT(!"NYI");
}

void BeamModuleAssembler::emit_i_call_ext(const ArgVal &Exp) {
    emit_nyi("emit_i_call_ext");
}

void BeamModuleAssembler::emit_i_call_ext_only(const ArgVal &Exp) {
    emit_nyi("emit_i_call_ext_only");
}

void BeamModuleAssembler::emit_i_call_ext_last(const ArgVal &Exp,
                                               const ArgVal &Deallocate) {
    emit_nyi("emit_i_call_ext_last");
}

static ErtsCodeMFA apply3_mfa = {am_erlang, am_apply, 3};

void BeamModuleAssembler::emit_i_apply() {
    emit_nyi("i_apply");
}

void BeamModuleAssembler::emit_i_apply_last(const ArgVal &Deallocate) {
    emit_nyi("emit_i_apply_last");
}

void BeamModuleAssembler::emit_i_apply_only() {
    emit_nyi("emit_i_apply_only");
}

void BeamModuleAssembler::emit_apply(const ArgVal &Arity) {
    emit_nyi("emit_apply");
}

void BeamModuleAssembler::emit_apply_last(const ArgVal &Arity,
                                          const ArgVal &Deallocate) {
    emit_nyi("emit_apply_last");
}

void BeamModuleAssembler::emit_i_apply_fun() {
    emit_nyi("emit_i_apply_fun");
}

void BeamModuleAssembler::emit_i_apply_fun_last(const ArgVal &Deallocate) {
    emit_nyi("emit_i_apply_fun_last");
}

void BeamModuleAssembler::emit_i_apply_fun_only() {
    emit_nyi("emit_i_apply_fun_only");
}

void BeamModuleAssembler::emit_i_call_fun(const ArgVal &Fun) {
    emit_nyi("emit_i_call_fun");
}

void BeamModuleAssembler::emit_i_call_fun_last(const ArgVal &Fun,
                                               const ArgVal &Deallocate) {
    emit_nyi("emit_i_call_fun_last");
}
