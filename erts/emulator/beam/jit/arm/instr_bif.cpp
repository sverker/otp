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
#include "code_ix.h"
#include "erl_bif_table.h"
#include "erl_nfunc_sched.h"
#include "bif.h"
#include "erl_msacc.h"
}

/* ARG2 = argument vector, ARG4 (!) = bif function pointer
 *
 * Result is returned in RET, error is indicated by ZF. */
void BeamGlobalAssembler::emit_i_bif_guard_shared() {
    ERTS_ASSERT(!"NYI");
}

/* ARG2 = argument vector, ARG4 (!) = bif function pointer
 *
 * Result is returned in RET. */
void BeamGlobalAssembler::emit_i_bif_body_shared() {
    ERTS_ASSERT(!"NYI");
}

void BeamModuleAssembler::emit_setup_guard_bif(const std::vector<ArgVal> &args,
                                               const ArgVal &bif) {
    emit_nyi("emit_setup_guard_bif");
}

void BeamModuleAssembler::emit_i_bif1(const ArgVal &Src1,
                                      const ArgVal &Fail,
                                      const ArgVal &Bif,
                                      const ArgVal &Dst) {
    emit_nyi("emit_i_bif1");
}

void BeamModuleAssembler::emit_i_bif2(const ArgVal &Src1,
                                      const ArgVal &Src2,
                                      const ArgVal &Fail,
                                      const ArgVal &Bif,
                                      const ArgVal &Dst) {
    emit_nyi("emit_i_bif2");
}

void BeamModuleAssembler::emit_i_bif3(const ArgVal &Src1,
                                      const ArgVal &Src2,
                                      const ArgVal &Src3,
                                      const ArgVal &Fail,
                                      const ArgVal &Bif,
                                      const ArgVal &Dst) {
    emit_nyi("emit_i_bif3");
}

/*
 * Emit code for guard BIFs that can't fail (e.g. is_list/1).  We
 * don't need to test for failure and even in a body there is no need
 * to align the call targeting the shared fragment.
 */

void BeamModuleAssembler::emit_nofail_bif1(const ArgVal &Src1,
                                           const ArgVal &Bif,
                                           const ArgVal &Dst) {
    emit_nyi("emit_nofail_bif1");
}

void BeamModuleAssembler::emit_nofail_bif2(const ArgVal &Src1,
                                           const ArgVal &Src2,
                                           const ArgVal &Bif,
                                           const ArgVal &Dst) {
    emit_nyi("emit_nofail_bif2");
}

void BeamModuleAssembler::emit_i_length_setup(const ArgVal &Fail,
                                              const ArgVal &Live,
                                              const ArgVal &Src) {
    emit_nyi("emit_i_length_setup");
}

/* ARG2 = live registers, ARG3 = entry address
 *
 * Result is returned in RET. */
arm::Mem BeamGlobalAssembler::emit_i_length_common(Label fail, int state_size) {
    ERTS_ASSERT(!"NYI");
    return arm::Mem();
}

/* ARG2 = live registers, ARG3 = entry address
 *
 * Result is returned in RET. */
void BeamGlobalAssembler::emit_i_length_body_shared() {
    ERTS_ASSERT(!"NYI");
}

/* ARG2 = live registers, ARG3 = entry address
 *
 * Result is returned in RET, error is indicated by ZF. */
void BeamGlobalAssembler::emit_i_length_guard_shared() {
    ERTS_ASSERT(!"NYI");
}

void BeamModuleAssembler::emit_i_length(const ArgVal &Fail,
                                        const ArgVal &Live,
                                        const ArgVal &Dst) {
    emit_nyi("emit_i_length");
}

#if defined(DEBUG) || defined(ERTS_ENABLE_LOCK_CHECK)

static Eterm debug_call_light_bif(Process *c_p,
                                  Eterm *reg,
                                  ErtsCodePtr I,
                                  ErtsBifFunc vbf) {
    Eterm result;

    ERTS_UNREQ_PROC_MAIN_LOCK(c_p);
    {
        ERTS_CHK_MBUF_SZ(c_p);
        ASSERT(!ERTS_PROC_IS_EXITING(c_p));
        result = vbf(c_p, reg, I);
        ASSERT(!ERTS_PROC_IS_EXITING(c_p) || is_non_value(result));
        ERTS_CHK_MBUF_SZ(c_p);

        ERTS_VERIFY_UNUSED_TEMP_ALLOC(c_p);
        ERTS_HOLE_CHECK(c_p);
    }
    PROCESS_MAIN_CHK_LOCKS(c_p);
    ERTS_REQ_PROC_MAIN_LOCK(c_p);

    return result;
}
#endif

/* It is important that the below code is as optimized as possible.
 * When doing any changes, make sure to look at the estone bif_dispatch
 * benchmark to make sure you don't introduce any regressions.
 *
 * ARG3 = entry
 * ARG4 = export entry
 * RET  = BIF pointer
 */
void BeamGlobalAssembler::emit_call_light_bif_shared() {
    ERTS_ASSERT(!"NYI");
}

void BeamModuleAssembler::emit_call_light_bif(const ArgVal &Bif,
                                              const ArgVal &Exp) {
    emit_nyi("call_light_bif");
}

void BeamModuleAssembler::emit_send() {
    emit_nyi("emit_send");
}

void BeamGlobalAssembler::emit_bif_nif_epilogue(void) {
    ERTS_ASSERT(!"NYI");
}

/* Used by call_bif, dispatch_bif, and export_trampoline.
 *
 * Note that we don't check reductions here as we may have jumped here through
 * interpreted code (e.g. an ErtsNativeFunc or export entry) and it's very
 * tricky to yield back. Reductions are checked in module code instead.
 *
 * ARG2 = BIF MFA
 * ARG3 = I (rip), doesn't need to point past an MFA
 * ARG4 = function to be called */
void BeamGlobalAssembler::emit_call_bif_shared(void) {
    ERTS_ASSERT(!"NYI");
}

void BeamGlobalAssembler::emit_dispatch_bif(void) {
    ERTS_ASSERT(!"NYI");
}

void BeamModuleAssembler::emit_call_bif(const ArgVal &Func) {
    emit_nyi("emit_call_bif");
}

void BeamModuleAssembler::emit_call_bif_mfa(const ArgVal &M,
                                            const ArgVal &F,
                                            const ArgVal &A) {
    emit_nyi("emit_call_bif_mfa");
}

void BeamGlobalAssembler::emit_call_nif_early() {
    ERTS_ASSERT(!"NYI");
}

/* Used by call_nif, call_nif_early, and dispatch_nif.
 *
 * Note that we don't check reductions here as we may have jumped here through
 * interpreted code (e.g. an ErtsNativeFunc or export entry) and it's very
 * tricky to yield back. Reductions are checked in module code instead.
 *
 * ARG3 = current I, just past the end of an ErtsCodeInfo. */
void BeamGlobalAssembler::emit_call_nif_shared(void) {
    ERTS_ASSERT(!"NYI");
}

void BeamGlobalAssembler::emit_dispatch_nif(void) {
    /* c_p->i points into the trampoline of a ErtsNativeFunc, right after the
     * `info` structure.
     *
     * ErtsNativeFunc already follows the NIF call layout, so we don't need to
     * do anything beyond loading the address. */
    ERTS_ASSERT(!"NYI");
}

/* WARNING: This stub is memcpy'd, so all code herein must be explicitly
 * position-independent. */
void BeamModuleAssembler::emit_call_nif(const ArgVal &Func,
                                        const ArgVal &NifMod,
                                        const ArgVal &DirtyFunc) {
    emit_nyi("emit_call_nif");
}

/* ARG2 = entry address. */
void BeamGlobalAssembler::emit_i_load_nif_shared() {
    ERTS_ASSERT(!"NYI");
}

#ifdef NATIVE_ERLANG_STACK

void BeamModuleAssembler::emit_i_load_nif() {
    emit_nyi("emit_i_load_nif");
}

#else

void BeamModuleAssembler::emit_i_load_nif() {
    emit_nyi("emit_i_load_nif");
}

#endif
