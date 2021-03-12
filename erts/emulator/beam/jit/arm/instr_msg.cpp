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
#include "bif.h"
#include "code_ix.h"
#include "erl_proc_sig_queue.h"
#ifdef USE_VM_PROBES
#    include "dtrace-wrapper.h"
#endif
}

#ifdef ERTS_SUPPORT_OLD_RECV_MARK_INSTRS

static void recv_mark(Process *p) {
    /* inlined here... */
    erts_msgq_recv_marker_insert_bind(p, erts_old_recv_marker_id);
}

static void recv_mark_set(Process *p) {
    /* inlined here... */
    erts_msgq_recv_marker_set_save(p, erts_old_recv_marker_id);
}

void BeamModuleAssembler::emit_i_recv_mark() {
    /*
     * OLD INSTRUCTION: This instruction is to be removed
     *                  in OTP 26.
     *
     * Save the current end of message queue
     */
    emit_nyi("emit_i_recv_mark");
}

void BeamModuleAssembler::emit_i_recv_set() {
    /*
     * OLD INSTRUCTION: This instruction is to be removed
     *                  in OTP 26.
     *
     * If previously saved recv mark, set save pointer to it
     */
    emit_nyi("emit_i_recv_set");
}

#endif /* ERTS_SUPPORT_OLD_RECV_MARK_INSTRS */

void BeamModuleAssembler::emit_recv_marker_reserve(const ArgVal &Dst) {
    emit_nyi("emit_recv_marker_reserve");
}

void BeamModuleAssembler::emit_recv_marker_bind(const ArgVal &Marker,
                                                const ArgVal &Reference) {
    emit_nyi("emit_recv_marker_bind");
}

void BeamModuleAssembler::emit_recv_marker_clear(const ArgVal &Reference) {
    emit_nyi("emit_recv_marker_clear");
}

void BeamModuleAssembler::emit_recv_marker_use(const ArgVal &Reference) {
    emit_nyi("emit_recv_marker_use");
}

#ifdef ERTS_ENABLE_LOCK_CHECK
int erts_lc_proc_sig_receive_helper(Process *c_p,
                                    int fcalls,
                                    int neg_o_reds,
                                    ErtsMessage **msgpp,
                                    int *get_outp) {
    int res;
    /*
     * erts_proc_sig_receive_helper() may temporarliy release
     * its own main lock...
     */
    ERTS_UNREQ_PROC_MAIN_LOCK(c_p);
    res = erts_proc_sig_receive_helper(c_p,
                                       fcalls,
                                       neg_o_reds,
                                       msgpp,
                                       get_outp);
    ERTS_REQ_PROC_MAIN_LOCK(c_p);
    return res;
}
#endif

void BeamGlobalAssembler::emit_i_loop_rec_shared() {
    ERTS_ASSERT(!"NYI");
}

void BeamModuleAssembler::emit_i_loop_rec(const ArgVal &Wait) {
    emit_nyi("i_loop_rec");
}

void BeamModuleAssembler::emit_remove_message() {
    emit_nyi("remove_message");
}

void BeamModuleAssembler::emit_loop_rec_end(const ArgVal &Dest) {
    emit_nyi("emit_loop_rec_end");
}

void BeamModuleAssembler::emit_wait_unlocked(const ArgVal &Dest) {
    emit_nyi("emit_wait_unlocked");
}

void BeamModuleAssembler::emit_wait_locked(const ArgVal &Dest) {
    emit_nyi("wait_locked");
}

void BeamModuleAssembler::emit_wait_timeout_unlocked(const ArgVal &Src,
                                                     const ArgVal &Dest) {
    emit_nyi("emit_wait_timeout_unlocked");
}

void BeamModuleAssembler::emit_wait_timeout_locked(const ArgVal &Src,
                                                   const ArgVal &Dest) {
    emit_nyi("emit_wait_timeout_locked");
}

void BeamModuleAssembler::emit_timeout_locked() {
    emit_nyi("emit_timeout_locked");
}

void BeamModuleAssembler::emit_timeout() {
    emit_nyi("emit_timeout");
}
