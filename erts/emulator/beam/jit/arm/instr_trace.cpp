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
#include "erl_bif_table.h"
#include "beam_bp.h"
};

/* This function is jumped to from the export entry of a function.
 *
 * RET = export entry */
void BeamGlobalAssembler::emit_generic_bp_global() {
    ERTS_ASSERT(!"NYI");
}

/* This function is called from the module header, which is in turn called from
 * the prologue of the traced function. As such, the real return address is at
 * RSP+8.
 *
 * See beam_asm.h about more details */
void BeamGlobalAssembler::emit_generic_bp_local() {
    ERTS_ASSERT(!"NYI");
}

/* This function is called from the module header which is called from the
 * prologue of the function to trace. See beam_asm.h about more details
 *
 * The only place that we can come to here is from generic_bp_local */
void BeamGlobalAssembler::emit_debug_bp() {
    ERTS_ASSERT(!"NYI");
}

void BeamModuleAssembler::emit_return_trace() {
    emit_nyi("return_trace");
}

void BeamModuleAssembler::emit_i_return_time_trace() {
    emit_nyi("i_return_time_trace");
}

static void i_return_to_trace(Process *c_p) {
    if (IS_TRACED_FL(c_p, F_TRACE_RETURN_TO)) {
        Uint *cpp = (Uint *)c_p->stop;
        while (is_not_CP(*cpp)) {
            cpp++;
        }
        for (;;) {
            ErtsCodePtr w = cp_val(*cpp);
            if (BeamIsReturnTrace(w)) {
                do
                    ++cpp;
                while (is_not_CP(*cpp));
                cpp += 2;
            } else if (BeamIsReturnToTrace(w)) {
                do
                    ++cpp;
                while (is_not_CP(*cpp));
            } else {
                break;
            }
        }
        ERTS_UNREQ_PROC_MAIN_LOCK(c_p);
        erts_trace_return_to(c_p, cp_val(*cpp));
        ERTS_REQ_PROC_MAIN_LOCK(c_p);
    }
}

void BeamModuleAssembler::emit_i_return_to_trace() {
    emit_nyi("emit_i_return_to_trace");
}

void BeamModuleAssembler::emit_i_hibernate() {
    emit_nyi("emit_i_hibernate");
}
