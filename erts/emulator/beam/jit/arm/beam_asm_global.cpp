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

using namespace asmjit;

extern "C"
{
#include "bif.h"
#include "beam_common.h"
}

#define STRINGIFY_(X) #X
#define STRINGIFY(X) STRINGIFY_(X)

#define DECL_EMIT(NAME) {NAME, &BeamGlobalAssembler::emit_##NAME},
const std::map<BeamGlobalAssembler::GlobalLabels, BeamGlobalAssembler::emitFptr>
        BeamGlobalAssembler::emitPtrs = {BEAM_GLOBAL_FUNCS(DECL_EMIT)};
#undef DECL_EMIT

#define DECL_LABEL_NAME(NAME) {NAME, STRINGIFY(NAME)},

const std::map<BeamGlobalAssembler::GlobalLabels, std::string>
        BeamGlobalAssembler::labelNames = {BEAM_GLOBAL_FUNCS(
                DECL_LABEL_NAME) PROCESS_MAIN_LABELS(DECL_LABEL_NAME)};
#undef DECL_LABEL_NAME

BeamGlobalAssembler::BeamGlobalAssembler(JitAllocator *allocator)
        : BeamAssembler("beam_asm_global") {
    labels.reserve(emitPtrs.size());

    /* These labels are defined up-front so global functions can refer to each
     * other freely without any order dependencies. */
    for (auto val : labelNames) {
        std::string name = "global::" + val.second;
        labels[val.first] = a.newNamedLabel(name.c_str());
    }

    /* Emit all of the code and bind all of the labels */
    for (auto val : emitPtrs) {
        a.align(kAlignCode, 8);
        a.bind(labels[val.first]);
        /* This funky syntax calls the function pointer within this instance
         * of BeamGlobalAssembler */
        (this->*val.second)();
    }

    {
        /* We have no need of the module pointers as we use `getCode(...)` for
         * everything. */
        const void *_ignored_exec;
        void *_ignored_rw;
        _codegen(allocator, &_ignored_exec, &_ignored_rw);
    }

#ifndef WIN32
    std::vector<AsmRange> ranges;

    ranges.reserve(emitPtrs.size());

    for (auto val : emitPtrs) {
        ErtsCodePtr start = (ErtsCodePtr)getCode(labels[val.first]);
        ErtsCodePtr stop;

        if (val.first + 1 < emitPtrs.size()) {
            stop = (ErtsCodePtr)getCode(labels[(GlobalLabels)(val.first + 1)]);
        } else {
            stop = (ErtsCodePtr)((char *)getBaseAddress() + code.codeSize());
        }

        ranges.push_back({.start = start,
                          .stop = stop,
                          .name = code.labelEntry(labels[val.first])->name()});
    }

    update_gdb_jit_info("global", ranges);
    beamasm_update_perf_info("global", ranges);
#endif

    /* `this->get_xxx` are populated last to ensure that we crash if we use them
     * instead of labels in global code. */

    for (auto val : labelNames) {
        ptrs[val.first] = (fptr)getCode(labels[val.first]);
    }
}

void BeamGlobalAssembler::emit_handle_error() {
    /* Move return address into ARG2 so we know where we crashed.
     *
     * This bluntly assumes that we haven't pushed anything to the (Erlang)
     * stack in the fragments that jump here. */
    ERTS_ASSERT(!"NYI");
}

/* ARG3 = (HTOP + bytes needed) !!
 * ARG4 = Live registers */
void BeamGlobalAssembler::emit_garbage_collect() {
    ERTS_ASSERT(!"NYI");
}

/* Handles trapping to exports from C code, setting registers up in the same
 * manner a normal call_ext instruction would so that save_calls, tracing, and
 * so on will work.
 *
 * Assumes that c_p->current points into the MFA of an export entry. */
void BeamGlobalAssembler::emit_bif_export_trap() {
    ERTS_ASSERT(!"NYI");
}

/* Handles export breakpoints, error handler, jump tracing, and so on.
 *
 * RET = export entry */
void BeamGlobalAssembler::emit_export_trampoline() {
    ERTS_ASSERT(!"NYI");
}

/*
 * Get the error address implicitly by calling the shared fragment and using
 * the return address as the error address.
 */
void BeamModuleAssembler::emit_handle_error() {
    emit_handle_error(nullptr);
}

void BeamModuleAssembler::emit_handle_error(const ErtsCodeMFA *exp) {
    ERTS_ASSERT(!"NYI");
}

void BeamModuleAssembler::emit_handle_error(Label I, const ErtsCodeMFA *exp) {
    ERTS_ASSERT(!"NYI");
}

/* This is an alias for handle_error */
void BeamGlobalAssembler::emit_error_action_code() {
    ERTS_ASSERT(!"NYI");
}

void BeamGlobalAssembler::emit_handle_error_shared_prologue() {
    ERTS_ASSERT(!"NYI");
}

void BeamGlobalAssembler::emit_handle_error_shared() {
    ERTS_ASSERT(!"NYI");
}

void BeamModuleAssembler::emit_proc_lc_unrequire(void) {
#ifdef ERTS_ENABLE_LOCK_CHECK
    ERTS_ASSERT(!"NYI");
#endif
}

void BeamModuleAssembler::emit_proc_lc_require(void) {
#ifdef ERTS_ENABLE_LOCK_CHECK
    ERTS_ASSERT(!"NYI");
#endif
}

extern "C"
{
    /* GDB puts a breakpoint in this function.
     *
     * Has to be on another file than the caller as otherwise gcc may
     * optimize away the call. */
    void ERTS_NOINLINE __jit_debug_register_code(void);
    void ERTS_NOINLINE __jit_debug_register_code(void) {
    }
}
