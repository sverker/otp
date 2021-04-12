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
 * Result is returned in ARG1 (will be THE_NON_VALUE if the BIF call failed). */
void BeamGlobalAssembler::emit_i_bif_guard_shared() {
    /* We use the X register array for the arguments for the BIF. The
     * actual contents of the first three X registers are kept safe in
     * callee-saved machine registers (XREG0 through XREG2).
     */
    ERTS_CT_ASSERT(ERTS_HIGHEST_CALLEE_SAVE_XREG >= 2);

    emit_enter_runtime_frame();
    emit_enter_runtime<Update::eReductions | Update::eFragileXregs>();

    a.mov(ARG1, c_p);
    lea(ARG2, getXRef(0));
    mov_imm(ARG3, 0);
    runtime_call(ARG4, 3); /* ARG3 is never used by guard BIFs. */

    emit_leave_runtime<Update::eReductions | Update::eFragileXregs>();
    emit_leave_runtime_frame();
    a.ret(a64::x30);
}

/* ARG2 = argument vector, ARG4 (!) = bif function pointer
 *
 * Result is returned in RET. */
void BeamGlobalAssembler::emit_i_bif_body_shared() {
    Label error = a.newLabel();

    /* See comment in emit_i_bif_guard_shared. */
    ERTS_CT_ASSERT(ERTS_HIGHEST_CALLEE_SAVE_XREG >= 2);

    emit_enter_runtime_frame();
    emit_enter_runtime<Update::eReductions | Update::eFragileXregs>();

    /* Save current BIF for the error path. */
    a.mov(ARG1, c_p);
    lea(ARG2, getXRef(0));
    a.str(ARG4, TMP_MEM1q);
    mov_imm(ARG3, 0); /* ARG3 is never used by guard BIFs. */

    runtime_call(ARG4, 3);
    emit_branch_if_not_value(ARG1, error);

    emit_leave_runtime<Update::eReductions | Update::eFragileXregs>();

    emit_leave_runtime_frame();
    a.ret(a64::x30);

    a.bind(error);
    {
        /* Find the correct MFA from the BIF's function address. */
        a.ldr(ARG1, TMP_MEM1q);
        runtime_call<1>(ubif2mfa);

        /* There is no need to restore the callee-saved X registers,
         * because they will never be used after an exception has
         * occurred. */
        emit_leave_runtime<Update::eReductions>();
        emit_leave_runtime_frame();

        a.mov(ARG4, ARG1);
        a.b(labels[raise_exception]);
    }
}

void BeamModuleAssembler::emit_i_bif1(const ArgVal &Src1,
                                      const ArgVal &Fail,
                                      const ArgVal &Bif,
                                      const ArgVal &Dst) {
    JitRegister src1 = init_src_reg(Src1, TMP1);

    a.str(src1.reg, getXRef(0));
    emit_i_bif(Fail, Bif, Dst);
}

void BeamModuleAssembler::emit_i_bif2(const ArgVal &Src1,
                                      const ArgVal &Src2,
                                      const ArgVal &Fail,
                                      const ArgVal &Bif,
                                      const ArgVal &Dst) {
    JitRegister src1 = init_src_reg(Src1, TMP1);
    JitRegister src2 = init_src_reg(Src2, TMP2);

    a.stp(src1.reg, src2.reg, getXRef(0));
    emit_i_bif(Fail, Bif, Dst);
}

void BeamModuleAssembler::emit_i_bif3(const ArgVal &Src1,
                                      const ArgVal &Src2,
                                      const ArgVal &Src3,
                                      const ArgVal &Fail,
                                      const ArgVal &Bif,
                                      const ArgVal &Dst) {
    JitRegister src1 = init_src_reg(Src1, TMP1);
    JitRegister src2 = init_src_reg(Src2, TMP2);
    JitRegister src3 = init_src_reg(Src3, TMP3);

    a.stp(src1.reg, src2.reg, getXRef(0));
    a.str(src3.reg, getXRef(2));
    emit_i_bif(Fail, Bif, Dst);
}

void BeamModuleAssembler::emit_i_bif(const ArgVal &Fail,
                                     const ArgVal &Bif,
                                     const ArgVal &Dst) {
    mov_arg(ARG4, Bif);

    if (Fail.getValue() != 0) {
        fragment_call(ga->get_i_bif_guard_shared());
        emit_branch_if_not_value(ARG1, labels[Fail.getValue()]);
    } else {
        fragment_call(ga->get_i_bif_body_shared());
    }

    mov_arg(Dst, ARG1);
}

/*
 * Emit code for guard BIFs that can't fail (e.g. is_list/1).  We
 * don't need to test for failure.
 */

void BeamModuleAssembler::emit_nofail_bif1(const ArgVal &Src1,
                                           const ArgVal &Bif,
                                           const ArgVal &Dst) {
    JitRegister src1 = init_src_reg(Src1, TMP1);

    a.str(src1.reg, getXRef(0));
    mov_arg(ARG4, Bif);
    fragment_call(ga->get_i_bif_guard_shared());
    mov_arg(Dst, ARG1);
}

void BeamModuleAssembler::emit_nofail_bif2(const ArgVal &Src1,
                                           const ArgVal &Src2,
                                           const ArgVal &Bif,
                                           const ArgVal &Dst) {
    JitRegister src1 = init_src_reg(Src1, TMP1);
    JitRegister src2 = init_src_reg(Src2, TMP2);

    a.stp(src1.reg, src2.reg, getXRef(0));
    mov_arg(ARG4, Bif);
    fragment_call(ga->get_i_bif_guard_shared());
    mov_arg(Dst, ARG1);
}

void BeamModuleAssembler::emit_i_length_setup(const ArgVal &Fail,
                                              const ArgVal &Live,
                                              const ArgVal &Src) {
    mov_arg(TMP1, Src);
    mov_imm(TMP2, make_small(0));

    /* Store trap state after the currently live registers. There are
     * 3 extra registers beyond the ordinary ones that we're free to
     * use for whatever purpose. */
    ERTS_CT_ASSERT(ERTS_X_REGS_ALLOCATED - MAX_REG >= 3);
    mov_arg(ArgVal(ArgVal::TYPE::x, Live.getValue() + 0), TMP1);
    mov_arg(ArgVal(ArgVal::TYPE::x, Live.getValue() + 1), TMP2);

    /* Store original argument. This is only needed for exceptions and can be
     * safely skipped in guards. */
    if (Fail.getValue() == 0) {
        mov_arg(ArgVal(ArgVal::TYPE::x, Live.getValue() + 2), TMP1);
    }
}

/* ARG2 = live registers, ARG3 = entry address
 *
 * Result is returned in RET. */
void BeamGlobalAssembler::emit_i_length_common(Label fail, int state_size) {
    Label trap_or_error = a.newLabel();

    ASSERT(state_size >= 2 && state_size <= ERTS_X_REGS_ALLOCATED - MAX_REG);

    /* Save arguments for error/trapping path. */
    a.stp(ARG2, ARG3, TMP_MEM1q);

    emit_enter_runtime_frame();
    emit_enter_runtime<Update::eReductions>(all_xregs);

    a.mov(ARG1, c_p);
    lea(TMP1, getXRef(0));
    a.lsl(ARG2, ARG2, imm(3));
    a.add(ARG2, TMP1, ARG2);
    runtime_call<2>(erts_trapping_length_1);

    emit_branch_if_not_value(ARG1, trap_or_error);

    emit_leave_runtime<Update::eReductions>(all_xregs);
    emit_leave_runtime_frame();

    a.ret(a64::x30);

    a.bind(trap_or_error);
    {
        a.ldp(ARG2, ARG3, TMP_MEM1q);
        a.ldr(TMP1, arm::Mem(c_p, offsetof(Process, freason)));
        a.cmp(TMP1, imm(TRAP));
        a.cond_ne().b(fail);

        emit_leave_runtime<Update::eReductions>(all_xregs);
        emit_leave_runtime_frame();

        /* The trap state is stored in the registers above the current live
         * ones, so we add the state size (in words) to keep it alive. */
        a.add(ARG2, ARG2, imm(state_size));

        a.str(ZERO, arm::Mem(c_p, offsetof(Process, current)));
        a.str(ARG2, arm::Mem(c_p, offsetof(Process, arity)));

        /* We'll find our way back through the entry address (ARG3). */
        a.b(labels[context_switch_simplified]);
    }
}

/* ARG2 = live registers, ARG3 = entry address
 *
 * Result is returned in RET. */
void BeamGlobalAssembler::emit_i_length_body_shared() {
    Label error = a.newLabel();
    /* `state_size = 3` to include the original argument. */
    emit_i_length_common(error, 3);

    a.bind(error);
    {
        static const ErtsCodeMFA bif_mfa = {am_erlang, am_length, 1};

        /* Move the original argument to x0. It's stored in the third word of
         * the trap state. */
        lea(TMP1, getXRef(0));
        a.lsl(ARG2, ARG2, imm(3));
        a.add(ARG2, TMP1, ARG2);
        a.ldr(TMP1, arm::Mem(ARG2, 2 * sizeof(Eterm)));

        emit_leave_runtime<Update::eReductions>(all_xregs);
        emit_leave_runtime_frame();

        a.mov(XREG0, TMP1);

        mov_imm(ARG4, (UWord)&bif_mfa);
        emit_raise_exception();
    }
}

/* ARG2 = live registers, ARG3 = entry address
 *
 * Result is returned in ARG. Error is indicated by THE_NON_VALUE. */
void BeamGlobalAssembler::emit_i_length_guard_shared() {
    Label error = a.newLabel();

    emit_i_length_common(error, 2);

    a.bind(error);
    {
        emit_leave_runtime<Update::eReductions>(all_xregs);
        emit_leave_runtime_frame();

        a.ret(a64::x30);
    }
}

void BeamModuleAssembler::emit_i_length(const ArgVal &Fail,
                                        const ArgVal &Live,
                                        const ArgVal &Dst) {
    Label entry = a.newLabel();

    a.bind(entry);

    mov_arg(ARG2, Live);
    a.adr(ARG3, entry);
    if (Fail.getValue() != 0) {
        fragment_call(ga->get_i_length_guard_shared());
        emit_branch_if_not_value(ARG1, labels[Fail.getValue()]);
    } else {
        fragment_call(ga->get_i_length_body_shared());
    }

    mov_arg(Dst, ARG1);
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
 * ARG8 = BIF pointer
 */
void BeamGlobalAssembler::emit_call_light_bif_shared() {
    /* We use the HTOP, FCALLS, and XREG1 registers as they are not
     * used on the runtime-stack and are caller save. */

    arm::Gp I = HTOP, exp = FCALLS;

    Label error = a.newLabel(), trace = a.newLabel(), trap = a.newLabel(),
          yield = a.newLabel(), call_save_calls = a.newLabel(),
          call_bif = a.newLabel(), gc_after_bif_call = a.newLabel(),
          check_bif_return = a.newLabel();

    /* Check if we should trace this bif call */
    a.ldr(TMP1.w(), arm::Mem(ARG4, offsetof(Export, is_bif_traced)));
    a.cbnz(TMP1, trace);

    a.subs(FCALLS, FCALLS, imm(1));
    a.cond_le().b(yield);
    {
        emit_enter_runtime_frame();
        emit_enter_runtime<Update::eReductions | Update::eStack |
                           Update::eHeap>(all_xregs);

        /* Spill the arguments we may need on the error path. */
        a.mov(I, ARG3);
        a.mov(exp, ARG4);

#ifdef ERTS_MSACC_EXTENDED_STATES
        {
            Label skip_msacc = a.newLabel();

            a.cmp(erts_msacc_cache, imm(0));
            a.short_().je(skip_msacc);

            a.mov(TMP_MEM1q, RET);

            a.mov(ARG1, erts_msacc_cache);
            a.mov(ARG2,
                  x86::qword_ptr(ARG4, offsetof(Export, info.mfa.module)));
            a.mov(ARG3, RET);
            runtime_call<3>(erts_msacc_set_bif_state);

            a.mov(ARG3, I);
            a.mov(RET, TMP_MEM1q);
            a.bind(skip_msacc);
        }
#endif

        /* Check if we need to call save_calls */
        a.cmp(active_code_ix, imm(ERTS_SAVE_CALLS_CODE_IX));
        a.cond_eq().b(call_save_calls);
        a.bind(call_bif);

        a.ldr(ARG1, arm::Mem(c_p, offsetof(Process, mbuf)));
        a.str(ARG1, TMP_MEM1q);

        a.mov(ARG1, c_p);
        load_x_reg_array(ARG2);

#if defined(DEBUG) || defined(ERTS_ENABLE_LOCK_CHECK)
        a.mov(ARG4, ARG8);
        runtime_call<4>(debug_call_light_bif);
#else
        runtime_call(ARG8, 3);
#endif

        /* ERTS_IS_GC_DESIRED_INTERNAL */
        {
            a.ldr(ARG2, arm::Mem(c_p, offsetof(Process, stop)));
            a.mov(ARG3, ARG1);
            a.ldr(ARG5, arm::Mem(c_p, offsetof(Process, htop)));

#if 0
            /* Test if binary heap size should trigger gc */
            a.mov(RET, x86::qword_ptr(c_p, offsetof(Process, bin_vheap_sz)));
            a.cmp(x86::qword_ptr(c_p, offsetof(Process, off_heap.overhead)),
                  RET);
            a.mov(RETd, x86::dword_ptr(c_p, offsetof(Process, flags)));
            a.seta(x86::cl); /* Clobber ARG1 on windows and ARG4 on Linux */
            a.and_(RETd, imm(F_FORCE_GC));
            a.or_(x86::cl, RETb);
            a.jne(gc_after_bif_call);
#endif

            /* Test if heap fragment size is larger than remaining heap size. */
            a.sub(TMP1, ARG2, ARG5);
            a.asr(TMP1, TMP1, imm(3));
            a.ldr(TMP2, arm::Mem(c_p, offsetof(Process, mbuf_sz)));
            a.cmp(TMP1, TMP2);
            a.cond_lt().b(gc_after_bif_call);
        }
        /*
           ARG2 is set to E
           ARG3 is set to bif return
           ARG5 is set to HTOP

           HTOP is exp
           E_saved|E is I
        */
        a.bind(check_bif_return);
        emit_branch_if_not_value(ARG3, trap);

        a.mov(HTOP, ARG5);
        a.mov(E, ARG2);

        /* We must update the active code index in case another process has
         * loaded new code, as the result of this BIF may be observable on both
         * ends.
         *
         * It doesn't matter whether the BIF modifies anything; if process A
         * loads new code and calls erlang:monotonic_time/0 soon after, we'd
         * break the illusion of atomic upgrades if process B still ran old code
         * after seeing a later timestamp from its own call to
         * erlang:monotonic_time/0. */

        emit_leave_runtime<Update::eReductions | Update::eCodeIndex>();
        emit_leave_runtime_frame();

        a.mov(XREG0, ARG3);
        a.ret(a64::x30);

        a.bind(call_save_calls);
        a.udf(973);
        {
#if 0
            /* Stash the bif function pointer */
            a.mov(TMP_MEM1q, RET);

            /* Setup the arguments to call */
            a.mov(ARG1, c_p);
            a.mov(ARG2, exp);
            runtime_call<2>(save_calls);

            /* Restore RET and ARG3 to the values expected
               by the bif call */
            a.mov(RET, TMP_MEM1q);
            a.mov(ARG3, I);
            a.jmp(call_bif);
#endif
        }

        a.bind(trap);
        {
            a.ldr(TMP1, arm::Mem(c_p, offsetof(Process, freason)));
            a.cmp(TMP1, imm(TRAP));
            a.cond_ne().b(error);

            emit_leave_runtime<Update::eHeap | Update::eStack |
                               Update::eReductions | Update::eCodeIndex>(
                    all_xregs);
            // FIXME: live = 1 should work.
            emit_leave_runtime_frame();

            a.str(a64::x30, arm::Mem(E, -8).pre());

            /* Trap out, our return address is on the Erlang stack.
             *
             * The BIF_TRAP macros all set up c_p->arity and c_p->current, so
             * we can use a simplified context switch. */
            a.ldr(ARG3, arm::Mem(c_p, offsetof(Process, i)));
            a.b(labels[context_switch_simplified]);
        }

        a.bind(error);
        {
            a.mov(ARG4, exp);
            a.mov(ARG2, I);

            emit_leave_runtime<Update::eHeap | Update::eStack |
                               Update::eReductions | Update::eCodeIndex>(
                    all_xregs);
            emit_leave_runtime_frame();

            /* raise_exception_shared expects current PC in ARG2 and MFA in
             * ARG4. */
            add(ARG4, ARG4, offsetof(Export, info.mfa));
            a.b(labels[raise_exception_shared]);
        }

        a.bind(gc_after_bif_call);
        {
            a.mov(ARG1, c_p);
            a.ldr(ARG2, TMP_MEM1q);
            /* ARG3 already contains result */
            load_x_reg_array(ARG4);
            a.ldr(ARG5, arm::Mem(exp, offsetof(Export, info.mfa.arity)));
            runtime_call<5>(erts_gc_after_bif_call_lhf);
            a.ldr(ARG2, arm::Mem(c_p, offsetof(Process, stop)));
            a.mov(ARG3, ARG1);
            a.ldr(ARG5, arm::Mem(c_p, offsetof(Process, htop)));
            a.b(check_bif_return);
        }
    }
    a.bind(trace);
    a.udf(65);
    {
#if 0
        /* Call the export entry instead of the BIF. If we use the
         * native stack as the Erlang stack our return address is
         * already on the Erlang stack. Otherwise we will have to move
         * the return address from the native stack to the Erlang
         * stack. */

        /* The return address must be on the Erlang stack. */
        a.pop(getCPRef());

        x86::Mem destination = emit_setup_export_call(ARG4);
        a.jmp(destination);
#endif
    }

    a.bind(yield);
    {
        a.ldr(ARG2, arm::Mem(ARG4, offsetof(Export, info.mfa.arity)));
        lea(ARG4, arm::Mem(ARG4, offsetof(Export, info.mfa)));
        a.str(ARG2, arm::Mem(c_p, offsetof(Process, arity)));
        a.str(ARG4, arm::Mem(c_p, offsetof(Process, current)));

        /* We'll find our way back through ARG3 (entry address). */
        a.b(labels[context_switch_simplified]);
    }
}

void BeamModuleAssembler::emit_call_light_bif(const ArgVal &Bif,
                                              const ArgVal &Exp) {
    Label entry = a.newLabel();

    a.bind(entry);

    make_move_patch(ARG4, imports[Exp.getValue()].patches);
    mov_imm(ARG8, Bif.getValue());
    a.adr(ARG3, entry);

    fragment_call(ga->get_call_light_bif_shared());
}

void BeamModuleAssembler::emit_send() {
    Label entry = a.newLabel();

    /* This is essentially a mirror of call_light_bif, there's no point to
     * specializing send/2 anymore.
     *
     * FIXME: Rewrite this to an ordinary BIF in the loader instead. */
    a.bind(entry);

    mov_imm(ARG4, (UWord)BIF_TRAP_EXPORT(BIF_send_2));
    mov_imm(ARG8, (UWord)send_2);
    a.adr(ARG3, entry);

    fragment_call(ga->get_call_light_bif_shared());
}

void BeamGlobalAssembler::emit_bif_nif_epilogue(void) {
    Label check_trap = a.newLabel(), trap = a.newLabel(), error = a.newLabel();

#ifdef ERTS_MSACC_EXTENDED_STATES
    {
        Label skip_msacc = a.newLabel();

        a.cmp(erts_msacc_cache, 0);
        a.short_().je(skip_msacc);
        a.mov(TMP_MEM1q, RET);
        a.mov(ARG1, erts_msacc_cache);
        a.mov(ARG2, imm(ERTS_MSACC_STATE_EMULATOR));
        a.mov(ARG3, imm(1));
        runtime_call<3>(erts_msacc_set_state_m__);
        a.mov(RET, TMP_MEM1q);
        a.bind(skip_msacc);
    }
#endif

    /* Another process may have loaded new code and somehow notified us through
     * this call, so we must update the active code index. */
    emit_leave_runtime<Update::eReductions | Update::eStack | Update::eHeap |
                       Update::eCodeIndex>(all_xregs);

    emit_branch_if_not_value(ARG1, check_trap);

    comment("Do return and dispatch to it");
    a.mov(XREG0, ARG1);
    a.ldr(a64::x30, arm::Mem(E).post(8));
    a.ret(a64::x30);

    a.bind(check_trap);
    a.ldr(TMP1, arm::Mem(c_p, offsetof(Process, freason)));
    a.cmp(TMP1, imm(TRAP));
    a.cond_ne().b(error);
    {
        comment("yield");

        comment("test trap to hibernate");
        a.ldr(TMP1, arm::Mem(c_p, offsetof(Process, flags)));
        a.tbz(TMP1, imm(bit_number(F_HIBERNATE_SCHED)), trap);

        comment("do hibernate trap");
        a.udf(23002);
        a.and_(TMP1, TMP1, imm(~F_HIBERNATE_SCHED));
        a.str(TMP1, arm::Mem(c_p, offsetof(Process, flags)));
        a.b(labels[do_schedule]);
    }

    a.bind(trap);
    {
        comment("do normal trap");

        /* The BIF_TRAP macros all set up c_p->arity and c_p->current, so we
         * can use a simplified context switch. */
        a.ldr(ARG3, arm::Mem(c_p, offsetof(Process, i)));
        a.b(labels[context_switch_simplified]);
    }

    a.bind(error);
    {
        a.mov(ARG2, E);

        emit_enter_runtime<Update::eStack>();

        a.mov(ARG1, c_p);
        runtime_call<2>(erts_printable_return_address);

        emit_leave_runtime<Update::eStack>();

        a.mov(ARG2, ARG1);
        a.ldr(ARG4, arm::Mem(c_p, offsetof(Process, current)));
        a.b(labels[raise_exception_shared]);
    }
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
    /* "Heavy" BIFs need up-to-date values for `c_p->i`, `c_p->current`, and
     * `c_p->arity`. */

    emit_enter_runtime_frame();
    a.str(ARG2, arm::Mem(c_p, offsetof(Process, current)));
    /* `call_bif` wants arity in ARG5. */
    a.ldr(ARG5, arm::Mem(ARG2, offsetof(ErtsCodeMFA, arity)));
    a.str(ARG5, arm::Mem(c_p, offsetof(Process, arity)));
    a.str(ARG3, arm::Mem(c_p, offsetof(Process, i)));

    /* The corresponding leave can be found in the epilogue. */
    emit_enter_runtime<Update::eReductions | Update::eStack | Update::eHeap>(
            all_xregs);

#ifdef ERTS_MSACC_EXTENDED_STATES
    {
        Label skip_msacc = a.newLabel();

        a.cmp(erts_msacc_cache, 0);
        a.short_().je(skip_msacc);

        a.mov(TMP_MEM1q, ARG3);
        a.mov(TMP_MEM2q, ARG4);
        a.mov(TMP_MEM3q, ARG5);

        a.mov(ARG1, erts_msacc_cache);
        a.mov(ARG2, x86::qword_ptr(ARG2, offsetof(ErtsCodeMFA, module)));
        a.mov(ARG3, ARG4);
        runtime_call<3>(erts_msacc_set_bif_state);

        a.mov(ARG3, TMP_MEM1q);
        a.mov(ARG4, TMP_MEM2q);
        a.mov(ARG5, TMP_MEM3q);
        a.bind(skip_msacc);
    }
#endif

    a.mov(ARG1, c_p);
    load_x_reg_array(ARG2);
    /* ARG3 (I), ARG4 (func), and ARG5 (arity) have already been provided. */
    runtime_call<5>(beam_jit_call_bif);

#ifdef ERTS_MSACC_EXTENDED_STATES
    a.mov(TMP_MEM1q, RET);
    a.lea(ARG1, erts_msacc_cache);
    runtime_call<1>(erts_msacc_update_cache);
    a.mov(RET, TMP_MEM1q);
#endif

    emit_leave_runtime_frame();
    emit_bif_nif_epilogue();
}

void BeamGlobalAssembler::emit_dispatch_bif(void) {
    /* c_p->i points into the trampoline of a ErtsNativeFunc, right after the
     * `info` structure. */
    a.ldr(ARG3, arm::Mem(c_p, offsetof(Process, i)));

    ERTS_CT_ASSERT(offsetof(ErtsNativeFunc, trampoline.trace) ==
                   sizeof(ErtsCodeInfo));

    ssize_t mfa_offset = offsetof(ErtsNativeFunc, trampoline.trace) -
                         offsetof(ErtsNativeFunc, trampoline.info.mfa);

    a.sub(ARG2, ARG3, imm(mfa_offset));

    ssize_t dfunc_offset = offsetof(ErtsNativeFunc, trampoline.dfunc) -
                           offsetof(ErtsNativeFunc, trampoline.trace);
    a.ldr(ARG4, arm::Mem(ARG3, dfunc_offset));

    a.b(labels[call_bif_shared]);
}

void BeamModuleAssembler::emit_call_bif(const ArgVal &Func) {
    int mfa_offset = (int)sizeof(ErtsCodeMFA);

    a.adr(ARG3, currLabel);
    a.sub(ARG2, ARG3, imm(mfa_offset));
    mov_arg(ARG4, Func);

    abs_jmp(ga->get_call_bif_shared());
}

void BeamModuleAssembler::emit_call_bif_mfa(const ArgVal &M,
                                            const ArgVal &F,
                                            const ArgVal &A) {
    BeamInstr func;
    Export *e;

    e = erts_active_export_entry(M.getValue(), F.getValue(), A.getValue());
    ASSERT(e != NULL && e->bif_number != -1);

    func = (BeamInstr)bif_table[e->bif_number].f;
    emit_call_bif(ArgVal(ArgVal::i, func));
}

void BeamGlobalAssembler::emit_call_nif_early() {
    a.mov(ARG2, a64::x30);
    a.sub(ARG2, ARG2, imm(BEAM_ASM_BP_RETURN_OFFSET + sizeof(ErtsCodeInfo)));

    emit_enter_runtime(all_xregs);

    a.mov(ARG1, c_p);
    runtime_call<2>(erts_call_nif_early);

    emit_leave_runtime(all_xregs);

    /* Emulate `emit_call_nif`, loading the current (phony) instruction
     * pointer into ARG3.
     *
     * Note that we "inherit" the frame that was pushed to the stack prior to
     * running the breakpoint instruction, discarding the current content of
     * LR (x30). */
    a.mov(ARG3, ARG1);
    a.b(labels[call_nif_shared]);
}

/* Used by call_nif, call_nif_early, and dispatch_nif.
 *
 * Note that we don't check reductions here as we may have jumped here through
 * interpreted code (e.g. an ErtsNativeFunc or export entry) and it's very
 * tricky to yield back. Reductions are checked in module code instead.
 *
 * ARG3 = current I, just past the end of an ErtsCodeInfo. */
void BeamGlobalAssembler::emit_call_nif_shared(void) {
    /* The corresponding leave can be found in the epilogue. */
    emit_enter_runtime<Update::eReductions | Update::eStack | Update::eHeap>(
            all_xregs);

#ifdef ERTS_MSACC_EXTENDED_STATES
    { /* ToDO: x86 -> arm */
        Label skip_msacc = a.newLabel();

        a.cmp(erts_msacc_cache, 0);
        a.short_().je(skip_msacc);
        a.mov(TMP_MEM1q, ARG3);
        a.mov(ARG1, erts_msacc_cache);
        a.mov(ARG2, imm(ERTS_MSACC_STATE_NIF));
        a.mov(ARG3, imm(1));
        runtime_call<3>(erts_msacc_set_state_m__);
        a.mov(ARG3, TMP_MEM1q);
        a.bind(skip_msacc);
    }
#endif

    a.mov(ARG1, c_p);
    a.mov(ARG2, ARG3);
    load_x_reg_array(ARG3);
    a.ldr(ARG4, arm::Mem(ARG2, 8 + BEAM_ASM_FUNC_PROLOGUE_SIZE));
    a.ldr(ARG5, arm::Mem(ARG2, 16 + BEAM_ASM_FUNC_PROLOGUE_SIZE));
    a.ldr(ARG6, arm::Mem(ARG2, 24 + BEAM_ASM_FUNC_PROLOGUE_SIZE));
    runtime_call<5>(beam_jit_call_nif);

    emit_bif_nif_epilogue();
}

void BeamGlobalAssembler::emit_dispatch_nif(void) {
    /* c_p->i points into the trampoline of a ErtsNativeFunc, right after the
     * `info` structure.
     *
     * ErtsNativeFunc already follows the NIF call layout, so we don't need to
     * do anything beyond loading the address. */
    a.ldr(ARG3, arm::Mem(c_p, offsetof(Process, i)));
    a.b(labels[call_nif_shared]);
}

void BeamGlobalAssembler::emit_call_nif_yield_helper() {
    Label yield = a.newLabel();

    a.subs(FCALLS, FCALLS, imm(1));
    a.cond_le().b(yield);
    a.b(labels[call_nif_shared]);

    a.bind(yield);
    {
        int mfa_offset = -(int)sizeof(ErtsCodeMFA);
        int arity_offset = mfa_offset + (int)offsetof(ErtsCodeMFA, arity);

        a.ldur(TMP1, arm::Mem(ARG3, arity_offset));
        a.str(TMP1, arm::Mem(c_p, offsetof(Process, arity)));

        a.sub(TMP1, ARG3, imm(-mfa_offset));
        a.str(TMP1, arm::Mem(c_p, offsetof(Process, current)));

        /* Yield to `dispatch` rather than `entry` to avoid pushing too many
         * frames to the stack. See `emit_call_nif` for details. */
        a.add(ARG3, ARG3, imm(BEAM_ASM_FUNC_PROLOGUE_SIZE + sizeof(UWord[4])));
        a.b(labels[context_switch_simplified]);
    }
}

/* WARNING: This stub is memcpy'd, so all code herein must be explicitly
 * position-independent. */
void BeamModuleAssembler::emit_call_nif(const ArgVal &Func,
                                        const ArgVal &NifMod,
                                        const ArgVal &DirtyFunc) {
    Label entry = a.newLabel(), dispatch = a.newLabel();

#ifdef DEBUG
    size_t entry_offset = a.offset();
#endif

    ASSERT(BEAM_ASM_FUNC_PROLOGUE_SIZE ==
           (a.offset() - code.labelOffsetFromBase(currLabel)));

    /* The start of this function must mimic the layout of ErtsNativeFunc.
     *
     * We jump here on the very first entry. */
    a.bind(entry);
    {
        a.b(dispatch); /* call_op */

        a.align(kAlignCode, 8);

        /* ErtsNativeFunc.func */
        a.embedUInt64(Func.getValue());

        /* ErtsNativeFunc.m */
        a.embedUInt64(NifMod.getValue());

        /* ErtsNativeFunc.dfunc */
        a.embedUInt64(DirtyFunc.getValue());
    }

    /* `emit_call_nif_yield_helper` relies on this to compute the address of
     * `dispatch` */
    ASSERT(a.offset() == entry_offset + sizeof(UWord[4]));

    a.bind(dispatch);
    {
        a.adr(ARG3, currLabel);
        pic_jmp(ga->get_call_nif_yield_helper());
    }
}

static ErtsCodePtr get_on_load_address(Process *c_p, Eterm module) {
    const Module *modp = erts_get_module(module, erts_active_code_ix());

    if (modp && modp->on_load) {
        const BeamCodeHeader *hdr = (modp->on_load)->code_hdr;

        if (hdr) {
            return erts_codeinfo_to_code(hdr->on_load);
        }
    }

    c_p->freason = BADARG;

    return NULL;
}

/* Implements the internal and undocumented erlang:call_on_load_function/1,
 * which is tricky to implement in the face of frame pointers. */
void BeamModuleAssembler::emit_i_call_on_load_function() {
    static ErtsCodeMFA mfa = {am_erlang, am_call_on_load_function, 1};
    Label next = a.newLabel();

    emit_enter_runtime();

    a.mov(ARG1, c_p);
    a.ldr(ARG2, getXRef(0));
    runtime_call<2>(get_on_load_address);

    emit_leave_runtime();

    a.cbnz(ARG1, next);

    emit_raise_exception(&mfa);

    a.bind(next);
    erlang_call(ARG1);
}

/* ARG2 = entry address. */
void BeamGlobalAssembler::emit_i_load_nif_shared() {
    ERTS_ASSERT(!"NYI");
}

void BeamModuleAssembler::emit_i_load_nif() {
    static ErtsCodeMFA mfa = {am_erlang, am_load_nif, 2};

    Label entry = a.newLabel(), next = a.newLabel(), schedule = a.newLabel();

    a.bind(entry);

    emit_enter_runtime<Update::eStack | Update::eHeap>(2);

    a.mov(ARG1, c_p);
    a.adr(ARG2, currLabel);
    load_x_reg_array(ARG3);
    runtime_call<3>(beam_jit_load_nif);

    emit_leave_runtime<Update::eStack | Update::eHeap>(2);

    a.cmp(ARG1, imm(RET_NIF_yield));
    a.cond_eq().b(schedule);

    a.cmp(ARG1, imm(RET_NIF_success));
    a.cond_eq().b(next);

    emit_raise_exception(currLabel, &mfa);

    a.bind(schedule);
    {
        a.adr(ARG3, entry);
        abs_jmp(ga->get_context_switch_simplified());
    }

    a.bind(next);
}
