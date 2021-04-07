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

#include <string>
#include <vector>
#include <unordered_map>
#include <map>

#ifndef ASMJIT_ASMJIT_H_INCLUDED
#    include <asmjit/asmjit.hpp>
#endif

#include <asmjit/a64.h>

extern "C"
{
#ifdef HAVE_CONFIG_H
#    include "config.h"
#endif

#include "sys.h"
#include "erl_vm.h"
#include "global.h"
#include "beam_catches.h"

#include "beam_asm.h"
}

#include "beam_jit_common.hpp"

using namespace asmjit;

class BeamAssembler : public ErrorHandler {
protected:
    /* Holds code and relocation information. */
    CodeHolder code;

    a64::Assembler a;

    FileLogger logger;

    Section *rodata = nullptr;

    /* * * * * * * * * */

    /* Points at x_reg_array inside an ErtsSchedulerRegisters struct, allowing
     * the aux_regs field to be addressed with an 8-bit displacement. */
    const arm::Gp scheduler_registers = a64::x19;

    const arm::Gp E = a64::x20;
    const arm::Gp c_p = a64::x21;
    const arm::Gp FCALLS = a64::x22;
    const arm::Gp HTOP = a64::x23;

    /* Local copy of the active code index.
     *
     * This is set to ERTS_SAVE_CALLS_CODE_IX when save_calls is active, which
     * routes us to a common handler routine that calls save_calls before
     * jumping to the actual code. */
    const arm::Gp active_code_ix = a64::x24;

    /* X registers */
#if defined(DEBUG)
    /*
     * To ensure that we thoroughly test flushing of caller-save X
     * registers, define more caller-save X registers in a DEBUG
     * build.
     */
#    define ERTS_HIGHEST_CALLEE_SAVE_XREG 2
#    define ERTS_HIGHEST_CALLER_SAVE_XREG 5
    const arm::Gp XREG0 = a64::x25;
    const arm::Gp XREG1 = a64::x26;
    const arm::Gp XREG2 = a64::x27;

    /*
     * Caller-save X registers. Must be flushed before calling C
     * code.
     */
    const arm::Gp XREG3 = a64::x15;
    const arm::Gp XREG4 = a64::x16;
    const arm::Gp XREG5 = a64::x17;
#else
#    define ERTS_HIGHEST_CALLEE_SAVE_XREG 3
#    define ERTS_HIGHEST_CALLER_SAVE_XREG 5
    const arm::Gp XREG0 = a64::x25;
    const arm::Gp XREG1 = a64::x26;
    const arm::Gp XREG2 = a64::x27;
    const arm::Gp XREG3 = a64::x28;

    /*
     * Caller-save X registers. Must be flushed before calling C
     * code.
     */
    const arm::Gp XREG4 = a64::x15;
    const arm::Gp XREG5 = a64::x16;
#endif
    static const int num_register_backed_xregs = 6;
    const arm::Gp register_backed_xregs[num_register_backed_xregs] =
            {XREG0, XREG1, XREG2, XREG3, XREG4, XREG5};

#ifdef ERTS_MSACC_EXTENDED_STATES
    const arm::Mem erts_msacc_cache = getSchedulerRegRef(
            offsetof(ErtsSchedulerRegisters, aux_regs.d.erts_msacc_cache));
#endif

    /* * * * * * * * * */
    const arm::Gp ZERO = a64::xzr;

    /*
     * All of the following registers are caller-save.
     *
     * Note that ARG1 is also the register for the return value.
     */
    const arm::Gp ARG1 = a64::x0;
    const arm::Gp ARG2 = a64::x1;
    const arm::Gp ARG3 = a64::x2;
    const arm::Gp ARG4 = a64::x3;
    const arm::Gp ARG5 = a64::x4;
    const arm::Gp ARG6 = a64::x5;
    const arm::Gp ARG7 = a64::x6;
    const arm::Gp ARG8 = a64::x7;

    const arm::Gp TMP1 = a64::x8;
    const arm::Gp TMP2 = a64::x9;
    const arm::Gp TMP3 = a64::x10;
    const arm::Gp TMP4 = a64::x11;
    const arm::Gp TMP5 = a64::x12;
    const arm::Gp TMP6 = a64::x13;

    /*
     * Assume that SUPER_TMP will be destroyed by any helper function.
     */
    const arm::Gp SUPER_TMP = a64::x14;

    /* Callee-saved floating-point registers.
     *
     * Note that only the bottom 64 bits of these (128-bit) registers are
     * callee-save, so we cannot pack two floats into each register. */
    const arm::VecD FREG0 = a64::d8;
    const arm::VecD FREG1 = a64::d9;
    const arm::VecD FREG2 = a64::d10;
    const arm::VecD FREG3 = a64::d11;
    const arm::VecD FREG4 = a64::d12;
    const arm::VecD FREG5 = a64::d13;
    const arm::VecD FREG6 = a64::d14;
    const arm::VecD FREG7 = a64::d15;
    static const int num_register_backed_fregs = 8;
    const arm::VecD register_backed_fregs[num_register_backed_fregs] =
            {FREG0, FREG1, FREG2, FREG3, FREG4, FREG5, FREG6, FREG7};

    /*
     * Note that x18 is reserved on Apple platforms and must not be used.
     */

    const arm::Mem TMP_MEM1q = getSchedulerRegRef(
            offsetof(ErtsSchedulerRegisters, aux_regs.d.TMP_MEM[0]));
    const arm::Mem TMP_MEM2q = getSchedulerRegRef(
            offsetof(ErtsSchedulerRegisters, aux_regs.d.TMP_MEM[1]));
    const arm::Mem TMP_MEM3q = getSchedulerRegRef(
            offsetof(ErtsSchedulerRegisters, aux_regs.d.TMP_MEM[2]));
    const arm::Mem TMP_MEM4q = getSchedulerRegRef(
            offsetof(ErtsSchedulerRegisters, aux_regs.d.TMP_MEM[3]));
    const arm::Mem TMP_MEM5q = getSchedulerRegRef(
            offsetof(ErtsSchedulerRegisters, aux_regs.d.TMP_MEM[4]));

    /* Fill registers with undefined contents to find bugs faster.
     * A boxed value is most likely to cause noticeable trouble.
     */
    const Uint64 bad_boxed_ptr = 0xcafedeadbeef2UL;

public:
    static bool hasCpuFeature(uint32_t featureId);

    BeamAssembler() : code() {
        /* Setup with default code info */
        Error err = code.init(hostEnvironment());
        ERTS_ASSERT(!err && "Failed to init codeHolder");

        err = code.newSection(&rodata,
                              ".rodata",
                              SIZE_MAX,
                              Section::kFlagConst,
                              8);
        ERTS_ASSERT(!err && "Failed to create .rodata section");

        err = code.attach(&a);

        ERTS_ASSERT(!err && "Failed to attach codeHolder");
#ifdef DEBUG
        a.addValidationOptions(BaseEmitter::kValidationOptionAssembler);
#endif
        a.addEncodingOptions(BaseEmitter::kEncodingOptionOptimizeForSize);
        code.setErrorHandler(this);
    }

    BeamAssembler(const std::string &log) : BeamAssembler() {
        if (erts_asm_dump) {
            setLogger(log + ".asm");
        }
    }

    ~BeamAssembler() {
        if (logger.file())
            fclose(logger.file());
    }

    void *getBaseAddress() {
        ASSERT(code.hasBaseAddress());
        return (void *)code.baseAddress();
    }

    size_t getOffset() {
        return a.offset();
    }

protected:
    void _codegen(JitAllocator *allocator,
                  const void **executable_ptr,
                  void **writable_ptr) {
        Error err = code.flatten();
        ERTS_ASSERT(!err && "Could not flatten code");
        err = code.resolveUnresolvedLinks();
        ERTS_ASSERT(!err && "Could not resolve all links");

        /* Verify that all labels are bound */
#ifdef DEBUG
        for (auto e : code.labelEntries()) {
            if (!e->isBound()) {
                if (e->hasName()) {
                    erts_exit(ERTS_ABORT_EXIT,
                              "Label %d with name %s is not bound\n",
                              e->id(),
                              e->name());
                } else {
                    erts_exit(ERTS_ABORT_EXIT,
                              "Label %d is not bound\n",
                              e->id());
                }
            }
        }
#endif

        err = allocator->alloc(const_cast<void **>(executable_ptr),
                               writable_ptr,
                               code.codeSize() + 16);

        if (err == ErrorCode::kErrorTooManyHandles) {
            ERTS_ASSERT(!"Failed to allocate module code: "
                         "out of file descriptors");
        } else if (err) {
            ERTS_ASSERT("Failed to allocate module code");
        }

        code.relocateToBase((uint64_t)*executable_ptr);
        code.copyFlattenedData(*writable_ptr,
                               code.codeSize(),
                               CodeHolder::kCopyPadSectionBuffer);
#ifdef DEBUG
        if (FileLogger *l = dynamic_cast<FileLogger *>(code.logger()))
            if (FILE *f = l->file())
                fprintf(f, "; CODE_SIZE: %zd\n", code.codeSize());
#endif
    }

    void *getCode(Label label) {
        ASSERT(label.isValid());
        return (char *)getBaseAddress() + code.labelOffsetFromBase(label);
    }

    byte *getCode(char *labelName) {
        return (byte *)getCode(code.labelByName(labelName, strlen(labelName)));
    }

    void handleError(Error err, const char *message, BaseEmitter *origin) {
        comment(message);
        fflush(logger.file());
        ASSERT(0 && "Failed to encode instruction");
    }

#ifdef JIT_HARD_DEBUG
    constexpr arm::Mem getInitialSPRef() const {
        int base = offsetof(ErtsSchedulerRegisters, initial_sp);

        return getSchedulerRegRef(base);
    }
#endif

    constexpr arm::Mem getCPRef() const {
        return arm::Mem(E);
    }

    constexpr arm::Mem getSchedulerRegRef(int offset) const {
        ERTS_ASSERT((offset & (sizeof(Eterm) - 1)) == 0);
        return arm::Mem(scheduler_registers, offset);
    }

    constexpr arm::Mem getFRef(int index, size_t size = sizeof(UWord)) const {
        int base = offsetof(ErtsSchedulerRegisters, f_reg_array.d);
        int offset = index * sizeof(FloatDef);

        ASSERT(0 <= index && index <= 1023);
        return getSchedulerRegRef(base + offset);
    }

    constexpr arm::Mem getXRef(int index) const {
        int base = offsetof(ErtsSchedulerRegisters, x_reg_array.d);
        int offset = index * sizeof(Eterm);

        ASSERT(0 <= index && index < ERTS_X_REGS_ALLOCATED);
        return getSchedulerRegRef(base + offset);
    }

    constexpr arm::Mem getYRef(int index) const {
        ASSERT(0 <= index && index <= 1023);

        return arm::Mem(E, index * sizeof(Eterm));
    }

    constexpr arm::Mem getCARRef(arm::Gp Src) const {
        return arm::Mem(Src, -TAG_PRIMARY_LIST);
    }

    constexpr arm::Mem getCDRRef(arm::Gp Src,
                                 size_t size = sizeof(UWord)) const {
        return arm::Mem(Src, -TAG_PRIMARY_LIST + sizeof(Eterm));
    }

    void load_x_reg_array(arm::Gp to) {
        int offset = offsetof(ErtsSchedulerRegisters, x_reg_array.d);

        lea(to, getSchedulerRegRef(offset));
    }

    void load_erl_bits_state(arm::Gp to) {
        int offset =
                offsetof(ErtsSchedulerRegisters, aux_regs.d.erl_bits_state);

        lea(to, getSchedulerRegRef(offset));
    }

    void emit_assert_redzone_unused() {
#ifdef JIT_HARD_DEBUG
        const int REDZONE_BYTES = S_REDZONE * sizeof(Eterm);
        Label next = a.newLabel();

        a.sub(SUPER_TMP, E, imm(REDZONE_BYTES));
        a.cmp(HTOP, SUPER_TMP);

        a.cond_ls().b(next);
        a.udf(0xbeef);

        a.bind(next);
#endif
    }

    /*
     * Calls an Erlang function.
     */
    template<typename Any>
    void erlang_call(Any Target) {
        emit_assert_redzone_unused();
        aligned_call(Target);
    }

    void branch(arm::Mem target) {
        a.ldr(SUPER_TMP, target);
        a.br(SUPER_TMP);
    }

    void branch(Label target) {
        a.b(target);
    }

    /*
     * Calls the given address in shared fragment, ensuring that the
     * redzone is unused and that the return address forms a valid
     * CP.
     */
    template<typename Any>
    void fragment_call(Any Target) {
        emit_assert_redzone_unused();

#if defined(JIT_HARD_DEBUG)
        /* Verify that the stack has not grown. */
        Label next = a.newLabel();
        a.ldr(SUPER_TMP, getInitialSPRef());
        a.cmp(a64::sp, SUPER_TMP);
        a.cond_eq().b(next);
        a.udf(0xdead);
        a.bind(next);
#endif

        aligned_call(Target);
    }

    template<typename FuncPtr>
    void aligned_call(FuncPtr(*target)) {
        mov_imm(SUPER_TMP, (UWord)target);
        a.blr(SUPER_TMP);
    }

    void aligned_call(Label target) {
        a.bl(target);
    }

    void aligned_call(arm::Gp target) {
        a.blr(target);
    }

    /* Calls the given address. In DEBUG builds, make
     * sure that the CP is aligned. */
    template<typename OperandType>
    void aligned_call(OperandType target) {
        ERTS_CT_ASSERT(_CPMASK == 3);
        ASSERT(is_CP(a.offset()));
        a.ldr(TMP1, target);
        a.blr(TMP1);
    }

    void runtime_call(arm::Gp func, unsigned args) {
        ASSERT(args < 5);
        a.blr(func);
    }

    template<typename T>
    struct function_arity;
    template<typename T, typename... Args>
    struct function_arity<T(Args...)>
            : std::integral_constant<int, sizeof...(Args)> {};

    template<int expected_arity, typename T>
    void runtime_call(T(*func)) {
        static_assert(expected_arity == function_arity<T>());

        a.mov(TMP1, func);
        a.blr(TMP1);
    }

    template<typename T>
    void abs_jmp(T(*addr)) {
        Label addr_label = a.newLabel();
        a.ldr(SUPER_TMP, arm::Mem(addr_label));
        a.br(SUPER_TMP);
        a.align(kAlignCode, 8);
        a.bind(addr_label);
        a.embed((char *)&addr, sizeof(UWord));
    }

    /* Explicitly position-independent absolute jump, for use in fragments that
     * need to be memcpy'd for performance reasons (e.g. export entries) */
    template<typename T>
    void pic_jmp(T(*addr)) {
        ERTS_ASSERT(!"NYI");
    }

    constexpr arm::Mem getArgRef(const ArgVal &val) const {
        switch (val.getType()) {
        case ArgVal::TYPE::l:
            return getFRef(val.getValue());
        case ArgVal::TYPE::x:
            return getXRef(val.getValue());
        case ArgVal::TYPE::y:
            return getYRef(val.getValue());
        default:
            ERTS_ASSERT(!"NYI");
            return arm::Mem();
        }
    }

    /* Returns the current code address for the export entry in `Src`
     *
     * Export tracing, save_calls, etc is implemented by shared fragments that
     * assume that the export entry is in ARG1, so we have to copy it over if it
     * isn't already. */
    arm::Mem emit_setup_export_call(const arm::Gp &Src) {
        return emit_setup_export_call(Src, active_code_ix);
    }

    arm::Mem emit_setup_export_call(const arm::Gp &Src,
                                    const arm::Gp &CodeIndex) {
        if (ARG1 != Src) {
            a.mov(ARG1, Src);
        }
        ERTS_CT_ASSERT(offsetof(Export, addresses) == 0);
        return arm::Mem(ARG1, CodeIndex, arm::lsl(3));
    }

    enum Update : int {
        eStack = (1 << 0),
        eHeap = (1 << 1),
        eReductions = (1 << 2),
        eCodeIndex = (1 << 3),
        eFragileXregs = (1 << 4)
    };

    void emit_enter_erlang_frame() {
        a.str(a64::x30, arm::Mem(E, -8).pre());
    }

    void emit_leave_erlang_frame() {
        a.ldr(a64::x30, arm::Mem(E).post(8));
    }

    void emit_discard_cp() {
        a.add(a64::x30, imm(8));
    }

    void emit_enter_runtime_frame() {
        a.stp(a64::x29, a64::x30, arm::Mem(a64::sp, -16).pre());
        a.mov(a64::x29, a64::sp);
    }

    void emit_leave_runtime_frame() {
        a.mov(a64::sp, a64::x29);
        a.ldp(a64::x29, a64::x30, arm::Mem(a64::sp).post(16));
    }

    /*
     * We keep the first 6 X registers in ARM registers. Some of
     * those registers are callee-saved and some are caller-saved.
     *
     * There are two mutually exclusive mechanisms for transferring the
     * terms in X register from the X register array to the ARM registers
     * and vice versa.
     *
     * To transfer only the caller-saved X registers, pass
     * Update::eFragileXregs. This is typically useful for guard BIFs and
     * instructions such as is_eq_exact that preserves all registers that
     * are not destination operands.
     *
     * To transfer all live register-backed X registers, call
     * emit_enter_runtime / emit_leave_runtime with the number of live
     * X registers or `all_xregs` if not known at load time. This is
     * typically useful when calling helper functions that must be
     * able to examine and/or modify all X registers, such as apply.
     */

    const int all_xregs = num_register_backed_xregs;

    template<int Spec = 0>
    void emit_enter_runtime(int live = 0) {
        ERTS_CT_ASSERT((Spec & (Update::eReductions | Update::eStack |
                                Update::eHeap | Update::eFragileXregs)) ==
                       Spec);

        if ((Spec & (Update::eHeap | Update::eStack)) ==
            (Update::eHeap | Update::eStack)) {
            /* Store HTOP and E in one go. */
            ERTS_CT_ASSERT(offsetof(Process, stop) - offsetof(Process, htop) ==
                           8);
            a.stp(HTOP, E, arm::Mem(c_p, offsetof(Process, htop)));
        } else {
            if ((Spec & Update::eStack)) {
                a.str(E, arm::Mem(c_p, offsetof(Process, stop)));
            }
            if (Spec & Update::eHeap) {
                a.str(HTOP, arm::Mem(c_p, offsetof(Process, htop)));
            }
        }

        if (Spec & Update::eReductions) {
            a.str(FCALLS, arm::Mem(c_p, offsetof(Process, fcalls)));
        }

        if (Spec & Update::eFragileXregs) {
            ERTS_ASSERT(live == 0);
#ifdef DEBUG
            a.str(XREG3, getXRef(3));
#endif
            a.stp(XREG4, XREG5, getXRef(4));
        } else if (live > 0) {
            save_x_regs(live);
        }

#ifdef DEBUG
        /*
         * Destroy the caller-saved X registers to find bugs sooner.
         */
        mov_imm(XREG3, bad_boxed_ptr);
        a.mov(XREG4, XREG3);
        a.mov(XREG5, XREG3);
#endif
    }

    template<int Spec = 0>
    void emit_leave_runtime(int live = 0) {
        ERTS_CT_ASSERT(
                (Spec & (Update::eReductions | Update::eStack | Update::eHeap |
                         Update::eFragileXregs | Update::eCodeIndex)) == Spec);

        if ((Spec & Update::eStack)) {
            a.ldr(E, arm::Mem(c_p, offsetof(Process, stop)));
        }

        if (Spec & Update::eHeap) {
            a.ldr(HTOP, arm::Mem(c_p, offsetof(Process, htop)));
        }

        if (Spec & Update::eReductions) {
            a.ldr(FCALLS, arm::Mem(c_p, offsetof(Process, fcalls)));
        }

        if (Spec & Update::eCodeIndex) {
            /* Updates the local copy of the active code index, retaining
             * save_calls if active. */
            mov_imm(TMP1, (UWord)&the_active_code_index);
            a.ldr(TMP1.w(), arm::Mem(TMP1));
            a.cmp(active_code_ix, imm(ERTS_SAVE_CALLS_CODE_IX));
            a.csel(active_code_ix, active_code_ix, TMP1, arm::Cond::kEQ);
        }

        if (Spec & Update::eFragileXregs) {
            ERTS_ASSERT(live == 0);
#ifdef DEBUG
            a.ldr(XREG3, getXRef(3));
#endif
            a.ldp(XREG4, XREG5, getXRef(4));
        } else if (live > 0) {
            load_x_regs(live);
        }
    }

    void emit_is_boxed(Label Fail, arm::Gp Src) {
        const int bitNumber = 0;
        ERTS_CT_ASSERT(_TAG_PRIMARY_MASK - TAG_PRIMARY_BOXED ==
                       (1 << bitNumber));
        a.tbnz(Src, imm(bitNumber), Fail);
    }

    arm::Gp emit_ptr_val(arm::Gp Dst, arm::Gp Src) {
#if !defined(TAG_LITERAL_PTR)
        return Src;
#else
        /* We intentionally skip TAG_PTR_MASK__ here, as we want to use
         * plain `emit_boxed_val` when we know the argument can't be a literal,
         * such as in bit-syntax matching.
         *
         * This comes at very little cost as `emit_boxed_val` nearly always has
         * a displacement. */
        a.and_(Dst, Src, imm(~TAG_LITERAL_PTR));
        return Dst;
#endif
    }

    constexpr arm::Mem emit_boxed_val(arm::Gp Src, int32_t bytes = 0) const {
        ASSERT(bytes % sizeof(Eterm) == 0);
        return arm::Mem(Src, bytes - TAG_PRIMARY_BOXED);
    }

    void emit_branch_if_not_value(arm::Gp Reg, Label lbl) {
        if (THE_NON_VALUE == 0) {
            a.cbz(Reg, lbl);
        } else {
            a.cmp(Reg, imm(THE_NON_VALUE));
            a.cond_eq().b(lbl);
        }
    }

    void emit_branch_if_value(arm::Gp Reg, Label lbl) {
        if (THE_NON_VALUE == 0) {
            a.cbnz(Reg, lbl);
        } else {
            a.cmp(Reg, imm(THE_NON_VALUE));
            a.cond_ne().b(lbl);
        }
    }

    /*
     * Generate the best instruction for setting a register to an
     * immediate value.
     */
    void mov_imm(arm::Gp to, Uint value) {
        if (value == 0) {
            a.mov(to, ZERO);
        } if (value == (Uint) -1) {
            // FIXME: There seems to be a bug in asmjit for a.mov(Reg, imm(-1)).
            // Use a workaround.
            a.mvn(to, ZERO);
        } else {
            a.mov(to, imm(value));
        }
    }

    void sub(arm::Gp to, arm::Gp src, int64_t val) {
        if (val < 0) {
            add(to, src, -val);
        } else {
            ERTS_ASSERT(val < (1 << 24));
            if ((val >> 12) == 0) {
                a.sub(to, src, val);
            } else {
                a.sub(to, src, val & ~0xfff);
                if (val & 0xfff) {
                    a.sub(to, src, val & 0xfff);
                }
            }
        }
    }

    void add(arm::Gp to, arm::Gp src, int64_t val) {
        if (val < 0) {
            sub(to, src, -val);
        } else {
            ERTS_ASSERT(val < (1 << 24));
            if ((val >> 12) == 0) {
                a.add(to, src, val);
            } else {
                a.add(to, src, val & ~0xfff);
                if (val & 0xfff) {
                    a.add(to, src, val & 0xfff);
                }
            }
        }
    }

    void and_(arm::Gp to, arm::Gp src, uint64_t val) {
        uint64_t v = val;
        while ((v & 1) == 0) {
            v >>= 1;
        }
        if (v < 0x20) {
            a.and_(to, src, imm(val));
        } else {
            ASSERT(src != SUPER_TMP);
            mov_imm(SUPER_TMP, val);
            a.and_(to, src, SUPER_TMP);
        }
    }

    void ldur(arm::Gp reg, arm::Mem mem) {
        safe_9bit_imm(a64::Inst::kIdLdur, reg, mem);
    }

    void stur(arm::Gp reg, arm::Mem mem) {
        safe_9bit_imm(a64::Inst::kIdStur, reg, mem);
    }

    void safe_9bit_imm(uint32_t instId, arm::Gp reg, arm::Mem mem) {
        ERTS_ASSERT(mem.hasBaseReg());
        ERTS_ASSERT(!mem.hasIndex());
        int64_t offset = mem.offset();

        if (Support::isInt9(offset)) {
            a.emit(instId, reg, mem);
        } else {
            lea(SUPER_TMP, mem);
            a.emit(instId, reg, arm::Mem(SUPER_TMP));
        }
    }

    /*
     * ARM has no LEA instruction. Implement our own to enable us
     * to use helpers based on getSchedulerRegRef() that return an
     * arm::Mem class.
     */
    void lea(arm::Gp to, arm::Mem mem) {
        ERTS_ASSERT(mem.hasBaseReg());
        ERTS_ASSERT(!mem.hasIndex());
        int64_t offset = mem.offset();
        if (offset == 0) {
            a.mov(to, arm::GpX(mem.baseId()));
        } else {
            add(to, arm::GpX(mem.baseId()), offset);
        }
    }

    /* Load register-backed X registers from the X register array to
     * CPU registers. (The corresponding registers in the X register
     * array must not be used after calling this function.)
     */
    void load_x_regs(int num_to_load = num_register_backed_xregs) {
        if (num_to_load > num_register_backed_xregs) {
            num_to_load = num_register_backed_xregs;
        }
        for (int i = 0; i < num_to_load; i += 2) {
            arm::Gp reg1 = register_backed_xregs[i];
            arm::Gp reg2 = register_backed_xregs[i + 1];
            if (i + 1 == num_to_load) {
                a.ldr(reg1, getXRef(i));
            } else {
                a.ldp(reg1, reg2, getXRef(i));
            }
#ifdef DEBUG
            // FIXME: Should proably be under JIT_HARD_DEBUG later.
            if (i == 0) {
                mov_imm(SUPER_TMP, bad_boxed_ptr);
            }
            if (i + 1 == num_to_load) {
                a.str(SUPER_TMP, getXRef(i));
            } else {
                a.stp(SUPER_TMP, SUPER_TMP, getXRef(i));
            }
#endif
        }
    }

    /* Save register-backed X registers to the X register array. (The
     * backing registers must NOT be used after calling this
     * function.) */
    void save_x_regs(int num_to_save = num_register_backed_xregs) {
        if (num_to_save > num_register_backed_xregs) {
            num_to_save = num_register_backed_xregs;
        }
        for (int i = 0; i < num_to_save; i += 2) {
            arm::Gp reg1 = register_backed_xregs[i];
            arm::Gp reg2 = register_backed_xregs[i + 1];
            if (i + 1 == num_to_save) {
                a.str(reg1, getXRef(i));
            } else {
                a.stp(reg1, reg2, getXRef(i));
            }
#ifdef DEBUG
            // FIXME: Should proably be under JIT_HARD_DEBUG later.
            if (i == 0) {
                mov_imm(SUPER_TMP, bad_boxed_ptr);
            }
            a.mov(reg1, SUPER_TMP);
            if (i + 1 < num_to_save) {
                a.mov(reg2, SUPER_TMP);
            }
#endif
        }
    }

    constexpr int bit_number(uint64_t singleBitMask) {
        ASSERT(singleBitMask != 0);
        int n = 0;
        while ((singleBitMask & 1) == 0) {
            n++;
            singleBitMask >>= 1;
        }
        ERTS_ASSERT(singleBitMask == 1);
        return n;
    }

public:
    void embed_rodata(const char *labelName, const char *buff, size_t size);
    void embed_bss(const char *labelName, size_t size);

    void embed_zeros(size_t size);

    void setLogger(std::string log) {
        FILE *f = fopen(log.data(), "w+");

        /* FIXME: Don't crash when loading multiple modules with the same name.
         *
         * setLogger(nullptr) disables logging. */
        if (f) {
            setvbuf(f, NULL, _IONBF, 0);
        }

        setLogger(f);
    }

    void setLogger(FILE *log) {
        logger.setFile(log);
        logger.setIndentation(FormatOptions::kIndentationCode, 4);
        code.setLogger(&logger);
    }

    template<typename... Ts>
    void comment(const char *format, Ts... args) {
        if (logger.file()) {
            char buff[1024];
            erts_snprintf(buff, sizeof(buff), format, args...);
            a.commentf("# %s", buff);
        }
    }

    struct AsmRange {
        ErtsCodePtr start;
        ErtsCodePtr stop;
        std::string name;

        /* Not used yet */
        std::string file;
        unsigned line;
    };

    void update_gdb_jit_info(std::string modulename,
                             std::vector<AsmRange> &functions);

    void embed(void *data, uint32_t size) {
        a.embed((char *)data, size);
    }
};

class BeamGlobalAssembler : public BeamAssembler {
    typedef void (BeamGlobalAssembler::*emitFptr)(void);
    typedef void (*fptr)(void);

    /* Please keep this in alphabetical order. */
#define BEAM_GLOBAL_FUNCS(_)                                                   \
    _(arith_compare_shared)                                                    \
    _(bif_nif_epilogue)                                                        \
    _(bif_export_trap)                                                         \
    _(call_bif_shared)                                                         \
    _(call_light_bif_shared)                                                   \
    _(catch_end_shared)                                                        \
    _(call_nif_early)                                                          \
    _(call_nif_shared)                                                         \
    _(check_float_error)                                                       \
    _(dispatch_bif)                                                            \
    _(dispatch_return)                                                         \
    _(dispatch_save_calls)                                                     \
    _(export_trampoline)                                                       \
    _(error_action_code)                                                       \
    _(fconv_shared)                                                            \
    _(generic_bp_local)                                                        \
    _(i_bif_body_shared)                                                       \
    _(i_bif_guard_shared)                                                      \
    _(i_func_info_shared)                                                      \
    _(i_length_guard_shared)                                                   \
    _(i_length_body_shared)                                                    \
    _(i_loop_rec_shared)                                                       \
    _(i_test_yield_shared)                                                     \
    _(garbage_collect)                                                         \
    _(process_main)                                                            \
    _(raise_exception)                                                         \
    _(raise_exception_shared)

#define BEAM_GLOBAL_STUBS(_)                                                   \
    _(arith_eq_shared)                                                         \
    _(bs_add_shared)                                                           \
    _(bs_size_check_shared)                                                    \
    _(bs_fixed_integer_shared)                                                 \
    _(bs_get_tail_shared)                                                      \
    _(dispatch_nif)                                                            \
    _(generic_bp_global)                                                       \
    _(debug_bp)                                                                \
    _(i_load_nif_shared)                                                       \
    _(i_new_small_map_lit_shared)                                              \
    _(new_map_shared)                                                          \
    _(update_map_assoc_shared)                                                 \
    _(update_map_exact_guard_shared)                                           \
    _(update_map_exact_body_shared)

/* Labels exported from within process_main */
#define PROCESS_MAIN_LABELS(_)                                                 \
    _(context_switch)                                                          \
    _(context_switch_simplified)                                               \
    _(context_switch_simplified_saved_xregs)                                   \
    _(do_schedule)

#define DECL_ENUM(NAME) NAME,

    enum GlobalLabels : uint32_t {
        BEAM_GLOBAL_FUNCS(DECL_ENUM) PROCESS_MAIN_LABELS(DECL_ENUM)
    };
#undef DECL_ENUM

    static const std::map<GlobalLabels, emitFptr> emitPtrs;
    static const std::map<GlobalLabels, std::string> labelNames;
    std::unordered_map<GlobalLabels, Label> labels;
    std::unordered_map<GlobalLabels, fptr> ptrs;

#define DECL_FUNC(NAME) void emit_##NAME(void);

    BEAM_GLOBAL_FUNCS(DECL_FUNC);
    BEAM_GLOBAL_STUBS(DECL_FUNC);
#undef DECL_FUNC

    template<typename T>
    void emit_bitwise_fallback_body(T(*func_ptr), const ErtsCodeMFA *mfa);

    template<typename T>
    void emit_bitwise_fallback_guard(T(*func_ptr));

    void emit_i_length_common(Label fail, int state_size);

public:
    BeamGlobalAssembler(JitAllocator *allocator);

    void (*get(GlobalLabels lbl))(void) {
        ASSERT(ptrs[lbl]);
        return ptrs[lbl];
    }

#define GET_CODE(NAME)                                                         \
    void (*get_##NAME(void))() {                                               \
        return get(NAME);                                                      \
    }

    BEAM_GLOBAL_FUNCS(GET_CODE)
    PROCESS_MAIN_LABELS(GET_CODE)
#undef GET_CODE
};

class BeamModuleAssembler : public BeamAssembler {
    typedef unsigned BeamLabel;

    /* Map of label number to asmjit Label */
    typedef std::unordered_map<BeamLabel, Label> LabelMap;
    LabelMap labels;

    struct patch {
        Label where;
        int64_t ptr_offs;
        int64_t val_offs;
    };

    struct patch_catch {
        struct patch patch;
        Label handler;
    };
    std::vector<struct patch_catch> catches;

    /* Map of import entry to patch labels and mfa */
    struct patch_import {
        std::vector<struct patch> patches;
        ErtsCodeMFA mfa;
    };
    typedef std::unordered_map<unsigned, struct patch_import> ImportMap;
    ImportMap imports;

    /* Map of fun entry to patch labels */
    struct patch_lambda {
        std::vector<struct patch> patches;
        ErlFunEntry fe;
    };
    typedef std::unordered_map<unsigned, struct patch_lambda> LambdaMap;
    LambdaMap lambdas;

    /* Map of literals to patch labels */
    struct patch_literal {
        std::vector<struct patch> patches;
    };
    typedef std::unordered_map<unsigned, struct patch_literal> LiteralMap;
    LiteralMap literals;

    /* All string patches */
    std::vector<struct patch> strings;

    /* All functions that have been seen so far */
    std::vector<BeamLabel> functions;

    BeamGlobalAssembler *ga;

    /* Used by emit to populate the labelToMFA map */
    Label currLabel;
    unsigned prev_op = 0;
    Label codeHeader;
    Label funcInfo;
    Label funcYield;
    Label genericBPTramp;
    Label on_load;

    Eterm mod;

    /* Save the last PC for an error. */
    size_t last_error_offset = 0;

public:
    BeamModuleAssembler(BeamGlobalAssembler *ga,
                        Eterm mod,
                        unsigned num_labels);
    BeamModuleAssembler(BeamGlobalAssembler *ga,
                        Eterm mod,
                        unsigned num_labels,
                        unsigned num_functions);

    bool emit(unsigned op, const std::vector<ArgVal> &args);

    void codegen(JitAllocator *allocator,
                 const void **executable_ptr,
                 void **writable_ptr,
                 const BeamCodeHeader *in_hdr,
                 const BeamCodeHeader **out_exec_hdr,
                 BeamCodeHeader **out_rw_hdr);

    void codegen(JitAllocator *allocator,
                 const void **executable_ptr,
                 void **writable_ptr);

    void codegen(char *buff, size_t len);

    ErtsCodePtr getCode(unsigned label);
    void *getCode(Label label) {
        return BeamAssembler::getCode(label);
    }
    byte *getCode(char *labelName) {
        return BeamAssembler::getCode(labelName);
    }

    Label embed_vararg_rodata(const std::vector<ArgVal> &args, int y_offset);

    unsigned getCodeSize() {
        ASSERT(code.hasBaseAddress());
        return code.codeSize();
    }

    void copyCodeHeader(BeamCodeHeader *hdr);
    BeamCodeHeader *getCodeHeader(void);
    const ErtsCodeInfo *getOnLoad(void);

    unsigned patchCatches(char *rw_base);
    void patchLambda(char *rw_base, unsigned index, BeamInstr I);
    void patchLiteral(char *rw_base, unsigned index, Eterm lit);
    void patchImport(char *rw_base, unsigned index, BeamInstr I);
    void patchStrings(char *rw_base, const byte *string);

protected:
    /* Helpers */
    void emit_gc_test(const ArgVal &Stack,
                      const ArgVal &Heap,
                      const ArgVal &Live);
    void emit_gc_test_preserve(const ArgVal &Need,
                               const ArgVal &Live,
                               arm::Gp term);

    arm::Mem emit_variable_apply(bool includeI);
    arm::Mem emit_fixed_apply(const ArgVal &arity, bool includeI);

    arm::Gp emit_call_fun(const ArgVal &Fun);
    arm::Gp emit_apply_fun(void);

    arm::Gp emit_is_binary(Label fail,
                           const ArgVal &Src,
                           Label next,
                           Label subbin);

    void emit_get_list(const arm::Gp boxed_ptr,
                       const ArgVal &Hd,
                       const ArgVal &Tl);

    void emit_div_rem(const ArgVal &Fail,
                      const ArgVal &LHS,
                      const ArgVal &RHS,
                      const ErtsCodeMFA *error_mfa);

    void emit_i_bif(const ArgVal &Fail, const ArgVal &Bif, const ArgVal &Dst);

    void emit_bif_arg_error(std::vector<ArgVal> args, const ErtsCodeMFA *mfa);
    void emit_error(int code);

    arm::Mem emit_bs_get_integer_prologue(Label next,
                                          Label fail,
                                          int flags,
                                          int size);

    int emit_bs_get_field_size(const ArgVal &Size,
                               int unit,
                               Label Fail,
                               const arm::Gp &out,
                               unsigned max_size = 0);

    void emit_bs_get_utf8(const ArgVal &Ctx, const ArgVal &Fail);
    void emit_bs_get_utf16(const ArgVal &Ctx,
                           const ArgVal &Fail,
                           const ArgVal &Flags);

    void emit_raise_exception();
    void emit_raise_exception(const ErtsCodeMFA *exp);
    void emit_raise_exception(Label I, const ErtsCodeMFA *exp);

    void emit_validate(const ArgVal &arity);
    void emit_bs_skip_bits(const ArgVal &Fail, const ArgVal &Ctx);

    void emit_linear_search(arm::Gp val,
                            const ArgVal &Fail,
                            const std::vector<ArgVal> &args);

    void emit_float_instr(uint32_t instId,
                          const ArgVal &LHS,
                          const ArgVal &RHS,
                          const ArgVal &Dst);

    void emit_is_small(Label fail, arm::Gp Reg);
    void emit_is_both_small(Label fail, arm::Gp A, arm::Gp B);

    void emit_validate_unicode(Label next, Label fail, arm::Gp value);

    void emit_bif_is_eq_ne_exact_immed(const ArgVal &Src,
                                       const ArgVal &Immed,
                                       const ArgVal &Dst,
                                       Eterm fail_value,
                                       Eterm succ_value);

    void emit_proc_lc_unrequire(void);
    void emit_proc_lc_require(void);

    void emit_nyi(const char *msg);
    void emit_nyi(void);

    void emit_binsearch_nodes(size_t Left,
                              size_t Right,
                              const ArgVal &Fail,
                              const std::vector<ArgVal> &args);

    bool emit_optimized_three_way_select(const ArgVal &Fail,
                                         const std::vector<ArgVal> &args);

#ifdef DEBUG
    void emit_tuple_assertion(const ArgVal &Src, arm::Gp tuple_reg);
#endif

#include "beamasm_protos.h"

    void make_move_patch(arm::Gp to,
                         std::vector<struct patch> &patches,
                         int64_t offset = 0) {
        const int MOV_IMM64_PAYLOAD_OFFSET = 0;
        Label data_lbl = a.newLabel(), next = a.newLabel();
        UWord word = LLONG_MAX;

        a.ldr(to, arm::Mem(data_lbl));
        a.b(next);
        a.align(kAlignCode, 8);
        a.bind(data_lbl);
        embed(&word, sizeof(word));
        a.bind(next);

        patches.push_back({data_lbl, MOV_IMM64_PAYLOAD_OFFSET, offset});
    }

    bool isRegisterBacked(const ArgVal &arg) {
        switch (arg.getType()) {
        case ArgVal::TYPE::x:
            return arg.getValue() < num_register_backed_xregs;
        case ArgVal::TYPE::l:
            return arg.getValue() < num_register_backed_fregs;
        default:
            return false;
        }
    }

    template<typename RegType = arm::Gp>
    struct JitRegister {
        RegType reg;
        const arm::Mem dst;
        JitRegister(RegType r) : reg(r), dst(arm::Mem()) {
        }
        JitRegister(RegType r, arm::Mem mem) : reg(r), dst(mem) {
        }
    };

    JitRegister<arm::Gp> init_dst_reg(const ArgVal &arg, arm::Gp tmp) {
        if (isRegisterBacked(arg)) {
            arm::Gp reg = register_backed_xregs[arg.getValue()];
            return JitRegister(reg);
        } else {
            return JitRegister(tmp, getArgRef(arg));
        }
    }

    JitRegister<arm::VecD> init_dst_reg(const ArgVal &arg, arm::VecD tmp) {
        if (isRegisterBacked(arg)) {
            return JitRegister(register_backed_fregs[arg.getValue()]);
        } else {
            return JitRegister(tmp, getArgRef(arg));
        }
    }

    JitRegister<arm::Gp> init_src_reg(const ArgVal &arg, arm::Gp tmp) {
        if (arg.isLiteral()) {
            make_move_patch(tmp, literals[arg.getValue()].patches);
            return JitRegister(tmp);
        } else if (isRegisterBacked(arg)) {
            arm::Gp xreg = register_backed_xregs[arg.getValue()];
            return JitRegister(xreg);
        } else if (arg.isImmed() || arg.getType() == ArgVal::TYPE::u) {
            mov_imm(tmp, arg.getValue());
            return JitRegister(tmp);
        } else {
            /* Register */
            a.ldr(tmp, getArgRef(arg));
            return JitRegister(tmp);
        }
    }

    JitRegister<arm::VecD> init_src_reg(const ArgVal &arg, arm::VecD tmp) {
        if (isRegisterBacked(arg)) {
            return JitRegister(register_backed_fregs[arg.getValue()]);
        } else {
            /* Register */
            a.ldr(tmp, getArgRef(arg));
            return JitRegister(tmp);
        }
    }

    void copy_reg(const JitRegister<arm::Gp> &to,
                  const JitRegister<arm::Gp> &from) {
        if (to.reg != from.reg) {
            a.mov(to.reg, from.reg);
        }
    }

    void copy_reg(arm::Gp to,
                  const JitRegister<arm::Gp> &from) {
        if (to != from.reg) {
            a.mov(to, from.reg);
        }
    }

    void flush_reg(const JitRegister<arm::Gp> &to) {
        if (to.dst.hasBase()) {
            a.str(to.reg, to.dst);
        }
    }

    void flush_reg(const JitRegister<arm::VecD> &to) {
        if (to.dst.hasBase()) {
            a.str(to.reg, to.dst);
        }
    }

    void mov_arg(const ArgVal &To, const ArgVal &From) {
        if (isRegisterBacked(To)) {
            JitRegister to = init_dst_reg(To, SUPER_TMP);
            JitRegister from = init_src_reg(From, to.reg);
            copy_reg(to, from);
            flush_reg(to);
        } else {
            JitRegister from = init_src_reg(From, SUPER_TMP);
            JitRegister to = init_dst_reg(To, from.reg);
            copy_reg(to, from);
            flush_reg(to);
        }
    }

    void mov_arg(const ArgVal &To, arm::Mem From) {
        JitRegister to = init_dst_reg(To, SUPER_TMP);
        a.ldr(to.reg, From);
        flush_reg(to);
    }

    void mov_arg(arm::Mem To, const ArgVal &From) {
        JitRegister from = init_src_reg(From, SUPER_TMP);
        JitRegister to = JitRegister(from.reg, To);
        flush_reg(to);
    }

    void mov_arg(arm::Gp to, const ArgVal &from) {
        JitRegister r = init_src_reg(from, to);
        if (r.reg != to) {
            a.mov(to, r.reg);
        }
    }

    void mov_arg(const ArgVal &to, arm::Gp from) {
        JitRegister r = init_dst_reg(to, from);
        if (r.reg != from) {
            a.mov(r.reg, from);
        }
        flush_reg(r);
    }

    void cmp_arg(arm::Gp gp, const ArgVal &val) {
        if ((val.isImmed() || val.getType() == ArgVal::TYPE::u) &&
            Support::isUInt12((Sint)val.getValue())) {
            a.cmp(gp, imm(val.getValue()));
        } else {
            mov_arg(SUPER_TMP, val);
            a.cmp(gp, SUPER_TMP);
        }
    }
};

void beamasm_update_perf_info(std::string modulename,
                              std::vector<BeamAssembler::AsmRange> &ranges);
