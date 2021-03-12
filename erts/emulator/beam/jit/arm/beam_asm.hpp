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

#ifdef NATIVE_ERLANG_STACK
    /* The Erlang stack pointer, note that it uses RSP and is therefore invalid
     * when running on the runtime stack. */
    const arm::Gp E = a64::sp;

    /* Cached copy of Erlang stack pointer used to speed up stack switches when
     * we know that the runtime doesn't read or modify the Erlang stack.
     *
     * If we find ourselves pressed for registers in the future, we could save
     * this in the same slot as `registers` as that can be trivially recomputed
     * from the top of the runtime stack. */
    const arm::Gp E_saved = a64::x20;

#else
    const arm::Gp E = a64::x20;
#endif

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
#ifdef DEBUG
    /*
     * To ensure that we thoroughly test flushing of caller-save X
     * registers, define more caller-save X registers.
     */
#   define ERTS_HIGHEST_CALLEE_SAVE_XREG 1
#   define ERTS_HIGHEST_CALLER_SAVE_XREG 5
    const arm::Gp XREG0 = a64::x25;
    const arm::Gp XREG1 = a64::x26;

    /*
     * Caller-save X registers. Must be flushed before calling C
     * code.
     */
    const arm::Gp XREG2 = a64::x15;
    const arm::Gp XREG3 = a64::x16;
    const arm::Gp XREG4 = a64::x17;
    const arm::Gp XREG5 = a64::x18;
#else
#   define ERTS_HIGHEST_CALLEE_SAVE_XREG 3
#   define ERTS_HIGHEST_CALLER_SAVE_XREG 5
    const arm::Gp XREG0 = a64::x25;
    const arm::Gp XREG1 = a64::x26;
    const arm::Gp XREG2 = a64::x27;
    const arm::Gp XREG3 = a64::x28;

    /*
     * Caller-save X registers. Must be flushed before calling C
     * code.
     */
    const arm::Gp XREG4 = a64::x17;
    const arm::Gp XREG5 = a64::x18;
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
    const arm::Gp TMP7 = a64::x14;
    const arm::Gp TMP8 = a64::x15;
    const arm::Gp TMP9 = a64::x16;

    /*
     * Assume that SUPER_TMP will be destroyed by any helper function.
     */
#ifdef DEBUG
    const arm::Gp SUPER_TMP = a64::x28;
#else
    const arm::Gp SUPER_TMP = a64::x17;
#endif

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

    enum Distance { dShort, dLong };

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
                    erts_exit(ERTS_ABORT_EXIT, "Label %d with name %s is not bound\n",
                              e->id(), e->name());
                } else {
                    erts_exit(ERTS_ABORT_EXIT, "Label %d is not bound\n", e->id());
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

#if defined(NATIVE_ERLANG_STACK)
    constexpr arm::Mem getRuntimeStackRef() const {
        int base = offsetof(ErtsSchedulerRegisters, aux_regs.d.runtime_stack);

        return getSchedulerRegRef(base);
    }
#endif

#if !defined(NATIVE_ERLANG_STACK)
#    ifdef JIT_HARD_DEBUG
    constexpr arm::Mem getInitialSPRef() const {
        int base = offsetof(ErtsSchedulerRegisters, initial_sp);

        return getSchedulerRegRef(base);
    }
#    endif

    constexpr arm::Mem getCPRef() const {
        return arm::Mem(E);
    }
#endif

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

#ifdef NATIVE_ERLANG_STACK
        return arm::Mem(E, index * sizeof(Eterm));
#else
        return arm::Mem(E, (index + CP_SIZE) * sizeof(Eterm));
#endif
    }

    constexpr arm::Mem getCARRef(arm::Gp Src) const {
        return arm::Mem(Src, -TAG_PRIMARY_LIST);
    }

    constexpr arm::Mem getCDRRef(arm::Gp Src,
                                 size_t size = sizeof(UWord)) const {
        return arm::Mem(Src, -TAG_PRIMARY_LIST + sizeof(Eterm));
    }

    void load_x_reg_array(arm::Gp to) {
        int offset =
            offsetof(ErtsSchedulerRegisters, x_reg_array.d);

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
    void erlang_call(Any Target, const arm::Gp &spill) {
#ifdef NATIVE_ERLANG_STACK
        /* We use the Erlang stack as the native stack. We can use a
         * native `call` instruction. */
        emit_assert_erlang_stack();
        emit_assert_redzone_unused();
        aligned_call(Target);
#else
        Label next = a.newLabel();

        /* Save the return CP on the stack. */
        a.adr(SUPER_TMP, next);
        a.str(SUPER_TMP, getCPRef());
        a.ldr(SUPER_TMP, Target);
        a.br(SUPER_TMP);

        a.bind(next);
#endif
    }

    /*
     * Calls the given address in shared fragment, ensuring that the
     * redzone is unused and that the return address forms a valid
     * CP.
     */
    template<typename Any>
    void fragment_call(Any Target) {
        emit_assert_erlang_stack();
        emit_assert_redzone_unused();

#if defined(JIT_HARD_DEBUG) && !defined(NATIVE_ERLANG_STACK)
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
        ERTS_ASSERT(!"NYI!");
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
        emit_assert_runtime_stack();
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

        emit_assert_runtime_stack();

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
        a.add(ARG1, ARG1, offsetof(Export, addresses));
        return arm::Mem(ARG1, CodeIndex, arm::lsl(3));
    }

    /* Discards a continuation pointer, including the frame pointer if
     * applicable. */
    void emit_discard_cp() {
        emit_assert_erlang_stack();
        ERTS_ASSERT(!"NYI");
    }

    void emit_assert_runtime_stack() {
#ifdef JIT_HARD_DEBUG
#endif
    }

    void emit_assert_erlang_stack() {
#ifdef JIT_HARD_DEBUG
#endif
    }

    /* Ensure that E is in a "normal" register that can be stored and
     * otherwise operated on. */
    arm::Gp load_E(arm::Gp to) {
#ifdef NATIVE_ERLANG_STACK
        /* E is in sp, which is not allowed to be used in all instructions. */
        a.mov(to, E);
        return to;
#else
        /* E is already in a "normal" register. */
        return E;
#endif
    }

    enum Update : int {
        eStack = (1 << 0),
        eHeap = (1 << 1),
        eReductions = (1 << 2),
        eCodeIndex = (1 << 3)
    };

    template<int Spec = 0>
    void emit_enter_runtime() {
        emit_assert_erlang_stack();

        ERTS_CT_ASSERT((Spec & (Update::eReductions | Update::eStack |
                                Update::eHeap)) == Spec);

#ifdef NATIVE_ERLANG_STACK
        if (!(Spec & Update::eStack)) {
            a.mov(E_saved, E);
        }
#endif

        if ((Spec & (Update::eHeap | Update::eStack)) ==
            (Update::eHeap | Update::eStack)) {
            /* Store HTOP and E in one go. */
            ERTS_CT_ASSERT(offsetof(Process, stop) - offsetof(Process, htop) ==
                           8);
            arm::Gp E_reg = load_E(SUPER_TMP);
            a.stp(HTOP, E_reg, arm::Mem(c_p, offsetof(Process, htop)));
        } else {
            if ((Spec & Update::eStack)) {
                arm::Gp E_reg = load_E(SUPER_TMP);
                a.str(E_reg, arm::Mem(c_p, offsetof(Process, stop)));
            }
            if (Spec & Update::eHeap) {
                a.str(HTOP, arm::Mem(c_p, offsetof(Process, htop)));
            }
        }

        if (Spec & Update::eReductions) {
            a.str(FCALLS, arm::Mem(c_p, offsetof(Process, fcalls)));
        }

#ifdef NATIVE_ERLANG_STACK
        lea(E, getRuntimeStackRef());
#endif
    }

    template<int Spec = 0>
    void emit_leave_runtime() {
        emit_assert_runtime_stack();

        ERTS_CT_ASSERT((Spec & (Update::eReductions | Update::eStack |
                                Update::eHeap | Update::eCodeIndex)) == Spec);

#ifdef NATIVE_ERLANG_STACK
        if (!(Spec & Update::eStack)) {
            a.mov(E, E_saved);
        }
#endif
        if ((Spec & Update::eStack)) {
#ifdef NATIVE_ERLANG_STACK
            a.ldr(SUPER_TMP, arm::Mem(c_p, offsetof(Process, stop)));
            a.mov(E, SUPER_TMP);
#else
            a.ldr(E, arm::Mem(c_p, offsetof(Process, stop)));
#endif
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
    }

    void emit_test_the_non_value(arm::Gp Reg, Label if_true) {
        if (THE_NON_VALUE == 0) {
            a.cbz(Reg, if_true);
        } else {
            a.cmp(Reg, imm(THE_NON_VALUE));
            a.cond_eq().b(if_true);
        }
    }

    /*
     * Generate the best instruction for setting a register to an
     * immediate value.
     */
    void mov_imm(arm::Gp to, Uint value) {
        if (value == 0) {
            a.mov(to, a64::xzr);
        } else {
            a.mov(to, value);
        }
    }

    void sub(arm::Gp to, arm::Gp src, int64_t val) {
        if (val < 0) {
            a.add(to, src, -val);
        }
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

    void add(arm::Gp to, arm::Gp src, int64_t val) {
        if (val < 0) {
            a.sub(to, src, -val);
        }
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

    /*
     * ARM has no LEA instruction. Implement our own to enable us
     * to use helpers based on getSchedulerRegRef() that return an
     * arm::Mem class.
     */
    void lea(arm::Gp to, arm::Mem mem) {
        ERTS_ASSERT(mem.hasBaseReg());
        ERTS_ASSERT(! mem.hasIndex());
        int64_t offset = mem.offset();
        if (offset == 0) {
            a.mov(to, arm::GpX(mem.baseId()));
        } else {
            add(to, arm::GpX(mem.baseId()), offset);
        }
    }

    /* Load low-numbered registers from the X register array to CPU registers. */
    void load_x_regs() {
        for (int i = 0; i < num_register_backed_xregs; i += 2) {
            arm::Gp reg1 = register_backed_xregs[i];
            arm::Gp reg2 = register_backed_xregs[i+1];
            a.ldp(reg1, reg2, getXRef(i));
        }
    }

    /* Save low-numbered X registers to the X register array. */
    void save_x_regs() {
        for (int i = 0; i < num_register_backed_xregs; i += 2) {
            arm::Gp reg1 = register_backed_xregs[i];
            arm::Gp reg2 = register_backed_xregs[i+1];
            a.stp(reg1, reg2, getXRef(i));
        }
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
    _(arith_eq_shared)                                                         \
    _(bif_nif_epilogue)                                                        \
    _(bif_export_trap)                                                         \
    _(bs_add_shared)                                                           \
    _(bs_size_check_shared)                                                    \
    _(bs_fixed_integer_shared)                                                 \
    _(bs_get_tail_shared)                                                      \
    _(call_bif_shared)                                                         \
    _(call_light_bif_shared)                                                   \
    _(call_nif_early)                                                          \
    _(call_nif_shared)                                                         \
    _(catch_end_shared)                                                        \
    _(dispatch_bif)                                                            \
    _(dispatch_nif)                                                            \
    _(dispatch_return)                                                         \
    _(dispatch_save_calls)                                                     \
    _(error_action_code)                                                       \
    _(export_trampoline)                                                       \
    _(garbage_collect)                                                         \
    _(generic_bp_global)                                                       \
    _(generic_bp_local)                                                        \
    _(debug_bp)                                                                \
    _(handle_error_shared_prologue)                                            \
    _(handle_error_shared)                                                     \
    _(i_bif_body_shared)                                                       \
    _(i_bif_guard_shared)                                                      \
    _(i_func_info_shared)                                                      \
    _(i_load_nif_shared)                                                       \
    _(i_length_guard_shared)                                                   \
    _(i_length_body_shared)                                                    \
    _(i_loop_rec_shared)                                                       \
    _(i_new_small_map_lit_shared)                                              \
    _(i_test_yield_shared)                                                     \
    _(new_map_shared)                                                          \
    _(process_main)                                                            \
    _(update_map_assoc_shared)                                                 \
    _(update_map_exact_guard_shared)                                           \
    _(update_map_exact_body_shared)

/* Labels exported from within process_main */
#define PROCESS_MAIN_LABELS(_)                                                 \
    _(context_switch)                                                          \
    _(context_switch_simplified)                                               \
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
#undef DECL_FUNC

    template<typename T>
    void emit_bitwise_fallback_body(T(*func_ptr), const ErtsCodeMFA *mfa);

    template<typename T>
    void emit_bitwise_fallback_guard(T(*func_ptr));

    arm::Mem emit_i_length_common(Label fail, int state_size);

    void emit_handle_error();

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

    Label floatMax;
    Label floatSignMask;

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

    void emit_is_binary(Label Fail, arm::Gp Src, Label next, Label subbin);

    void emit_get_list(const arm::Gp boxed_ptr,
                       const ArgVal &Hd,
                       const ArgVal &Tl);

    void emit_div_rem(const ArgVal &Fail,
                      const ArgVal &LHS,
                      const ArgVal &RHS,
                      const ErtsCodeMFA *error_mfa);

    void emit_setup_guard_bif(const std::vector<ArgVal> &args,
                              const ArgVal &bif);

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

    void emit_handle_error();
    void emit_handle_error(const ErtsCodeMFA *exp);
    void emit_handle_error(Label I, const ErtsCodeMFA *exp);
    void emit_validate(const ArgVal &arity);
    void emit_bs_skip_bits(const ArgVal &Fail, const ArgVal &Ctx);

    void emit_linear_search(arm::Gp val,
                            const ArgVal &Fail,
                            const std::vector<ArgVal> &args);

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

    template<typename A, typename B>
    void mov_arg(A to, B from) {
        /* We can't move to or from Y registers when we're on the runtime
         * stack, so we'll conservatively disallow all mov_args in the hopes of
         * finding such bugs sooner. */
        emit_assert_erlang_stack();

        arm::Mem flush_dst;
        arm::Gp to_reg = mov_dst_reg(to, &flush_dst);
        mov_to_reg(&to_reg, from);
        if (flush_dst.hasBase()) {
            a.str(to_reg, flush_dst);
        }
    }

    arm::Gp mov_dst_reg(const ArgVal &to, arm::Mem *flush_dst) {
        if (to.getType() == ArgVal::TYPE::x &&
            to.getValue() < num_register_backed_xregs) {
            *flush_dst = arm::Mem();
            return register_backed_xregs[to.getValue()];
        } else {
            *flush_dst = getArgRef(to);
            return SUPER_TMP;
        }
    }

    void mov_to_reg(arm::Gp *to_p, const ArgVal &from) {
        arm::Gp to = *to_p;
        if (from.isLiteral()) {
            make_move_patch(to, literals[from.getValue()].patches);
        } else {
            switch (from.getType()) {
            case ArgVal::TYPE::l:
                a.ldr(to, getFRef(from.getValue()));
                break;
            case ArgVal::TYPE::x:
                {
                    int index = from.getValue();
                    if (index < num_register_backed_xregs) {
                        arm::Gp xreg = register_backed_xregs[index];
                        if (to == SUPER_TMP) {
                            *to_p = xreg;
                        } else {
                            a.mov(to, xreg);
                        }
                    } else {
                        a.ldr(to, getXRef(index));
                    }
                }
                break;
            case ArgVal::TYPE::y:
                a.ldr(to, getYRef(from.getValue()));
                break;
            default:
                mov_imm(to, from.getValue());
                break;
            }
        }
    }
};

void beamasm_update_perf_info(std::string modulename,
                              std::vector<BeamAssembler::AsmRange> &ranges);
