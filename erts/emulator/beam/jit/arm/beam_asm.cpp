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
#include "beam_common.h"
#include "code_ix.h"
#include "export.h"

#if defined(__APPLE__)
#    include <libkern/OSCacheControl.h>
#endif
}

/* Global configuration variables (under the `+J` prefix) */
#ifdef HAVE_LINUX_PERF_SUPPORT
int erts_jit_perf_support;
#endif

/*
 * Special Beam instructions.
 */

ErtsCodePtr beam_apply;
ErtsCodePtr beam_normal_exit;
ErtsCodePtr beam_exit;
ErtsCodePtr beam_export_trampoline;
ErtsCodePtr beam_bif_export_trap;
ErtsCodePtr beam_continue_exit;
ErtsCodePtr beam_save_calls;

/* NOTE These should be the only variables containing trace instructions.
**      Sometimes tests are for the instruction value, and sometimes
**      for the variable reference (one of these), and rogue references
**      will most likely cause chaos.
*/
ErtsCodePtr beam_return_to_trace;   /* OpCode(i_return_to_trace) */
ErtsCodePtr beam_return_trace;      /* OpCode(i_return_trace) */
ErtsCodePtr beam_exception_trace;   /* UGLY also OpCode(i_return_trace) */
ErtsCodePtr beam_return_time_trace; /* OpCode(i_return_time_trace) */

static JitAllocator *jit_allocator;

static BeamGlobalAssembler *bga;
static BeamModuleAssembler *bma;
static CpuInfo cpuinfo;

static void beamasm_init_gdb_jit_info(void);

/*
 * Enter all BIFs into the export table.
 *
 * Note that they will all call the error_handler until their modules have been
 * loaded, which may prevent the system from booting if BIFs from non-preloaded
 * modules are apply/3'd while loading code. Ordinary BIF calls will work fine
 * however since they won't go through export entries.
 */
static void install_bifs(void) {
    typedef Eterm (*bif_func_type)(Process *, Eterm *, ErtsCodePtr);
    int i;

    ASSERT(beam_export_trampoline != NULL);
    ASSERT(beam_save_calls != NULL);

    for (i = 0; i < BIF_SIZE; i++) {
        BifEntry *entry;
        Export *ep;
        int j;

        entry = &bif_table[i];

        ep = erts_export_put(entry->module, entry->name, entry->arity);

        ep->info.op = op_i_func_info_IaaI;
        ep->info.mfa.module = entry->module;
        ep->info.mfa.function = entry->name;
        ep->info.mfa.arity = entry->arity;
        ep->bif_number = i;

        for (j = 0; j < ERTS_NUM_CODE_IX; j++) {
            erts_activate_export_trampoline(ep, j);
        }

        /* Set up a hidden export entry so we can trap to this BIF without
         * it being seen when tracing. */
        erts_init_trap_export(BIF_TRAP_EXPORT(i),
                              entry->module,
                              entry->name,
                              entry->arity,
                              (bif_func_type)entry->f);
    }
}

static JitAllocator *create_allocator(JitAllocator::CreateParams *params) {
    void *test_ro, *test_rw;
    Error err;

    auto *allocator = new JitAllocator(params);

    err = allocator->alloc(&test_ro, &test_rw, 1);
    allocator->release(test_ro);

    if (err == ErrorCode::kErrorOk) {
        return allocator;
    }

    delete allocator;
    return nullptr;
}

static JitAllocator *pick_allocator() {
    JitAllocator::CreateParams single_params;
    single_params.reset();

#if defined(HAVE_LINUX_PERF_SUPPORT)
    /* `perf` has a hard time showing symbols for dual-mapped memory, so we'll
     * use single-mapped memory when enabled. */
    if (erts_jit_perf_support & (BEAMASM_PERF_DUMP | BEAMASM_PERF_MAP)) {
        if (auto *alloc = create_allocator(&single_params)) {
            return alloc;
        }

        ERTS_INTERNAL_ERROR("jit: Failed to allocate executable+writable "
                            "memory. Either allow this or disable the "
                            "'+JPperf' option.");
    }
#endif

#if !defined(VALGRIND)
    /* Default to dual-mapped memory with separate executable and writable
     * regions of the same code. This is required for platforms that enforce
     * W^X, and we prefer it when available to catch errors sooner.
     *
     * `blockSize` is analogous to "carrier size," and we pick something
     * much larger than the default since dual-mapping implies having one
     * file descriptor per block on most platforms. The block sizes do grow
     * over time, but we don't want to waste half a dozen fds just to get to
     * the shell on platforms that are very fd-constrained. */
    JitAllocator::CreateParams dual_params;

    dual_params.reset();
    dual_params.options = JitAllocator::kOptionUseDualMapping,
    dual_params.blockSize = 4 << 20;

    if (auto *alloc = create_allocator(&dual_params)) {
        return alloc;
    } else if (auto *alloc = create_allocator(&single_params)) {
        return alloc;
    }

    ERTS_INTERNAL_ERROR("jit: Cannot allocate executable memory. Use the "
                        "interpreter instead.");
#elif defined(VALGRIND)
    if (auto *alloc = create_allocator(&single_params)) {
        return alloc;
    }

    ERTS_INTERNAL_ERROR("jit: the valgrind emulator requires the ability to "
                        "allocate executable+writable memory.");
#endif
}

void beamasm_init() {
    unsigned label = 1;

    ASSERT(bga == nullptr && bma == nullptr);

    struct operands {
        Eterm name;
        BeamInstr operand;
        ErtsCodePtr *target;
    };

    std::vector<struct operands> operands = {
            {am_exit, op_error_action_code, &beam_exit},
            {am_continue_exit, op_continue_exit, &beam_continue_exit},
            {am_return_trace, op_return_trace, &beam_return_trace},
            {am_return_to_trace, op_i_return_to_trace, &beam_return_to_trace},
            {am_return_time_trace,
             op_i_return_time_trace,
             &beam_return_time_trace},
            {am_exception_trace, op_return_trace, &beam_exception_trace}};

    Eterm mod_name;
    ERTS_DECL_AM(erts_beamasm);
    mod_name = AM_erts_beamasm;

    beamasm_init_perf();
    beamasm_init_gdb_jit_info();

    /*
     * Ensure that commonly used fields in the PCB can be accessed with
     * short instructions. Before removing any of these assertions, please
     * consider the effect it will have on code size and/or performance.
     */

    ERTS_CT_ASSERT(offsetof(Process, htop) < 128);
    ERTS_CT_ASSERT(offsetof(Process, stop) < 128);
    ERTS_CT_ASSERT(offsetof(Process, fcalls) < 128);
    ERTS_CT_ASSERT(offsetof(Process, freason) < 128);
    ERTS_CT_ASSERT(offsetof(Process, fvalue) < 128);

    cpuinfo = CpuInfo::host();

    jit_allocator = pick_allocator();

    bga = new BeamGlobalAssembler(jit_allocator);

    bma = new BeamModuleAssembler(bga, mod_name, 4 + operands.size() * 2);

    for (auto &op : operands) {
        unsigned func_label, entry_label;

        func_label = label++;
        entry_label = label++;

        bma->emit(op_aligned_label_Lt,
                  {ArgVal(ArgVal::i, func_label),
                   ArgVal(ArgVal::u, sizeof(UWord))});
        bma->emit(op_i_func_info_IaaI,
                  {ArgVal(ArgVal::i, func_label),
                   ArgVal(ArgVal::i, am_erts_internal),
                   ArgVal(ArgVal::i, op.name),
                   ArgVal(ArgVal::i, 0)});
        bma->emit(op_aligned_label_Lt,
                  {ArgVal(ArgVal::i, entry_label),
                   ArgVal(ArgVal::u, sizeof(UWord))});
        bma->emit(op.operand, {});

        op.operand = entry_label;
    }

    {
        unsigned func_label, apply_label, normal_exit_label;

        func_label = label++;
        apply_label = label++;
        normal_exit_label = label++;

        bma->emit(op_aligned_label_Lt,
                  {ArgVal(ArgVal::i, func_label),
                   ArgVal(ArgVal::u, sizeof(UWord))});
        bma->emit(op_i_func_info_IaaI,
                  {ArgVal(ArgVal::i, func_label),
                   ArgVal(ArgVal::i, am_erts_internal),
                   ArgVal(ArgVal::i, am_apply),
                   ArgVal(ArgVal::i, 3)});
        bma->emit(op_aligned_label_Lt,
                  {ArgVal(ArgVal::i, apply_label),
                   ArgVal(ArgVal::u, sizeof(UWord))});
        bma->emit(op_i_apply, {});
        bma->emit(op_aligned_label_Lt,
                  {ArgVal(ArgVal::i, normal_exit_label),
                   ArgVal(ArgVal::u, sizeof(UWord))});
        bma->emit(op_normal_exit, {});

        bma->emit(op_int_code_end, {});

        {
            /* We have no need of the module pointers as we use `getCode(...)`
             * for everything, and the code will live as long as the emulator
             * itself. */
            const void *_ignored_exec;
            void *_ignored_rw;
            bma->codegen(jit_allocator, &_ignored_exec, &_ignored_rw);
        }

        beam_apply = bma->getCode(apply_label);
        beam_normal_exit = bma->getCode(normal_exit_label);
    }

    for (auto op : operands) {
        if (op.target) {
            *op.target = bma->getCode(op.operand);
        }
    }

    /* This instruction relies on register contents, and can only be reached
     * from a `call_ext_*`-instruction, hence the lack of a wrapper function. */
    beam_save_calls = (ErtsCodePtr)bga->get_dispatch_save_calls();
    beam_export_trampoline = (ErtsCodePtr)bga->get_export_trampoline();
    beam_bif_export_trap = (ErtsCodePtr)bga->get_bif_export_trap();
}

bool BeamAssembler::hasCpuFeature(uint32_t featureId) {
    return cpuinfo.hasFeature(featureId);
}

void init_emulator(void) {
    install_bifs();
}

void process_main(ErtsSchedulerData *esdp) {
    typedef void (*pmain_type)(ErtsSchedulerData *);

    pmain_type pmain = (pmain_type)bga->get_process_main();
    pmain(esdp);
}

#if defined(DEBUG) || defined(ERTS_ENABLE_LOCK_CHECK)
static Process *erts_debug_schedule(ErtsSchedulerData *esdp,
                                    Process *c_p,
                                    int calls) {
    PROCESS_MAIN_CHK_LOCKS(c_p);
    ERTS_UNREQ_PROC_MAIN_LOCK(c_p);
    ERTS_VERIFY_UNUSED_TEMP_ALLOC(c_p);
    c_p = erts_schedule(esdp, c_p, calls);
    ERTS_VERIFY_UNUSED_TEMP_ALLOC(c_p);
    ERTS_REQ_PROC_MAIN_LOCK(c_p);
    PROCESS_MAIN_CHK_LOCKS(c_p);
    return c_p;
}
#endif

/* void process_main(ErtsSchedulerData *esdp); */
void BeamGlobalAssembler::emit_process_main() {
    Label context_switch_local = a.newLabel(),
          context_switch_simplified_local = a.newLabel(),
          do_schedule_local = a.newLabel(), schedule_next = a.newLabel();

    const arm::Mem start_time_i =
            getSchedulerRegRef(offsetof(ErtsSchedulerRegisters, start_time_i));
    const arm::Mem start_time =
            getSchedulerRegRef(offsetof(ErtsSchedulerRegisters, start_time));

    /* Be kind to debuggers and perf by setting up a proper stack frame. */
    a.stp(a64::x29, a64::x30, arm::Mem(a64::sp, -16).pre());

    /* Allocate the register structure on the stack to allow computing the
     * runtime stack address from it, greatly reducing the cost of stack
     * swapping. */
    a.mov(TMP1, a64::sp);
    sub(TMP1, TMP1, sizeof(ErtsSchedulerRegisters) + ERTS_CACHE_LINE_SIZE);
    a.and_(TMP1, TMP1, imm(~ERTS_CACHE_LINE_MASK));
    a.mov(a64::sp, TMP1);
    a.mov(a64::x29, a64::sp);

    a.str(TMP1, arm::Mem(ARG1, offsetof(ErtsSchedulerData, registers)));

    a.mov(scheduler_registers, a64::sp);

    load_erl_bits_state(ARG1);
    runtime_call<1>(erts_bits_init_state);

    /* Save the initial SP of the thread so that we can verify that it
     * doesn't grow. */
#ifdef JIT_HARD_DEBUG
    a.mov(TMP1, a64::sp);
    a.str(TMP1, getInitialSPRef());
#endif

    a.str(a64::xzr, start_time_i);
    a.str(a64::xzr, start_time);

    mov_imm(c_p, 0);
    mov_imm(FCALLS, 0);
    mov_imm(ARG3, 0); /* Set reds_used for erts_schedule call */

    a.b(schedule_next);

    a.bind(do_schedule_local);
    {
        /* Figure out reds_used. def_arg_reg[5] = REDS_IN */
        a.ldr(TMP1, arm::Mem(c_p, offsetof(Process, def_arg_reg[5])));
        a.sub(ARG3, TMP1, FCALLS);
        a.b(schedule_next);
    }

    /*
     * The *next* instruction pointer is provided in ARG3, and must be preceded
     * by an ErtsCodeMFA.
     */
    a.bind(context_switch_local);
    comment("Context switch, unknown arity/MFA");
    {
        Sint arity_offset = offsetof(ErtsCodeMFA, arity) - sizeof(ErtsCodeMFA);

        a.ldur(TMP1, arm::Mem(ARG3, arity_offset));
        a.str(TMP1, arm::Mem(c_p, offsetof(Process, arity)));

        a.sub(TMP1, ARG3, imm((Uint)sizeof(ErtsCodeMFA)));
        a.str(TMP1, arm::Mem(c_p, offsetof(Process, current)));

        /* !! Fall through !! */
    }

    a.bind(context_switch_simplified_local);
    comment("Context switch, known arity and MFA");
    {
        Label not_exiting = a.newLabel();

#ifdef DEBUG
        Label check_i = a.newLabel();
        /* Check that ARG3 is set to a valid CP. */
        a.tst(ARG3, imm(_CPMASK));
        a.cond_eq().b(check_i);
        a.udf(1);
        a.bind(check_i);
#endif

        a.str(ARG3, arm::Mem(c_p, offsetof(Process, i)));
        a.ldr(TMP1.w(), arm::Mem(c_p, offsetof(Process, state.value)));

        a.tst(TMP1, imm(ERTS_PSFLG_EXITING));
        a.cond_eq().b(not_exiting);
        {
            comment("Process exiting");

            /* We load the beam_exit pointer from memory because it
             * has not yet been set when the global assembler is
             * created. */
            mov_imm(TMP1, (UWord)&beam_exit);
            a.ldr(TMP1, arm::Mem(TMP1));
            a.str(TMP1, arm::Mem(c_p, offsetof(Process, i)));
            a.str(ZERO, arm::Mem(c_p, offsetof(Process, arity)));
            a.str(ZERO, arm::Mem(c_p, offsetof(Process, current)));
            a.b(do_schedule_local);
        }

        a.bind(not_exiting);

        /* Figure out reds_used. def_arg_reg[5] = REDS_IN */
        a.ldr(TMP1, arm::Mem(c_p, offsetof(Process, def_arg_reg[5])));
        a.sub(FCALLS, TMP1, FCALLS);

        comment("Copy out X registers");
        a.mov(ARG1, c_p);
        load_x_reg_array(ARG2);
        runtime_call<2>(copy_out_registers);

        /* Restore reds_used from FCALLS */
        a.mov(ARG3, FCALLS);

        /* !! Fall through !! */
    }

    a.bind(schedule_next);
    comment("schedule_next");

    {
        Label schedule = a.newLabel(), skip_long_schedule = a.newLabel();

        /* ARG3 contains reds_used at this point */

        a.ldr(TMP1, start_time);
        a.cbz(TMP1, schedule);
        {
            a.mov(ARG1, c_p);
            a.ldr(ARG2, start_time);

            /* Spill reds_used in start_time slot */
            a.str(ARG3, start_time);

            a.ldr(ARG3, start_time_i);
            runtime_call<3>(check_monitor_long_schedule);

            /* Restore reds_used */
            a.ldr(ARG3, start_time);
        }

        a.bind(schedule);
        mov_imm(ARG1, 0);
        a.mov(ARG2, c_p);
#if defined(DEBUG) || defined(ERTS_ENABLE_LOCK_CHECK)
        runtime_call<3>(erts_debug_schedule);
#else
        runtime_call<3>(erts_schedule);
#endif
        a.mov(c_p, ARG1);

#ifdef ERTS_MSACC_EXTENDED_STATES
        lea(ARG1,
            arm::Mem(registers,
                     offsetof(ErtsSchedulerRegisters,
                              aux_regs.d.erts_msacc_cache)));
        runtime_call<1>(erts_msacc_update_cache);
#endif

        a.str(ZERO, start_time);
        mov_imm(ARG1, (UWord)&erts_system_monitor_long_schedule);
        a.ldr(TMP1, arm::Mem(ARG1));
        a.cbz(TMP1, skip_long_schedule);

        {
            /* Enable long schedule test */
            runtime_call<0>(erts_timestamp_millis);
            a.str(ARG1, start_time);
            a.ldr(TMP1, arm::Mem(c_p, offsetof(Process, i)));
            a.str(TMP1, start_time_i);
        }

        a.bind(skip_long_schedule);
        comment("skip_long_schedule");

        /* Copy arguments */
        a.mov(ARG1, c_p);
        load_x_reg_array(ARG2);
        runtime_call<2>(copy_in_registers);

        /* Setup reduction counting */
        a.ldr(FCALLS, arm::Mem(c_p, offsetof(Process, fcalls)));
        a.str(FCALLS, arm::Mem(c_p, offsetof(Process, def_arg_reg[5])));

#ifdef DEBUG
        a.str(FCALLS, a64::Mem(c_p, offsetof(Process, debug_reds_in)));
#endif

        comment("check whether save calls is on");
        a.mov(ARG1, c_p);
        mov_imm(ARG2, ERTS_PSD_SAVED_CALLS_BUF);
        runtime_call<2>(erts_psd_get);

        /* Read the active code index, overriding it with
         * ERTS_SAVE_CALLS_CODE_IX when save_calls is enabled (RET != 0). */
        mov_imm(TMP1, (UWord)&the_active_code_index);
        a.ldr(TMP1.w(), arm::Mem(TMP1));
        a.mov(TMP2, imm(ERTS_SAVE_CALLS_CODE_IX));
        a.cmp(ARG1, ZERO);
        a.csel(active_code_ix, TMP1, TMP2, arm::Cond::kEQ);

        /* Start executing the Erlang process. Note that reductions have
         * already been set up above. */
        emit_leave_runtime<Update::eStack | Update::eHeap>(all_xregs);

        /* Check if we are just returning from a dirty nif/bif call and if so we
         * need to do a bit of cleaning up before continuing. */
        a.ldr(ARG1, arm::Mem(c_p, offsetof(Process, i)));

        a.ldr(TMP1, arm::Mem(ARG1));
        a.cmp(TMP1, imm(op_call_nif_WWW));
        a.cond_eq().b(labels[dispatch_nif]);
        a.cmp(TMP1, imm(op_call_bif_W));
        a.cond_eq().b(labels[dispatch_bif]);

        a.br(ARG1);
    }

    /* Processes may jump to the exported entry points below, executing on the
     * Erlang stack when entering. These are separate from the `_local` labels
     * above as we don't want to worry about which stack we're on when the
     * cases overlap. */

    /* `ga->get_context_switch()`
     *
     * The *next* instruction pointer is provided in ARG3, and must be preceded
     * by an ErtsCodeMFA.
     *
     * The X registers are expected to be in CPU registers.
     */
    a.bind(labels[context_switch]);
    {
        emit_enter_runtime<Update::eStack | Update::eHeap>(all_xregs);

        a.b(context_switch_local);
    }

    /* `ga->get_context_switch_simplified()`
     *
     * The next instruction pointer is provided in ARG3, which does not need to
     * point past an ErtsCodeMFA as the process structure has already been
     * updated.
     *
     * The X registers are expected to be in CPU registers.
     */
    a.bind(labels[context_switch_simplified]);
    {
        emit_enter_runtime<Update::eStack | Update::eHeap>(all_xregs);

        a.b(context_switch_simplified_local);
    }

    /* `ga->get_context_switch_simplified_saved_xregs()`
     *
     * The next instruction pointer is provided in ARG3, which does not need to
     * point past an ErtsCodeMFA as the process structure has already been
     * updated.
     *
     * The X registers values are expected to be in the X register array.
     */
    a.bind(labels[context_switch_simplified_saved_xregs]);
    {
        emit_enter_runtime<Update::eStack | Update::eHeap>();

        a.b(context_switch_simplified_local);
    }

    /* `ga->get_do_schedule()`
     *
     * `c_p->i` must be set prior to jumping here.
     *
     * The X registers are expected to be in CPU registers.
     */
    a.bind(labels[do_schedule]);
    {
        emit_enter_runtime<Update::eStack | Update::eHeap>(all_xregs);

        a.b(do_schedule_local);
    }
}

enum jit_actions : uint32_t {
    JIT_NOACTION = 0,
    JIT_REGISTER_FN,
    JIT_UNREGISTER_FN,
};

struct jit_code_entry {
    struct jit_code_entry *next_entry;
    struct jit_code_entry *prev_entry;
    const char *symfile_addr;
    uint64_t symfile_size;
};

struct jit_descriptor {
    uint32_t version;
    jit_actions action_flag;
    struct jit_code_entry *relevant_entry;
    struct jit_code_entry *first_entry;
};

extern "C"
{
    extern void ERTS_NOINLINE __jit_debug_register_code(void);

    /* Make sure to specify the version statically, because the
     * debugger may check the version before we can set it. */
    struct jit_descriptor __jit_debug_descriptor = {1,
                                                    JIT_NOACTION,
                                                    NULL,
                                                    NULL};
} /* extern "C" */

static void beamasm_init_gdb_jit_info(void) {
    Sint symfile_size = sizeof(uint64_t) * 2;
    uint64_t *symfile = (uint64_t *)malloc(symfile_size);
    jit_code_entry *entry;

    symfile[0] = 0;
    symfile[1] = (uint64_t)beam_normal_exit;

    entry = (jit_code_entry *)malloc(sizeof(jit_code_entry));

    /* Add address description */
    entry->symfile_addr = (char *)symfile;
    entry->symfile_size = symfile_size;

    /* Insert into linked list */
    entry->next_entry = __jit_debug_descriptor.first_entry;
    if (entry->next_entry) {
        entry->next_entry->prev_entry = entry;
    } else {
        entry->prev_entry = nullptr;
    }

    /* register with dbg */
    __jit_debug_descriptor.action_flag = JIT_REGISTER_FN;
    __jit_debug_descriptor.first_entry = entry;
    __jit_debug_descriptor.relevant_entry = entry;
    __jit_debug_register_code();
}

void BeamAssembler::update_gdb_jit_info(std::string modulename,
                                        std::vector<AsmRange> &functions) {
    Sint symfile_size = sizeof(uint64_t) * 3 + modulename.size() + 1;

    for (auto fun : functions) {
        symfile_size += sizeof(uint64_t) * 2;
        symfile_size += fun.name.size() + 1;
    }

    char *symfile = (char *)malloc(symfile_size);
    jit_code_entry *entry;

    entry = (jit_code_entry *)malloc(sizeof(jit_code_entry));

    /* Add address description */
    entry->symfile_addr = symfile;
    entry->symfile_size = symfile_size;

    ((uint64_t *)symfile)[0] = functions.size();
    ((uint64_t *)symfile)[1] = code.baseAddress();
    ((uint64_t *)symfile)[2] = (uint64_t)code.codeSize();

    symfile += sizeof(uint64_t) * 3;

    sys_memcpy(symfile, modulename.c_str(), modulename.size() + 1);
    symfile += modulename.size() + 1;

    for (unsigned i = 0; i < functions.size(); i++) {
        ((uint64_t *)symfile)[0] = (uint64_t)functions[i].start;
        ((uint64_t *)symfile)[1] = (uint64_t)functions[i].stop;

        ASSERT(functions[i].start <= functions[i].stop);

        symfile += sizeof(uint64_t) * 2;

        sys_memcpy(symfile,
                   functions[i].name.c_str(),
                   functions[i].name.size() + 1);
        symfile += functions[i].name.size() + 1;
    }

    ASSERT(symfile_size == (symfile - entry->symfile_addr));

    /* Insert into linked list */
    entry->next_entry = __jit_debug_descriptor.first_entry;
    if (entry->next_entry) {
        entry->next_entry->prev_entry = entry;
    } else {
        entry->prev_entry = nullptr;
    }

    /* register with dbg */
    __jit_debug_descriptor.action_flag = JIT_REGISTER_FN;
    __jit_debug_descriptor.first_entry = entry;
    __jit_debug_descriptor.relevant_entry = entry;
    __jit_debug_register_code();
}

extern "C"
{
    int erts_beam_jump_table(void) {
#if defined(NO_JUMP_TABLE)
        return 0;
#else
        return 1;
#endif
    }

    void *beamasm_new_assembler(Eterm mod, int num_labels, int num_functions) {
        return new BeamModuleAssembler(bga, mod, num_labels, num_functions);
    }

    int beamasm_emit(void *instance, unsigned specific_op, BeamOp *op) {
        BeamModuleAssembler *ba = static_cast<BeamModuleAssembler *>(instance);
        const std::vector<ArgVal> args(&op->a[0], &op->a[op->arity]);

        return ba->emit(specific_op, args);
    }

    void beamasm_emit_call_nif(const ErtsCodeInfo *info,
                               void *normal_fptr,
                               void *lib,
                               void *dirty_fptr,
                               char *buff,
                               unsigned buff_len) {
        BeamModuleAssembler ba(bga, info->mfa.module, 3);

        ba.emit(op_aligned_label_Lt,
                {ArgVal(ArgVal::i, 1), ArgVal(ArgVal::u, sizeof(UWord))});
        ba.emit(op_i_func_info_IaaI,
                {ArgVal(ArgVal::i, 1),
                 ArgVal(ArgVal::i, info->mfa.module),
                 ArgVal(ArgVal::i, info->mfa.function),
                 ArgVal(ArgVal::i, info->mfa.arity)});
        ba.emit(op_aligned_label_Lt,
                {ArgVal(ArgVal::i, 2), ArgVal(ArgVal::u, sizeof(UWord))});
        ba.emit(op_i_breakpoint_trampoline, {});
        ba.emit(op_call_nif_WWW,
                {ArgVal(ArgVal::i, (BeamInstr)normal_fptr),
                 ArgVal(ArgVal::i, (BeamInstr)lib),
                 ArgVal(ArgVal::i, (BeamInstr)dirty_fptr)});

        ba.codegen(buff, buff_len);

        /* FIXME: break out into a helper */
#if defined(__APPLE__)
        sys_icache_invalidate(buff, buff_len);
#elif defined(__GNUC__)
        __builtin___clear_cache(buff, buff + buff_len);
#endif
    }

    void beamasm_emit_call_bif(const ErtsCodeInfo *info,
                               Eterm (*bif)(BIF_ALIST),
                               char *buff,
                               unsigned buff_len) {
        BeamModuleAssembler ba(bga, info->mfa.module, 3);

        ba.emit(op_aligned_label_Lt,
                {ArgVal(ArgVal::i, 1), ArgVal(ArgVal::u, sizeof(UWord))});
        ba.emit(op_i_func_info_IaaI,
                {ArgVal(ArgVal::i, 1),
                 ArgVal(ArgVal::i, info->mfa.module),
                 ArgVal(ArgVal::i, info->mfa.function),
                 ArgVal(ArgVal::i, info->mfa.arity)});
        ba.emit(op_aligned_label_Lt,
                {ArgVal(ArgVal::i, 2), ArgVal(ArgVal::u, sizeof(UWord))});
        ba.emit(op_i_breakpoint_trampoline, {});
        ba.emit(op_call_bif_W, {ArgVal(ArgVal::i, (BeamInstr)bif)});

        ba.codegen(buff, buff_len);

        /* FIXME: break out into a helper */
#if defined(__APPLE__)
        sys_icache_invalidate(buff, buff_len);
#elif defined(__GNUC__)
        __builtin___clear_cache(buff, buff + buff_len);
#endif
    }

    void beamasm_delete_assembler(void *instance) {
        BeamModuleAssembler *ba = static_cast<BeamModuleAssembler *>(instance);
        delete ba;
    }

    void beamasm_purge_module(const void *native_module_exec,
                              void *native_module_rw) {
        ASSERT(native_module_exec != native_module_rw);

        jit_allocator->release(const_cast<void *>(native_module_exec));
    }

    ErtsCodePtr beamasm_get_code(void *instance, int label) {
        BeamModuleAssembler *ba = static_cast<BeamModuleAssembler *>(instance);
        return reinterpret_cast<ErtsCodePtr>(ba->getCode(label));
    }

    const byte *beamasm_get_rodata(void *instance, char *label) {
        BeamModuleAssembler *ba = static_cast<BeamModuleAssembler *>(instance);
        return reinterpret_cast<const byte *>(ba->getCode(label));
    }

    void beamasm_embed_rodata(void *instance,
                              const char *labelName,
                              const char *buff,
                              size_t size) {
        BeamModuleAssembler *ba = static_cast<BeamModuleAssembler *>(instance);
        if (size) {
            ba->embed_rodata(labelName, buff, size);
        }
    }

    void beamasm_embed_bss(void *instance, char *labelName, size_t size) {
        BeamModuleAssembler *ba = static_cast<BeamModuleAssembler *>(instance);
        if (size) {
            ba->embed_bss(labelName, size);
        }
    }

    void beamasm_codegen(void *instance,
                         const void **native_module_exec,
                         void **native_module_rw,
                         const BeamCodeHeader *in_hdr,
                         const BeamCodeHeader **out_exec_hdr,
                         BeamCodeHeader **out_rw_hdr) {
        BeamModuleAssembler *ba = static_cast<BeamModuleAssembler *>(instance);

        ba->codegen(jit_allocator,
                    native_module_exec,
                    native_module_rw,
                    in_hdr,
                    out_exec_hdr,
                    out_rw_hdr);
    }

    Uint beamasm_get_header(void *instance, const BeamCodeHeader **hdr) {
        BeamModuleAssembler *ba = static_cast<BeamModuleAssembler *>(instance);

        *hdr = ba->getCodeHeader();

        return ba->getCodeSize();
    }

    char *beamasm_get_base(void *instance) {
        BeamModuleAssembler *ba = static_cast<BeamModuleAssembler *>(instance);
        return (char *)ba->getBaseAddress();
    }

    size_t beamasm_get_offset(void *instance) {
        BeamModuleAssembler *ba = static_cast<BeamModuleAssembler *>(instance);
        return ba->getOffset();
    }

    const ErtsCodeInfo *beamasm_get_on_load(void *instance) {
        BeamModuleAssembler *ba = static_cast<BeamModuleAssembler *>(instance);
        return ba->getOnLoad();
    }

    unsigned int beamasm_patch_catches(void *instance, char *rw_base) {
        BeamModuleAssembler *ba = static_cast<BeamModuleAssembler *>(instance);
        return ba->patchCatches(rw_base);
    }

    void beamasm_patch_import(void *instance,
                              char *rw_base,
                              int index,
                              BeamInstr import) {
        BeamModuleAssembler *ba = static_cast<BeamModuleAssembler *>(instance);
        ba->patchImport(rw_base, index, import);
    }

    void beamasm_patch_literal(void *instance,
                               char *rw_base,
                               int index,
                               Eterm lit) {
        BeamModuleAssembler *ba = static_cast<BeamModuleAssembler *>(instance);
        ba->patchLiteral(rw_base, index, lit);
    }

    void beamasm_patch_lambda(void *instance,
                              char *rw_base,
                              int index,
                              BeamInstr fe) {
        BeamModuleAssembler *ba = static_cast<BeamModuleAssembler *>(instance);
        ba->patchLambda(rw_base, index, fe);
    }

    void beamasm_patch_strings(void *instance,
                               char *rw_base,
                               const byte *string_table) {
        BeamModuleAssembler *ba = static_cast<BeamModuleAssembler *>(instance);
        ba->patchStrings(rw_base, string_table);
    }
}
