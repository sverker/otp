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

#include <algorithm>
#include "beam_asm.hpp"

extern "C"
{
#include "erl_bif_table.h"
#include "big.h"
#include "beam_catches.h"
#include "beam_common.h"
#include "code_ix.h"
}

using namespace asmjit;

/* Helpers */

void BeamModuleAssembler::emit_error(int reason) {
    mov_imm(TMP1, reason);
    a.str(TMP1, arm::Mem(c_p, offsetof(Process, freason)));
    emit_raise_exception();
}

void BeamModuleAssembler::emit_gc_test(const ArgVal &Ns,
                                       const ArgVal &Nh,
                                       const ArgVal &Live) {
    int32_t bytes_needed =
            (Ns.getValue() + Nh.getValue() + S_RESERVED) * sizeof(Eterm);
    Label after_gc_check = a.newLabel();

    add(ARG3, HTOP, bytes_needed);
    a.cmp(ARG3, E);
    a.cond_ls().b(after_gc_check);

    mov_imm(ARG4, Live.getValue());
    fragment_call(ga->get_garbage_collect());

    a.bind(after_gc_check);
}

#if defined(DEBUG) && defined(JIT_HARD_DEBUG)
static void validate_term(Eterm term) {
    if (is_boxed(term)) {
        Eterm header = *boxed_val(term);

        if (header_is_bin_matchstate(header)) {
            return;
        }
    }

    size_object_x(term, nullptr);
}
#endif

void BeamModuleAssembler::emit_validate(const ArgVal &arity) {
#ifdef DEBUG
    Label next = a.newLabel(), crash = a.newLabel();

    /* Crash if the Erlang heap is not word-aligned */
    a.tst(HTOP, imm(sizeof(Eterm) - 1));
    a.cond_ne().b(crash);

    /* Crash if the Erlang stack is not word-aligned */
    a.tst(E, imm(sizeof(Eterm) - 1));
    a.cond_ne().b(crash);

    /* Crash if we've overrun the stack */
    lea(TMP1, arm::Mem(E, -(int32_t)(S_REDZONE * sizeof(Eterm))));
    a.cmp(HTOP, TMP1);
    a.cond_hi().b(crash);

    a.b(next);

    a.bind(crash);
    a.udf(0xbad);
    a.bind(next);

#    ifdef JIT_HARD_DEBUG
    emit_enter_runtime();

    for (unsigned i = 0; i < arity.getValue(); i++) {
        a.mov(ARG1, register_backed_xregs[i]);
        runtime_call<1>(beam_jit_validate_term);
    }

    emit_leave_runtime();
#    endif

#endif
}

/* Instrs */

void BeamModuleAssembler::emit_i_validate(const ArgVal &Arity) {
    emit_validate(Arity);
}

void BeamModuleAssembler::emit_allocate_heap(const ArgVal &NeedStack,
                                             const ArgVal &NeedHeap,
                                             const ArgVal &Live) {
    ASSERT(NeedStack.getType() == ArgVal::TYPE::u);
    ASSERT(NeedStack.getValue() <= MAX_REG);
    ArgVal needed = NeedStack;

    emit_gc_test(needed, NeedHeap, Live);

    if (needed.getValue() > 0) {
        sub(E, E, needed.getValue() * sizeof(Eterm));
    }
}

void BeamModuleAssembler::emit_allocate(const ArgVal &NeedStack,
                                        const ArgVal &Live) {
    emit_allocate_heap(NeedStack, ArgVal(ArgVal::TYPE::u, 0), Live);
}

void BeamModuleAssembler::emit_deallocate(const ArgVal &Deallocate) {
    ASSERT(Deallocate.getType() == ArgVal::TYPE::u);
    ASSERT(Deallocate.getValue() <= 1023);

    if (Deallocate.getValue() > 0) {
        add(E, E, Deallocate.getValue() * sizeof(Eterm));
    }
}

void BeamModuleAssembler::emit_test_heap(const ArgVal &Nh, const ArgVal &Live) {
    emit_gc_test(ArgVal(ArgVal::u, 0), Nh, Live);
}

void BeamModuleAssembler::emit_normal_exit() {
    /* This is implictly global; it does not normally appear in modules and
     * doesn't require size optimization. */

    emit_enter_runtime<Update::eReductions | Update::eStack | Update::eHeap>();
    emit_proc_lc_unrequire();

    mov_imm(TMP1, EXC_NORMAL);
    a.str(TMP1, arm::Mem(c_p, offsetof(Process, freason)));
    a.str(ZERO, arm::Mem(c_p, offsetof(Process, arity)));
    a.mov(ARG1, c_p);
    mov_imm(ARG2, am_normal);
    runtime_call<2>(erts_do_exit_process);

    emit_proc_lc_require();
    emit_leave_runtime<Update::eReductions | Update::eStack | Update::eHeap>();

    abs_jmp(ga->get_do_schedule());
}

void BeamModuleAssembler::emit_continue_exit() {
    /* This is implictly global; it does not normally appear in modules and
     * doesn't require size optimization. */

    emit_enter_runtime<Update::eReductions | Update::eStack | Update::eHeap>();
    emit_proc_lc_unrequire();

    a.mov(ARG1, c_p);
    runtime_call<1>(erts_continue_exit_process);

    emit_proc_lc_require();
    emit_leave_runtime<Update::eReductions | Update::eStack | Update::eHeap>();

    abs_jmp(ga->get_do_schedule());
}

/* This is an alias for handle_error */
void BeamModuleAssembler::emit_error_action_code() {
    abs_jmp(ga->get_error_action_code());
}

/* Psuedo-instruction for signalling lambda load errors. Never actually runs. */
void BeamModuleAssembler::emit_i_lambda_error(const ArgVal &Dummy) {
    emit_nyi("emit_i_lambda_error");
}

void BeamModuleAssembler::emit_i_make_fun3(const ArgVal &Fun,
                                           const ArgVal &Dst,
                                           const ArgVal &NumFree,
                                           const std::vector<ArgVal> &env) {
    size_t num_free = env.size();
    ASSERT(NumFree.getValue() == num_free);

    mov_arg(ARG3, NumFree);

    emit_enter_runtime<Update::eHeap>();

    a.mov(ARG1, c_p);
    make_move_patch(ARG2, lambdas[Fun.getValue()].patches);
    runtime_call<3>(new_fun_thing);

    emit_leave_runtime<Update::eHeap>();

    comment("Move fun environment");
    for (unsigned i = 0; i < num_free; i++) {
        mov_arg(arm::Mem(ARG1, offsetof(ErlFunThing, env) + i * sizeof(Eterm)),
                env[i]);
    }

    comment("Create boxed ptr");
    JitRegister dst = init_dst_reg(Dst, TMP1);
    a.orr(dst.reg, ARG1, imm(TAG_PRIMARY_BOXED));
    flush_reg(dst);
}

void BeamModuleAssembler::emit_get_list(const ArgVal &Src,
                                        const ArgVal &Hd,
                                        const ArgVal &Tl) {
    JitRegister src = init_src_reg(Src, TMP1);
    JitRegister hd = init_dst_reg(Hd, TMP2);
    JitRegister tl = init_dst_reg(Tl, TMP3);
    arm::Gp cons_ptr = emit_ptr_val(TMP1, src.reg);

    /* The `ldp` instruction does not accept a negative offset, so we
     * will need subtract the LIST tag beforehand. (This also nicely
     * take care of the potential overwriting issue when Src == Hd.) */
    a.sub(TMP1, cons_ptr, imm(TAG_PRIMARY_LIST));
    a.ldp(hd.reg, tl.reg, arm::Mem(TMP1));
    flush_reg(hd);
    flush_reg(tl);
}

void BeamModuleAssembler::emit_get_hd(const ArgVal &Src, const ArgVal &Hd) {
    JitRegister src = init_src_reg(Src, TMP1);
    JitRegister hd = init_dst_reg(Hd, TMP2);
    arm::Gp cons_ptr = emit_ptr_val(TMP1, src.reg);

    a.ldur(hd.reg, getCARRef(cons_ptr));
    flush_reg(hd);
}

void BeamModuleAssembler::emit_get_tl(const ArgVal &Src, const ArgVal &Tl) {
    JitRegister src = init_src_reg(Src, TMP1);
    JitRegister tl = init_dst_reg(Tl, TMP2);
    arm::Gp cons_ptr = emit_ptr_val(TMP1, src.reg);

    a.ldur(tl.reg, getCDRRef(cons_ptr));
    flush_reg(tl);
}

void BeamModuleAssembler::emit_i_get(const ArgVal &Src, const ArgVal &Dst) {
    emit_nyi("emit_i_get");
}

void BeamModuleAssembler::emit_i_get_hash(const ArgVal &Src,
                                          const ArgVal &Hash,
                                          const ArgVal &Dst) {
    emit_nyi("emit_i_get_hash");
}

/* Store the pointer to a tuple in ARG2. Remove any LITERAL_PTR tag. */
void BeamModuleAssembler::emit_load_tuple_ptr(const ArgVal &Term) {
    mov_arg(ARG2, Term);
    (void)emit_ptr_val(ARG2, ARG2);
}

#ifdef DEBUG
/* Emit an assertion to ensure that tuple_reg points into the same
 * tuple as Src. */
void BeamModuleAssembler::emit_tuple_assertion(const ArgVal &Src,
                                               arm::Gp tuple_reg) {
    Label ok = a.newLabel(), fatal = a.newLabel();
    ASSERT(tuple_reg != TMP1);
    mov_arg(TMP1, Src);
    emit_is_boxed(fatal, TMP1);
    (void)emit_ptr_val(TMP1, TMP1);
    a.cmp(TMP1, tuple_reg);
    a.cond_eq().b(ok);

    a.bind(fatal);
    a.udf(0xaaaa);
    a.bind(ok);
}
#endif

/* Fetch an element from the tuple pointed to by the boxed pointer
 * in ARG2. */
void BeamModuleAssembler::emit_i_get_tuple_element(const ArgVal &Src,
                                                   const ArgVal &Element,
                                                   const ArgVal &Dst) {
#ifdef DEBUG
    emit_tuple_assertion(Src, ARG2);
#endif

    JitRegister dst = init_dst_reg(Dst, TMP1);
    ldur(dst.reg, emit_boxed_val(ARG2, Element.getValue()));
    flush_reg(dst);
}

void BeamModuleAssembler::emit_init(const ArgVal &Y) {
    mov_imm(TMP1, NIL);
    a.str(TMP1, getArgRef(Y));
}

void BeamModuleAssembler::emit_init_yregs(const ArgVal &Size,
                                          const std::vector<ArgVal> &args) {
    unsigned count = Size.getValue();
    ASSERT(count == args.size());

    unsigned i = 0;

    mov_imm(TMP1, NIL);

    while (i < count) {
        unsigned slots = 1;
        unsigned first_y = args.at(i).getValue();

        while (i + slots < count) {
            ArgVal current_y = args.at(i + slots);
            if (first_y + slots != current_y.getValue()) {
                break;
            }
            slots++;
        }
        i += slots;

        /*
         * Now first_y is the number of the first y register to be initialized
         * and slots is the number of y registers to be initialized.
         */

        while (slots >= 2) {
            a.stp(TMP1, TMP1, getYRef(first_y));
            first_y += 2;
            slots -= 2;
        }
        if (slots == 1) {
            a.str(TMP1, getYRef(first_y));
        }
    }
}

void BeamModuleAssembler::emit_trim(const ArgVal &Words,
                                    const ArgVal &Remaining) {
    ASSERT(Words.getType() == ArgVal::TYPE::u);
    ASSERT(Words.getValue() <= 1023);

    if (Words.getValue() > 0) {
        add(E, E, Words.getValue() * sizeof(Eterm));
    }
}

void BeamModuleAssembler::emit_i_move(const ArgVal &Src, const ArgVal &Dst) {
    mov_arg(Dst, Src);
}

void BeamModuleAssembler::emit_swap(const ArgVal &R1, const ArgVal &R2) {
    if (isRegisterBacked(R1)) {
        JitRegister r1 = init_src_reg(R1, ZERO);
        mov_arg(TMP1, R2);
        mov_arg(R2, R1);
        a.mov(r1.reg, TMP1);
    } else if (isRegisterBacked(R2)) {
        return emit_swap(R2, R1);
    } else {
        switch (ArgVal::register_relation(R1, R2)) {
        case ArgVal::Relation::consecutive:
            a.ldp(TMP1, TMP2, getArgRef(R1));
            a.stp(TMP2, TMP1, getArgRef(R1));
            break;
        case ArgVal::Relation::reverse_consecutive:
            a.ldp(TMP1, TMP2, getArgRef(R2));
            a.stp(TMP2, TMP1, getArgRef(R2));
            break;
        case ArgVal::Relation::none:
            a.ldr(TMP1, getArgRef(R1));
            a.ldr(TMP2, getArgRef(R2));
            a.str(TMP1, getArgRef(R2));
            a.str(TMP2, getArgRef(R1));
            break;
        }
    }
}

void BeamModuleAssembler::emit_node(const ArgVal &Dst) {
    mov_imm(TMP1, (UWord)&erts_this_node);
    a.ldr(TMP1, arm::Mem(TMP1));
    mov_arg(Dst, arm::Mem(TMP1, offsetof(ErlNode, sysname)));
}

void BeamModuleAssembler::emit_put_list(const ArgVal &Hd,
                                        const ArgVal &Tl,
                                        const ArgVal &Dst) {
    JitRegister dst = init_dst_reg(Dst, TMP3);
    JitRegister hd = init_src_reg(Hd, TMP1);
    JitRegister tl = init_src_reg(Tl, TMP2);
    a.stp(hd.reg, tl.reg, arm::Mem(HTOP).post(2 * sizeof(Eterm)));
    a.sub(dst.reg, HTOP, imm(2 * sizeof(Eterm) - TAG_PRIMARY_LIST));
    flush_reg(dst);
}

void BeamModuleAssembler::emit_put_tuple2(const ArgVal &Dst,
                                          const ArgVal &Arity,
                                          const std::vector<ArgVal> &args) {
    size_t size = args.size();
    ASSERT(arityval(Arity.getValue()) == size);

    a.add(TMP1, HTOP, TAG_PRIMARY_BOXED);

    comment("Move arity word");

    mov_arg(arm::Mem(HTOP).post(sizeof(Eterm)), Arity);
    comment("Move tuple data");
    for (unsigned i = 0; i < size; i++) {
        mov_arg(arm::Mem(HTOP).post(sizeof(Eterm)), args[i]);
    }

    comment("Store boxed ptr");
    JitRegister ptr = init_dst_reg(Dst, TMP1);
    copy_reg(ptr, JitRegister(TMP1));
    flush_reg(ptr);
}

void BeamModuleAssembler::emit_self(const ArgVal &Dst) {
    mov_arg(Dst, arm::Mem(c_p, offsetof(Process, common.id)));
}

void BeamModuleAssembler::emit_set_tuple_element(const ArgVal &Element,
                                                 const ArgVal &Tuple,
                                                 const ArgVal &Offset) {
    JitRegister tuple = init_src_reg(Tuple, TMP1);
    JitRegister element = init_src_reg(Element, TMP2);
    arm::Gp boxed_ptr = emit_ptr_val(TMP1, tuple.reg);
    arm::Mem boxed_val = emit_boxed_val(boxed_ptr, Offset.getValue());

    stur(element.reg, boxed_val);
}

void BeamModuleAssembler::emit_is_nonempty_list(const ArgVal &Fail,
                                                const ArgVal &Src) {
    JitRegister list_ptr = init_src_reg(Src, TMP1);
    const int bitNumber = 1;

    ERTS_CT_ASSERT(_TAG_PRIMARY_MASK - TAG_PRIMARY_LIST == (1 << bitNumber));
    a.tbnz(list_ptr.reg, bitNumber, labels[Fail.getValue()]);
}

void BeamModuleAssembler::emit_jump(const ArgVal &Fail) {
    a.b(labels[Fail.getValue()]);
}

void BeamModuleAssembler::emit_is_atom(const ArgVal &Fail, const ArgVal &Src) {
    JitRegister src = init_src_reg(Src, TMP1);

    a.and_(TMP1, src.reg, imm(_TAG_IMMED2_MASK));
    a.cmp(TMP1, imm(_TAG_IMMED2_ATOM));
    a.cond_ne().b(labels[Fail.getValue()]);
}

void BeamModuleAssembler::emit_is_boolean(const ArgVal &Fail,
                                          const ArgVal &Src) {
    /* Since am_true and am_false differ by a single bit, we can simplify the
     * check by clearing said bit and comparing against the lesser one. */
    ERTS_CT_ASSERT(am_false == make_atom(0));
    ERTS_CT_ASSERT(am_true == make_atom(1));

    JitRegister src = init_src_reg(Src, TMP1);
    a.and_(TMP1, src.reg, imm(~(am_true & ~_TAG_IMMED1_MASK)));
    a.cmp(TMP1, imm(am_false));
    a.cond_ne().b(labels[Fail.getValue()]);
}

arm::Gp BeamModuleAssembler::emit_is_binary(Label fail,
                                            const ArgVal &Src,
                                            Label next,
                                            Label subbin) {
    JitRegister src = init_src_reg(Src, ARG1);

    emit_is_boxed(fail, src.reg);

    arm::Gp boxed_ptr = emit_ptr_val(ARG1, src.reg);
    a.ldur(TMP1, emit_boxed_val(boxed_ptr));
    a.and_(TMP1, TMP1, imm(_TAG_HEADER_MASK));
    a.cmp(TMP1, imm(_TAG_HEADER_SUB_BIN));
    a.cond_eq().b(subbin);

    ERTS_CT_ASSERT(_TAG_HEADER_REFC_BIN + 4 == _TAG_HEADER_HEAP_BIN);
    a.and_(TMP1, TMP1, imm(~4));
    a.cmp(TMP1, imm(_TAG_HEADER_REFC_BIN));
    a.cond_eq().b(next);
    a.b(fail);

    return boxed_ptr;
}

void BeamModuleAssembler::emit_is_binary(const ArgVal &Fail,
                                         const ArgVal &Src) {
    Label next = a.newLabel(), subbin = a.newLabel();
    arm::Gp boxed_ptr;

    boxed_ptr = emit_is_binary(labels[Fail.getValue()], Src, next, subbin);

    a.bind(subbin);
    {
        /* emit_is_binary() has already removed the literal tag (if
         * applicable) from the copy of Src. */
        a.ldrb(TMP1.w(),
               emit_boxed_val(boxed_ptr, offsetof(ErlSubBin, bitsize)));
        a.cbnz(TMP1, labels[Fail.getValue()]);
    }

    a.bind(next);
}

void BeamModuleAssembler::emit_is_bitstring(const ArgVal &Fail,
                                            const ArgVal &Src) {
    Label next = a.newLabel();
    (void)emit_is_binary(labels[Fail.getValue()], Src, next, next);

    a.bind(next);
}

void BeamModuleAssembler::emit_is_float(const ArgVal &Fail, const ArgVal &Src) {
    Label next = a.newLabel();
    Label fail = labels[Fail.getValue()];
    JitRegister src = init_src_reg(Src, TMP1);

    emit_is_boxed(fail, src.reg);

    arm::Gp boxed_ptr = emit_ptr_val(TMP1, src.reg);
    a.ldur(TMP1, emit_boxed_val(boxed_ptr));

    a.cmp(TMP1, imm(HEADER_FLONUM));
    a.cond_ne().b(fail);

    a.bind(next);
}

void BeamModuleAssembler::emit_is_function(const ArgVal &Fail,
                                           const ArgVal &Src) {
    emit_nyi("emit_is_function");
}

void BeamModuleAssembler::emit_is_function2(const ArgVal &Fail,
                                            const ArgVal &Src,
                                            const ArgVal &Arity) {
    emit_nyi("emit_is_function2");
}

void BeamModuleAssembler::emit_is_integer(const ArgVal &Fail,
                                          const ArgVal &Src) {
    Label next = a.newLabel();
    Label fail = labels[Fail.getValue()];
    JitRegister src = init_src_reg(Src, TMP1);

    a.and_(TMP2, src.reg, imm(_TAG_IMMED1_MASK));
    a.cmp(TMP2, imm(_TAG_IMMED1_SMALL));
    a.cond_eq().b(next);

    emit_is_boxed(fail, TMP2);

    arm::Gp boxed_ptr = emit_ptr_val(TMP1, src.reg);
    a.ldur(TMP1, emit_boxed_val(boxed_ptr));
    and_(TMP1, TMP1, _TAG_HEADER_MASK - _BIG_SIGN_BIT);
    a.cmp(TMP1, imm(_TAG_HEADER_POS_BIG));
    a.cond_ne().b(fail);

    a.bind(next);
}

void BeamModuleAssembler::emit_is_list(const ArgVal &Fail, const ArgVal &Src) {
    JitRegister src = init_src_reg(Src, TMP1);

    a.tst(src.reg, imm(_TAG_PRIMARY_MASK - TAG_PRIMARY_LIST));
    a.mov(TMP2, NIL);
    a.ccmp(src.reg, TMP2, 4, arm::Cond::kNE);
    a.cond_ne().b(labels[Fail.getValue()]);
}

void BeamModuleAssembler::emit_is_map(const ArgVal &Fail, const ArgVal &Src) {
    JitRegister src = init_src_reg(Src, TMP1);
    Label fail = labels[Fail.getValue()];

    emit_is_boxed(fail, src.reg);
    arm::Gp boxed_ptr = emit_ptr_val(TMP1, src.reg);
    a.ldur(TMP1, emit_boxed_val(boxed_ptr));
    a.and_(TMP1, TMP1, imm(_TAG_HEADER_MASK));
    a.cmp(TMP1, imm(_TAG_HEADER_MAP));
    a.cond_ne().b(fail);
}

void BeamModuleAssembler::emit_is_nil(const ArgVal &Fail, const ArgVal &Src) {
    JitRegister src = init_src_reg(Src, TMP1);
    a.cmp(src.reg, imm(NIL));
    a.cond_ne().b(labels[Fail.getValue()]);
}

void BeamModuleAssembler::emit_is_number(const ArgVal &Fail,
                                         const ArgVal &Src) {
    Label next = a.newLabel();
    Label fail = labels[Fail.getValue()];
    JitRegister src = init_src_reg(Src, TMP1);

    a.and_(TMP2, src.reg, imm(_TAG_IMMED1_MASK));
    a.cmp(TMP2, imm(_TAG_IMMED1_SMALL));
    a.cond_eq().b(next);

    emit_is_boxed(fail, TMP2);

    arm::Gp boxed_ptr = emit_ptr_val(TMP1, src.reg);
    a.ldur(TMP1, emit_boxed_val(boxed_ptr));
    and_(TMP2, TMP1, _TAG_HEADER_MASK - _BIG_SIGN_BIT);
    a.cmp(TMP2, imm(_TAG_HEADER_POS_BIG));

    a.mov(TMP3, imm(HEADER_FLONUM));
    a.ccmp(TMP1, TMP3, 4, arm::Cond::kNE);
    a.cond_ne().b(fail);

    a.bind(next);
}

void BeamModuleAssembler::emit_is_pid(const ArgVal &Fail, const ArgVal &Src) {
    Label next = a.newLabel();
    Label fail = labels[Fail.getValue()];
    JitRegister src = init_src_reg(Src, TMP1);

    a.and_(TMP2, src.reg, imm(_TAG_IMMED1_MASK));
    a.cmp(TMP2, imm(_TAG_IMMED1_PID));
    a.cond_eq().b(next);

    /* Reuse TMP2 as the important bits are still available. */
    emit_is_boxed(fail, TMP2);

    arm::Gp boxed_ptr = emit_ptr_val(TMP1, src.reg);
    a.ldur(TMP2, emit_boxed_val(boxed_ptr));
    a.and_(TMP2, TMP2, imm(_TAG_HEADER_MASK));
    a.cmp(TMP2, imm(_TAG_HEADER_EXTERNAL_PID));
    a.cond_ne().b(fail);

    a.bind(next);
}

void BeamModuleAssembler::emit_is_port(const ArgVal &Fail, const ArgVal &Src) {
    emit_nyi("emit_is_port");
}

void BeamModuleAssembler::emit_is_reference(const ArgVal &Fail,
                                            const ArgVal &Src) {
    Label next = a.newLabel();
    Label fail = labels[Fail.getValue()];
    JitRegister src = init_src_reg(Src, TMP1);

    emit_is_boxed(fail, src.reg);
    arm::Gp boxed_ptr = emit_ptr_val(TMP1, src.reg);
    a.ldur(TMP1, emit_boxed_val(boxed_ptr));
    a.and_(TMP1, TMP1, imm(_TAG_HEADER_MASK));
    a.cmp(TMP1, imm(_TAG_HEADER_EXTERNAL_REF));
    a.ccmp(TMP1, imm(_TAG_HEADER_REF), 4, arm::Cond::kNE);
    a.cond_ne().b(fail);

    a.bind(next);
}

/* Note: This instruction leaves the pointer to the tuple in ARG2. */
void BeamModuleAssembler::emit_i_is_tagged_tuple(const ArgVal &Fail,
                                                 const ArgVal &Src,
                                                 const ArgVal &Arity,
                                                 const ArgVal &Tag) {
    mov_arg(ARG2, Src);
    (void)emit_ptr_val(ARG2, ARG2);
    emit_is_boxed(labels[Fail.getValue()], ARG2);
    a.ldur(TMP1, emit_boxed_val(ARG2));
    a.ldur(TMP2, emit_boxed_val(ARG2, sizeof(Eterm)));
    cmp_arg(TMP1, Arity);
    mov_arg(TMP3, Tag);
    a.ccmp(TMP2, TMP3, 0, arm::Cond::kEQ);

    a.cond_ne().b(labels[Fail.getValue()]);
}

/* Note: This instruction leaves the pointer to the tuple in ARG2. */
void BeamModuleAssembler::emit_i_is_tagged_tuple_ff(const ArgVal &NotTuple,
                                                    const ArgVal &NotRecord,
                                                    const ArgVal &Src,
                                                    const ArgVal &Arity,
                                                    const ArgVal &Tag) {
    Label not_tuple = labels[NotTuple.getValue()];
    Label not_record = labels[NotRecord.getValue()];

    mov_arg(ARG2, Src);
    emit_is_boxed(not_tuple, ARG2);
    (void)emit_ptr_val(ARG2, ARG2);
    a.ldur(TMP1, emit_boxed_val(ARG2));

    ERTS_CT_ASSERT(_TAG_HEADER_ARITYVAL == 0);
    a.tst(TMP1, imm(_TAG_HEADER_MASK));
    a.cond_ne().b(not_tuple);

    a.ldur(TMP2, emit_boxed_val(ARG2, sizeof(Eterm)));
    cmp_arg(TMP1, Arity);

    mov_arg(TMP3, Tag);
    a.ccmp(TMP2, TMP3, 0, arm::Cond::kEQ);

    a.cond_ne().b(not_record);
}

/* Note: This instruction leaves the pointer to the tuple in ARG2. */
void BeamModuleAssembler::emit_i_is_tuple(const ArgVal &Fail,
                                          const ArgVal &Src) {
    mov_arg(ARG2, Src);
    (void)emit_ptr_val(ARG2, ARG2);
    emit_is_boxed(labels[Fail.getValue()], ARG2);
    a.ldur(TMP1, emit_boxed_val(ARG2));
    ERTS_CT_ASSERT(_TAG_HEADER_ARITYVAL == 0);
    a.tst(TMP1, imm(_TAG_HEADER_MASK));
    a.cond_ne().b(labels[Fail.getValue()]);
}

/* Note: This instruction leaves the pointer to the tuple in ARG2. */
void BeamModuleAssembler::emit_i_is_tuple_of_arity(const ArgVal &Fail,
                                                   const ArgVal &Src,
                                                   const ArgVal &Arity) {
    mov_arg(ARG2, Src);
    (void)emit_ptr_val(ARG2, ARG2);
    emit_is_boxed(labels[Fail.getValue()], ARG2);
    a.ldur(TMP1, emit_boxed_val(ARG2));
    cmp_arg(TMP1, Arity);
    a.cond_ne().b(labels[Fail.getValue()]);
}

/* Note: This instruction leaves the pointer to the tuple in ARG2. */
void BeamModuleAssembler::emit_i_test_arity(const ArgVal &Fail,
                                            const ArgVal &Src,
                                            const ArgVal &Arity) {
    mov_arg(ARG2, Src);
    (void)emit_ptr_val(ARG2, ARG2);
    a.ldur(TMP1, emit_boxed_val(ARG2));
    cmp_arg(TMP1, Arity);
    a.cond_ne().b(labels[Fail.getValue()]);
}

void BeamModuleAssembler::emit_is_eq_exact(const ArgVal &Fail,
                                           const ArgVal &X,
                                           const ArgVal &Y) {
    Label fail = labels[Fail.getValue()];
    Label next = a.newLabel();
    JitRegister x = init_src_reg(X, ARG1);
    JitRegister y = init_src_reg(Y, ARG2);

    a.cmp(x.reg, y.reg);
    a.cond_eq().b(next);

    /* Fancy way of checking whether both are immediates. */
    ERTS_CT_ASSERT(TAG_PRIMARY_IMMED1 == _TAG_PRIMARY_MASK);
    a.and_(TMP1, x.reg, y.reg);
    a.and_(TMP1, TMP1, imm(_TAG_PRIMARY_MASK));
    a.cmp(TMP1, imm(TAG_PRIMARY_IMMED1));
    a.cond_eq().b(fail);

    copy_reg(ARG1, x);
    copy_reg(ARG2, y);

    emit_enter_runtime<Update::eFragileXregs>();

    runtime_call<2>(eq);

    emit_leave_runtime<Update::eFragileXregs>();

    a.cbz(ARG1, fail);

    a.bind(next);
}

void BeamModuleAssembler::emit_is_ne_exact(const ArgVal &Fail,
                                           const ArgVal &X,
                                           const ArgVal &Y) {
    Label fail = labels[Fail.getValue()];
    Label next = a.newLabel();
    JitRegister x = init_src_reg(X, ARG1);
    JitRegister y = init_src_reg(Y, ARG2);

    a.cmp(x.reg, y.reg);
    a.cond_eq().b(fail);

    /* Fancy way of checking whether both are immediates. */
    ERTS_CT_ASSERT(TAG_PRIMARY_IMMED1 == _TAG_PRIMARY_MASK);
    a.and_(TMP1, x.reg, y.reg);
    a.and_(TMP1, TMP1, imm(_TAG_PRIMARY_MASK));
    a.cmp(TMP1, imm(TAG_PRIMARY_IMMED1));
    a.cond_eq().b(next);

    copy_reg(ARG1, x);
    copy_reg(ARG2, y);

    emit_enter_runtime<Update::eFragileXregs>();

    runtime_call<2>(eq);

    emit_leave_runtime<Update::eFragileXregs>();

    a.cbnz(ARG1, fail);

    a.bind(next);
}

void BeamGlobalAssembler::emit_arith_eq_shared() {
    ERTS_ASSERT(!"NYI");
}

void BeamModuleAssembler::emit_is_eq(const ArgVal &Fail,
                                     const ArgVal &A,
                                     const ArgVal &B) {
    emit_nyi("emit_is_eq");
}

void BeamModuleAssembler::emit_is_ne(const ArgVal &Fail,
                                     const ArgVal &A,
                                     const ArgVal &B) {
    emit_nyi("emit_is_ne");
}

/*
 * ARG1 = LHS
 * ARG2 = RHS
 *
 * Result is returned in the flags.
 */
void BeamGlobalAssembler::emit_arith_compare_shared() {
    Label atom_compare = a.newLabel(), generic_compare = a.newLabel();

    /* Are both floats?
     *
     * This is done first as relative comparisons on atoms doesn't make much
     * sense. */
    a.orr(TMP1, ARG1, ARG2);
    emit_is_boxed(atom_compare, TMP1);

    arm::Gp boxed_ptr1 = emit_ptr_val(TMP1, ARG1);
    a.ldur(TMP3, emit_boxed_val(boxed_ptr1));
    arm::Gp boxed_ptr2 = emit_ptr_val(TMP2, ARG2);
    a.ldur(TMP4, emit_boxed_val(boxed_ptr2));

    mov_imm(TMP5, HEADER_FLONUM);
    a.cmp(TMP3, TMP5);
    a.ccmp(TMP4, TMP5, 0, arm::Cond::kEQ);
    a.cond_ne().b(generic_compare);

    a.ldur(a64::d0, emit_boxed_val(boxed_ptr1, sizeof(Eterm)));
    a.ldur(a64::d1, emit_boxed_val(boxed_ptr2, sizeof(Eterm)));
    a.fcmpe(a64::d0, a64::d1);
    a.ret(a64::x30);

    a.bind(atom_compare);
    {
        a.and_(TMP1, ARG1, imm(_TAG_IMMED2_MASK));
        a.and_(TMP2, ARG2, imm(_TAG_IMMED2_MASK));
        a.sub(TMP1, TMP1, imm(_TAG_IMMED2_ATOM));
        a.sub(TMP2, TMP2, imm(_TAG_IMMED2_ATOM));
        a.orr(TMP1, TMP1, TMP2);
        a.cbnz(TMP1, generic_compare);

        emit_enter_runtime_frame();
        emit_enter_runtime<Update::eFragileXregs>();

        runtime_call<2>(erts_cmp_atoms);

        emit_leave_runtime<Update::eFragileXregs>();
        emit_leave_runtime_frame();

        /* Note: erts_cmp_atoms() returns int, not Sint. */
        a.tst(ARG1.w(), ARG1.w());
        a.ret(a64::x30);
    }

    a.bind(generic_compare);
    {
        emit_enter_runtime_frame();
        emit_enter_runtime<Update::eFragileXregs>();

        comment("erts_cmp_compound(X, Y, 0, 0);");
        mov_imm(ARG3, 0);
        mov_imm(ARG4, 0);
        runtime_call<4>(erts_cmp_compound);

        emit_leave_runtime<Update::eFragileXregs>();
        emit_leave_runtime_frame();

        a.tst(ARG1, ARG1);

        a.ret(a64::x30);
    }
}

void BeamModuleAssembler::emit_is_lt(const ArgVal &Fail,
                                     const ArgVal &LHS,
                                     const ArgVal &RHS) {
    Label fail = labels[Fail.getValue()];
    Label generic = a.newLabel(), next = a.newLabel();
    JitRegister lhs = init_src_reg(LHS, ARG1);
    JitRegister rhs = init_src_reg(RHS, ARG2);

    a.cmp(lhs.reg, rhs.reg);
    a.cond_eq().b(fail);

    /* Relative comparisons are overwhelmingly likely to be used on smalls, so
     * we'll specialize those and keep the rest in a shared fragment. */

    if (RHS.isImmed() && is_small(RHS.getValue())) {
        a.and_(TMP1, lhs.reg, imm(_TAG_IMMED1_MASK));
    } else if (LHS.isImmed() && is_small(LHS.getValue())) {
        a.and_(TMP1, rhs.reg, imm(_TAG_IMMED1_MASK));
    } else {
        ERTS_CT_ASSERT(_TAG_IMMED1_SMALL == _TAG_IMMED1_MASK);
        a.and_(TMP1, lhs.reg, rhs.reg);
        a.and_(TMP1, TMP1, imm(_TAG_IMMED1_MASK));
    }

    a.cmp(TMP1, imm(_TAG_IMMED1_SMALL));
    a.cond_ne().b(generic);

    a.cmp(lhs.reg, rhs.reg);
    a.cond_lt().b(next);
    a.b(fail);

    a.bind(generic);
    {
        copy_reg(ARG1, lhs);
        copy_reg(ARG2, rhs);
        fragment_call(ga->get_arith_compare_shared());
        a.cond_ge().b(fail);
    }

    a.bind(next);
}

void BeamModuleAssembler::emit_is_ge(const ArgVal &Fail,
                                     const ArgVal &LHS,
                                     const ArgVal &RHS) {
    Label fail = labels[Fail.getValue()];
    Label generic = a.newLabel(), next = a.newLabel();
    JitRegister lhs = init_src_reg(LHS, ARG1);
    JitRegister rhs = init_src_reg(RHS, ARG2);

    a.cmp(lhs.reg, rhs.reg);
    a.cond_eq().b(next);

    /* Relative comparisons are overwhelmingly likely to be used on smalls, so
     * we'll specialize those and keep the rest in a shared fragment. */

    if (RHS.isImmed() && is_small(RHS.getValue())) {
        a.and_(TMP1, lhs.reg, imm(_TAG_IMMED1_MASK));
    } else if (LHS.isImmed() && is_small(LHS.getValue())) {
        a.and_(TMP1, rhs.reg, imm(_TAG_IMMED1_MASK));
    } else {
        ERTS_CT_ASSERT(_TAG_IMMED1_SMALL == _TAG_IMMED1_MASK);
        a.and_(TMP1, lhs.reg, rhs.reg);
        a.and_(TMP1, TMP1, imm(_TAG_IMMED1_MASK));
    }

    a.cmp(TMP1, imm(_TAG_IMMED1_SMALL));
    a.cond_ne().b(generic);

    a.cmp(lhs.reg, rhs.reg);
    a.cond_ge().b(next);
    a.b(fail);

    a.bind(generic);
    {
        copy_reg(ARG1, lhs);
        copy_reg(ARG2, rhs);
        fragment_call(ga->get_arith_compare_shared());
        a.cond_lt().b(fail);
    }

    a.bind(next);
}

void BeamModuleAssembler::emit_badmatch(const ArgVal &Src) {
    mov_arg(arm::Mem(c_p, offsetof(Process, fvalue)), Src);
    emit_error(BADMATCH);
}

void BeamModuleAssembler::emit_case_end(const ArgVal &Src) {
    mov_arg(arm::Mem(c_p, offsetof(Process, fvalue)), Src);
    emit_error(EXC_CASE_CLAUSE);
}

void BeamModuleAssembler::emit_system_limit_body() {
    emit_error(SYSTEM_LIMIT);
}

void BeamModuleAssembler::emit_if_end() {
    emit_error(EXC_IF_CLAUSE);
}

void BeamModuleAssembler::emit_catch(const ArgVal &Y, const ArgVal &Fail) {
    a.ldr(TMP1, arm::Mem(c_p, offsetof(Process, catches)));
    a.add(TMP1, TMP1, imm(1));
    a.str(TMP1, arm::Mem(c_p, offsetof(Process, catches)));

    Label next = a.newLabel(), data_lbl = a.newLabel();

    a.ldr(TMP1.w(), arm::Mem(data_lbl));
    a.b(next);
    a.bind(data_lbl);
    a.embedUInt32(0x7fffffff);

    a.bind(next);
    mov_arg(Y, TMP1);

    catches.push_back({{data_lbl, 0, 0}, labels[Fail.getValue()]});
}

void BeamGlobalAssembler::emit_catch_end_shared() {
    Label not_throw = a.newLabel(), not_error = a.newLabel(),
          after_gc = a.newLabel();

    /*
     * At this point, the x(1) through x(3) registers in the X register
     * array are valid.
     */

    mov_imm(TMP1, NIL);
    a.str(TMP1, arm::Mem(c_p, offsetof(Process, fvalue)));
    a.ldp(TMP1, XREG0, getXRef(1));
    a.cmp(TMP1, imm(am_throw));
    a.cond_ne().b(not_throw);

    /* Return thrown value. */
    a.ret(a64::x30);

    a.bind(not_throw);
    {
        emit_enter_runtime_frame();
        a.mov(ARG1, XREG0);
        a.cmp(TMP1, imm(am_error));
        a.cond_ne().b(not_error);

        /* This is an error, attach a stacktrace to the reason. */
        emit_enter_runtime<Update::eStack | Update::eHeap>();

        a.mov(ARG1, c_p);
        a.mov(ARG2, XREG0);
        a.ldr(ARG3, getXRef(3));
        runtime_call<3>(add_stacktrace);

        emit_leave_runtime<Update::eStack | Update::eHeap>();
    }

    /* Error term from exit/1 or stack backtrace from error/{1,2,3} is
     * now in ARG1. */
    a.bind(not_error);
    {
        const int32_t bytes_needed = (3 + S_RESERVED) * sizeof(Eterm);
        add(ARG3, HTOP, bytes_needed);
        a.cmp(ARG3, E);
        a.cond_ls().b(after_gc);

        /* Preserve stacktrace / reason. */
        a.str(ARG1, getXRef(0));
        mov_imm(ARG4, 1);
        aligned_call(labels[garbage_collect]);
        a.ldr(ARG1, getXRef(0));

        a.bind(after_gc);
        a.add(XREG0, HTOP, imm(TAG_PRIMARY_BOXED));
        mov_imm(TMP1, make_arityval(2));
        mov_imm(TMP2, am_EXIT);
        a.stp(TMP1, TMP2, arm::Mem(HTOP).post(2 * sizeof(Eterm)));
        a.str(ARG1, arm::Mem(HTOP).post(sizeof(Eterm)));
    }

    emit_leave_runtime_frame();
    a.ret(a64::x30);
}

void BeamModuleAssembler::emit_catch_end(const ArgVal &Y) {
    Label next = a.newLabel();

    emit_try_end(Y);
    emit_branch_if_value(XREG0, next);
    fragment_call(ga->get_catch_end_shared());
    a.bind(next);
}

void BeamModuleAssembler::emit_try_end(const ArgVal &Y) {
    a.ldr(TMP1, arm::Mem(c_p, offsetof(Process, catches)));
    a.sub(TMP1, TMP1, imm(1));
    a.str(TMP1, arm::Mem(c_p, offsetof(Process, catches)));
    emit_init(Y);
}

void BeamModuleAssembler::emit_try_case(const ArgVal &Y) {
    a.ldr(TMP1, arm::Mem(c_p, offsetof(Process, catches)));
    a.sub(TMP1, TMP1, imm(1));
    a.str(TMP1, arm::Mem(c_p, offsetof(Process, catches)));
    mov_imm(TMP1, NIL);
    mov_arg(Y, TMP1);
    a.str(TMP1, arm::Mem(c_p, offsetof(Process, fvalue)));
    a.ldp(XREG0, XREG1, getXRef(1));
    a.ldr(XREG2, getXRef(3));
}

void BeamModuleAssembler::emit_try_case_end(const ArgVal &Src) {
    mov_arg(arm::Mem(c_p, offsetof(Process, fvalue)), Src);
    emit_error(EXC_TRY_CLAUSE);
}

void BeamModuleAssembler::emit_raise(const ArgVal &Trace, const ArgVal &Value) {
    mov_arg(TMP1, Value);
    mov_arg(ARG2, Trace);

    /* This is an error, attach a stacktrace to the reason. */
    a.str(TMP1, arm::Mem(c_p, offsetof(Process, fvalue)));
    a.str(ARG2, arm::Mem(c_p, offsetof(Process, ftrace)));

    emit_enter_runtime();

    a.mov(ARG1, c_p);
    runtime_call<2>(erts_sanitize_freason);

    emit_leave_runtime();

    emit_raise_exception();
}

void BeamModuleAssembler::emit_build_stacktrace() {
    emit_enter_runtime<Update::eStack | Update::eHeap>();

    a.mov(ARG1, c_p);
    a.mov(ARG2, XREG0);
    runtime_call<2>(build_stacktrace);

    emit_leave_runtime<Update::eStack | Update::eHeap>();

    a.mov(XREG0, ARG1);
}

/* This instruction has the same semantics as the erlang:raise/3 BIF,
 * except that it can rethrow a raw stack backtrace. */
void BeamModuleAssembler::emit_raw_raise() {
    Label next = a.newLabel();

    emit_enter_runtime();

    a.mov(ARG1, XREG2);
    a.mov(ARG2, XREG0);
    a.mov(ARG3, XREG1);
    a.mov(ARG4, c_p);
    runtime_call<4>(raw_raise);

    emit_leave_runtime();
    a.cbnz(ARG1, next);

    emit_raise_exception();

    a.bind(next);
    mov_imm(XREG0, am_badarg);
}

void BeamGlobalAssembler::emit_i_test_yield_shared() {
    int mfa_offset =
            -(int)sizeof(ErtsCodeMFA) - BEAM_ASM_FUNC_PROLOGUE_SIZE - 4;

    /* Yield address is in ARG3. */
    add(ARG2, ARG3, mfa_offset);
    a.str(ARG2, arm::Mem(c_p, offsetof(Process, current)));
    a.ldr(ARG2, arm::Mem(ARG2, offsetof(ErtsCodeMFA, arity)));
    a.str(ARG2, arm::Mem(c_p, offsetof(Process, arity)));

    a.b(labels[context_switch_simplified]);
}

void BeamModuleAssembler::emit_i_test_yield() {
    Label next = a.newLabel(), entry = a.newLabel();

    /* When present, this is guaranteed to be the first instruction after the
     * breakpoint trampoline. */

    ASSERT(a.offset() % 8 == 0);

    emit_enter_erlang_frame();

    a.bind(entry);
    a.subs(FCALLS, FCALLS, imm(1));
    a.cond_gt().b(next);
    a.adr(ARG3, entry);
    a.bl(funcYield);
    a.bind(next);
}

void BeamModuleAssembler::emit_i_yield() {
    emit_nyi("emit_i_yield");
}

void BeamModuleAssembler::emit_i_perf_counter() {
    emit_nyi("emit_i_perf_counter");
}
