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
#include "erl_binary.h"
#include "erl_bits.h"
#include "beam_common.h"
}

/* Clobbers TMP1+TMP2
 *
 * If max_size > 0, we jump to the fail label when Size > max_size
 *
 * Returns -1 when the field check always fails, 1 if it may fail, and 0 if it
 * never fails. */
int BeamModuleAssembler::emit_bs_get_field_size(const ArgVal &Size,
                                                int unit,
                                                Label fail,
                                                const arm::Gp &out,
                                                unsigned max_size) {
    if (Size.isImmed()) {
        if (is_small(Size.getValue())) {
            Sint sval = signed_val(Size.getValue());

            if (sval < 0) {
                /* badarg */
            } else if (max_size && sval > max_size) {
                /* badarg */
            } else if (sval > (MAX_SMALL / unit)) {
                /* system_limit */
            } else {
                mov_imm(out, sval * unit);
                return 0;
            }
        }

        a.b(fail);
        return -1;
    } else {
        JitRegister size_reg = init_src_reg(Size, TMP2);

        /* Negate the tag bits and then rotate them out, forcing all the checks
         * below to fail for non-smalls. */
        ERTS_CT_ASSERT(_TAG_IMMED1_SMALL == _TAG_IMMED1_MASK);
        a.eor(out, size_reg.reg, imm(_TAG_IMMED1_SMALL));
        a.ror(out, out, imm(_TAG_IMMED1_SIZE));

        if (max_size > 0) {
            ASSERT(Support::isInt10(max_size));
            a.cmp(out, imm(max_size));
            a.cond_hi().b(fail);
        }

        if (unit == 1) {
            if (max_size == 0) {
                /* Bail out on negative/non-small sizes. */
                a.tst(out, imm(0xF800000000000000UL));
                a.cond_mi().b(fail);
            }
        } else if (unit != 1) {
            /* Size = (Size) * (Unit) */
            mov_imm(TMP1, unit);

            a.umulh(TMP2, out, TMP1);
            a.mul(out, out, TMP1);
            a.cbnz(TMP2, fail);
        }

        return 1;
    }
}

void BeamModuleAssembler::emit_i_bs_init_heap(const ArgVal &Size,
                                              const ArgVal &Heap,
                                              const ArgVal &Live,
                                              const ArgVal &Dst) {
    mov_arg(ARG4, Size);
    mov_arg(ARG5, Heap);
    mov_arg(ARG6, Live);

    emit_enter_runtime<Update::eReductions | Update::eStack | Update::eHeap |
                       Update::eFragileXregs>();

    /* Must be last since mov_arg() may clobber ARG1 */
    a.mov(ARG1, c_p);
    load_x_reg_array(ARG2);
    load_erl_bits_state(ARG3);
    runtime_call<6>(beam_jit_bs_init);

    emit_leave_runtime<Update::eReductions | Update::eStack | Update::eHeap |
                       Update::eFragileXregs>();

    mov_arg(Dst, ARG1);
}

/* Set the error reason when a size check has failed. */
void BeamGlobalAssembler::emit_bs_size_check_shared() {
    emit_enter_runtime_frame();
    emit_enter_runtime();

    a.mov(ARG1, c_p);
    runtime_call<2>(beam_jit_bs_field_size_argument_error);

    emit_leave_runtime();
    emit_leave_runtime_frame();

    mov_imm(ARG4, 0);
    a.b(labels[raise_exception]);
}

void BeamModuleAssembler::emit_i_bs_init_fail_heap(const ArgVal &Size,
                                                   const ArgVal &Heap,
                                                   const ArgVal &Fail,
                                                   const ArgVal &Live,
                                                   const ArgVal &Dst) {
    Label fail;

    if (Fail.getValue() != 0) {
        fail = labels[Fail.getValue()];
    } else {
        fail = a.newLabel();
    }

    /* Clobbers RET + ARG3 */
    if (emit_bs_get_field_size(Size, 1, fail, ARG4) >= 0) {
        mov_arg(ARG5, Heap);
        mov_arg(ARG6, Live);

        emit_enter_runtime<Update::eReductions | Update::eStack |
                           Update::eHeap>(Live.getValue());

        a.mov(ARG1, c_p);
        load_x_reg_array(ARG2);
        load_erl_bits_state(ARG3);
        runtime_call<6>(beam_jit_bs_init);

        emit_leave_runtime<Update::eReductions | Update::eStack |
                           Update::eHeap>(Live.getValue());

        mov_arg(Dst, ARG1);
    }

    if (Fail.getValue() == 0) {
        Label next = a.newLabel();

        a.b(next);

        a.bind(fail);
        {
            mov_arg(ARG2, Size);
            fragment_call(ga->get_bs_size_check_shared());
        }

        a.bind(next);
    }
}

void BeamModuleAssembler::emit_i_bs_init(const ArgVal &Size,
                                         const ArgVal &Live,
                                         const ArgVal &Dst) {
    const ArgVal Heap(ArgVal::TYPE::u, 0);
    emit_i_bs_init_heap(Size, Heap, Live, Dst);
}

void BeamModuleAssembler::emit_i_bs_init_fail(const ArgVal &Size,
                                              const ArgVal &Fail,
                                              const ArgVal &Live,
                                              const ArgVal &Dst) {
    const ArgVal Heap(ArgVal::TYPE::u, 0);
    emit_i_bs_init_fail_heap(Size, Heap, Fail, Live, Dst);
}

void BeamModuleAssembler::emit_i_bs_init_bits(const ArgVal &NumBits,
                                              const ArgVal &Live,
                                              const ArgVal &Dst) {
    const ArgVal heap(ArgVal::TYPE::u, 0);
    emit_i_bs_init_bits_heap(NumBits, heap, Live, Dst);
}

void BeamModuleAssembler::emit_i_bs_init_bits_heap(const ArgVal &NumBits,
                                                   const ArgVal &Alloc,
                                                   const ArgVal &Live,
                                                   const ArgVal &Dst) {
    mov_arg(ARG4, NumBits);
    mov_arg(ARG5, Alloc);
    mov_arg(ARG6, Live);

    emit_enter_runtime<Update::eReductions | Update::eStack | Update::eHeap>(
            Live.getValue());

    /* Must be last since mov_arg() may clobber ARG1 */
    a.mov(ARG1, c_p);
    load_x_reg_array(ARG2);
    load_erl_bits_state(ARG3);
    runtime_call<6>(beam_jit_bs_init_bits);

    emit_leave_runtime<Update::eReductions | Update::eStack | Update::eHeap>(
            Live.getValue());

    mov_arg(Dst, ARG1);
}

void BeamModuleAssembler::emit_i_bs_init_bits_fail(const ArgVal &NumBits,
                                                   const ArgVal &Fail,
                                                   const ArgVal &Live,
                                                   const ArgVal &Dst) {
    const ArgVal Heap(ArgVal::TYPE::u, 0);

    emit_i_bs_init_bits_fail_heap(NumBits, Heap, Fail, Live, Dst);
}

void BeamModuleAssembler::emit_i_bs_init_bits_fail_heap(const ArgVal &NumBits,
                                                        const ArgVal &Alloc,
                                                        const ArgVal &Fail,
                                                        const ArgVal &Live,
                                                        const ArgVal &Dst) {
    Label fail;

    if (Fail.getValue() != 0) {
        fail = labels[Fail.getValue()];
    } else {
        fail = a.newLabel();
    }

    if (emit_bs_get_field_size(NumBits, 1, fail, ARG4) >= 0) {
        mov_arg(ARG5, Alloc);
        mov_arg(ARG6, Live);

        emit_enter_runtime<Update::eReductions | Update::eStack |
                           Update::eHeap>(Live.getValue());

        a.mov(ARG1, c_p);
        load_x_reg_array(ARG2);
        load_erl_bits_state(ARG3);
        runtime_call<6>(beam_jit_bs_init_bits);

        emit_leave_runtime<Update::eReductions | Update::eStack |
                           Update::eHeap>(Live.getValue());

        mov_arg(Dst, ARG1);
    }

    if (Fail.getValue() == 0) {
        Label next = a.newLabel();

        a.b(next);
        a.bind(fail);
        {
            mov_arg(ARG2, NumBits);
            fragment_call(ga->get_bs_size_check_shared());
        }

        a.bind(next);
    }
}

void BeamModuleAssembler::emit_bs_put_string(const ArgVal &Size,
                                             const ArgVal &Ptr) {
    mov_arg(ARG3, Size);

    emit_enter_runtime<Update::eFragileXregs>();

    make_move_patch(ARG2, strings, Ptr.getValue());
    load_erl_bits_state(ARG1);
    runtime_call<3>(erts_new_bs_put_string);

    emit_leave_runtime<Update::eFragileXregs>();
}

void BeamModuleAssembler::emit_i_new_bs_put_integer_imm(const ArgVal &Src,
                                                        const ArgVal &Fail,
                                                        const ArgVal &Sz,
                                                        const ArgVal &Flags) {
    mov_arg(ARG2, Src);
    mov_arg(ARG3, Sz);
    mov_arg(ARG4, Flags);

    emit_enter_runtime<Update::eFragileXregs>();

    load_erl_bits_state(ARG1);
    runtime_call<4>(erts_new_bs_put_integer);

    emit_leave_runtime<Update::eFragileXregs>();

    if (Fail.getValue() != 0) {
        a.cbz(ARG1, labels[Fail.getValue()]);
    } else {
        Label next = a.newLabel();

        a.cbnz(ARG1, next);
        emit_error(BADARG);
        a.bind(next);
    }
}

void BeamModuleAssembler::emit_i_new_bs_put_integer(const ArgVal &Fail,
                                                    const ArgVal &Sz,
                                                    const ArgVal &Flags,
                                                    const ArgVal &Src) {
    int unit = Flags.getValue() >> 3;
    Label next, fail;

    if (Fail.getValue() != 0) {
        fail = labels[Fail.getValue()];
    } else {
        fail = a.newLabel();
        next = a.newLabel();
    }

    /* Clobbers RET + ARG3 */
    if (emit_bs_get_field_size(Sz, unit, fail, ARG3) >= 0) {
        mov_arg(ARG2, Src);
        mov_arg(ARG4, Flags);

        emit_enter_runtime<Update::eFragileXregs>();

        load_erl_bits_state(ARG1);
        runtime_call<4>(erts_new_bs_put_integer);

        emit_leave_runtime<Update::eFragileXregs>();

        if (Fail.getValue() != 0) {
            a.cbz(ARG1, fail);
        } else {
            a.cbnz(ARG1, next);
        }
    }

    if (Fail.getValue() == 0) {
        a.bind(fail);
        emit_error(BADARG);
        a.bind(next);
    }
}

void BeamModuleAssembler::emit_i_new_bs_put_binary(const ArgVal &Fail,
                                                   const ArgVal &Sz,
                                                   const ArgVal &Flags,
                                                   const ArgVal &Src) {
    int unit = Flags.getValue() >> 3;
    Label next, fail;

    if (Fail.getValue() != 0) {
        fail = labels[Fail.getValue()];
    } else {
        fail = a.newLabel();
        next = a.newLabel();
    }

    if (emit_bs_get_field_size(Sz, unit, fail, ARG3) >= 0) {
        mov_arg(ARG2, Src);

        emit_enter_runtime<Update::eReductions | Update::eFragileXregs>();

        a.mov(ARG1, c_p);
        runtime_call<3>(erts_new_bs_put_binary);

        emit_leave_runtime<Update::eReductions | Update::eFragileXregs>();

        if (Fail.getValue() != 0) {
            a.cbz(ARG1, fail);
        } else {
            a.cbnz(ARG1, next);
        }
    }

    if (Fail.getValue() == 0) {
        a.bind(fail);
        emit_error(BADARG);
        a.bind(next);
    }
}

void BeamModuleAssembler::emit_i_new_bs_put_binary_all(const ArgVal &Src,
                                                       const ArgVal &Fail,
                                                       const ArgVal &Unit) {
    mov_arg(ARG2, Src);
    mov_arg(ARG3, Unit);

    emit_enter_runtime<Update::eReductions | Update::eFragileXregs>();

    a.mov(ARG1, c_p);
    runtime_call<3>(erts_new_bs_put_binary_all);

    emit_leave_runtime<Update::eReductions | Update::eFragileXregs>();

    if (Fail.getValue() == 0) {
        Label next = a.newLabel();

        a.cbnz(ARG1, next);
        emit_error(BADARG);
        a.bind(next);
    } else {
        a.cbz(ARG1, labels[Fail.getValue()]);
    }
}

void BeamModuleAssembler::emit_i_new_bs_put_binary_imm(const ArgVal &Fail,
                                                       const ArgVal &Sz,
                                                       const ArgVal &Src) {
    mov_arg(ARG2, Src);
    mov_arg(ARG3, Sz);

    emit_enter_runtime<Update::eReductions | Update::eFragileXregs>();

    a.mov(ARG1, c_p);
    runtime_call<3>(erts_new_bs_put_binary);

    emit_leave_runtime<Update::eReductions | Update::eFragileXregs>();

    if (Fail.getValue() == 0) {
        Label next = a.newLabel();

        a.cbnz(ARG1, next);
        emit_error(BADARG);
        a.bind(next);
    } else {
        a.cbz(ARG1, labels[Fail.getValue()]);
    }
}

void BeamModuleAssembler::emit_i_new_bs_put_float(const ArgVal &Fail,
                                                  const ArgVal &Sz,
                                                  const ArgVal &Flags,
                                                  const ArgVal &Src) {
    int unit = Flags.getValue() >> 3;
    Label next, fail;

    if (Fail.getValue() != 0) {
        fail = labels[Fail.getValue()];
    } else {
        fail = a.newLabel();
        next = a.newLabel();
    }

    if (emit_bs_get_field_size(Sz, unit, fail, ARG3) >= 0) {
        mov_arg(ARG2, Src);
        mov_arg(ARG4, Flags);

        emit_enter_runtime<Update::eFragileXregs>();

        a.mov(ARG1, c_p);
        runtime_call<4>(erts_new_bs_put_float);

        emit_leave_runtime<Update::eFragileXregs>();

        if (Fail.getValue() != 0) {
            a.cbz(ARG1, fail);
        } else {
            a.cbnz(ARG1, next);
        }
    }

    if (Fail.getValue() == 0) {
        a.bind(fail);
        emit_error(BADARG);
        a.bind(next);
    }
}

void BeamModuleAssembler::emit_i_new_bs_put_float_imm(const ArgVal &Fail,
                                                      const ArgVal &Sz,
                                                      const ArgVal &Flags,
                                                      const ArgVal &Src) {
    mov_arg(ARG2, Src);
    mov_arg(ARG3, Sz);
    mov_arg(ARG4, Flags);

    emit_enter_runtime<Update::eFragileXregs>();

    a.mov(ARG1, c_p);
    runtime_call<4>(erts_new_bs_put_float);

    emit_leave_runtime<Update::eFragileXregs>();

    if (Fail.getValue() != 0) {
        a.cbz(ARG1, labels[Fail.getValue()]);
    } else {
        Label next = a.newLabel();

        a.cbnz(ARG1, next);
        emit_error(BADARG);
        a.bind(next);
    }
}

void BeamModuleAssembler::emit_i_bs_start_match3(const ArgVal &Src,
                                                 const ArgVal &Live,
                                                 const ArgVal &Fail,
                                                 const ArgVal &Dst) {
    Label is_binary = a.newLabel(), next = a.newLabel();

    mov_arg(ARG2, Src);

    if (Fail.getValue() != 0) {
        emit_is_boxed(labels[Fail.getValue()], ARG2);
    } else {
        /* bs_start_match3 may not throw, and the compiler will only emit {f,0}
         * when it knows that the source is a match state or binary, so we're
         * free to skip the binary tests. */
    }

    arm::Gp boxed_ptr = emit_ptr_val(TMP1, ARG2);
    a.ldur(TMP1, emit_boxed_val(boxed_ptr, 0));

    a.and_(TMP1, TMP1, imm(_HEADER_SUBTAG_MASK));
    a.cmp(TMP1, imm(BIN_MATCHSTATE_SUBTAG));
    a.cond_eq().b(next);

    if (Fail.getValue() != 0) {
        comment("is_binary_header");
        a.cmp(TMP1, _TAG_HEADER_SUB_BIN);
        a.cond_eq().b(is_binary);
        ERTS_CT_ASSERT(_TAG_HEADER_REFC_BIN + 4 == _TAG_HEADER_HEAP_BIN);
        a.and_(TMP1, TMP1, imm(~4));
        a.cmp(TMP1, imm(_TAG_HEADER_REFC_BIN));
        a.cond_ne().b(labels[Fail.getValue()]);
    }

    a.bind(is_binary);
    {
        /* Src is not guaranteed to be inside the live range, so we need to
         * stash it during GC. */
        emit_gc_test_preserve(ArgVal(ArgVal::i, ERL_BIN_MATCHSTATE_SIZE(0)),
                              Live,
                              ARG2);

        emit_enter_runtime<Update::eStack | Update::eHeap |
                           Update::eFragileXregs>();

        a.mov(ARG1, c_p);
        /* ARG2 was set above */
        runtime_call<2>(erts_bs_start_match_3);

        emit_leave_runtime<Update::eStack | Update::eHeap |
                           Update::eFragileXregs>();

        a.add(ARG2, ARG1, imm(TAG_PRIMARY_BOXED));
    }

    a.bind(next);
    mov_arg(Dst, ARG2);
}

void BeamModuleAssembler::emit_i_bs_match_string(const ArgVal &Ctx,
                                                 const ArgVal &Fail,
                                                 const ArgVal &Bits,
                                                 const ArgVal &Ptr) {
    const int position_offset = offsetof(ErlBinMatchState, mb.offset);
    const int size_offset = offsetof(ErlBinMatchState, mb.size);
    const int base_offset = offsetof(ErlBinMatchState, mb.base);

    const UWord size = Bits.getValue();
    Label fail = labels[Fail.getValue()];

    {
        JitRegister ctx_reg = init_src_reg(Ctx, TMP1);

        a.ldur(TMP2, emit_boxed_val(ctx_reg.reg, position_offset));
        a.ldur(TMP3, emit_boxed_val(ctx_reg.reg, size_offset));
        a.add(TMP4, TMP2, imm(size));
        a.cmp(TMP4, TMP3);
        a.cond_hi().b(fail);

        /* ARG4 = mb->offset & 7 */
        a.and_(ARG4, TMP2, imm(7));

        /* ARG3 = mb->base + (mb->offset >> 3) */
        a.lsr(TMP2, TMP2, imm(3));
        a.ldur(TMP1, emit_boxed_val(ctx_reg.reg, base_offset));
        a.add(ARG3, TMP1, TMP2);
    }

    emit_enter_runtime<Update::eFragileXregs>();

    make_move_patch(ARG1, strings, Ptr.getValue());
    mov_imm(ARG2, 0);
    mov_imm(ARG5, size);
    runtime_call<5>(erts_cmp_bits);

    emit_leave_runtime<Update::eFragileXregs>();
    a.cbnz(ARG1, fail);

    {
        JitRegister ctx_reg = init_src_reg(Ctx, TMP1);

        a.ldur(TMP2, emit_boxed_val(ctx_reg.reg, position_offset));
        a.add(TMP2, TMP2, imm(size));
        a.stur(TMP2, emit_boxed_val(ctx_reg.reg, position_offset));
    }
}

void BeamModuleAssembler::emit_i_bs_get_position(const ArgVal &Ctx,
                                                 const ArgVal &Dst) {
    const int position_offset = offsetof(ErlBinMatchState, mb.offset);
    JitRegister ctx_reg = init_src_reg(Ctx, TMP1);
    JitRegister dst_reg = init_dst_reg(Dst, TMP2);

    /* Match contexts can never be literals, so we can skip clearing literal
     * tags. */
    a.ldur(dst_reg.reg, emit_boxed_val(ctx_reg.reg, position_offset));
    a.lsl(dst_reg.reg, dst_reg.reg, imm(_TAG_IMMED1_SIZE));
    a.orr(dst_reg.reg, dst_reg.reg, imm(_TAG_IMMED1_SMALL));

    flush_reg(dst_reg);
}

/* ARG3 = flags | (size << 3),
 * ARG4 = tagged match context */
void BeamGlobalAssembler::emit_bs_fixed_integer_shared() {
    ERTS_ASSERT(!"NYI");
}

arm::Mem BeamModuleAssembler::emit_bs_get_integer_prologue(Label next,
                                                           Label fail,
                                                           int flags,
                                                           int size) {
    ERTS_ASSERT(!"NYI");
    return arm::Mem();
}

void BeamModuleAssembler::emit_i_bs_get_integer(const ArgVal &Ctx,
                                                const ArgVal &Fail,
                                                const ArgVal &Live,
                                                const ArgVal &FlagsAndUnit,
                                                const ArgVal &Sz,
                                                const ArgVal &Dst) {
    Label fail;
    int unit;

    fail = labels[Fail.getValue()];
    unit = FlagsAndUnit.getValue() >> 3;

    if (emit_bs_get_field_size(Sz, unit, fail, ARG5) >= 0) {
        mov_arg(ARG3, Ctx);
        mov_arg(ARG4, FlagsAndUnit);
        mov_arg(ARG6, Live);

        emit_enter_runtime<Update::eReductions | Update::eStack |
                           Update::eHeap>(Live.getValue());

        a.mov(ARG1, c_p);
        load_x_reg_array(ARG2);
        runtime_call<6>(beam_jit_bs_get_integer);

        emit_leave_runtime<Update::eReductions | Update::eStack |
                           Update::eHeap>(Live.getValue());

        emit_branch_if_not_value(ARG1, fail);
        mov_arg(Dst, ARG1);
    }
}

void BeamModuleAssembler::emit_bs_test_tail2(const ArgVal &Fail,
                                             const ArgVal &Ctx,
                                             const ArgVal &Offset) {
    const int position_offset = offsetof(ErlBinMatchState, mb.offset);
    const int size_offset = offsetof(ErlBinMatchState, mb.size);

    JitRegister ctx_reg = init_src_reg(Ctx, TMP1);

    ASSERT(Offset.getType() == ArgVal::TYPE::u);

    a.ldur(TMP2, emit_boxed_val(ctx_reg.reg, size_offset));
    a.ldur(TMP3, emit_boxed_val(ctx_reg.reg, position_offset));
    a.sub(TMP2, TMP2, TMP3);

    if (Offset.getValue() != 0) {
        a.cmp(TMP2, imm(Offset.getValue()));
        a.cond_ne().b(labels[Fail.getValue()]);
    } else {
        a.cbnz(TMP2, labels[Fail.getValue()]);
    }
}

void BeamModuleAssembler::emit_bs_set_position(const ArgVal &Ctx,
                                               const ArgVal &Pos) {
    const int position_offset = offsetof(ErlBinMatchState, mb.offset);

    JitRegister ctx_reg = init_src_reg(Ctx, TMP1);
    JitRegister pos_reg = init_src_reg(Pos, TMP2);

    a.lsr(TMP2, pos_reg.reg, imm(_TAG_IMMED1_SIZE));
    a.stur(TMP2, emit_boxed_val(ctx_reg.reg, position_offset));
}

void BeamModuleAssembler::emit_i_bs_get_binary_all2(const ArgVal &Ctx,
                                                    const ArgVal &Fail,
                                                    const ArgVal &Live,
                                                    const ArgVal &Unit,
                                                    const ArgVal &Dst) {
    unsigned unit = Unit.getValue();

    mov_arg(ARG1, Ctx);

    /* Ctx is not guaranteed to be inside the live range, so we need to stash
     * it during GC. */
    emit_gc_test_preserve(ArgVal(ArgVal::i, EXTRACT_SUB_BIN_HEAP_NEED),
                          Live,
                          ARG1);

    a.ldur(TMP2, emit_boxed_val(ARG1, offsetof(ErlBinMatchState, mb.size)));
    a.ldur(TMP3, emit_boxed_val(ARG1, offsetof(ErlBinMatchState, mb.offset)));

    a.sub(TMP1, TMP2, TMP3);

    /* Unit may be 1 if compiling with +no_bsm3, which lacks the
     * bs_get_tail instruction. */
    if (unit > 1) {
        if ((unit & (unit - 1))) {
            mov_imm(TMP2, unit);

            a.udiv(TMP3, TMP1, TMP2);
            a.msub(TMP1, TMP3, TMP2, TMP1);
        } else {
            a.and_(TMP1, TMP1, imm(unit - 1));
        }

        a.cbnz(TMP1, labels[Fail.getValue()]);
    }

    emit_enter_runtime<Update::eHeap | Update::eFragileXregs>();

    lea(ARG2, emit_boxed_val(ARG1, offsetof(ErlBinMatchState, mb)));
    a.mov(ARG1, c_p);
    runtime_call<2>(erts_bs_get_binary_all_2);

    emit_leave_runtime<Update::eHeap | Update::eFragileXregs>();

    mov_arg(Dst, ARG1);
}

void BeamGlobalAssembler::emit_bs_get_tail_shared() {
    emit_enter_runtime_frame();
    emit_enter_runtime<Update::eHeap | Update::eFragileXregs>();

    a.ldur(ARG2, emit_boxed_val(ARG1, offsetof(ErlBinMatchState, mb.orig)));
    a.ldur(ARG3, emit_boxed_val(ARG1, offsetof(ErlBinMatchState, mb.base)));
    a.ldur(ARG4, emit_boxed_val(ARG1, offsetof(ErlBinMatchState, mb.offset)));

    /* Extracted size = mb->size - mb->offset */
    a.ldur(TMP1, emit_boxed_val(ARG1, offsetof(ErlBinMatchState, mb.size)));
    a.sub(ARG5, TMP1, ARG4);

    lea(ARG1, arm::Mem(c_p, offsetof(Process, htop)));
    runtime_call<5>(erts_extract_sub_binary);

    emit_leave_runtime<Update::eHeap | Update::eFragileXregs>();
    emit_leave_runtime_frame();

    a.ret(a64::x30);
}

void BeamModuleAssembler::emit_bs_get_tail(const ArgVal &Ctx,
                                           const ArgVal &Dst,
                                           const ArgVal &Live) {
    mov_arg(ARG1, Ctx);

    /* Ctx is not guaranteed to be inside the live range, so we need to stash
     * it during GC. */
    emit_gc_test_preserve(ArgVal(ArgVal::i, EXTRACT_SUB_BIN_HEAP_NEED),
                          Live,
                          ARG1);

    fragment_call(ga->get_bs_get_tail_shared());

    mov_arg(Dst, ARG1);
}

/* Bits to skip are passed in ARG1 */
void BeamModuleAssembler::emit_bs_skip_bits(const ArgVal &Fail,
                                            const ArgVal &Ctx) {
    const int position_offset = offsetof(ErlBinMatchState, mb.offset);
    const int size_offset = offsetof(ErlBinMatchState, mb.size);

    JitRegister ctx_reg = init_src_reg(Ctx, TMP1);

    a.ldur(TMP2, emit_boxed_val(ctx_reg.reg, position_offset));
    a.ldur(TMP3, emit_boxed_val(ctx_reg.reg, size_offset));

    a.add(TMP2, TMP2, ARG1);
    a.cmp(TMP2, TMP3);
    a.cond_hi().b(labels[Fail.getValue()]);

    a.stur(TMP2, emit_boxed_val(ctx_reg.reg, position_offset));
}

void BeamModuleAssembler::emit_i_bs_skip_bits2(const ArgVal &Ctx,
                                               const ArgVal &Bits,
                                               const ArgVal &Fail,
                                               const ArgVal &Unit) {
    Label fail = labels[Fail.getValue()];

    if (emit_bs_get_field_size(Bits, Unit.getValue(), fail, ARG1) >= 0) {
        emit_bs_skip_bits(Fail, Ctx);
    }
}

void BeamModuleAssembler::emit_i_bs_skip_bits_imm2(const ArgVal &Fail,
                                                   const ArgVal &Ctx,
                                                   const ArgVal &Bits) {
    mov_arg(ARG1, Bits);
    emit_bs_skip_bits(Fail, Ctx);
}

void BeamModuleAssembler::emit_i_bs_get_binary2(const ArgVal &Ctx,
                                                const ArgVal &Fail,
                                                const ArgVal &Live,
                                                const ArgVal &Size,
                                                const ArgVal &Flags,
                                                const ArgVal &Dst) {
    Label fail;
    int unit;

    fail = labels[Fail.getValue()];
    unit = Flags.getValue() >> 3;

    if (emit_bs_get_field_size(Size, unit, fail, ARG2) >= 0) {
        a.str(ARG2, TMP_MEM1q);

        mov_arg(ARG4, Ctx);

        /* Ctx is not guaranteed to be inside the live range, so we need to
         * stash it during GC. */
        emit_gc_test_preserve(ArgVal(ArgVal::i, EXTRACT_SUB_BIN_HEAP_NEED),
                              Live,
                              ARG4);

        lea(ARG4, emit_boxed_val(ARG4, offsetof(ErlBinMatchState, mb)));

        emit_enter_runtime<Update::eHeap | Update::eFragileXregs>();

        a.mov(ARG1, c_p);
        a.ldr(ARG2, TMP_MEM1q);
        mov_imm(ARG3, Flags.getValue());
        runtime_call<4>(erts_bs_get_binary_2);

        emit_leave_runtime<Update::eHeap | Update::eFragileXregs>();

        emit_branch_if_not_value(ARG1, fail);

        mov_arg(Dst, ARG1);
    }
}

void BeamModuleAssembler::emit_i_bs_get_float2(const ArgVal &Ctx,
                                               const ArgVal &Fail,
                                               const ArgVal &Live,
                                               const ArgVal &Sz,
                                               const ArgVal &Flags,
                                               const ArgVal &Dst) {
    Label fail;
    Sint unit;

    fail = labels[Fail.getValue()];
    unit = Flags.getValue() >> 3;

    mov_arg(ARG4, Ctx);

    /* Ctx is not guaranteed to be inside the live range, so we need to stash
     * it during GC. */
    emit_gc_test_preserve(ArgVal(ArgVal::i, FLOAT_SIZE_OBJECT), Live, ARG4);

    if (emit_bs_get_field_size(Sz, unit, fail, ARG2, 64) >= 0) {
        lea(ARG4, emit_boxed_val(ARG4, offsetof(ErlBinMatchState, mb)));

        emit_enter_runtime<Update::eHeap | Update::eFragileXregs>();

        a.mov(ARG1, c_p);
        mov_imm(ARG3, Flags.getValue());
        runtime_call<4>(erts_bs_get_float_2);

        emit_leave_runtime<Update::eHeap | Update::eFragileXregs>();

        emit_branch_if_not_value(ARG1, fail);

        mov_arg(Dst, ARG1);
    }
}

void BeamModuleAssembler::emit_i_bs_utf8_size(const ArgVal &Src,
                                              const ArgVal &Dst) {
    JitRegister src_reg = init_src_reg(Src, TMP1);
    JitRegister dst_reg = init_dst_reg(Dst, TMP2);

    Label next = a.newLabel();

    mov_imm(dst_reg.reg, make_small(1));
    a.cmp(src_reg.reg, imm(make_small(0x7F)));
    a.cond_ls().b(next);

    a.lsr(TMP1, src_reg.reg, imm(_TAG_IMMED1_SIZE));

    mov_imm(dst_reg.reg, make_small(2));
    a.cmp(TMP1, imm(0x7FFUL));
    a.cond_ls().b(next);

    a.cmp(TMP1, imm(0x10000UL));
    mov_imm(TMP2, make_small(3));
    mov_imm(TMP3, make_small(4));
    a.csel(dst_reg.reg, TMP2, TMP3, arm::Cond::kLO);

    a.bind(next);
    flush_reg(dst_reg);
}

void BeamModuleAssembler::emit_i_bs_put_utf8(const ArgVal &Fail,
                                             const ArgVal &Src) {
    mov_arg(ARG2, Src);

    emit_enter_runtime<Update::eFragileXregs>();

    load_erl_bits_state(ARG1);
    runtime_call<2>(erts_bs_put_utf8);

    emit_leave_runtime<Update::eFragileXregs>();

    if (Fail.getValue() != 0) {
        a.cbz(ARG1, labels[Fail.getValue()]);
    } else {
        Label next = a.newLabel();

        a.cbnz(ARG1, next);
        emit_error(BADARG);
        a.bind(next);
    }
}

void BeamModuleAssembler::emit_bs_get_utf8(const ArgVal &Ctx,
                                           const ArgVal &Fail) {
    mov_arg(ARG1, Ctx);
    lea(ARG1, emit_boxed_val(ARG1, offsetof(ErlBinMatchState, mb)));

    emit_enter_runtime<Update::eFragileXregs>();

    runtime_call<1>(erts_bs_get_utf8);

    emit_leave_runtime<Update::eFragileXregs>();

    emit_branch_if_not_value(ARG1, labels[Fail.getValue()]);
}

void BeamModuleAssembler::emit_i_bs_get_utf8(const ArgVal &Ctx,
                                             const ArgVal &Fail,
                                             const ArgVal &Dst) {
    emit_bs_get_utf8(Ctx, Fail);
    mov_arg(Dst, ARG1);
}

void BeamModuleAssembler::emit_i_bs_skip_utf8(const ArgVal &Ctx,
                                              const ArgVal &Fail) {
    emit_bs_get_utf8(Ctx, Fail);
}

void BeamModuleAssembler::emit_i_bs_utf16_size(const ArgVal &Src,
                                               const ArgVal &Dst) {
    JitRegister src_reg = init_src_reg(Src, TMP1);
    JitRegister dst_reg = init_dst_reg(Dst, TMP2);

    /* erts_bs_put_utf16 errors out whenever something's fishy, so we can
     * return garbage (2 or 4) if our input is not a small. */
    a.asr(TMP1, src_reg.reg, imm(_TAG_IMMED1_SIZE));
    a.cmp(TMP1, imm(0x100000UL));
    mov_imm(TMP1, make_small(2));
    mov_imm(TMP2, make_small(4));
    a.csel(dst_reg.reg, TMP1, TMP2, arm::Cond::kLO);

    flush_reg(dst_reg);
}

void BeamModuleAssembler::emit_bs_put_utf16(const ArgVal &Fail,
                                            const ArgVal &Flags,
                                            const ArgVal &Src) {
    mov_arg(ARG2, Src);
    mov_arg(ARG3, Flags);

    emit_enter_runtime<Update::eFragileXregs>();

    load_erl_bits_state(ARG1);
    runtime_call<3>(erts_bs_put_utf16);

    emit_leave_runtime<Update::eFragileXregs>();

    if (Fail.getValue() != 0) {
        a.cbz(ARG1, labels[Fail.getValue()]);
    } else {
        Label next = a.newLabel();

        a.cbnz(ARG1, next);
        emit_error(BADARG);
        a.bind(next);
    }
}

void BeamModuleAssembler::emit_bs_get_utf16(const ArgVal &Ctx,
                                            const ArgVal &Fail,
                                            const ArgVal &Flags) {
    JitRegister ctx_reg = init_src_reg(Ctx, TMP1);

    lea(ARG1, emit_boxed_val(ctx_reg.reg, offsetof(ErlBinMatchState, mb)));

    emit_enter_runtime<Update::eFragileXregs>();

    mov_imm(ARG2, Flags.getValue());
    runtime_call<2>(erts_bs_get_utf16);

    emit_leave_runtime<Update::eFragileXregs>();

    emit_branch_if_not_value(ARG1, labels[Fail.getValue()]);
}

void BeamModuleAssembler::emit_i_bs_get_utf16(const ArgVal &Ctx,
                                              const ArgVal &Fail,
                                              const ArgVal &Flags,
                                              const ArgVal &Dst) {
    emit_bs_get_utf16(Ctx, Fail, Flags);
    mov_arg(Dst, ARG1);
}

void BeamModuleAssembler::emit_i_bs_skip_utf16(const ArgVal &Ctx,
                                               const ArgVal &Fail,
                                               const ArgVal &Flags) {
    emit_bs_get_utf16(Ctx, Fail, Flags);
}

void BeamModuleAssembler::emit_validate_unicode(Label next,
                                                Label fail,
                                                arm::Gp value) {
    ASSERT(value != TMP2);

    a.and_(TMP2, value, imm(_TAG_IMMED1_MASK));
    a.cmp(TMP2, imm(_TAG_IMMED1_SMALL));
    a.cond_ne().b(fail);

    mov_imm(TMP2, make_small(0xD800UL));
    a.cmp(value, TMP2);
    a.cond_lo().b(next);

    mov_imm(TMP2, make_small(0xDFFFUL));
    a.cmp(value, TMP2);
    a.cond_ls().b(fail);

    mov_imm(TMP2, make_small(0x10FFFFUL));
    a.cmp(value, TMP2);
    a.cond_hi().b(fail);

    a.b(next);
}

void BeamModuleAssembler::emit_i_bs_validate_unicode(const ArgVal &Fail,
                                                     const ArgVal &Src) {
    JitRegister src_reg = init_src_reg(Src, TMP1);
    Label fail, next = a.newLabel();

    if (Fail.getValue() != 0) {
        fail = labels[Fail.getValue()];
    } else {
        fail = a.newLabel();
    }

    emit_validate_unicode(next, fail, src_reg.reg);

    if (Fail.getValue() == 0) {
        a.bind(fail);
        emit_error(BADARG);
    }

    a.bind(next);
}

void BeamModuleAssembler::emit_i_bs_validate_unicode_retract(const ArgVal &Fail,
                                                             const ArgVal &Src,
                                                             const ArgVal &Ms) {
    Label fail = a.newLabel(), next = a.newLabel();
    JitRegister src_reg = init_src_reg(Src, TMP1);

    emit_validate_unicode(next, fail, src_reg.reg);

    a.bind(fail);
    {
        const int position_offset = offsetof(ErlBinMatchState, mb.offset);
        JitRegister ctx_reg = init_src_reg(Ms, TMP2);

        a.ldur(TMP1, emit_boxed_val(ctx_reg.reg, position_offset));
        a.sub(TMP1, TMP1, imm(32));
        a.stur(TMP1, emit_boxed_val(ctx_reg.reg, position_offset));

        if (Fail.getValue() != 0) {
            a.b(labels[Fail.getValue()]);
        } else {
            emit_error(BADARG);
        }
    }

    a.bind(next);
}

void BeamModuleAssembler::emit_bs_test_unit(const ArgVal &Fail,
                                            const ArgVal &Ctx,
                                            const ArgVal &Unit) {
    JitRegister ctx_reg = init_src_reg(Ctx, TMP1);
    unsigned int unit = Unit.getValue();

    a.ldur(TMP2,
           emit_boxed_val(ctx_reg.reg, offsetof(ErlBinMatchState, mb.size)));
    a.ldur(TMP3,
           emit_boxed_val(ctx_reg.reg, offsetof(ErlBinMatchState, mb.offset)));

    a.sub(TMP1, TMP2, TMP3);

    if ((unit & (unit - 1))) {
        mov_imm(TMP2, unit);

        a.udiv(TMP3, TMP1, TMP2);
        a.msub(TMP1, TMP3, TMP2, TMP1);
    } else {
        a.and_(TMP1, TMP1, imm(unit - 1));
    }

    a.cbnz(TMP1, labels[Fail.getValue()]);
}

/* FIXME: rewrite this in asm */
static Eterm bs_add_helper(Process *c_p, Eterm size, Eterm add, Uint unit) {
    Uint size_val, add_val;

    if (is_small(size) && is_small(add)) {
        size_val = unsigned_val(size);
        add_val = unsigned_val(add);

        if (add_val <= (MAX_SMALL / unit)) {
            add_val *= unit;

            if (size_val <= (MAX_SMALL - add_val)) {
                return make_small(size_val + add_val);
            }
        }
    }

    beam_jit_bs_add_argument_error(c_p, size, add);
    return THE_NON_VALUE;
}

/* Set the error reason when bs_add has failed. */
void BeamGlobalAssembler::emit_bs_add_shared() {
    emit_enter_runtime_frame();
    emit_enter_runtime<Update::eFragileXregs>();

    a.mov(ARG1, c_p);
    runtime_call<4>(bs_add_helper);

    emit_leave_runtime<Update::eFragileXregs>();
    emit_leave_runtime_frame();

    a.ret(a64::x30);
}

void BeamModuleAssembler::emit_bs_add(const ArgVal &Fail,
                                      const ArgVal &Size,
                                      const ArgVal &Add,
                                      const ArgVal &Unit,
                                      const ArgVal &Dst) {
    mov_arg(ARG2, Size);
    mov_arg(ARG3, Add);
    mov_arg(ARG4, Unit);

    fragment_call(ga->get_bs_add_shared());

    if (Fail.getValue() != 0) {
        emit_branch_if_not_value(ARG1, labels[Fail.getValue()]);
    } else {
        Label next = a.newLabel();

        emit_branch_if_value(ARG1, next);
        emit_raise_exception();

        a.bind(next);
    }

    mov_arg(Dst, ARG1);
}

void BeamModuleAssembler::emit_i_bs_append(const ArgVal &Fail,
                                           const ArgVal &ExtraHeap,
                                           const ArgVal &Live,
                                           const ArgVal &Unit,
                                           const ArgVal &Size,
                                           const ArgVal &Bin,
                                           const ArgVal &Dst) {
    mov_arg(ARG3, Live);
    mov_arg(ARG4, Size);
    mov_arg(ARG5, ExtraHeap);
    mov_arg(ARG6, Unit);

    mov_arg(ArgVal(ArgVal::TYPE::x, Live.getValue()), Bin);

    emit_enter_runtime<Update::eReductions | Update::eStack | Update::eHeap>(
            Live.getValue() + 1);

    a.mov(ARG1, c_p);
    load_x_reg_array(ARG2);
    runtime_call<6>(erts_bs_append);

    emit_leave_runtime<Update::eReductions | Update::eStack | Update::eHeap>(
            Live.getValue() + 1);

    if (Fail.getValue() != 0) {
        emit_branch_if_not_value(ARG1, labels[Fail.getValue()]);
    } else {
        Label next = a.newLabel();

        emit_branch_if_value(ARG1, next);
        /* The error has been prepared in `erts_bs_append` */
        emit_raise_exception();

        a.bind(next);
    }

    mov_arg(Dst, ARG1);
}

void BeamModuleAssembler::emit_i_bs_private_append(const ArgVal &Fail,
                                                   const ArgVal &Unit,
                                                   const ArgVal &Size,
                                                   const ArgVal &Src,
                                                   const ArgVal &Dst) {
    mov_arg(ARG2, Src);
    mov_arg(ARG3, Size);
    mov_arg(ARG4, Unit);

    emit_enter_runtime<Update::eFragileXregs>();

    a.mov(ARG1, c_p);
    runtime_call<4>(erts_bs_private_append);

    emit_leave_runtime<Update::eFragileXregs>();

    if (Fail.getValue() != 0) {
        emit_branch_if_not_value(ARG1, labels[Fail.getValue()]);
    } else {
        Label next = a.newLabel();

        emit_branch_if_value(ARG1, next);
        /* The error has been prepared in `erts_bs_private_append` */
        emit_raise_exception();

        a.bind(next);
    }

    mov_arg(Dst, ARG1);
}

void BeamModuleAssembler::emit_bs_init_writable() {
    ERTS_CT_ASSERT(ERTS_HIGHEST_CALLEE_SAVE_XREG >= 0);

    a.mov(ARG1, c_p);
    a.mov(ARG2, XREG0);

    /* We have an implicit liveness of 0, so we don't need to stash X
     * registers. */
    emit_enter_runtime<Update::eReductions | Update::eStack | Update::eHeap>();

    runtime_call<2>(erts_bs_init_writable);

    emit_leave_runtime<Update::eReductions | Update::eStack | Update::eHeap>();

    a.mov(XREG0, ARG1);
}

/* Old compatibility instructions for <= OTP-21. Kept in order to be able to
 * load old code. While technically we could remove these in OTP-24, we've
 * decided to keep them until at least OTP-25 to make things easier for
 * users. */
void BeamModuleAssembler::emit_i_bs_start_match2(const ArgVal &Src,
                                                 const ArgVal &Fail,
                                                 const ArgVal &Live,
                                                 const ArgVal &Slots,
                                                 const ArgVal &Dst) {
    mov_arg(ARG1, Src);
    mov_arg(ARG2, Live);
    mov_arg(ARG3, Slots);

    emit_enter_runtime<Update::eReductions | Update::eStack | Update::eHeap>(
            Live.getValue());

    a.mov(ARG4, c_p);
    load_x_reg_array(ARG5);
    runtime_call<5>(beam_jit_bs_start_match2);

    emit_leave_runtime<Update::eReductions | Update::eStack | Update::eHeap>(
            Live.getValue());

    emit_branch_if_not_value(ARG1, labels[Fail.getValue()]);
    mov_arg(Dst, ARG1);
}

void BeamModuleAssembler::emit_i_bs_save2(const ArgVal &Ctx,
                                          const ArgVal &Slot) {
    const int slot_offset = offsetof(ErlBinMatchState, save_offset) +
                            (sizeof(Eterm) * Slot.getValue());
    const int mb_offset = offsetof(ErlBinMatchState, mb.offset);
    JitRegister ctx_reg = init_src_reg(Ctx, TMP1);

    a.ldur(TMP2, emit_boxed_val(ctx_reg.reg, mb_offset));
    a.stur(TMP2, emit_boxed_val(ctx_reg.reg, slot_offset));
}

void BeamModuleAssembler::emit_i_bs_restore2(const ArgVal &Ctx,
                                             const ArgVal &Slot) {
    const int slot_offset = offsetof(ErlBinMatchState, save_offset) +
                            (sizeof(Eterm) * Slot.getValue());
    const int mb_offset = offsetof(ErlBinMatchState, mb.offset);

    JitRegister ctx_reg = init_src_reg(Ctx, TMP1);

    a.ldur(TMP2, emit_boxed_val(ctx_reg.reg, slot_offset));
    a.stur(TMP2, emit_boxed_val(ctx_reg.reg, mb_offset));
}

void BeamModuleAssembler::emit_bs_context_to_binary(const ArgVal &Src) {
    mov_arg(ARG1, Src);

    emit_enter_runtime<Update::eFragileXregs>();

    runtime_call<1>(beam_jit_bs_context_to_binary);

    emit_leave_runtime<Update::eFragileXregs>();
}
