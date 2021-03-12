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

void BeamModuleAssembler::emit_i_bs_init_heap(const ArgVal &Size,
                                              const ArgVal &Heap,
                                              const ArgVal &Live,
                                              const ArgVal &Dst) {
    emit_nyi("emit_i_bs_init_heap");
}

/* Set the error reason when a size check has failed. */
void BeamGlobalAssembler::emit_bs_size_check_shared() {
    ERTS_ASSERT(!"NYI");
}

void BeamModuleAssembler::emit_i_bs_init_fail_heap(const ArgVal &Size,
                                                   const ArgVal &Heap,
                                                   const ArgVal &Fail,
                                                   const ArgVal &Live,
                                                   const ArgVal &Dst) {
    emit_nyi("emit_i_bs_init_fail_heap");
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
    emit_nyi("emit_i_bs_init_bits_heap");
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
    emit_nyi("emit_i_bs_init_bits_fail_heap");
}

void BeamModuleAssembler::emit_bs_put_string(const ArgVal &Size,
                                             const ArgVal &Ptr) {
    emit_nyi("emit_bs_put_string");
}

void BeamModuleAssembler::emit_i_new_bs_put_integer_imm(const ArgVal &Src,
                                                        const ArgVal &Fail,
                                                        const ArgVal &Sz,
                                                        const ArgVal &Flags) {
    emit_nyi("emit_i_new_bs_put_integer_imm");
}

void BeamModuleAssembler::emit_i_new_bs_put_integer(const ArgVal &Fail,
                                                    const ArgVal &Sz,
                                                    const ArgVal &Flags,
                                                    const ArgVal &Src) {
    emit_nyi("emit_i_new_bs_put_integer");
}

void BeamModuleAssembler::emit_i_new_bs_put_binary(const ArgVal &Fail,
                                                   const ArgVal &Sz,
                                                   const ArgVal &Flags,
                                                   const ArgVal &Src) {
    emit_nyi("emit_i_new_bs_put_binary");
}

void BeamModuleAssembler::emit_i_new_bs_put_binary_all(const ArgVal &Src,
                                                       const ArgVal &Fail,
                                                       const ArgVal &Unit) {
    emit_nyi("emit_i_new_bs_put_binary_all");
}

void BeamModuleAssembler::emit_i_new_bs_put_binary_imm(const ArgVal &Fail,
                                                       const ArgVal &Sz,
                                                       const ArgVal &Src) {
    emit_nyi("emit_i_new_bs_put_binary_imm");
}

void BeamModuleAssembler::emit_i_new_bs_put_float(const ArgVal &Fail,
                                                  const ArgVal &Sz,
                                                  const ArgVal &Flags,
                                                  const ArgVal &Src) {
    emit_nyi("emit_i_new_bs_put_float");
}

void BeamModuleAssembler::emit_i_new_bs_put_float_imm(const ArgVal &Fail,
                                                      const ArgVal &Sz,
                                                      const ArgVal &Flags,
                                                      const ArgVal &Src) {
    emit_nyi("emit_i_new_bs_put_float_imm");
}

void BeamModuleAssembler::emit_i_bs_start_match3(const ArgVal &Src,
                                                 const ArgVal &Live,
                                                 const ArgVal &Fail,
                                                 const ArgVal &Dst) {
    emit_nyi("emit_i_bs_start_match3");
}

void BeamModuleAssembler::emit_i_bs_match_string(const ArgVal &Ctx,
                                                 const ArgVal &Fail,
                                                 const ArgVal &Bits,
                                                 const ArgVal &Ptr) {
    emit_nyi("emit_i_bs_match_string");
}

void BeamModuleAssembler::emit_i_bs_get_position(const ArgVal &Ctx,
                                                 const ArgVal &Dst) {
    emit_nyi("emit_i_bs_get_position");
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
    emit_nyi("emit_i_bs_get_integer");
}

void BeamModuleAssembler::emit_bs_test_tail2(const ArgVal &Fail,
                                             const ArgVal &Ctx,
                                             const ArgVal &Offset) {
    emit_nyi("emit_bs_test_tail2");
}

void BeamModuleAssembler::emit_bs_set_position(const ArgVal &Ctx,
                                               const ArgVal &Pos) {
    emit_nyi("emit_bs_set_position");
}

void BeamModuleAssembler::emit_i_bs_get_binary_all2(const ArgVal &Ctx,
                                                    const ArgVal &Fail,
                                                    const ArgVal &Live,
                                                    const ArgVal &Unit,
                                                    const ArgVal &Dst) {
    emit_nyi("emit_i_bs_get_binary_all2");
}

void BeamGlobalAssembler::emit_bs_get_tail_shared() {
    ERTS_ASSERT(!"NYI");
}

void BeamModuleAssembler::emit_bs_get_tail(const ArgVal &Ctx,
                                           const ArgVal &Dst,
                                           const ArgVal &Live) {
    emit_nyi("emit_bs_get_tail");
}

/* Bits to skip are passed in RET */
void BeamModuleAssembler::emit_bs_skip_bits(const ArgVal &Fail,
                                            const ArgVal &Ctx) {
    emit_nyi("emit_bs_skip_bits");
}

void BeamModuleAssembler::emit_i_bs_skip_bits2(const ArgVal &Ctx,
                                               const ArgVal &Bits,
                                               const ArgVal &Fail,
                                               const ArgVal &Unit) {
    emit_nyi("emit_i_bs_skip_bits2");
}

void BeamModuleAssembler::emit_i_bs_skip_bits_imm2(const ArgVal &Fail,
                                                   const ArgVal &Ctx,
                                                   const ArgVal &Bits) {
    emit_nyi("emit_i_bs_skip_bits_imm2");
}

void BeamModuleAssembler::emit_i_bs_get_binary2(const ArgVal &Ctx,
                                                const ArgVal &Fail,
                                                const ArgVal &Live,
                                                const ArgVal &Size,
                                                const ArgVal &Flags,
                                                const ArgVal &Dst) {
    emit_nyi("emit_i_bs_get_binary2");
}

void BeamModuleAssembler::emit_i_bs_get_float2(const ArgVal &Ctx,
                                               const ArgVal &Fail,
                                               const ArgVal &Live,
                                               const ArgVal &Sz,
                                               const ArgVal &Flags,
                                               const ArgVal &Dst) {
    emit_nyi("emit_i_bs_get_float2");
}

void BeamModuleAssembler::emit_i_bs_utf8_size(const ArgVal &Src,
                                              const ArgVal &Dst) {
    emit_nyi("emit_i_bs_utf8_size");
}

void BeamModuleAssembler::emit_i_bs_put_utf8(const ArgVal &Fail,
                                             const ArgVal &Src) {
    emit_nyi("emit_i_bs_put_utf8");
}

void BeamModuleAssembler::emit_bs_get_utf8(const ArgVal &Ctx,
                                           const ArgVal &Fail) {
    emit_nyi("emit_bs_get_utf8");
}

void BeamModuleAssembler::emit_i_bs_get_utf8(const ArgVal &Ctx,
                                             const ArgVal &Fail,
                                             const ArgVal &Dst) {
    emit_nyi("emit_i_bs_get_utf8");
}

void BeamModuleAssembler::emit_i_bs_skip_utf8(const ArgVal &Ctx,
                                              const ArgVal &Fail) {
    emit_nyi("emit_i_bs_skip_utf8");
}

void BeamModuleAssembler::emit_i_bs_utf16_size(const ArgVal &Src,
                                               const ArgVal &Dst) {
    emit_nyi("emit_i_bs_utf16_size");
}

void BeamModuleAssembler::emit_bs_put_utf16(const ArgVal &Fail,
                                            const ArgVal &Flags,
                                            const ArgVal &Src) {
    emit_nyi("emit_bs_put_utf16");
}

void BeamModuleAssembler::emit_bs_get_utf16(const ArgVal &Ctx,
                                            const ArgVal &Fail,
                                            const ArgVal &Flags) {
    emit_nyi("emit_bs_get_utf16");
}

void BeamModuleAssembler::emit_i_bs_get_utf16(const ArgVal &Ctx,
                                              const ArgVal &Fail,
                                              const ArgVal &Flags,
                                              const ArgVal &Dst) {
    emit_nyi("emit_i_bs_get_utf16");
}

void BeamModuleAssembler::emit_i_bs_skip_utf16(const ArgVal &Ctx,
                                               const ArgVal &Fail,
                                               const ArgVal &Flags) {
    emit_nyi("emit_i_bs_skip_utf16");
}

void BeamModuleAssembler::emit_validate_unicode(Label next,
                                                Label fail,
                                                arm::Gp value) {
    emit_nyi("emit_validate_unicode");
}

void BeamModuleAssembler::emit_i_bs_validate_unicode(const ArgVal &Fail,
                                                     const ArgVal &Src) {
    emit_nyi("emit_i_bs_validate_unicode");
}

void BeamModuleAssembler::emit_i_bs_validate_unicode_retract(const ArgVal &Fail,
                                                             const ArgVal &Src,
                                                             const ArgVal &Ms) {
    emit_nyi("emit_i_bs_validate_unicode_retract");
}

void BeamModuleAssembler::emit_bs_test_unit(const ArgVal &Fail,
                                            const ArgVal &Ctx,
                                            const ArgVal &Unit) {
    emit_nyi("emit_bs_test_unit");
}

/* Set the error reason when bs_add has failed. */
void BeamGlobalAssembler::emit_bs_add_shared() {
    ERTS_ASSERT(!"NYI");
}

void BeamModuleAssembler::emit_bs_add(const ArgVal &Fail,
                                      const ArgVal &Src1,
                                      const ArgVal &Src2,
                                      const ArgVal &Unit,
                                      const ArgVal &Dst) {
    emit_nyi("emit_bs_add");
}

void BeamModuleAssembler::emit_i_bs_append(const ArgVal &Fail,
                                           const ArgVal &ExtraHeap,
                                           const ArgVal &Live,
                                           const ArgVal &Unit,
                                           const ArgVal &Size,
                                           const ArgVal &Bin,
                                           const ArgVal &Dst) {
    emit_nyi("emit_i_bs_append");
}

void BeamModuleAssembler::emit_i_bs_private_append(const ArgVal &Fail,
                                                   const ArgVal &Unit,
                                                   const ArgVal &Size,
                                                   const ArgVal &Src,
                                                   const ArgVal &Dst) {
    emit_nyi("emit_i_bs_private_append");
}

void BeamModuleAssembler::emit_bs_init_writable() {
    emit_nyi("emit_bs_init_writable");
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
    emit_nyi("emit_i_bs_start_match2");
}

void BeamModuleAssembler::emit_i_bs_save2(const ArgVal &Ctx,
                                          const ArgVal &Slot) {
    emit_nyi("emit_i_bs_save2");
}

void BeamModuleAssembler::emit_i_bs_restore2(const ArgVal &Ctx,
                                             const ArgVal &Slot) {
    emit_nyi("emit_i_bs_restore2");
}

void BeamModuleAssembler::emit_bs_context_to_binary(const ArgVal &Src) {
    emit_nyi("emit_bs_context_to_binary");
}
