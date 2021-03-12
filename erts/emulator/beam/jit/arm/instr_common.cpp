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
    emit_nyi("emit_error");
}

void BeamModuleAssembler::emit_gc_test(const ArgVal &Ns,
                                       const ArgVal &Nh,
                                       const ArgVal &Live) {
    emit_nyi("emit_gc_test");
}

#if defined(DEBUG) && defined(HARD_DEBUG)
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
    emit_nyi("emit_validate");
#endif
}

/* Instrs */

void BeamModuleAssembler::emit_i_validate(const ArgVal &Arity) {
    emit_validate(Arity);
}

void BeamModuleAssembler::emit_allocate_heap(const ArgVal &NeedStack,
                                             const ArgVal &NeedHeap,
                                             const ArgVal &Live) {
    emit_nyi("allocate_heap");
}

void BeamModuleAssembler::emit_allocate(const ArgVal &NeedStack,
                                        const ArgVal &Live) {
    emit_allocate_heap(NeedStack, ArgVal(ArgVal::TYPE::u, 0), Live);
}

void BeamModuleAssembler::emit_deallocate(const ArgVal &Deallocate) {
    emit_nyi("deallocate");
}

void BeamModuleAssembler::emit_test_heap(const ArgVal &Nh, const ArgVal &Live) {
    emit_gc_test(ArgVal(ArgVal::u, 0), Nh, Live);
}

void BeamModuleAssembler::emit_normal_exit() {
    /* This is implictly global; it does not normally appear in modules and
     * doesn't require size optimization. */

    emit_nyi("normal_exit");
}

void BeamModuleAssembler::emit_continue_exit() {
    /* This is implictly global; it does not normally appear in modules and
     * doesn't require size optimization. */

    emit_nyi("continue_exit");
}

/* This is an alias for handle_error */
void BeamModuleAssembler::emit_error_action_code() {
    emit_nyi("error_action_code");
}

/* Psuedo-instruction for signalling lambda load errors. Never actually runs. */
void BeamModuleAssembler::emit_i_lambda_error(const ArgVal &Dummy) {
    emit_nyi("emit_i_lambda_error");
}

void BeamModuleAssembler::emit_i_make_fun3(const ArgVal &Fun,
                                           const ArgVal &Dst,
                                           const ArgVal &NumFree,
                                           const std::vector<ArgVal> &env) {
    emit_nyi("emit_i_make_fun3");
}

void BeamModuleAssembler::emit_get_list(const ArgVal &Src,
                                        const ArgVal &Hd,
                                        const ArgVal &Tl) {
    emit_nyi("emit_get_list");
}

void BeamModuleAssembler::emit_get_hd(const ArgVal &Src, const ArgVal &Hd) {
    emit_nyi("emit_get_hd");
}

void BeamModuleAssembler::emit_get_tl(const ArgVal &Src, const ArgVal &Tl) {
    emit_nyi("emit_get_tl");
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
    emit_nyi("emit_load_tuple_ptr");
}

#ifdef DEBUG
/* Emit an assertion to ensure that tuple_reg points into the same
 * tuple as Src. */
void BeamModuleAssembler::emit_tuple_assertion(const ArgVal &Src,
                                               arm::Gp tuple_reg) {
}
#endif

/* Fetch an element from the tuple pointed to by the boxed pointer
 * in ARG2. */
void BeamModuleAssembler::emit_i_get_tuple_element(const ArgVal &Src,
                                                   const ArgVal &Element,
                                                   const ArgVal &Dst) {
    emit_nyi("emit_i_get_tuple_element");
}

void BeamModuleAssembler::emit_init(const ArgVal &Y) {
    emit_nyi("emit_init");
}

void BeamModuleAssembler::emit_init_yregs(const ArgVal &Size,
                                          const std::vector<ArgVal> &args) {
    emit_nyi("emit_init_yregs");
}

void BeamModuleAssembler::emit_i_trim(const ArgVal &Words) {
    emit_nyi("i_trim");
}

void BeamModuleAssembler::emit_i_move(const ArgVal &Src, const ArgVal &Dst) {
    emit_nyi("move");
}

void BeamModuleAssembler::emit_swap(const ArgVal &R1, const ArgVal &R2) {
    emit_nyi("emit_swap");
}

void BeamModuleAssembler::emit_node(const ArgVal &Dst) {
    emit_nyi("emit_node");
}

void BeamModuleAssembler::emit_put_list(const ArgVal &Hd, const ArgVal &Tl, const ArgVal &Dst) {
    emit_nyi("emit_put_list");
}

void BeamModuleAssembler::emit_put_tuple2(const ArgVal &Dst,
                                          const ArgVal &Arity,
                                          const std::vector<ArgVal> &args) {
    emit_nyi("emit_put_tuple2");
}

void BeamModuleAssembler::emit_self(const ArgVal &Dst) {
    emit_nyi("self");
}

void BeamModuleAssembler::emit_set_tuple_element(const ArgVal &Element,
                                                 const ArgVal &Tuple,
                                                 const ArgVal &Offset) {
    emit_nyi("emit_set_tuple_element");
}

void BeamModuleAssembler::emit_is_nonempty_list(const ArgVal &Fail,
                                                const ArgVal &Src) {
    emit_nyi("emit_is_nonempty_list");
}

void BeamModuleAssembler::emit_jump(const ArgVal &Fail) {
    emit_nyi("emit_jump");
}

void BeamModuleAssembler::emit_is_atom(const ArgVal &Fail, const ArgVal &Src) {
    emit_nyi("emit_is_atom");
}

void BeamModuleAssembler::emit_is_boolean(const ArgVal &Fail,
                                          const ArgVal &Src) {
    emit_nyi("emit_is_boolean");
}

void BeamModuleAssembler::emit_is_binary(const ArgVal &Fail,
                                         const ArgVal &Src) {
    emit_nyi("emit_is_binary");
}

void BeamModuleAssembler::emit_is_bitstring(const ArgVal &Fail,
                                            const ArgVal &Src) {
    emit_nyi("emit_is_bitstring");
}

void BeamModuleAssembler::emit_is_float(const ArgVal &Fail, const ArgVal &Src) {
    emit_nyi("emit_is_float");
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
    emit_nyi("emit_is_integer");
}

void BeamModuleAssembler::emit_is_list(const ArgVal &Fail, const ArgVal &Src) {
    emit_nyi("emit_is_list");
}

void BeamModuleAssembler::emit_is_map(const ArgVal &Fail, const ArgVal &Src) {
    emit_nyi("emit_is_map");
}

void BeamModuleAssembler::emit_is_nil(const ArgVal &Fail, const ArgVal &Src) {
    emit_nyi("emit_is_nil");
}

void BeamModuleAssembler::emit_is_number(const ArgVal &Fail,
                                         const ArgVal &Src) {
    emit_nyi("emit_is_number");
}

void BeamModuleAssembler::emit_is_pid(const ArgVal &Fail, const ArgVal &Src) {
    emit_nyi("emit_is_pid");
}

void BeamModuleAssembler::emit_is_port(const ArgVal &Fail, const ArgVal &Src) {
    emit_nyi("emit_is_port");
}

void BeamModuleAssembler::emit_is_reference(const ArgVal &Fail,
                                            const ArgVal &Src) {
    emit_nyi("emit_is_reference");
}

/* Note: This instruction leaves the pointer to the tuple in ARG2. */
void BeamModuleAssembler::emit_i_is_tagged_tuple(const ArgVal &Fail,
                                                 const ArgVal &Src,
                                                 const ArgVal &Arity,
                                                 const ArgVal &Tag) {
    emit_nyi("emit_i_is_tagged_tuple");
}

/* Note: This instruction leaves the pointer to the tuple in ARG2. */
void BeamModuleAssembler::emit_i_is_tagged_tuple_ff(const ArgVal &NotTuple,
                                                    const ArgVal &NotRecord,
                                                    const ArgVal &Src,
                                                    const ArgVal &Arity,
                                                    const ArgVal &Tag) {
    emit_nyi("emit_i_is_tagged_tuple_ff");
}

/* Note: This instruction leaves the pointer to the tuple in ARG2. */
void BeamModuleAssembler::emit_i_is_tuple(const ArgVal &Fail,
                                          const ArgVal &Src) {
    emit_nyi("emit_i_is_tuple");
}

/* Note: This instruction leaves the pointer to the tuple in ARG2. */
void BeamModuleAssembler::emit_i_is_tuple_of_arity(const ArgVal &Fail,
                                                   const ArgVal &Src,
                                                   const ArgVal &Arity) {
    emit_nyi("emit_i_is_tuple_of_arity");
}

/* Note: This instruction leaves the pointer to the tuple in ARG2. */
void BeamModuleAssembler::emit_i_test_arity(const ArgVal &Fail,
                                            const ArgVal &Src,
                                            const ArgVal &Arity) {
    emit_nyi("emit_i_test_arity");
}

void BeamModuleAssembler::emit_is_eq_exact(const ArgVal &Fail,
                                           const ArgVal &X,
                                           const ArgVal &Y) {
    emit_nyi("emit_is_eq_exact");
}

void BeamModuleAssembler::emit_is_ne_exact(const ArgVal &Fail,
                                           const ArgVal &X,
                                           const ArgVal &Y) {
    emit_nyi("emit_is_ne_exact");
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

void BeamGlobalAssembler::emit_arith_compare_shared() {
    ERTS_ASSERT(!"NYI");
}

void BeamModuleAssembler::emit_is_lt(const ArgVal &Fail,
                                     const ArgVal &LHS,
                                     const ArgVal &RHS) {
    emit_nyi("emit_is_lt");
}

void BeamModuleAssembler::emit_is_ge(const ArgVal &Fail,
                                     const ArgVal &LHS,
                                     const ArgVal &RHS) {
    emit_nyi("emit_is_ge");
}

void BeamModuleAssembler::emit_badmatch(const ArgVal &Src) {
    emit_nyi("emit_badmatch");
}

void BeamModuleAssembler::emit_case_end(const ArgVal &Src) {
    emit_nyi("emit_case_end");
}

void BeamModuleAssembler::emit_system_limit_body() {
    emit_error(SYSTEM_LIMIT);
}

void BeamModuleAssembler::emit_if_end() {
    emit_error(EXC_IF_CLAUSE);
}

void BeamModuleAssembler::emit_catch(const ArgVal &Y, const ArgVal &Fail) {
    emit_nyi("emit_catch");
}

void BeamGlobalAssembler::emit_catch_end_shared() {
    ERTS_ASSERT(!"NYI");
}

void BeamModuleAssembler::emit_catch_end(const ArgVal &Y) {
    emit_nyi("emit_catch_end");
}

void BeamModuleAssembler::emit_try_end(const ArgVal &Y) {
    emit_nyi("emit_try_end");
}

void BeamModuleAssembler::emit_try_case(const ArgVal &Y) {
    emit_nyi("emit_try_case");
}

void BeamModuleAssembler::emit_try_case_end(const ArgVal &Src) {
    emit_nyi("emit_try_case_end");
}

void BeamModuleAssembler::emit_raise(const ArgVal &Trace, const ArgVal &Value) {
    emit_nyi("emit_raise");
}

void BeamModuleAssembler::emit_build_stacktrace() {
    emit_nyi("emit_build_stacktrace");
}

void BeamModuleAssembler::emit_raw_raise() {
    emit_nyi("emit_raw_raise");
}

void BeamGlobalAssembler::emit_i_test_yield_shared() {
    ERTS_ASSERT(!"NYI");
}

void BeamModuleAssembler::emit_i_test_yield() {
    emit_nyi("i_test_yield");
}

void BeamModuleAssembler::emit_i_yield() {
    emit_nyi("emit_i_yield");
}

void BeamModuleAssembler::emit_i_perf_counter() {
    emit_nyi("emit_i_perf_counter");
}
