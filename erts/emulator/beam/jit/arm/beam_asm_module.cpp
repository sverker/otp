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
#include <float.h>

#include "beam_asm.hpp"
using namespace asmjit;

static std::string getAtom(Eterm atom) {
    Atom *ap = atom_tab(atom_val(atom));
    return std::string((char *)ap->name, ap->len);
}

#ifdef BEAMASM_DUMP_SIZES
#    include <mutex>

typedef std::pair<Uint64, Uint64> op_stats;

static std::unordered_map<char *, op_stats> sizes;
static std::mutex size_lock;

extern "C" void beamasm_dump_sizes() {
    std::lock_guard<std::mutex> lock(size_lock);

    std::vector<std::pair<char *, op_stats>> flat(sizes.cbegin(), sizes.cend());
    double total_size = 0.0;

    for (const auto &op : flat) {
        total_size += op.second.second;
    }

    /* Sort instructions by total size, in descending order. */
    std::sort(
            flat.begin(),
            flat.end(),
            [](std::pair<char *, op_stats> &a, std::pair<char *, op_stats> &b) {
                return a.second.second > b.second.second;
            });

    for (const auto &op : flat) {
        fprintf(stderr,
                "%34s:\t%zu\t%f\t%zu\t%zu\r\n",
                op.first,
                op.second.second,
                op.second.second / total_size,
                op.second.first,
                op.second.first ? (op.second.second / op.second.first) : 0);
    }
}
#endif

BeamModuleAssembler::BeamModuleAssembler(BeamGlobalAssembler *ga,
                                         Eterm mod,
                                         unsigned num_labels)
        : BeamAssembler(getAtom(mod)) {
    this->ga = ga;
    this->mod = mod;

    labels.reserve(num_labels + 1);
    for (unsigned i = 1; i < num_labels; i++) {
        Label lbl;

#ifdef DEBUG
        std::string lblName = "label_" + std::to_string(i);
        lbl = a.newNamedLabel(lblName.data());
#else
        lbl = a.newLabel();
#endif

        labels[i] = lbl;
    }
}

BeamModuleAssembler::BeamModuleAssembler(BeamGlobalAssembler *ga,
                                         Eterm mod,
                                         unsigned num_labels,
                                         unsigned num_functions)
        : BeamModuleAssembler(ga, mod, num_labels) {
    emit_nyi("BeamModuleAssembler() constructor");
}

ErtsCodePtr BeamModuleAssembler::getCode(unsigned label) {
    ASSERT(label < labels.size() + 1);
    return (ErtsCodePtr)getCode(labels[label]);
}

void BeamAssembler::embed_rodata(const char *labelName,
                                 const char *buff,
                                 size_t size) {
    Label label = a.newNamedLabel(labelName);

    a.section(rodata);
    a.bind(label);
    a.embed(buff, size);
    a.section(code.textSection());
}

void BeamAssembler::embed_bss(const char *labelName, size_t size) {
    Label label = a.newNamedLabel(labelName);

    /* Reuse rodata section for now */
    a.section(rodata);
    a.bind(label);
    embed_zeros(size);
    a.section(code.textSection());
}

void BeamAssembler::embed_zeros(size_t size) {
    static constexpr size_t buf_size = 16384;
    static const char zeros[buf_size] = {};

    while (size >= buf_size) {
        a.embed(zeros, buf_size);
        size -= buf_size;
    }

    if (size > 0) {
        a.embed(zeros, size);
    }
}

Label BeamModuleAssembler::embed_vararg_rodata(const std::vector<ArgVal> &args,
                                               int y_offset) {
    ERTS_ASSERT(!"NYI");
}

static void i_emit_nyi(char *msg) {
    erts_exit(ERTS_ERROR_EXIT, "NYI: %s\n", msg);
}

void BeamModuleAssembler::emit_i_nif_padding() {
    const size_t minimum_size = sizeof(UWord[BEAM_NATIVE_MIN_FUNC_SZ]);
    size_t prev_func_start, diff;

    prev_func_start = code.labelOffsetFromBase(labels[functions.back() + 1]);
    diff = a.offset() - prev_func_start;

    if (diff < minimum_size) {
        embed_zeros(minimum_size - diff);
    }
}

void BeamModuleAssembler::emit_i_breakpoint_trampoline() {
    emit_nyi("i_breakpoint_trampoline");
}

void BeamModuleAssembler::emit_nyi(const char *msg) {
    emit_enter_runtime();

    a.mov(ARG1, imm(msg));
    runtime_call<1>(i_emit_nyi);

    /* Never returns */
}

void BeamModuleAssembler::emit_nyi() {
    emit_nyi("<unspecified>");
}

bool BeamModuleAssembler::emit(unsigned specific_op,
                               const std::vector<ArgVal> &args) {
    comment(opc[specific_op].name);

#ifdef BEAMASM_DUMP_SIZES
    uint64_t before = a.offset();
#endif

#define InstrCnt()
    switch (specific_op) {
#include "beamasm_emit.h"
    default:
        ERTS_ASSERT(0 && "Invalid instruction");
        break;
    }

    if (getOffset() == last_error_offset) {
        /*
         * The previous PC where an exception may occur is equal to the
         * current offset, which is also the offset of the next
         * instruction. If the next instruction happens to be a
         * line instruction, the location for the exception will
         * be that line instruction, which is probably wrong.
         * To avoid that, bump the instruction offset.
         */
        a.nop();
    }

#ifdef BEAMASM_DUMP_SIZES
    {
        std::lock_guard<std::mutex> lock(size_lock);

        sizes[opc[specific_op].name].first++;
        sizes[opc[specific_op].name].second += a.offset() - before;
    }
#endif

    return true;
}

/*
 * Here follows meta instructions.
 */

void BeamGlobalAssembler::emit_i_func_info_shared() {
    ERTS_ASSERT(!"NYI");
}

void BeamModuleAssembler::emit_i_func_info(const ArgVal &Label,
                                           const ArgVal &Module,
                                           const ArgVal &Function,
                                           const ArgVal &Arity) {
    emit_nyi("i_func_info");
}

void BeamModuleAssembler::emit_label(const ArgVal &Label) {
    currLabel = labels[Label.getValue()];
    a.bind(currLabel);
}

void BeamModuleAssembler::emit_aligned_label(const ArgVal &Label,
                                             const ArgVal &Alignment) {
    ASSERT(Alignment.getType() == ArgVal::u);
    a.align(kAlignCode, Alignment.getValue());
    emit_label(Label);
}

void BeamModuleAssembler::emit_on_load() {
    on_load = currLabel;
}

void BeamModuleAssembler::emit_int_code_end() {
    /* This label is used to figure out the end of the last function */
    labels[labels.size() + 1] = a.newLabel();
    a.bind(labels[labels.size()]);

    emit_nyi("int_code_end");
}

void BeamModuleAssembler::emit_line(const ArgVal &) {
    /*
     * There is no need to align the line instruction. In the loaded
     * code, the type of the pointer will be void* and that pointer
     * will only be used in comparisons.
     */
}

void BeamModuleAssembler::emit_func_line(const ArgVal &Loc) {
    emit_line(Loc);
}

void BeamModuleAssembler::emit_empty_func_line() {
}

/*
 * Here follows stubs for instructions that should never be called.
 */

void BeamModuleAssembler::emit_i_debug_breakpoint() {
    emit_nyi("i_debug_breakpoint should never be called");
}

void BeamModuleAssembler::emit_i_generic_breakpoint() {
    emit_nyi("i_generic_breakpoint should never be called");
}

void BeamModuleAssembler::emit_trace_jump(const ArgVal &) {
    emit_nyi("trace_jump should never be called");
}

void BeamModuleAssembler::emit_call_error_handler() {
    emit_nyi("call_error_handler should never be called");
}

void BeamModuleAssembler::codegen(JitAllocator *allocator,
                                  const void **executable_ptr,
                                  void **writable_ptr,
                                  const BeamCodeHeader *in_hdr,
                                  const BeamCodeHeader **out_exec_hdr,
                                  BeamCodeHeader **out_rw_hdr) {
    const BeamCodeHeader *code_hdr_exec;
    BeamCodeHeader *code_hdr_rw;

    codegen(allocator, executable_ptr, writable_ptr);

    {
        auto offset = code.labelOffsetFromBase(codeHeader);

        auto base_exec = (const char *)(*executable_ptr);
        code_hdr_exec = (const BeamCodeHeader *)&base_exec[offset];

        auto base_rw = (const char *)(*writable_ptr);
        code_hdr_rw = (BeamCodeHeader *)&base_rw[offset];
    }

    sys_memcpy(code_hdr_rw, in_hdr, sizeof(BeamCodeHeader));
    code_hdr_rw->on_load = getOnLoad();

    for (unsigned i = 0; i < functions.size(); i++) {
        ErtsCodeInfo *ci = (ErtsCodeInfo *)getCode(functions[i]);
        code_hdr_rw->functions[i] = ci;
    }

    char *module_end = (char *)code.baseAddress() + a.offset();
    code_hdr_rw->functions[functions.size()] = (ErtsCodeInfo *)module_end;

    *out_exec_hdr = code_hdr_exec;
    *out_rw_hdr = code_hdr_rw;
}

void BeamModuleAssembler::codegen(JitAllocator *allocator,
                                  const void **executable_ptr,
                                  void **writable_ptr) {
    _codegen(allocator, executable_ptr, writable_ptr);
}

void BeamModuleAssembler::codegen(char *buff, size_t len) {
    code.flatten();
    code.resolveUnresolvedLinks();
    ERTS_ASSERT(code.codeSize() <= len);
    code.relocateToBase((uint64_t)buff);
    code.copyFlattenedData(buff,
                           code.codeSize(),
                           CodeHolder::kCopyPadSectionBuffer);
}

BeamCodeHeader *BeamModuleAssembler::getCodeHeader() {
    return (BeamCodeHeader *)getCode(codeHeader);
}

const ErtsCodeInfo *BeamModuleAssembler::getOnLoad() {
    ERTS_ASSERT(!"NYI");
    if (on_load.isValid()) {
        return erts_code_to_codeinfo((ErtsCodePtr)getCode(on_load));
    } else {
        return 0;
    }
}

unsigned BeamModuleAssembler::patchCatches(char *rw_base) {
    ERTS_ASSERT(!"NYI");
    unsigned catch_no = BEAM_CATCHES_NIL;

    for (const auto &c : catches) {
        const auto &patch = c.patch;
        ErtsCodePtr handler;

        handler = (ErtsCodePtr)getCode(c.handler);
        catch_no = beam_catches_cons(handler, catch_no, nullptr);

        /* Patch the `mov` instruction with the catch tag */
        auto offset = code.labelOffsetFromBase(patch.where);
        auto where = (unsigned *)&rw_base[offset + patch.ptr_offs];

        ASSERT(0x7fffffff == *where);
        Eterm catch_term = make_catch(catch_no);

        /* With the current tag scheme, more than 33 million
         * catches can exist at once. */
        ERTS_ASSERT(catch_term >> 31 == 0);
        *where = (unsigned)catch_term;
    }

    return catch_no;
}

void BeamModuleAssembler::patchImport(char *rw_base,
                                      unsigned index,
                                      BeamInstr I) {
    ERTS_ASSERT(!"NYI");
    for (const auto &patch : imports[index].patches) {
        auto offset = code.labelOffsetFromBase(patch.where);
        auto where = (Eterm *)&rw_base[offset + patch.ptr_offs];

        ASSERT(LLONG_MAX == *where);
        *where = I + patch.val_offs;
    }
}

void BeamModuleAssembler::patchLambda(char *rw_base,
                                      unsigned index,
                                      BeamInstr I) {
    ERTS_ASSERT(!"NYI");
    for (const auto &patch : lambdas[index].patches) {
        auto offset = code.labelOffsetFromBase(patch.where);
        auto where = (Eterm *)&rw_base[offset + patch.ptr_offs];

        ASSERT(LLONG_MAX == *where);
        *where = I + patch.val_offs;
    }
}

void BeamModuleAssembler::patchLiteral(char *rw_base,
                                       unsigned index,
                                       Eterm lit) {
    ERTS_ASSERT(!"NYI");
    for (const auto &patch : literals[index].patches) {
        auto offset = code.labelOffsetFromBase(patch.where);
        auto where = (Eterm *)&rw_base[offset + patch.ptr_offs];

        ASSERT(LLONG_MAX == *where);
        *where = lit + patch.val_offs;
    }
}

void BeamModuleAssembler::patchStrings(char *rw_base,
                                       const byte *string_table) {
    ERTS_ASSERT(!"NYI");
    for (const auto &patch : strings) {
        auto offset = code.labelOffsetFromBase(patch.where);
        auto where = (const byte **)&rw_base[offset + 2];

        ASSERT(LLONG_MAX == (Eterm)*where);
        *where = string_table + patch.val_offs;
    }
}
