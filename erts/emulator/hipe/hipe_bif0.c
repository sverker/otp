/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2001-2012. All Rights Reserved.
 *
 * The contents of this file are subject to the Erlang Public License,
 * Version 1.1, (the "License"); you may not use this file except in
 * compliance with the License. You should have received a copy of the
 * Erlang Public License along with this software. If not, it can be
 * retrieved online at http://www.erlang.org/.
 *
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
 * the License for the specific language governing rights and limitations
 * under the License.
 *
 * %CopyrightEnd%
 */
/*
 * hipe_bif0.c
 *
 * Compiler and linker support.
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif
#include "sys.h"
#include "error.h"
#include "erl_vm.h"
#include "global.h"
#include "erl_process.h"
#include "bif.h"
#include "big.h"
#include "beam_load.h"
#include "erl_db.h"
#include "hash.h"
#include "erl_bits.h"
#include "erl_binary.h"
#ifdef HIPE
#include <stddef.h>	/* offsetof() */
#include "hipe_arch.h"
#include "hipe_stack.h"
#include "hipe_mode_switch.h"
#include "hipe_native_bif.h"
#include "hipe_bif0.h"
/* We need hipe_literals.h for HIPE_SYSTEM_CRC, but it redefines
   a few constants. #undef them here to avoid warnings. */
#undef F_TIMO
#undef THE_NON_VALUE
#undef ERL_FUN_SIZE
#include "hipe_literals.h"
#endif

#define BeamOpCode(Op)	((Uint)BeamOp(Op))

#define SVERK_DEBUG

#ifdef SVERK_DEBUG
#define SVERK_TRACE1(FMT,A)           erts_fprintf(stderr, "SVERK: "FMT"\n", A)
#define SVERK_TRACE2(FMT,A,B)         erts_fprintf(stderr, "SVERK: "FMT"\n", A,B)
#define SVERK_TRACE3(FMT,A,B,C)       erts_fprintf(stderr, "SVERK: "FMT"\n", A,B,C)
#define SVERK_TRACE6(FMT,A,B,C,D,E,F) erts_fprintf(stderr, "SVERK: "FMT"\n", A,B,C,D,E,F)

#else
#define SVERK_TRACE1(FMT, A)
#define SVERK_TRACE2(FMT, A,B)
#define SVERK_TRACE3(FMT, A,B,C)
#define SVERK_TRACE6(FMT, A,B,C,D,E,F)
#endif

int term_to_Sint32(Eterm term, Sint *sp)
{
    Sint val;

    if (!term_to_Sint(term, &val))
	return 0;
    if ((Sint)(Sint32)val != val)
	return 0;
    *sp = val;
    return 1;
}

static Eterm Uint_to_term(Uint x, Process *p)
{
    if (IS_USMALL(0, x)) {
	return make_small(x);
    } else {
	Eterm *hp = HAlloc(p, BIG_UINT_HEAP_SIZE);
	return uint_to_big(x, hp);
    }
}

void *term_to_address(Eterm arg)
{
    Uint u;
    return term_to_Uint(arg, &u) ? (void*)u : NULL;
}

static Eterm address_to_term(const void *address, Process *p)
{
    return Uint_to_term((Uint)address, p);
}

/*
 * BIFs for reading and writing memory. Used internally by HiPE.
 */
#if 0 /* XXX: unused */
BIF_RETTYPE hipe_bifs_read_u8_1(BIF_ALIST_1)
{
    unsigned char *address = term_to_address(BIF_ARG_1);
    if (!address)
	BIF_ERROR(BIF_P, BADARG);
    BIF_RET(make_small(*address));
}
#endif

#if 0 /* XXX: unused */
BIF_RETTYPE hipe_bifs_read_u32_1(BIF_ALIST_1)
{
    Uint32 *address = term_to_address(BIF_ARG_1);
    if (!address || !hipe_word32_address_ok(address))
	BIF_ERROR(BIF_P, BADARG);
    BIF_RET(Uint_to_term(*address, BIF_P));
}
#endif

BIF_RETTYPE hipe_bifs_write_u8_2(BIF_ALIST_2)
{
    unsigned char *address;

    address = term_to_address(BIF_ARG_1);
    if (!address || is_not_small(BIF_ARG_2))
	BIF_ERROR(BIF_P, BADARG);
    *address = unsigned_val(BIF_ARG_2);
    BIF_RET(NIL);
}

#if 0 /* XXX: unused */
BIF_RETTYPE hipe_bifs_write_s32_2(BIF_ALIST_2)
{
    Sint32 *address;
    Sint value;

    address = term_to_address(BIF_ARG_1);
    if (!address || !hipe_word32_address_ok(address))
	BIF_ERROR(BIF_P, BADARG);
    if (!term_to_Sint32(BIF_ARG_2, &value))
	BIF_ERROR(BIF_P, BADARG);
    *address = value;
    BIF_RET(NIL);
}
#endif

BIF_RETTYPE hipe_bifs_write_u32_2(BIF_ALIST_2)
{
    Uint32 *address;
    Uint value;

    address = term_to_address(BIF_ARG_1);
    if (!address || !hipe_word32_address_ok(address))
	BIF_ERROR(BIF_P, BADARG);
    if (!term_to_Uint(BIF_ARG_2, &value))
	BIF_ERROR(BIF_P, BADARG);
    if ((Uint)(Uint32)value != value)
	BIF_ERROR(BIF_P, BADARG);
    *address = value;
    hipe_flush_icache_word(address);
    BIF_RET(NIL);
}

/*
 * BIFs for mutable bytearrays.
 */
BIF_RETTYPE hipe_bifs_bytearray_2(BIF_ALIST_2)
{
    Sint nelts;
    Eterm bin;

    if (is_not_small(BIF_ARG_1) ||
	(nelts = signed_val(BIF_ARG_1)) < 0 ||
	!is_byte(BIF_ARG_2))
	BIF_ERROR(BIF_P, BADARG);
    bin = new_binary(BIF_P, NULL, nelts);
    memset(binary_bytes(bin), unsigned_val(BIF_ARG_2), nelts);
    BIF_RET(bin);
}

static inline unsigned char *bytearray_lvalue(Eterm bin, Eterm idx)
{
    Sint i;
    unsigned char *bytes;
#ifndef DEBUG
    ERTS_DECLARE_DUMMY(Uint bitoffs);
    ERTS_DECLARE_DUMMY(Uint bitsize);
#else
    Uint bitoffs;
    Uint bitsize;
#endif

    if (is_not_binary(bin) ||
	is_not_small(idx) ||
	(i = unsigned_val(idx)) >= binary_size(bin))
	return NULL;
    ERTS_GET_BINARY_BYTES(bin, bytes, bitoffs, bitsize);
    ASSERT(bitoffs == 0);
    ASSERT(bitsize == 0);
    return bytes + i;
}

BIF_RETTYPE hipe_bifs_bytearray_sub_2(BIF_ALIST_2)
{
    unsigned char *bytep;

    bytep = bytearray_lvalue(BIF_ARG_1, BIF_ARG_2);
    if (!bytep)
	BIF_ERROR(BIF_P, BADARG);
    BIF_RET(make_small(*bytep));
}

BIF_RETTYPE hipe_bifs_bytearray_update_3(BIF_ALIST_3)
{
    unsigned char *bytep;

    bytep = bytearray_lvalue(BIF_ARG_1, BIF_ARG_2);
    if (!bytep || !is_byte(BIF_ARG_3))
	BIF_ERROR(BIF_P, BADARG);
    *bytep = unsigned_val(BIF_ARG_3);
    BIF_RET(BIF_ARG_1);
}

BIF_RETTYPE hipe_bifs_bitarray_2(BIF_ALIST_2)
{
    Sint nbits;
    Uint nbytes;
    Eterm bin;
    int bytemask;

    if (is_not_small(BIF_ARG_1))
	BIF_ERROR(BIF_P, BADARG);
    nbits = signed_val(BIF_ARG_1);
    if (nbits < 0)
	BIF_ERROR(BIF_P, BADARG);
    if (BIF_ARG_2 == am_false)
	bytemask = 0;
    else if (BIF_ARG_2 == am_true)
	bytemask = ~0;
    else
	BIF_ERROR(BIF_P, BADARG);
    nbytes = ((Uint)nbits + ((1 << 3) - 1)) >> 3;
    bin = new_binary(BIF_P, NULL, nbytes);
    memset(binary_bytes(bin), bytemask, nbytes);
    BIF_RET(bin);
}

BIF_RETTYPE hipe_bifs_bitarray_update_3(BIF_ALIST_3)
{
    unsigned char *bytes, bytemask;
    Uint bitnr, bytenr;
    int set;
#ifndef DEBUG
    ERTS_DECLARE_DUMMY(Uint bitoffs);
    ERTS_DECLARE_DUMMY(Uint bitsize);
#else
    Uint bitoffs;
    Uint bitsize;
#endif

    if (is_not_binary(BIF_ARG_1))
	BIF_ERROR(BIF_P, BADARG);
    if (is_not_small(BIF_ARG_2))
	BIF_ERROR(BIF_P, BADARG);
    bitnr = unsigned_val(BIF_ARG_2);
    bytenr = bitnr >> 3;
    if (bytenr >= binary_size(BIF_ARG_1))
	BIF_ERROR(BIF_P, BADARG);
    if (BIF_ARG_3 == am_false)
	set = 0;
    else if (BIF_ARG_3 == am_true)
	set = 1;
    else
	BIF_ERROR(BIF_P, BADARG);
    ERTS_GET_BINARY_BYTES(BIF_ARG_1, bytes, bitoffs, bitsize);
    ASSERT(bitoffs == 0);
    ASSERT(bitsize == 0);
    bytemask = 1 << (bitnr & ((1 << 3) - 1));
    if (set)
	bytes[bytenr] |= bytemask;
    else
	bytes[bytenr] &= ~bytemask;
    BIF_RET(BIF_ARG_1);
}

BIF_RETTYPE hipe_bifs_bitarray_sub_2(BIF_ALIST_2)
{
    unsigned char *bytes, bytemask;
    Uint bitnr, bytenr;
#ifndef DEBUG
    ERTS_DECLARE_DUMMY(Uint bitoffs);
    ERTS_DECLARE_DUMMY(Uint bitsize);
#else
    Uint bitoffs;
    Uint bitsize;
#endif


    if (is_not_binary(BIF_ARG_1))
	BIF_ERROR(BIF_P, BADARG);
    if (is_not_small(BIF_ARG_2))
	BIF_ERROR(BIF_P, BADARG);
    bitnr = unsigned_val(BIF_ARG_2);
    bytenr = bitnr >> 3;
    if (bytenr >= binary_size(BIF_ARG_1))
	BIF_ERROR(BIF_P, BADARG);
    ERTS_GET_BINARY_BYTES(BIF_ARG_1, bytes, bitoffs, bitsize);
    ASSERT(bitoffs == 0);
    ASSERT(bitsize == 0);
    bytemask = 1 << (bitnr & ((1 << 3) - 1));
    if ((bytes[bytenr] & bytemask) == 0)
	BIF_RET(am_false);
    else
	BIF_RET(am_true);
}

/*
 * BIFs for SML-like mutable arrays and reference cells.
 * For now, limited to containing immediate data.
 */
#if 1	/* use bignums as carriers, easier on the gc */
#define make_array_header(sz)	make_pos_bignum_header((sz))
#define array_header_arity(h)	header_arity((h))
#define make_array(hp)		make_big((hp))
#define is_not_array(x)		is_not_big((x))
#define array_val(x)		big_val((x))
#else	/* use tuples as carriers, easier debugging, harder on the gc */
#define make_array_header(sz)	make_arityval((sz))
#define array_header_arity(h)	arityval((h))
#define make_array(hp)		make_tuple((hp))
#define is_not_array(x)		is_not_tuple((x))
#define array_val(x)		tuple_val((x))
#endif
#define array_length(a)		array_header_arity(array_val((a))[0])

BIF_RETTYPE hipe_bifs_array_2(BIF_ALIST_2)
{
    Eterm *hp;
    Sint nelts, i;

    if (is_not_small(BIF_ARG_1) ||
	(nelts = signed_val(BIF_ARG_1)) < 0 ||
	is_not_immed(BIF_ARG_2))
	BIF_ERROR(BIF_P, BADARG);
    if (nelts == 0)	/* bignums must not be empty */
	BIF_RET(make_small(0));
    hp = HAlloc(BIF_P, 1+nelts);
    hp[0] = make_array_header(nelts);
    for (i = 1; i <= nelts; ++i)
	hp[i] = BIF_ARG_2;
    BIF_RET(make_array(hp));
}

BIF_RETTYPE hipe_bifs_array_length_1(BIF_ALIST_1)
{
    if (is_not_array(BIF_ARG_1)) {
	if (BIF_ARG_1 == make_small(0))	/* fixnum 0 represents empty arrays */
	    BIF_RET(make_small(0));
	BIF_ERROR(BIF_P, BADARG);
    }
    BIF_RET(make_small(array_header_arity(array_val(BIF_ARG_1)[0])));
}

BIF_RETTYPE hipe_bifs_array_sub_2(BIF_ALIST_2)
{
    Uint i;

    if (is_not_small(BIF_ARG_2) ||
	is_not_array(BIF_ARG_1) ||
	(i = unsigned_val(BIF_ARG_2)) >= array_length(BIF_ARG_1))
	BIF_ERROR(BIF_P, BADARG);
    BIF_RET(array_val(BIF_ARG_1)[i+1]);
}

BIF_RETTYPE hipe_bifs_array_update_3(BIF_ALIST_3)
{
    Uint i;

    if (is_not_immed(BIF_ARG_3) ||
	is_not_small(BIF_ARG_2) ||
	is_not_array(BIF_ARG_1) ||
	(i = unsigned_val(BIF_ARG_2)) >= array_length(BIF_ARG_1))
	BIF_ERROR(BIF_P, BADARG);
    array_val(BIF_ARG_1)[i+1] = BIF_ARG_3;
    BIF_RET(BIF_ARG_1);
}

BIF_RETTYPE hipe_bifs_ref_1(BIF_ALIST_1)
{
    Eterm *hp;

    if (is_not_immed(BIF_ARG_1))
	BIF_RET(BADARG);
    hp = HAlloc(BIF_P, 1+1);
    hp[0] = make_array_header(1);
    hp[1] = BIF_ARG_1;
    BIF_RET(make_array(hp));
}

BIF_RETTYPE hipe_bifs_ref_get_1(BIF_ALIST_1)
{
    if (is_not_array(BIF_ARG_1) ||
	array_val(BIF_ARG_1)[0] != make_array_header(1))
	BIF_ERROR(BIF_P, BADARG);
    BIF_RET(array_val(BIF_ARG_1)[1]);
}

BIF_RETTYPE hipe_bifs_ref_set_2(BIF_ALIST_2)
{
    if (is_not_immed(BIF_ARG_2) ||
	is_not_array(BIF_ARG_1) ||
	array_val(BIF_ARG_1)[0] != make_array_header(1))
	BIF_ERROR(BIF_P, BADARG);
    array_val(BIF_ARG_1)[1] = BIF_ARG_2;
    BIF_RET(BIF_ARG_1);
}

/*
 * Allocate memory and copy machine code to it.
 */
BIF_RETTYPE hipe_bifs_enter_code_2(BIF_ALIST_2)
{
    Uint nrbytes;
    void *bytes;
    void *address;
    Eterm trampolines;
    Eterm *hp;
#ifndef DEBUG
    ERTS_DECLARE_DUMMY(Uint bitoffs);
    ERTS_DECLARE_DUMMY(Uint bitsize);
#else
    Uint bitoffs;
    Uint bitsize;
#endif

    if (is_not_binary(BIF_ARG_1))
	BIF_ERROR(BIF_P, BADARG);
    nrbytes = binary_size(BIF_ARG_1);
    ERTS_GET_BINARY_BYTES(BIF_ARG_1, bytes, bitoffs, bitsize);
    ASSERT(bitoffs == 0);
    ASSERT(bitsize == 0);
    trampolines = NIL;
#ifdef HIPE_ALLOC_CODE
    address = HIPE_ALLOC_CODE(nrbytes, BIF_ARG_2, &trampolines, BIF_P);
    if (!address)
	BIF_ERROR(BIF_P, BADARG);
#else
    if (is_not_nil(BIF_ARG_2))
	BIF_ERROR(BIF_P, BADARG);
    address = erts_alloc(ERTS_ALC_T_HIPE, nrbytes);
#endif
    memcpy(address, bytes, nrbytes);
    hipe_flush_icache_range(address, nrbytes);
    hp = HAlloc(BIF_P, 3);
    hp[0] = make_arityval(2);
    hp[1] = address_to_term(address, BIF_P);
    hp[2] = trampolines;
    BIF_RET(make_tuple(hp));
}

/*
 * Allocate memory for arbitrary non-Erlang data.
 */
BIF_RETTYPE hipe_bifs_alloc_data_2(BIF_ALIST_2)
{
    Uint align, nrbytes;
    void *block;

    if (is_not_small(BIF_ARG_1) || is_not_small(BIF_ARG_2) ||
	(align = unsigned_val(BIF_ARG_1),
	 align != sizeof(long) && align != sizeof(double)))
	BIF_ERROR(BIF_P, BADARG);
    nrbytes = unsigned_val(BIF_ARG_2);
    if (nrbytes == 0)
	BIF_RET(make_small(0));
    block = erts_alloc(ERTS_ALC_T_HIPE, nrbytes);
    if ((unsigned long)block & (align-1))
	fprintf(stderr, "%s: erts_alloc(%lu) returned %p which is not %lu-byte aligned\r\n",
		__FUNCTION__, (unsigned long)nrbytes, block, (unsigned long)align);
    BIF_RET(address_to_term(block, BIF_P));
}

/*
 * Statistics on hipe constants: size of HiPE constants, in words.
 */
unsigned int hipe_constants_size = 0;

BIF_RETTYPE hipe_bifs_constants_size_0(BIF_ALIST_0)
{
    BIF_RET(make_small(hipe_constants_size));
}

/*
 * Merging constant Erlang terms.
 * Uses the constants pool and a hash table of all top-level
 * terms merged so far. (Sub-terms are not merged.)
 */
struct const_term {
    HashBucket bucket;
    Eterm val;		/* tagged pointer to mem[0] */
    Eterm mem[1];	/* variable size */
};

static Hash const_term_table;
static ErlOffHeap const_term_table_off_heap;

static HashValue const_term_hash(void *tmpl)
{
    return make_hash2((Eterm)tmpl);
}

static int const_term_cmp(void *tmpl, void *bucket)
{
    return !eq((Eterm)tmpl, ((struct const_term*)bucket)->val);
}

static void *const_term_alloc(void *tmpl)
{
    Eterm obj;
    Uint size;
    Uint alloc_size;
    Eterm *hp;
    struct const_term *p;

    obj = (Eterm)tmpl;
    ASSERT(is_not_immed(obj));
    size = size_object(obj);
    alloc_size = size + (offsetof(struct const_term, mem)/sizeof(Eterm));
    hipe_constants_size += alloc_size;

    p = (struct const_term*)erts_alloc(ERTS_ALC_T_HIPE, alloc_size * sizeof(Eterm));

    /* I have absolutely no idea if having a private 'off_heap'
       works or not. _Some_ off_heap object is required for
       REFC_BINARY and FUN values, but _where_ it should be is
       a complete mystery to me. */
    hp = &p->mem[0];
    p->val = copy_struct(obj, size, &hp, &const_term_table_off_heap);

    return &p->bucket;
}

static void init_const_term_table(void)
{
    HashFunctions f;
    f.hash = (H_FUN) const_term_hash;
    f.cmp = (HCMP_FUN) const_term_cmp;
    f.alloc = (HALLOC_FUN) const_term_alloc;
    f.free = (HFREE_FUN) NULL;
    hash_init(ERTS_ALC_T_HIPE, &const_term_table, "const_term_table", 97, f);
}

BIF_RETTYPE hipe_bifs_merge_term_1(BIF_ALIST_1)
{
    static int init_done = 0;
    struct const_term *p;
    Eterm val;

    val = BIF_ARG_1;
    if (is_not_immed(val)) {
	if (!init_done) {
	    init_const_term_table();
	    init_done = 1;
	}
	p = (struct const_term*)hash_put(&const_term_table, (void*)val);
	val = p->val;
    }
    BIF_RET(val);
}

struct hipe_mfa {
    Eterm mod;
    Eterm fun;
    Uint  ari;
};

static int term_to_mfa(Eterm term, struct hipe_mfa *mfa)
{
    Eterm mod, fun, a;
    Uint ari;

    if (is_not_tuple(term))
	return 0;
    if (tuple_val(term)[0] != make_arityval(3))
	return 0;
    mod = tuple_val(term)[1];
    if (is_not_atom(mod))
	return 0;
    mfa->mod = mod;
    fun = tuple_val(term)[2];
    if (is_not_atom(fun))
	return 0;
    mfa->fun = fun;
    a = tuple_val(term)[3];
    if (is_not_small(a))
	return 0;
    ari = unsigned_val(a);
    if (ari > 255)
	return 0;
    mfa->ari = ari;
    return 1;
}

#ifdef DEBUG_LINKER
static void print_mfa(Eterm mod, Eterm fun, unsigned int ari)
{
    erts_printf("%T:%T/%u", mod, fun, ari);
}
#endif

/*
 * Convert {M,F,A} to pointer to first insn after initial func_info.
 */
static Uint *hipe_find_emu_address(Eterm mod, Eterm name, unsigned int arity)
{
    Module *modp;
    Uint *code_base;
    int i, n;

    modp = erts_get_module(mod, erts_active_code_ix());
    if (modp == NULL || (code_base = modp->curr.code) == NULL)
	return NULL;
    n = code_base[MI_NUM_FUNCTIONS];
    for (i = 0; i < n; ++i) {
	Uint *code_ptr = (Uint*)code_base[MI_FUNCTIONS+i];
	ASSERT(code_ptr[0] == BeamOpCode(op_i_func_info_IaaI));
	if (code_ptr[3] == name && code_ptr[4] == arity)
	    return code_ptr+5;
    }
    return NULL;
}

Uint *hipe_bifs_find_pc_from_mfa(Eterm term)
{
    struct hipe_mfa mfa;

    if (!term_to_mfa(term, &mfa))
	return NULL;
    return hipe_find_emu_address(mfa.mod, mfa.fun, mfa.ari);
}

BIF_RETTYPE hipe_bifs_fun_to_address_1(BIF_ALIST_1)
{
    Eterm *pc = hipe_bifs_find_pc_from_mfa(BIF_ARG_1);
    if (!pc)
	BIF_ERROR(BIF_P, BADARG);
    BIF_RET(address_to_term(pc, BIF_P));
}


BIF_RETTYPE hipe_bifs_set_native_address_3(BIF_ALIST_3)
{
    Eterm *pc;
    void *address;
    int is_closure;
    struct hipe_mfa mfa;

    switch (BIF_ARG_3) {
      case am_false:
	is_closure = 0;
	break;
      case am_true:
	is_closure = 1;
	break;
      default:
	BIF_ERROR(BIF_P, BADARG);
    }
    address = term_to_address(BIF_ARG_2);
    if (!address)
	BIF_ERROR(BIF_P, BADARG);

    /* The mfa is needed again later, otherwise we could
       simply have called hipe_bifs_find_pc_from_mfa(). */
    if (!term_to_mfa(BIF_ARG_1, &mfa))
	BIF_ERROR(BIF_P, BADARG);
    pc = hipe_find_emu_address(mfa.mod, mfa.fun, mfa.ari);

    if (pc) {
	DBG_TRACE_MFA(mfa.mod,mfa.fun,mfa.ari, "set beam call trap at %p -> %p", pc, address);
	hipe_set_call_trap(pc, address, is_closure);
	BIF_RET(am_true);
    }
    DBG_TRACE_MFA(mfa.mod,mfa.fun,mfa.ari, "failed set call trap to %p, no beam code found", address);
    BIF_RET(am_false);
}


BIF_RETTYPE hipe_bifs_enter_sdesc_1(BIF_ALIST_1)
{
    struct sdesc *sdesc;

    sdesc = hipe_decode_sdesc(BIF_ARG_1);
    if (!sdesc) {
	fprintf(stderr, "%s: bad sdesc!\r\n", __FUNCTION__);
	BIF_ERROR(BIF_P, BADARG);
    }
    if (hipe_put_sdesc(sdesc) != sdesc) {
	fprintf(stderr, "%s: duplicate entry!\r\n", __FUNCTION__);
	BIF_ERROR(BIF_P, BADARG);
    }
    BIF_RET(NIL);
}

/*
 * Hash table mapping {M,F,A} to nbif address.
 */
struct nbif {
    HashBucket bucket;
    Eterm mod;
    Eterm fun;
    unsigned arity;
    const void *address;
};

static struct nbif nbifs[BIF_SIZE] = {
#define BIF_LIST(MOD,FUN,ARY,CFUN,IX)	\
	{ {0,0}, MOD, FUN, ARY, &nbif_##CFUN },
#include "erl_bif_list.h"
#undef BIF_LIST
};

#define NBIF_HASH(m,f,a)	((m)*(f)+(a))
static Hash nbif_table;

static HashValue nbif_hash(struct nbif *x)
{
    return NBIF_HASH(x->mod, x->fun, x->arity);
}

static int nbif_cmp(struct nbif *x, struct nbif *y)
{
    return !(x->mod == y->mod && x->fun == y->fun && x->arity == y->arity);
}

static struct nbif *nbif_alloc(struct nbif *x)
{
    return x;	/* pre-allocated */
}

static void init_nbif_table(void)
{
    HashFunctions f;
    int i;

    f.hash = (H_FUN) nbif_hash;
    f.cmp = (HCMP_FUN) nbif_cmp;
    f.alloc = (HALLOC_FUN) nbif_alloc;
    f.free = NULL;

    hash_init(ERTS_ALC_T_NBIF_TABLE, &nbif_table, "nbif_table", 500, f);

    for (i = 0; i < BIF_SIZE; ++i)
	hash_put(&nbif_table, &nbifs[i]);
}

static const void *nbif_address(Eterm mod, Eterm fun, unsigned arity)
{
    struct nbif tmpl;
    struct nbif *nbif;

    tmpl.mod = mod;
    tmpl.fun = fun;
    tmpl.arity = arity;

    nbif = hash_get(&nbif_table, &tmpl);
    return nbif ? nbif->address : NULL;
}

/*
 * hipe_bifs_bif_address(M,F,A) -> address or false
 */
BIF_RETTYPE hipe_bifs_bif_address_3(BIF_ALIST_3)
{
    const void *address;
    static int init_done = 0;

    if (!init_done) {
	init_nbif_table();
	init_done = 1;
    }

    if (is_not_atom(BIF_ARG_1) ||
	is_not_atom(BIF_ARG_2) ||
	is_not_small(BIF_ARG_3) ||
	signed_val(BIF_ARG_3) < 0)
	BIF_RET(am_false);

    address = nbif_address(BIF_ARG_1, BIF_ARG_2, unsigned_val(BIF_ARG_3));
    if (address)
	BIF_RET(address_to_term(address, BIF_P));
    BIF_RET(am_false);
}

/*
 * Hash table mapping primops to their addresses.
 */
struct primop {
    HashBucket bucket;	/* bucket.hvalue == atom_val(name) */
    const void *address;
#if defined(__arm__)
    void *trampoline;
#endif
};

static struct primop primops[] = {
#define PRIMOP_LIST(ATOM,ADDRESS)	{ {0,_unchecked_atom_val(ATOM)}, ADDRESS },
#include "hipe_primops.h"
#undef PRIMOP_LIST
};

static Hash primop_table;

static HashValue primop_hash(void *tmpl)
{
    return ((struct primop*)tmpl)->bucket.hvalue;	/* pre-initialised */
}

static int primop_cmp(void *tmpl, void *bucket)
{
    return 0;	/* hvalue matched so nothing further to do */
}

static void *primop_alloc(void *tmpl)
{
    return tmpl;	/* pre-allocated */
}

static void init_primop_table(void)
{
    HashFunctions f;
    int i;
    static int init_done = 0;

    if (init_done)
	return;
    init_done = 1;

    f.hash = (H_FUN) primop_hash;
    f.cmp = (HCMP_FUN) primop_cmp;
    f.alloc = (HALLOC_FUN) primop_alloc;
    f.free = NULL;

    hash_init(ERTS_ALC_T_HIPE, &primop_table, "primop_table", 50, f);

    for (i = 0; i < sizeof(primops)/sizeof(primops[0]); ++i)
	hash_put(&primop_table, &primops[i]);
}

static struct primop *primop_table_get(Eterm name)
{
    struct primop tmpl;

    init_primop_table();
    tmpl.bucket.hvalue = atom_val(name);
    return hash_get(&primop_table, &tmpl);
}

#if defined(__arm__)
static struct primop *primop_table_put(Eterm name)
{
    struct primop tmpl;

    init_primop_table();
    tmpl.bucket.hvalue = atom_val(name);
    return hash_put(&primop_table, &tmpl);
}

void *hipe_primop_get_trampoline(Eterm name)
{
    struct primop *primop = primop_table_get(name);
    return primop ? primop->trampoline : NULL;
}

void hipe_primop_set_trampoline(Eterm name, void *trampoline)
{
    struct primop *primop = primop_table_put(name);
    primop->trampoline = trampoline;
}
#endif

/*
 * hipe_bifs_primop_address(Atom) -> address or false
 */
BIF_RETTYPE hipe_bifs_primop_address_1(BIF_ALIST_1)
{
    const struct primop *primop;

    if (is_not_atom(BIF_ARG_1))
	BIF_RET(am_false);
    primop = primop_table_get(BIF_ARG_1);
    if (!primop)
	BIF_RET(am_false);
    BIF_RET(address_to_term(primop->address, BIF_P));
}

BIF_RETTYPE hipe_bifs_atom_to_word_1(BIF_ALIST_1)
{
    if (is_not_atom(BIF_ARG_1))
	BIF_ERROR(BIF_P, BADARG);
    BIF_RET(Uint_to_term(BIF_ARG_1, BIF_P));
}

BIF_RETTYPE hipe_bifs_term_to_word_1(BIF_ALIST_1)
{
    BIF_RET(Uint_to_term(BIF_ARG_1, BIF_P));
}

/* XXX: this is really a primop, not a BIF */
BIF_RETTYPE hipe_conv_big_to_float(BIF_ALIST_1)
{
    Eterm res;
    Eterm *hp;
    FloatDef f;

    if (is_not_big(BIF_ARG_1))
	BIF_ERROR(BIF_P, BADARG);
    if (big_to_double(BIF_ARG_1, &f.fd) < 0)
	BIF_ERROR(BIF_P, BADARG);
    hp = HAlloc(BIF_P, FLOAT_SIZE_OBJECT);
    res = make_float(hp);
    PUT_DOUBLE(f, hp);
    BIF_RET(res);
}

#ifdef NO_FPE_SIGNALS

/* 
   This is the current solution to make hipe run without FPE.
   The native code is the same except that a call to this primop
   is made after _every_ float operation to check the result.
   The native fcheckerror still done later will detect if an
   "emulated" FPE has occured.
   We use p->hipe.float_result to avoid passing a 'double' argument,
   which has its own calling convention (on amd64 at least).
   Simple and slow...
*/
void hipe_emulate_fpe(Process* p)
{
    if (!finite(p->hipe.float_result)) {
	p->fp_exception = 1;
    }
}
#endif


/*
 * args: Nativecodeaddress, Module, {Uniq, Index, BeamAddress}
 */
BIF_RETTYPE hipe_bifs_make_fe_3(BIF_ALIST_3)
{
    Eterm mod;
    Uint index;
    Uint uniq;
    void *beam_address;
    ErlFunEntry *fe;
    Eterm *tp;
    void *native_address;

    native_address = term_to_address(BIF_ARG_1);
    if (!native_address)
	BIF_ERROR(BIF_P, BADARG);

    if (is_not_atom(BIF_ARG_2))
	BIF_ERROR(BIF_P, BADARG);
    mod = BIF_ARG_2;

    if (is_not_tuple(BIF_ARG_3) ||
	(arityval(*tuple_val(BIF_ARG_3)) != 3))
	BIF_ERROR(BIF_P, BADARG);
    tp = tuple_val(BIF_ARG_3);
    if (term_to_Uint(tp[1], &uniq) == 0)
	BIF_ERROR(BIF_P, BADARG);
    if (term_to_Uint(tp[2], &index) == 0)
	BIF_ERROR(BIF_P, BADARG);

    beam_address = term_to_address(tp[3]);
    if (!beam_address)
	BIF_ERROR(BIF_P, BADARG);

    fe = erts_get_fun_entry(mod, uniq, index);
    if (fe == NULL) {
	int i = atom_val(mod);
	char atom_buf[256];

	atom_buf[0] = '\0';
	strncat(atom_buf, (char*)atom_tab(i)->name, atom_tab(i)->len);
	printf("no fun entry for %s %ld:%ld\n", atom_buf, uniq, index);
	BIF_ERROR(BIF_P, BADARG);
    }
    fe->native_address = native_address;
    if (erts_refc_dectest(&fe->refc, 0) == 0)
	erts_erase_fun_entry(fe);
    BIF_RET(address_to_term((void *)fe, BIF_P));
}


/*
 * MFA info hash table:
 * - maps MFA to native code entry point
 * - the MFAs it calls (first_callee)
 * - the references to it (callers)
 * - maps MFA to most recent trampoline [if powerpc or arm]
 */
struct hipe_mfa_info {
    struct {
	unsigned long hvalue;
	struct hipe_mfa_info *next;
    } bucket;
    Eterm m;	/* atom */
    Eterm f;	/* atom */
    unsigned int a;
    void *remote_address;
    void *local_address;
    void *old_address;
    struct hipe_ref* first_caller;
    struct hipe_mfa_info* next_in_mod;
#if defined(__powerpc__) || defined(__ppc__) || defined(__powerpc64__) || defined(__arm__)
    void *trampoline;
#endif
};

static struct {
    unsigned int log2size;
    unsigned int mask;		/* INV: mask == (1 << log2size)-1 */
    unsigned int used;
    struct hipe_mfa_info **bucket;
    /*
     * The mfa info table is normally updated by the loader,
     * which runs in non-concurrent mode. Unfortunately runtime
     * apply operations (get_na_nofail) update the table if
     * they create a new stub for the mfa, which forces locking.
     * XXX: Redesign apply et al to avoid those updates.
     */
    erts_smp_mtx_t lock;
} hipe_mfa_info_table;


/*
 * Patch Reference Handling.
 */
struct hipe_ref {
    struct hipe_ref* next, **prevp;   /* list of refs to same callee */
    void *address;
    void *trampoline;
    unsigned int flags;
    struct hipe_ref* next_callee; /* list of refs from same caller */
#if defined(DEBUG) || defined(SVERK_DEBUG)
    struct hipe_mfa_info* callee;
    Eterm caller_m, caller_f, caller_a;
#endif
};
#define REF_FLAG_IS_LOAD_MFA		1	/* bit 0: 0 == call, 1 == load_mfa */
//SVERK #define REF_FLAG_IS_REMOTE		2	/* bit 1: 0 == local, 1 == remote */
//SVERK #define REF_FLAG_PENDING_REDIRECT	4	/* bit 2: 1 == pending redirect */
//SVERK #define REF_FLAG_PENDING_REMOVE		8	/* bit 3: 1 == pending remove */


static inline void hipe_mfa_info_table_init_lock(void)
{
    erts_smp_mtx_init(&hipe_mfa_info_table.lock, "hipe_mfait_lock");
}

static inline void hipe_mfa_info_table_lock(void)
{
    erts_smp_mtx_lock(&hipe_mfa_info_table.lock);
}

static inline void hipe_mfa_info_table_unlock(void)
{
    erts_smp_mtx_unlock(&hipe_mfa_info_table.lock);
}

#define HIPE_MFA_HASH(M,F,A)	((M) * (F) + (A))

static struct hipe_mfa_info **hipe_mfa_info_table_alloc_bucket(unsigned int size)
{
    unsigned long nbytes = size * sizeof(struct hipe_mfa_info*);
    struct hipe_mfa_info **bucket = erts_alloc(ERTS_ALC_T_HIPE, nbytes);
    sys_memzero(bucket, nbytes);
    return bucket;
}

static void hipe_mfa_info_table_grow(void)
{
    unsigned int old_size, new_size, new_mask;
    struct hipe_mfa_info **old_bucket, **new_bucket;
    unsigned int i;

    old_size = 1 << hipe_mfa_info_table.log2size;
    hipe_mfa_info_table.log2size += 1;
    new_size = 1 << hipe_mfa_info_table.log2size;
    new_mask = new_size - 1;
    hipe_mfa_info_table.mask = new_mask;
    old_bucket = hipe_mfa_info_table.bucket;
    new_bucket = hipe_mfa_info_table_alloc_bucket(new_size);
    hipe_mfa_info_table.bucket = new_bucket;
    for (i = 0; i < old_size; ++i) {
	struct hipe_mfa_info *b = old_bucket[i];
	while (b != NULL) {
	    struct hipe_mfa_info *next = b->bucket.next;
	    unsigned int j = b->bucket.hvalue & new_mask;
	    b->bucket.next = new_bucket[j];
	    new_bucket[j] = b;
	    b = next;
	}
    }
    erts_free(ERTS_ALC_T_HIPE, old_bucket);
}

static struct hipe_mfa_info *hipe_mfa_info_table_alloc(Eterm m, Eterm f, unsigned int arity)
{
    struct hipe_mfa_info *res;

    res = (struct hipe_mfa_info*)erts_alloc(ERTS_ALC_T_HIPE, sizeof(*res));
    res->m = m;
    res->f = f;
    res->a = arity;
    res->remote_address = NULL;
    res->local_address = NULL;
    res->old_address = NULL;
    res->first_caller = NULL;
    res->next_in_mod = NULL;
#if defined(__powerpc__) || defined(__ppc__) || defined(__powerpc64__) || defined(__arm__)
    res->trampoline = NULL;
#endif

    return res;
}

void hipe_mfa_info_table_init(void)
{
    unsigned int log2size, size;

    log2size = 10;
    size = 1 << log2size;
    hipe_mfa_info_table.log2size = log2size;
    hipe_mfa_info_table.mask = size - 1;
    hipe_mfa_info_table.used = 0;
    hipe_mfa_info_table.bucket = hipe_mfa_info_table_alloc_bucket(size);

    hipe_mfa_info_table_init_lock();
}

static inline struct hipe_mfa_info *hipe_mfa_info_table_get_locked(Eterm m, Eterm f, unsigned int arity)
{
    unsigned long h;
    unsigned int i;
    struct hipe_mfa_info *p;

    h = HIPE_MFA_HASH(m, f, arity);
    i = h & hipe_mfa_info_table.mask;
    p = hipe_mfa_info_table.bucket[i];
    for (; p; p = p->bucket.next)
	/* XXX: do we want to compare p->bucket.hvalue as well? */
	if (p->m == m && p->f == f && p->a == arity)
	    return p;
    return NULL;
}

static struct hipe_mfa_info *hipe_mfa_info_table_put_locked(Eterm m, Eterm f, unsigned int arity)
{
    unsigned long h;
    unsigned int i;
    struct hipe_mfa_info *p;
    unsigned int size;
    Module* modp;

    h = HIPE_MFA_HASH(m, f, arity);
    i = h & hipe_mfa_info_table.mask;
    p = hipe_mfa_info_table.bucket[i];
    for (; p; p = p->bucket.next)
	/* XXX: do we want to compare p->bucket.hvalue as well? */
	if (p->m == m && p->f == f && p->a == arity)
	    return p;
    p = hipe_mfa_info_table_alloc(m, f, arity);
    p->bucket.hvalue = h;
    p->bucket.next = hipe_mfa_info_table.bucket[i];
    hipe_mfa_info_table.bucket[i] = p;
    hipe_mfa_info_table.used += 1;
    size = 1 << hipe_mfa_info_table.log2size;
    if (hipe_mfa_info_table.used > (4*size/5))		/* rehash at 80% */
	hipe_mfa_info_table_grow();

    modp = erts_put_active_module(m);
    ASSERT(modp);
    p->next_in_mod = modp->first_hipe_mfa;
    modp->first_hipe_mfa = p;

    erts_fprintf(stderr, "hipe_mfa_info created for %T:%T/%u\n", m, f, arity);
    DBG_TRACE_MFA(m,f,arity, "hipe_mfa_info allocated at %p", p);

    return p;
}

static void hipe_mfa_set_na(Eterm m, Eterm f, unsigned int arity, void *address, int is_exported)
{
    struct hipe_mfa_info *p;

    ASSERT(is_exported);  // SVERK

    hipe_mfa_info_table_lock();
    p = hipe_mfa_info_table_put_locked(m, f, arity);
    DBG_TRACE_MFA(m,f,arity,"set native address in hipe_mfa_info at %p is_exported=%d",
		  p, is_exported);
    ASSERT(!p->local_address);
    p->local_address = address;
    p->remote_address = is_exported ? address : NULL;
	
    hipe_mfa_info_table_unlock();

}

#if defined(__powerpc__) || defined(__ppc__) || defined(__powerpc64__) || defined(__arm__)
void *hipe_mfa_get_trampoline(Eterm m, Eterm f, unsigned int arity)
{
    struct hipe_mfa_info *p;
    void *trampoline;

    hipe_mfa_info_table_lock();
    p = hipe_mfa_info_table_put_locked(m, f, arity);
    trampoline = p->trampoline;
    hipe_mfa_info_table_unlock();
    return trampoline;
}

void hipe_mfa_set_trampoline(Eterm m, Eterm f, unsigned int arity, void *trampoline)
{
    struct hipe_mfa_info *p;

    hipe_mfa_info_table_lock();
    p = hipe_mfa_info_table_put_locked(m, f, arity);
    p->trampoline = trampoline;
    hipe_mfa_info_table_unlock();
}
#endif

BIF_RETTYPE hipe_bifs_set_funinfo_native_address_3(BIF_ALIST_3)
{
    struct hipe_mfa mfa;
    void *address;
    int is_exported;

    if (!term_to_mfa(BIF_ARG_1, &mfa))
	BIF_ERROR(BIF_P, BADARG);
    address = term_to_address(BIF_ARG_2);
    if (!address)
	BIF_ERROR(BIF_P, BADARG);
    if (BIF_ARG_3 == am_true)
	is_exported = 1;
    else if (BIF_ARG_3 == am_false) {
	//is_exported = 0;
	BIF_RET(NIL);  //SVERK Try ignore local functions
    } else
	BIF_ERROR(BIF_P, BADARG);
    hipe_mfa_set_na(mfa.mod, mfa.fun, mfa.ari, address, is_exported);
    BIF_RET(NIL);
}


/* Ask if we need to block all threads
 * while loading/deleting (beam) code for this module?
 */
int hipe_need_blocking(Module* modp)
{
    struct hipe_mfa_info *p;

    /* Need to block if we have at least one native caller to this module
     */
    for (p = modp->first_hipe_mfa; p; p = p->next_in_mod) {
	if (p->first_caller) {
	    return 1;
	}
    }
    return 0;
}

void hipe_delete_code(Module* modp)
{
    struct hipe_mfa_info *p;

    for (p = modp->first_hipe_mfa; p; p = p->next_in_mod) {
	DBG_TRACE_MFA(p->m,p->f,p->a,"INVALIDATE hipe_mfa_info at %p", p);
	p->remote_address = NULL; 
	ASSERT(p->old_address == NULL);
	p->old_address = p->local_address;
	p->local_address = NULL;
    }
}

static void *hipe_make_stub(Eterm m, Eterm f, unsigned int arity, int is_remote)
{
    BeamInstr* BEAMAddress;
    void *StubAddress;
    Export* ep = NULL;

    ASSERT(is_remote);  // SVERK or?

    BEAMAddress = NULL;
    if (!is_remote)
	BEAMAddress = (BeamInstr*) hipe_find_emu_address(m, f, arity);
    if (!BEAMAddress) {
	/* if not found, stub it via the export entry */
	/* no lock needed around erts_export_get_or_make_stub() */
	ep = erts_export_get_or_make_stub(m, f, arity);
	BEAMAddress = (BeamInstr*) ep->addressv[erts_active_code_ix()];
    }
    StubAddress = hipe_make_native_stub(BEAMAddress, arity);

#if defined(DEBUG) || defined(ENABLE_DBG_TRACE_MFA)
    if (BEAMAddress != &ep->code[3]) {
	ASSERT(*BEAMAddress != (BeamInstr)em_call_error_handler);
	DBG_TRACE_MFA(m,f,arity,"hipe_make_stub to beam=%p hipe=%p is_remote=%d",
	BEAMAddress, StubAddress, is_remote);
    }
    else if (*BEAMAddress == (BeamInstr)em_call_error_handler) {
	DBG_TRACE_MFA(m,f,arity,"hipe_make_stub to error_handler hipe=%p is_remote=%d",
		      StubAddress, is_remote);
    }
    else if (*BEAMAddress == (BeamInstr)em_apply_bif) {
	DBG_TRACE_MFA(m,f,arity,"hipe_make_stub to BIF hipe=%p is_remote=%d",
	       StubAddress, is_remote);
    }
    else ASSERT(!"strange beam instruction in export entry");
#endif
    return StubAddress;
}

static void *hipe_get_na_nofail_locked(Eterm m, Eterm f, unsigned int a, int is_remote)
{
    struct hipe_mfa_info *p;
    void *address;

    p = hipe_mfa_info_table_get_locked(m, f, a);
    if (p) {
	/* find address, predicting for a runtime apply call */
	address = is_remote ? p->remote_address : p->local_address;
	if (address)
	    return address;

	/* bummer, install stub, checking if one already existed */
	address = p->remote_address;
	if (address) {
	    abort();
	    return address;
	}
    } else
	p = hipe_mfa_info_table_put_locked(m, f, a);
    address = hipe_make_stub(m, f, a, is_remote);
    /* XXX: how to tell if a BEAM MFA is exported or not? */
    p->remote_address = address;
    return address;
}

static void *hipe_get_na_nofail(Eterm m, Eterm f, unsigned int a, int is_remote)
{
    void *p;

    hipe_mfa_info_table_lock();
    p = hipe_get_na_nofail_locked(m, f, a, is_remote);
    hipe_mfa_info_table_unlock();
    return p;
}

/* used for apply/3 in hipe_mode_switch */
void *hipe_get_remote_na(Eterm m, Eterm f, unsigned int a)
{
    if (is_not_atom(m) || is_not_atom(f) || a > 255)
	return NULL;
    return hipe_get_na_nofail(m, f, a, 1);
}

/* primop, but called like a BIF for error handling purposes */
BIF_RETTYPE hipe_find_na_or_make_stub(BIF_ALIST_3)
{
    Uint arity;
    void *address;

    if (is_not_atom(BIF_ARG_1) || is_not_atom(BIF_ARG_2))
	BIF_ERROR(BIF_P, BADARG);
    arity = unsigned_val(BIF_ARG_3); /* no error check */
    address = hipe_get_na_nofail(BIF_ARG_1, BIF_ARG_2, arity, 1);
    BIF_RET((Eterm)address);	/* semi-Ok */
}

BIF_RETTYPE hipe_bifs_find_na_or_make_stub_2(BIF_ALIST_2)
{
    struct hipe_mfa mfa;
    void *address;
    int is_remote;

    if (!term_to_mfa(BIF_ARG_1, &mfa))
	BIF_ERROR(BIF_P, BADARG);
    if (BIF_ARG_2 == am_true)
	is_remote = 1;
    else if (BIF_ARG_2 == am_false)
	is_remote = 0;
    else
	BIF_ERROR(BIF_P, BADARG);
    address = hipe_get_na_nofail(mfa.mod, mfa.fun, mfa.ari, is_remote);
    BIF_RET(address_to_term(address, BIF_P));
}

/* primop, but called like a BIF for error handling purposes */
BIF_RETTYPE hipe_nonclosure_address(BIF_ALIST_2)
{
    Eterm hdr, m, f;
    void *address;

    if (!is_boxed(BIF_ARG_1))
	goto badfun;
    hdr = *boxed_val(BIF_ARG_1);
    if (is_export_header(hdr)) {
	Export *ep = (Export*)(export_val(BIF_ARG_1)[1]);
	unsigned int actual_arity = ep->code[2];
	if (actual_arity != BIF_ARG_2)
	    goto badfun;
	m = ep->code[0];
	f = ep->code[1];
    } else if (hdr == make_arityval(2)) {
	Eterm *tp = tuple_val(BIF_ARG_1);
	m = tp[1];
	f = tp[2];
	if (is_not_atom(m) || is_not_atom(f))
	    goto badfun;
	if (!erts_active_export_entry(m, f, BIF_ARG_2))
	    goto badfun;
    } else
	goto badfun;
    address = hipe_get_na_nofail(m, f, BIF_ARG_2, 1);
    BIF_RET((Eterm)address);

 badfun:
    BIF_P->current = NULL;
    BIF_P->fvalue = BIF_ARG_1;
    BIF_ERROR(BIF_P, EXC_BADFUN);
}

int hipe_find_mfa_from_ra(const void *ra, Eterm *m, Eterm *f, unsigned int *a)
{
    struct hipe_mfa_info *mfa;
    long mfa_offset, ra_offset;
    struct hipe_mfa_info **bucket;
    unsigned int i, nrbuckets;

    /* Note about locking: the table is only updated from the
       loader, which runs with the rest of the system suspended. */
    /* XXX: alas not true; see comment at hipe_mfa_info_table.lock */
    hipe_mfa_info_table_lock();
    bucket = hipe_mfa_info_table.bucket;
    nrbuckets = 1 << hipe_mfa_info_table.log2size;
    mfa = NULL;
    mfa_offset = LONG_MAX;
    for (i = 0; i < nrbuckets; ++i) {
	struct hipe_mfa_info *b = bucket[i];
	while (b != NULL) {
	    ra_offset = (char*)ra - (char*)b->local_address;
	    if (ra_offset > 0 && ra_offset < mfa_offset) {
		mfa_offset = ra_offset;
		mfa = b;
	    }
	    ra_offset = (char*)ra - (char*)b->old_address;
	    if (ra_offset > 0 && ra_offset < mfa_offset) {
		mfa_offset = ra_offset;
		mfa = b;
	    }
	    b = b->bucket.next;
	}
    }
    if (mfa) {
	*m = mfa->m;
	*f = mfa->f;
	*a = mfa->a;
    }
    hipe_mfa_info_table_unlock();
    return mfa ? 1 : 0;
}


/* add_ref(CalleeMFA, {CallerMFA,Address,'call'|'load_mfa',Trampoline,'remote'|'local'})
 */
BIF_RETTYPE hipe_bifs_add_ref_2(BIF_ALIST_2)
{
    struct hipe_mfa callee;
    Eterm *tuple;
    struct hipe_mfa caller;
    void *address;
    void *trampoline;
    unsigned int flags;
    struct hipe_mfa_info *callee_mfa;
    //SVERK struct hipe_mfa_info *caller_mfa;
    struct hipe_ref *ref;
    Module* modp;

    if (!term_to_mfa(BIF_ARG_1, &callee))
	goto badarg;
    if (is_not_tuple(BIF_ARG_2))
	goto badarg;
    tuple = tuple_val(BIF_ARG_2);
    if (tuple[0] != make_arityval(5))
	goto badarg;
    if (!term_to_mfa(tuple[1], &caller))
	goto badarg;
    address = term_to_address(tuple[2]);
    if (!address)
	goto badarg;
    switch (tuple[3]) {
      case am_call:
	flags = 0;
	break;
      case am_load_mfa:
	flags = REF_FLAG_IS_LOAD_MFA;
	break;
      default:
	goto badarg;
    }
    if (is_nil(tuple[4]))
	trampoline = NULL;
    else {
	trampoline = term_to_address(tuple[4]);
	if (!trampoline)
	    goto badarg;
    }
    switch (tuple[5]) {
      case am_local:
	fprintf(stderr, "SVERK doesn't like local refs\r\n");
	abort();
	break;
      case am_remote:
	//SVERK flags |= REF_FLAG_IS_REMOTE;
	break;
      default:
	goto badarg;
    }
    hipe_mfa_info_table_lock();
    callee_mfa = hipe_mfa_info_table_put_locked(callee.mod, callee.fun, callee.ari);

    ref = erts_alloc(ERTS_ALC_T_HIPE, sizeof(struct hipe_ref));
    ref->address = address;
    ref->trampoline = trampoline;
    ref->flags = flags;

    ASSERT(!callee_mfa->first_caller ||
	   callee_mfa->first_caller->prevp == &callee_mfa->first_caller);
    ref->next = callee_mfa->first_caller;
    ref->prevp = &callee_mfa->first_caller;
    callee_mfa->first_caller = ref;
    if (ref->next) {
	ref->next->prevp = &ref->next;
    }
    
    modp = erts_put_active_module(caller.mod);
    ASSERT(modp);
    ref->next_callee = modp->curr.first_hipe_ref;
    modp->curr.first_hipe_ref = ref;

#if defined(DEBUG) || defined(SVERK_DEBUG)
    ref->callee = callee_mfa;
    ref->caller_m = caller.mod;
    ref->caller_f = caller.fun;
    ref->caller_a = caller.ari;
#endif
    hipe_mfa_info_table_unlock();

    DBG_TRACE_MFA(caller.mod, caller.fun, caller.ari, "add_ref at %p TO %T:%T/%u (from %p)",
		  ref, callee.mod, callee.fun, callee.ari, ref->address);
    DBG_TRACE_MFA(callee.mod, callee.fun, callee.ari, "add_ref at %p FROM %T:%T/%u (from %p)",
		  ref, caller.mod, caller.fun, caller.ari, ref->address);
    BIF_RET(NIL);

 badarg:
    BIF_ERROR(BIF_P, BADARG);
}


/* Given a CalleeMFA, mark each ref to it as pending-redirect.
 * This ensures that remove_refs_from() won't remove them: any
 * removal is instead done at the end of redirect_referred_from().
 */
BIF_RETTYPE hipe_bifs_mark_referred_from_1(BIF_ALIST_1) /* get_refs_from */
{
    ASSERT(!"don't call mark_referred_from()");
    BIF_RET(NIL);
}

BIF_RETTYPE hipe_bifs_remove_refs_from_1(BIF_ALIST_1)
{
    ASSERT(!"hipe_bifs_remove_refs_from_1() called");
    BIF_ERROR(BIF_P, BADARG);
}

void hipe_purge_module(Module* modp)
{
    struct hipe_mfa_info* p;
    struct hipe_ref* ref;

    ASSERT(modp);

    /* 
     * Remove all hipe_ref's (external calls) from the old module instance   
     */
    ref = modp->old.first_hipe_ref;
    while (ref) {
	struct hipe_ref* free_ref = ref;

	DBG_TRACE_MFA(ref->caller_m, ref->caller_f, ref->caller_a, "PURGE ref at %p to %T:%T/%u", ref,
		      ref->callee->m, ref->callee->f, ref->callee->a);		 
	DBG_TRACE_MFA(ref->callee->m, ref->callee->f, ref->callee->a, "PURGE ref at %p from %T:%T/%u", ref,
		      ref->caller_m, ref->caller_f, ref->caller_a);
	ASSERT(ref->caller_m == make_atom(modp->module));

	if (ref->next) {
	    ASSERT(ref->next->prevp == &ref->next);
	    ref->next->prevp = ref->prevp;
	}
	ASSERT(*ref->prevp == ref);
	*ref->prevp = ref->next;
	ref = ref->next_callee;
	erts_free(ERTS_ALC_T_HIPE, free_ref);
    }
    modp->old.first_hipe_ref = NULL;

    for (p = modp->first_hipe_mfa; p; p = p->next_in_mod) {
	if (p->old_address) {
	    DBG_TRACE_MFA(p->m, p->f, p->a, "CLEAR old_address in hipe_mfa_info at %p", p);
	    p->old_address = NULL;	    
	}
	else {
	    DBG_TRACE_MFA(p->m, p->f, p->a, "NO old_address in hipe_mfa_info at %p", p);
	}
    }

    /*
     * This could be a good place to deallocate old native code
     */
}

void hipe_export_beam(Eterm m, Eterm f, Uint a, BeamInstr* BeamAddress)
{
    struct hipe_mfa_info *p;
    hipe_mfa_info_table_lock();
    p = hipe_mfa_info_table_get_locked(m, f, a);
    if (p) {
	void* StubAddress = hipe_make_native_stub(BeamAddress, a);

	DBG_TRACE_MFA(m,f,a, "hipe_export_beam at beam=%p, hipe stub=%p",
		      BeamAddress, StubAddress);
	ASSERT(!p->local_address);
	p->remote_address = StubAddress;
    }
    hipe_mfa_info_table_unlock();
}


/* redirect_referred_from(CalleeMFA)
 * Redirect all pending-redirect refs in CalleeMFA's referred_from.
 * Then remove any pending-redirect && pending-remove refs from CalleeMFA's referred_from.
 */
void hipe_redirect_to_module(Module* modp)
{
    struct hipe_mfa_info *p;
    struct hipe_ref* ref;

#ifdef ERTS_SMP
    ASSERT(erts_smp_thr_progress_is_blocking());
#endif

    for (p = modp->first_hipe_mfa; p; p = p->next_in_mod) {
	DBG_TRACE_MFA(p->m,p->f,p->a,"START REDIRECT towards hipe_mfa_info at %p", p);
	for (ref = p->first_caller; ref; ref = ref->next) {
	    void *new_address;
	    int res;
	    
	    new_address = hipe_get_na_nofail_locked(p->m, p->f, p->a, 1);
	    	    
	    DBG_TRACE_MFA(p->m,p->f,p->a, "  REDIRECT ref at %p FROM %T:%T/%u (%p -> %p)",
			  ref, ref->caller_m, ref->caller_f, ref->caller_a,
			  ref->address, new_address);
	    
	    DBG_TRACE_MFA(ref->caller_m, ref->caller_f, ref->caller_a,
			  "  REDIRECT ref at %p TO %T:%T/%u (%p -> %p)",
			  ref, p->m,p->f,p->a, ref->address, new_address);

	    if (ref->flags & REF_FLAG_IS_LOAD_MFA)
		res = hipe_patch_insn(ref->address, (Uint)new_address, am_load_mfa);
	    else
		res = hipe_patch_call(ref->address, new_address, ref->trampoline);
	    if (res)
		fprintf(stderr, "%s: patch failed", __FUNCTION__);
	}
	DBG_TRACE_MFA(p->m,p->f,p->a,"DONE REDIRECT towards hipe_mfa_info at %p", p);
    }
}

BIF_RETTYPE hipe_bifs_check_crc_1(BIF_ALIST_1)
{
    Uint crc;

    if (!term_to_Uint(BIF_ARG_1, &crc))
	BIF_ERROR(BIF_P, BADARG);
    if (crc == HIPE_SYSTEM_CRC)
	BIF_RET(am_true);
    BIF_RET(am_false);
}

BIF_RETTYPE hipe_bifs_system_crc_1(BIF_ALIST_1)
{
    Uint crc;

    if (!term_to_Uint(BIF_ARG_1, &crc))
	BIF_ERROR(BIF_P, BADARG);
    crc ^= (HIPE_SYSTEM_CRC ^ HIPE_LITERALS_CRC);
    BIF_RET(Uint_to_term(crc, BIF_P));
}

BIF_RETTYPE hipe_bifs_get_rts_param_1(BIF_ALIST_1)
{
    unsigned int is_defined;
    unsigned long value;

    if (is_not_small(BIF_ARG_1))
	BIF_ERROR(BIF_P, BADARG);
    is_defined = 1;
    value = 0;
    switch (unsigned_val(BIF_ARG_1)) {
	RTS_PARAMS_CASES
      default:
	BIF_ERROR(BIF_P, BADARG);
    }
    if (!is_defined)
	BIF_RET(NIL);
    BIF_RET(Uint_to_term(value, BIF_P));
}

void hipe_patch_address(Uint *address, Eterm patchtype, Uint value)
{
    switch (patchtype) {
      case am_load_fe:
	hipe_patch_load_fe(address, value);
	return;
      default:
	fprintf(stderr, "%s: unknown patchtype %#lx\r\n",
		__FUNCTION__, patchtype);
	return;
    }
}

struct modinfo {
    HashBucket bucket;		/* bucket.hvalue == atom_val(the module name) */
    unsigned int code_size;
};

static Hash modinfo_table;

static HashValue modinfo_hash(void *tmpl)
{
    Eterm mod = (Eterm)tmpl;
    return atom_val(mod);
}

static int modinfo_cmp(void *tmpl, void *bucket)
{
    /* bucket->hvalue == modinfo_hash(tmpl), so just return 0 (match) */
    return 0;
}

static void *modinfo_alloc(void *tmpl)
{
    struct modinfo *p;

    p = (struct modinfo*)erts_alloc(ERTS_ALC_T_HIPE, sizeof(*p));
    p->code_size = 0;
    return &p->bucket;
}

static void init_modinfo_table(void)
{
    HashFunctions f;
    static int init_done = 0;

    if (init_done)
	return;
    init_done = 1;
    f.hash = (H_FUN) modinfo_hash;
    f.cmp = (HCMP_FUN) modinfo_cmp;
    f.alloc = (HALLOC_FUN) modinfo_alloc;
    f.free = (HFREE_FUN) NULL;
    hash_init(ERTS_ALC_T_HIPE, &modinfo_table, "modinfo_table", 11, f);
}

BIF_RETTYPE hipe_bifs_update_code_size_3(BIF_ALIST_3)
{
    struct modinfo *p;
    Sint code_size;

    init_modinfo_table();

    if (is_not_atom(BIF_ARG_1) ||
	is_not_small(BIF_ARG_3) ||
	(code_size = signed_val(BIF_ARG_3)) < 0)
	BIF_ERROR(BIF_P, BADARG);

    p = (struct modinfo*)hash_put(&modinfo_table, (void*)BIF_ARG_1);

    if (is_nil(BIF_ARG_2))	/* some MFAs, not whole module */
	p->code_size += code_size;
    else			/* whole module */
	p->code_size = code_size;
    BIF_RET(NIL);
}

BIF_RETTYPE hipe_bifs_code_size_1(BIF_ALIST_1)
{
    struct modinfo *p;
    unsigned int code_size;

    init_modinfo_table();

    if (is_not_atom(BIF_ARG_1))
	BIF_ERROR(BIF_P, BADARG);

    p = (struct modinfo*)hash_get(&modinfo_table, (void*)BIF_ARG_1);

    code_size = p ? p->code_size : 0;
    BIF_RET(make_small(code_size));
}

BIF_RETTYPE hipe_bifs_patch_insn_3(BIF_ALIST_3)
{
    Uint *address, value;

    address = term_to_address(BIF_ARG_1);
    if (!address)
	BIF_ERROR(BIF_P, BADARG);
    if (!term_to_Uint(BIF_ARG_2, &value))
	BIF_ERROR(BIF_P, BADARG);
    if (hipe_patch_insn(address, value, BIF_ARG_3))
	BIF_ERROR(BIF_P, BADARG);
    BIF_RET(NIL);
}

BIF_RETTYPE hipe_bifs_patch_call_3(BIF_ALIST_3)
{
    Uint *callAddress, *destAddress, *trampAddress;

    callAddress = term_to_address(BIF_ARG_1);
    if (!callAddress)
	BIF_ERROR(BIF_P, BADARG);
    destAddress = term_to_address(BIF_ARG_2);
    if (!destAddress)
	BIF_ERROR(BIF_P, BADARG);
    if (is_nil(BIF_ARG_3))
	trampAddress = NULL;
    else {
	trampAddress = term_to_address(BIF_ARG_3);
	if (!trampAddress)
	    BIF_ERROR(BIF_P, BADARG);
    }
    if (hipe_patch_call(callAddress, destAddress, trampAddress))
	BIF_ERROR(BIF_P, BADARG);
    BIF_RET(NIL);
}
