/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 1998-2011. All Rights Reserved.
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
#include <string.h>
#include <limits.h>
#include "eidef.h"
#include "eiext.h"
#include "putget.h"

int ei_encode_atom(char *buf, int *index, const char *p)
{
    size_t len = strlen(p);

    if (len >= MAXATOMLEN)
	len = MAXATOMLEN - 1;
    return ei_encode_atom_len2(buf, index, p, len, ERLANG_LATIN1, ERLANG_LATIN1);
}

int ei_encode_atom_len(char *buf, int *index, const char *p, int len)
{
    /* This function is documented to truncate at MAXATOMLEN (256) */ 
    if (len >= MAXATOMLEN)
	len = MAXATOMLEN - 1;
    return ei_encode_atom_len2(buf, index, p, len, ERLANG_LATIN1, ERLANG_LATIN1);
}

int ei_encode_atom2(char *buf, int *index, const char *p,
		    enum erlang_char_encoding from_enc,
		    enum erlang_char_encoding to_enc)
{
    return ei_encode_atom_len2(buf, index, p, strlen(p), from_enc, to_enc);
}

int ei_encode_atom_len2(char *buf, int *index, const char *p, int len,
			enum erlang_char_encoding from_enc,
			enum erlang_char_encoding to_enc)
{
  char *s = buf + *index;
  char *s0 = s;
  int offs;

  if (from_enc == ERLANG_LATIN1 && len >= MAXATOMLEN) {
      return -1;
  }

  switch(to_enc) {
  case ERLANG_LATIN1:
      if (buf) {
	  put8(s,ERL_ATOM_EXT);
	  switch (from_enc) {
	  case ERLANG_UTF8:
	      len = utf8_to_latin1(s+2, p, len, MAXATOMLEN-1, NULL);
	      if (len < 0) return -1;
	      break;
	  case ERLANG_LATIN1:
	      memcpy(s+2, p, len);
	      break;
	  default:
	      return -1;
	  }
	  put16be(s,len);
      }
      else {
	  s += 3;
	  if (from_enc == ERLANG_UTF8) {
	      len = utf8_to_latin1(NULL, p, len, MAXATOMLEN-1, NULL);
	      if (len < 0) return -1;
	  }
      }
      break;
      
  case ERLANG_UTF8:
      offs =  1 + 1 + (len >= 256/2);
      if (buf) {
	  switch (from_enc) {
	  case ERLANG_LATIN1:
	      len = latin1_to_utf8(s+offs, p, len, MAXATOMLEN_UTF8-1, NULL);
	      break;
	  case ERLANG_UTF8:
	      memcpy(s+offs, p, len);
	      break;
	  default:
	      return -1;
	  }
	  if (offs == 2) {
	      put8(s, ERL_SMALL_ATOM_UTF8_EXT);
	      put8(s, len);
	  }
	  else {
	      put8(s, ERL_ATOM_UTF8_EXT);
	      put16be(s, len);
	  }
      }
      else {
	  s += offs;
	  if (from_enc == ERLANG_LATIN1) {
	      len = latin1_to_utf8(NULL, p, len, MAXATOMLEN_UTF8-1, NULL);
	  }
      }
      break;

  default:
      return -1;
  }
  s += len;

  *index += s-s0; 

  return 0; 
}

int
ei_internal_put_atom(char** bufp, const char* p, int slen,
		     enum erlang_char_encoding to_enc)
{
    int ix = 0;
   if (ei_encode_atom_len2(*bufp, &ix, p, slen, ERLANG_UTF8, to_enc) < 0)
	return -1;
    *bufp += ix;
    return 0;
}
