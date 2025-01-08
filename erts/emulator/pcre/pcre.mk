#
# %CopyrightBegin%
#
# Copyright Ericsson AB 2012-2016. All Rights Reserved.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
# %CopyrightEnd%
#

PCRE_O = \
pcre2_auto_possess.o \
pcre2_chartables.o \
pcre2_chkdint.o \
pcre2_compile.o \
pcre2_config.o \
pcre2_context.o \
pcre2_error.o \
pcre2_extuni.o \
pcre2_find_bracket.o \
pcre2_match.o \
pcre2_match_data.o \
pcre2_newline.o \
pcre2_ord2utf.o \
pcre2_pattern_info.o \
pcre2_script_run.o \
pcre2_string_utils.o \
pcre2_study.o \
pcre2_substring.o \
pcre2_tables.o \
pcre2_ucd.o \
pcre2_valid_utf.o \
pcre2_xclass.o

PCRE_OBJS = $(PCRE_O:%=$(PCRE_OBJDIR)/%)

PCRE_GENINC = $(ERL_TOP)/erts/emulator/pcre/pcre2_match_loop_break_cases.inc

PCRE_OBJDIR = $(ERL_TOP)/erts/emulator/pcre/obj/$(TARGET)/$(TYPE)

PCRE_DIR =  $(ERL_TOP)/erts/emulator/pcre

PCRE_CFLAGS = $(filter-out -DDEBUG,$(CFLAGS)) -DERLANG_INTEGRATION

ifeq ($(TARGET), win32)
$(EPCRE_LIB): $(PCRE_OBJS)
	$(V_AR) -out:$@ $(PCRE_OBJS)
else
$(EPCRE_LIB): $(PCRE_OBJS)
	$(V_AR) $(ARFLAGS) $@ $(PCRE_OBJS)
	-@ ($(RANLIB) $@ || true) 2>/dev/null
endif

$(PCRE_OBJDIR)/%.o: $(PCRE_DIR)/%.c
	$(V_CC) -c $(PCRE_CFLAGS) -o $@ $<

$(PCRE_GENINC): $(PCRE_DIR)/pcre2_match.c
	$(gen_verbose)for x in `grep -n COST_CHK $(PCRE_DIR)/pcre2_match.c | grep -v 'COST_CHK(N)' | awk -F: '{print $$1}'`; \
	do \
		N=`expr $$x + 100`; \
		echo "case $$N: goto L_LOOP_COUNT_$${x};"; \
	done > $(PCRE_GENINC)

# Dependencies.

$(PCRE_OBJDIR)/pcre2_match.o: $(PCRE_DIR)/pcre2_match.c \
	$(PCRE_DIR)/pcre2_internal.h $(PCRE_DIR)/pcre2_intmodedep.h \
	$(PCRE_DIR)/pcre2.h $(PCRE_DIR)/pcre2_ucp.h $(PCRE_GENINC)
