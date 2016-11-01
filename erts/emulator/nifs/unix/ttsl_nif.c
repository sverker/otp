/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1996-2016. All Rights Reserved.
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
/*
 * Tty nif lib that reads one character at the time and provides a
 * smart line for output.
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#define STATIC_ERLANG_NIF 1
#include "erl_nif.h"

#ifdef HAVE_TERMCAP  /* else make an empty driver that can not be opened */

#ifndef WANT_NONBLOCKING
#define WANT_NONBLOCKING
#endif

#include "sys.h"
#include <ctype.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <signal.h>
#include <fcntl.h>
#include <limits.h>
#include <locale.h>
#include <unistd.h>
#include <termios.h>
#ifdef HAVE_WCWIDTH
#include <wchar.h>
#endif
#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif
#ifdef HAVE_SYS_IOCTL_H
#include <sys/ioctl.h>
#endif
#if !defined(HAVE_SETLOCALE) || !defined(HAVE_NL_LANGINFO) || !defined(HAVE_LANGINFO_H)
#define PRIMITIVE_UTF8_CHECK 1
#else
#include <langinfo.h>
#endif

#if defined IOV_MAX
#define MAXIOV IOV_MAX
#elif defined UIO_MAXIOV
#define MAXIOV UIO_MAXIOV
#else
#define MAXIOV 16
#endif

#define TRUE 1
#define FALSE 0

/* Termcap functions. */
int tgetent(char* bp, char *name);
int tgetnum(char* cap);
int tgetflag(char* cap);
char *tgetstr(char* cap, char** buf);
char *tgoto(char* cm, int col, int line);
int tputs(char* cp, int affcnt, int (*outc)(int c));

/* Terminal capabilites in which we are interested. */
static char *capbuf;
static char *up, *down, *left, *right;
static int cols, xn;
static volatile int cols_needs_update = FALSE;

/* The various opcodes. */
#define OP_PUTC 0
#define OP_MOVE 1
#define OP_INSC 2
#define OP_DELC 3
#define OP_BEEP 4
#define OP_PUTC_SYNC 5
/* Control op */
#define CTRL_OP_GET_WINSIZE 100
#define CTRL_OP_GET_UNICODE_STATE 101
#define CTRL_OP_SET_UNICODE_STATE 102

/* We use 1024 as the buf size as that was the default buf size of FILE streams
   on all platforms that I checked. */
#define TTY_BUFFSIZE 1024

static int lbuf_size = BUFSIZ;
static Uint32 *lbuf;		/* The current line buffer */
static int llen;		/* The current line length */
static int lpos;                /* The current "cursor position" in the line buffer */

/* 
 * Tags used in line buffer to show that these bytes represent special characters,
 * Max unicode is 0x0010ffff, so we have lots of place for meta tags... 
 */
#define CONTROL_TAG 0x10000000U /* Control character, value in first position */
#define ESCAPED_TAG 0x01000000U /* Escaped character, value in first position */
#ifdef HAVE_WCWIDTH
#define WIDE_TAG    0x02000000U /* Wide character, value in first position    */
#endif
#define TAG_MASK    0xFF000000U

#define MAXSIZE (1 << 16)

#define COL(_l) ((_l) % cols)
#define LINE(_l) ((_l) / cols)

#define NL '\n'

/* Main interface functions. */
static void ttsl_rt_stop(ErlNifEnv* env, void* resource, int, int);

static int ttsl_is_started = 0;
static int ttysl_fd;
//static int ttysl_terminate = 0;
static int putcpos;
static ErlNifResourceType* ttsl_resource_type;

struct ttsl_resource_t {
    int Dum_di_dum_dum;
};

struct ttsl_resource_t* ttsl_resource;


/* Functions that work on the line buffer. */
static int start_lbuf(void);
static int stop_lbuf(void);
static int put_chars(byte*,int);
static int move_rel(int);
static int ins_chars(byte *,int);
static int del_chars(int);
static int step_over_chars(int);
static int insert_buf(byte*,int);
static int write_buf(Uint32 *,int);
static int outc(int c);
static int move_cursor(int,int);

/* Termcap functions. */
static int start_termcap(void);
//static int stop_termcap(void);
static int move_left(int);
static int move_right(int);
static int move_up(int);
static int move_down(int);
static void update_cols(void);

/* Terminal setting functions. */
static int tty_init(int,int,int,int);
static int tty_set(int);
static int tty_reset(int);

#ifdef ERTS_NOT_USED
static RETSIGTYPE suspend(int);
#endif
static RETSIGTYPE cont(int);
static RETSIGTYPE winch(int);

#define LOG_DEBUG

#ifdef LOG_DEBUG
FILE *debuglog = NULL;

#define DEBUGLOG(X) 				\
do {						\
    if (debuglog != NULL) {			\
	my_debug_printf X;    			\
    }						\
} while (0)

static void my_debug_printf(char *fmt, ...)
{
    char buffer[1024];
    va_list args;

    va_start(args, fmt);
    erts_vsnprintf(buffer,1024,fmt,args);
    va_end(args);
    erts_fprintf(debuglog,"%s\n",buffer);
    /*erts_printf("Debuglog = %s\n",buffer);*/
}

#else

#define DEBUGLOG(X)

#endif

static int utf8_mode = 0;
static byte utf8buf[4]; /* for incomplete input */
static int utf8buf_size; /* size of incomplete input */

#  define IF_IMPL(x) x
#else
#  define IF_IMPL(x) NULL
#endif /* HAVE_TERMCAP */

static ERL_NIF_TERM atom_ok;
static ERL_NIF_TERM atom_true;
static ERL_NIF_TERM atom_false;
static ERL_NIF_TERM atom_error;
static ERL_NIF_TERM atom_undefined;

static int ttsl_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
#ifdef HAVE_TERMCAP
    ErlNifResourceTypeInit rt_init = {NULL, ttsl_rt_stop};
    ttysl_fd = -1;
    lbuf = NULL;		/* For line buffer handling */
    capbuf = NULL;		/* For termcap handling */

    ttsl_resource_type = enif_open_resource_type_x(env, "tty_sl", &rt_init,
                                                   ERL_NIF_RT_CREATE, NULL);

    atom_ok = enif_make_atom(env, "ok");
    atom_true = enif_make_atom(env, "true");
    atom_false = enif_make_atom(env, "false");
    atom_error = enif_make_atom(env, "error");
    atom_undefined = enif_make_atom(env, "undefined");

#endif
#ifdef LOG_DEBUG
    {
	char *dl;
	if ((dl = getenv("TTYSL_DEBUG_LOG")) != NULL && *dl) {
	    debuglog = fopen(dl,"w+");
	    if (debuglog != NULL)
		setbuf(debuglog,NULL);
	}
	DEBUGLOG(("ttysl_init: Debuglog = %s(0x%ld)\n",dl,(long) debuglog));
    }
#endif
    return 0;
}

static ERL_NIF_TERM start_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
#ifndef HAVE_TERMCAP
    return __LINE__;
#else
    ErlNifBinary buf_bin;
    char *buf, *s, *t, *l;
    int canon, echo, sig;	/* Terminal characteristics */
    int flag;
    extern int using_oldshell; /* set this to let the rest of erts know */

    if (!enif_inspect_binary(env, argv[0], &buf_bin)) {
        enif_make_badarg(env);
    }
    /* A null terminated and writable copy */
    buf = enif_alloc(buf_bin.size+1);
    memcpy(buf, buf_bin.data, buf_bin.size);
    buf[buf_bin.size] = 0;

    DEBUGLOG(("ttysl_start: driver input \"%s\", started = %d (0 expected)",
              buf, ttsl_is_started));
    utf8buf_size = 0;
    if (ttsl_is_started) {
        DEBUGLOG(("ttsl_start: failure, already started?\n"));
        goto error;
    }

    DEBUGLOG(("ttysl_start: isatty(0) = %d (1 expected), isatty(1) = %d (1 expected)",
              isatty(0), isatty(1)));
    if (!isatty(0) || !isatty(1)) {
        DEBUGLOG(("ttysl_start: failure in isatty, isatty(0) = %d, isatty(1) = %d",
                  isatty(0), isatty(1)));
	goto error;
    }

    /* Set the terminal modes to default leave as is. */
    canon = echo = sig = 0;

    /* Parse the input parameters. */
    for (s = strchr(buf, ' '); s; s = t) {
        s++;
        /* Find end of this argument (start of next) and insert NUL. */
        if ((t = strchr(s, ' '))) {
            *t = '\0';
        }
        if ((flag = ((*s == '+') ? 1 : ((*s == '-') ? -1 : 0)))) {
            if (s[1] == 'c') canon = flag;
            if (s[1] == 'e') echo = flag;
            if (s[1] == 's') sig = flag;
        }
        else if ((ttysl_fd = open(s, O_RDWR, 0)) < 0) {
            DEBUGLOG(("ttysl_start: failed to open ttysl_fd, open(%s, O_RDWR, 0)) = %d\n", s, ttysl_fd));
            goto error;
        }
    }

    if (ttysl_fd < 0)
      ttysl_fd = 0;

    if (tty_init(ttysl_fd, canon, echo, sig) < 0 ||
        tty_set(ttysl_fd) < 0) {
        DEBUGLOG(("ttysl_start: failed init tty or set tty\n"));
	ttsl_is_started = 0;
	tty_reset(ttysl_fd);
	goto error;
    }

    /* Set up smart line and termcap stuff. */
    if (!start_lbuf() || !start_termcap()) {
        DEBUGLOG(("ttysl_start: failed to start_lbuf or start_termcap\n"));
	stop_lbuf();		/* Must free this */
	tty_reset(ttysl_fd);
        goto error;
    }

    SET_NONBLOCKING(ttysl_fd);

#ifdef PRIMITIVE_UTF8_CHECK
    setlocale(LC_CTYPE, "");  /* Set international environment, 
				 ignore result */
    if (((l = getenv("LC_ALL"))   && *l) ||
	((l = getenv("LC_CTYPE")) && *l) ||
	((l = getenv("LANG"))     && *l)) {
	if (strstr(l, "UTF-8"))
	    utf8_mode = 1;
    }
    
#else
    l = setlocale(LC_CTYPE, "");  /* Set international environment */
    if (l != NULL) {
	utf8_mode = (strcmp(nl_langinfo(CODESET), "UTF-8") == 0);
	DEBUGLOG(("ttysl_start: setlocale: %s",l));
    }
#endif
    DEBUGLOG(("ttysl_start: utf8_mode is %s",(utf8_mode) ? "on" : "off"));
    sys_signal(SIGCONT, cont);
    sys_signal(SIGWINCH, winch);

    ttsl_resource = enif_alloc_resource(ttsl_resource_type, sizeof(struct ttsl_resource_t));
    enif_select(env, (ErlNifEvent)(UWord)ttysl_fd, ERL_NIF_SELECT_READ,
                ttsl_resource, atom_undefined);
    ttsl_is_started = 1;

    /* we need to know this when we enter the break handler */
    using_oldshell = 0;

    DEBUGLOG(("ttysl_start: successful start\n"));
    return atom_ok;

error:
    enif_free(buf);
    return atom_error;
#endif /* HAVE_TERMCAP */
}

#ifdef HAVE_TERMCAP

#define DEF_HEIGHT 24
#define DEF_WIDTH 80
static void ttysl_get_window_size(Uint32 *width, Uint32 *height)
{
#ifdef TIOCGWINSZ 
    struct winsize ws;
    if (ioctl(ttysl_fd,TIOCGWINSZ,&ws) == 0) {
	*width = (Uint32) ws.ws_col;
	*height = (Uint32) ws.ws_row;
	if (*width <= 0) 
	    *width = DEF_WIDTH;
	if (*height <= 0) 
	    *height = DEF_HEIGHT;
	return;
    }
#endif
    *width = DEF_WIDTH;
    *height = DEF_HEIGHT;
}
    
static ERL_NIF_TERM get_window_size_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    Uint32 w,h;
    ttysl_get_window_size(&w,&h);
    return enif_make_tuple2(env, enif_make_uint(env, w), enif_make_uint(env, h));
}

static ERL_NIF_TERM get_unicode_state_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return utf8_mode ? atom_true : atom_false;
}

static ERL_NIF_TERM set_unicode_state_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM res = get_unicode_state_nif(env, 0, argv);

    if (argv[0] == atom_true)
        utf8_mode = 1;
    else if (argv[0] == atom_false)
        utf8_mode = 0;
    else
        return enif_make_badarg(env);

    return res;
}


/*
static void ttysl_stop(void)
{
    DEBUGLOG(("ttysl_stop: is_started = %d\n",ttsl_is_started));
    if (ttsl_is_started) {
	stop_lbuf();
	stop_termcap();
	tty_reset(ttysl_fd);
	enif_select(env, (ErlNifEvent)(UWord)ttysl_fd,
                    ERL_NIF_SELECT_STOP, ttsl_resource, atom_undefined);
	sys_signal(SIGCONT, SIG_DFL);
	sys_signal(SIGWINCH, SIG_DFL);
    }
    ttsl_is_started = 0;
    ttysl_fd = -1;
    ttysl_terminate = 0;
}
*/

static int put_utf8(int ch, byte *target, int sz, int *pos)
{
    Uint x = (Uint) ch;
    if (x < 0x80) {
    if (*pos >= sz) {
	return -1;
    }
	target[(*pos)++] = (byte) x;
    }
    else if (x < 0x800) {
	if (((*pos) + 1) >= sz) {
	    return -1;
	}
	target[(*pos)++] = (((byte) (x >> 6)) | 
			    ((byte) 0xC0));
	target[(*pos)++] = (((byte) (x & 0x3F)) | 
			    ((byte) 0x80));
    } else if (x < 0x10000) {
	if ((x >= 0xD800 && x <= 0xDFFF) ||
	    (x == 0xFFFE) ||
	    (x == 0xFFFF)) { /* Invalid unicode range */
	    return -1;
	}
	if (((*pos) + 2) >= sz) {
	    return -1;
	}

	target[(*pos)++] = (((byte) (x >> 12)) | 
			    ((byte) 0xE0));
	target[(*pos)++] = ((((byte) (x >> 6)) & 0x3F)  | 
			    ((byte) 0x80));
	target[(*pos)++] = (((byte) (x & 0x3F)) | 
			    ((byte) 0x80));
    } else if (x < 0x110000) { /* Standard imposed max */
	if (((*pos) + 3) >= sz) {
	    return -1;
	}
	target[(*pos)++] = (((byte) (x >> 18)) | 
			    ((byte) 0xF0));
	target[(*pos)++] = ((((byte) (x >> 12)) & 0x3F)  | 
			    ((byte) 0x80));
	target[(*pos)++] = ((((byte) (x >> 6)) & 0x3F)  | 
			    ((byte) 0x80));
	target[(*pos)++] = (((byte) (x & 0x3F)) | 
			    ((byte) 0x80));
    } else {
	return -1;
    }
    return 0;
}
    

static int pick_utf8(byte *s, int sz, int *pos) 
{
    int size = sz - (*pos);
    byte *source;
    Uint unipoint;

    if (size > 0) {
	source = s + (*pos);
	if (((*source) & ((byte) 0x80)) == 0) {
	    unipoint = (int) *source;
	    ++(*pos);
	    return (int) unipoint;
	} else if (((*source) & ((byte) 0xE0)) == 0xC0) {
	    if (size < 2) {
		return -2;
	    }
	    if (((source[1] & ((byte) 0xC0)) != 0x80) ||
		((*source) < 0xC2) /* overlong */) {
		return -1;
	    }
	    (*pos) += 2;
	    unipoint = 
		(((Uint) ((*source) & ((byte) 0x1F))) << 6) |
		((Uint) (source[1] & ((byte) 0x3F))); 	
	    return (int) unipoint;
	} else if (((*source) & ((byte) 0xF0)) == 0xE0) {
	    if (size < 3) {
		return -2;
	    }
	    if (((source[1] & ((byte) 0xC0)) != 0x80) ||
		((source[2] & ((byte) 0xC0)) != 0x80) ||
		(((*source) == 0xE0) && (source[1] < 0xA0)) /* overlong */ ) {
		return -1;
	    }
	    if ((((*source) & ((byte) 0xF)) == 0xD) && 
		((source[1] & 0x20) != 0)) {
		return -1;
	    }
	    if (((*source) == 0xEF) && (source[1] == 0xBF) &&
		((source[2] == 0xBE) || (source[2] == 0xBF))) {
		return -1;
	    }
	    (*pos) += 3;
	    unipoint = 
		(((Uint) ((*source) & ((byte) 0xF))) << 12) |
		(((Uint) (source[1] & ((byte) 0x3F))) << 6) |
		((Uint) (source[2] & ((byte) 0x3F))); 	 	
	    return (int) unipoint;
	} else if (((*source) & ((byte) 0xF8)) == 0xF0) {
	    if (size < 4) {
		return -2 ;
	    }
	    if (((source[1] & ((byte) 0xC0)) != 0x80) ||
		((source[2] & ((byte) 0xC0)) != 0x80) ||
		((source[3] & ((byte) 0xC0)) != 0x80) ||
		(((*source) == 0xF0) && (source[1] < 0x90)) /* overlong */) {
		return -1;
	    }
	    if ((((*source) & ((byte)0x7)) > 0x4U) ||
		((((*source) & ((byte)0x7)) == 0x4U) && 
		 ((source[1] & ((byte)0x3F)) > 0xFU))) {
		return -1;
	    }
	    (*pos) += 4;
	    unipoint = 
		(((Uint) ((*source) & ((byte) 0x7))) << 18) |
		(((Uint) (source[1] & ((byte) 0x3F))) << 12) |
		(((Uint) (source[2] & ((byte) 0x3F))) << 6) |
		((Uint) (source[3] & ((byte) 0x3F))); 	 	
	    return (int) unipoint;
	} else {
	    return -1;
	}
    } else {
	return -1;
    }
}

static int octal_or_hex_positions(Uint c) 
{
    int x = 0;
    Uint ch = c;
    if (!ch) {
	return 1;
    }
    while(ch) {
	++x;
	ch >>= 3;
    }
    if (x <= 3) {
	return 3;
    }
    /* \x{H ...} format when larger than \777 */
    x = 0;
    ch = c;
    while(ch) {
	++x;
	ch >>= 4;
    }
    return x+3;
}

static void octal_or_hex_format(Uint ch, byte *buf, int *pos)
{
    static byte hex_chars[] = { '0','1','2','3','4','5','6','7','8','9',
				'A','B','C','D','E','F'};
    int num = octal_or_hex_positions(ch);
    if (num != 3) {
	buf[(*pos)++] = 'x';
	buf[(*pos)++] = '{';
	num -= 3;
	while(num--) {
	    buf[(*pos)++] = hex_chars[((ch >> (4*num)) & 0xFU)];
	}
	buf[(*pos)++] = '}';
    } else {
	while(num--) {
	    buf[(*pos)++] = ((byte) ((ch >> (3*num)) & 0x7U) + '0');
	}
    }	
}

/*
 * Check that there is enough room in all buffers to copy all pad chars
 * and stiff we need If not, realloc lbuf.
 */
static int check_buf_size(byte *s, int n)
{
    int pos = 0;
    int ch;
    int size = 10;

    DEBUGLOG(("check_buf_size: n = %d",n));
    while(pos < n) {
	/* Indata is always UTF-8 */
	if ((ch = pick_utf8(s,n,&pos)) < 0) {
	    /* XXX temporary allow invalid chars */
	    ch = (int) s[pos];
	    DEBUGLOG(("check_buf_size: Invalid UTF8:%d",ch));
	    ++pos;
	} 
	if (utf8_mode) { /* That is, terminal is UTF8 compliant */
	    if (ch >= 128 || isprint(ch)) {
#ifdef HAVE_WCWIDTH
		int width;
#endif
		DEBUGLOG(("check_buf_size: Printable(UTF-8:%d):%d",pos,ch));
		size++;
#ifdef HAVE_WCWIDTH
		if ((width = wcwidth(ch)) > 1) {
		    size += width - 1;
		}
#endif
	    } else if (ch == '\t') {
		size += 8;
	    } else {
		DEBUGLOG(("check_buf_size: Magic(UTF-8:%d):%d",pos,ch));
		size += 2;
	    }
	} else {
	    if (ch <= 255 && isprint(ch)) {
		DEBUGLOG(("check_buf_size: Printable:%d",ch));
		size++;
	    } else if (ch == '\t') 
		size += 8;
	    else if (ch >= 128) {
		DEBUGLOG(("check_buf_size: Non printable:%d",ch));
		size += (octal_or_hex_positions(ch) + 1);
	    }
	    else {
		DEBUGLOG(("check_buf_size: Magic:%d",ch));
		size += 2;
	    }
	}
    }
		
    if (size + lpos >= lbuf_size) {

	lbuf_size = size + lpos + BUFSIZ;
	if ((lbuf = enif_realloc(lbuf, lbuf_size * sizeof(Uint32))) == NULL) {
            DEBUGLOG(("check_buf_size: alloc failure of %d bytes", lbuf_size * sizeof(Uint32)));
	    abort(); //driver_failure(ttysl_port, -1);
	    return(0);
	}
    }
    DEBUGLOG(("check_buf_size: success\n"));
    return(1);
}


struct queued_buf
{
    struct queued_buf* next;
    int nbytes;
    byte buf[1];
};

static struct queued_buf* ttsl_q_head = NULL;
static struct queued_buf* ttsl_q_tail = NULL;
static struct queued_buf* putcbuf;
static unsigned int ttsl_q_head_written = 0;
static unsigned int ttsl_q_total = 0;
static int waiting_to_write = 0;

static void alloc_buf(int count);
static ERL_NIF_TERM try_write(ErlNifEnv*);
static ERL_NIF_TERM do_write(ErlNifEnv*);

static ERL_NIF_TERM putc_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary bin;
    if (!enif_inspect_binary(env, argv[0], &bin)) {
        enif_make_badarg(env);
    }
    DEBUGLOG(("ttysl_from_erlang: OP: Putc(%lu)",(unsigned long) bin.size));

    alloc_buf(bin.size + 1); // SVERK: Really? Only 1 extra byte?
    check_buf_size(bin.data, bin.size);
    put_chars(bin.data, bin.size);
    return try_write(env);
}

static ERL_NIF_TERM move_rel_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int n;
    if (!enif_get_int(env, argv[0], &n))
        enif_make_badarg(env);

    alloc_buf(3); // SVERK: Really? Only 3 bytes?
    move_rel(n);
    return try_write(env);
}

static ERL_NIF_TERM insc_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary bin;
    if (!enif_inspect_binary(env, argv[0], &bin))
        enif_make_badarg(env);

    alloc_buf(bin.size + 1); // SVERK: Really? Only 1 extra byte?
    check_buf_size(bin.data, bin.size);
    ins_chars(bin.data, bin.size);
    return try_write(env);
}

static ERL_NIF_TERM delc_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int n;
    if (!enif_get_int(env, argv[0], &n))
        enif_make_badarg(env);

    alloc_buf(3); // SVERK: Really? Only 3 bytes?
    del_chars(n);
    return try_write(env);
}

static ERL_NIF_TERM beep_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    alloc_buf(1);
    outc('\007');
    return try_write(env);
}

static ERL_NIF_TERM continue_write_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    waiting_to_write = 0;
    return do_write(env);
}

static void alloc_buf(int count)
{
    if (count > TTY_BUFFSIZE)
        count = TTY_BUFFSIZE;
    putcbuf = enif_alloc(sizeof(struct queued_buf) + count);
    putcbuf->nbytes = count;
    putcpos = 0;

    if (lpos > MAXSIZE) 
        put_chars((byte*)"\n", 1);
}

static void enqueue_buf(void)
{
    putcbuf->nbytes = putcpos;
    putcbuf->next = NULL;
    if (ttsl_q_tail) {
        ASSERT(ttsl_q_head);
        ttsl_q_tail->next = putcbuf;
    }
    else {
        ASSERT(!ttsl_q_head);
        ASSERT(ttsl_q_total == 0);
        ttsl_q_head = putcbuf;
        ttsl_q_head_written = 0;
    }
    ttsl_q_tail = putcbuf;
    ttsl_q_total += putcpos;
}

static ERL_NIF_TERM try_write(ErlNifEnv* env)
{
    enqueue_buf();

    if (!waiting_to_write) {
        return do_write(env);
    }
    else
        return enif_make_uint(env, ttsl_q_total);
}

static ERL_NIF_TERM do_write(ErlNifEnv* env)
{
    ASSERT(ttsl_q_head);
    for (;;) {
        int written = write(ttysl_fd,
                            ttsl_q_head->buf + ttsl_q_head_written,
                            ttsl_q_head->nbytes - ttsl_q_head_written);
        if (written < 0) {
            if (errno == ERRNO_BLOCK) {
                enif_select(env, (ErlNifEvent)(long)ttysl_fd,
                            ERL_NIF_SELECT_WRITE, ttsl_resource, atom_undefined);
                waiting_to_write = 1;
                break;
            }
            else if (errno == EINTR) {
                continue;
            }
            else {
                ERL_NIF_TERM errno_term = enif_make_int(env, errno); 
                return enif_make_tuple3(env, enif_make_atom(env, "error"),
                                        enif_make_string(env, "write errno=", ERL_NIF_LATIN1),
                                        errno_term);
            }
        } else {
            ttsl_q_head_written += written;
            ttsl_q_total -= written;
            if (ttsl_q_head_written >= ttsl_q_head->nbytes) {
                struct queued_buf* free_me = ttsl_q_head;
                ASSERT(ttsl_q_head_written == ttsl_q_head->nbytes);
                ttsl_q_head = ttsl_q_head->next;
                ttsl_q_head_written = 0;
                enif_free(free_me);
                if (!ttsl_q_head) {
                    ASSERT(ttsl_q_tail == free_me);
                    ASSERT(ttsl_q_total == 0);
                    ttsl_q_tail = NULL;
                    break;
                }
            }
        }
    }
    return enif_make_uint(env, ttsl_q_total);
}

/*
static void ttysl_flush_tty(ErlDrvData ttysl_data) {
    DEBUGLOG(("ttysl_flush_tty: .."));
    ttysl_terminate = 1;
    return;
}
*/

#define READ_BUF_SZ 1024

static ERL_NIF_TERM read_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    byte b[READ_BUF_SZ];
    ssize_t i;
    int ch = 0, pos = 0;
    int left = READ_BUF_SZ;
    byte *p = b;
    byte t[READ_BUF_SZ*2];  /* Max 2 bytes per latin1 as UTF8 */
    int tpos;
    ERL_NIF_TERM res;

    if (utf8buf_size > 0) {
	memcpy(b,utf8buf,utf8buf_size);
	left -= utf8buf_size;
	p += utf8buf_size;
	utf8buf_size = 0;
    }

    DEBUGLOG(("ttysl_from_tty: remainder = %d", left));
    
    if ((i = read(ttysl_fd, (char *) p, left)) >= 0) {
	if (p != b) {
	    i += (p - b);
	}
	if (utf8_mode) { /* Hopefully an UTF8 terminal */
	    while(pos < i && (ch = pick_utf8(b,i,&pos)) >= 0)
		;
	    if (ch == -2 && i - pos <= 4) {
		/* bytes left to care for */
		utf8buf_size = i -pos;
		memcpy(utf8buf,b+pos,utf8buf_size);
	    } else if (ch == -1) {
		DEBUGLOG(("ttysl_from_tty: Giving up on UTF8 mode, invalid character"));
		utf8_mode = 0;
		goto latin_terminal;
            }
            p = enif_make_new_binary(env, pos, &res);
            memcpy(p, b, pos);
	}
        else {
	latin_terminal:
	    tpos = 0;
            while (pos < i) {
                put_utf8((int) b[pos++], t, sizeof(t), &tpos);
                ASSERT(tpos <= sizeof(t));
            }
            p = enif_make_new_binary(env, tpos, &res);
            memcpy(p, t, tpos);
	}
    }
    else if (errno == ERRNO_BLOCK || errno == EINTR) {
        enif_make_new_binary(env, 0, &res);
    }
    else {
        DEBUGLOG(("ttsl:read(): failure in read(%d,..) = %d\n",
                  (int)(SWord)ttysl_fd, i));
	return atom_error;
    }
    enif_select(env, (ErlNifEvent)ttysl_fd, ERL_NIF_SELECT_READ, ttsl_resource,
                atom_undefined);

    DEBUGLOG(("ttsl:read(): returning %T\n", res));
    return res;
}

static void ttsl_rt_stop(ErlNifEnv* env, void* resource, int fd, int is_direct_call)
{
    ASSERT(resource == ttsl_resource);
    if (ttysl_fd != 0) {
	close(ttysl_fd);
    }
}

static int start_lbuf(void)
{
    if (!lbuf && !(lbuf = ( Uint32*) enif_alloc(lbuf_size * sizeof(Uint32))))
      return FALSE;
    llen = 0;
    lpos = 0;
    return TRUE;
}

static int stop_lbuf(void)
{
    if (lbuf) {
	enif_free(lbuf);
	lbuf = NULL;
    }
    return TRUE;
}

/* Put l bytes (in UTF8) from s into the buffer and output them. */
static int put_chars(byte *s, int l)
{
    int n;

    n = insert_buf(s, l);
    if (n > 0)
      write_buf(lbuf + lpos - n, n);
    if (lpos > llen)
      llen = lpos;
    return TRUE;
}

/*
 * Move the current postition forwards or backwards within the current
 * line. We know about padding.
 */
static int move_rel(int n)
{
    int npos;			/* The new position */

    /* Step forwards or backwards over the buffer. */
    npos = step_over_chars(n);

    /* Calculate move, updates pointers and move the cursor. */
    move_cursor(lpos, npos);
    lpos = npos;
    return TRUE;
}

/* Insert characters into the buffer at the current position. */
static int ins_chars(byte *s, int l)
{
    int n, tl;
    Uint32 *tbuf = NULL;    /* Suppress warning about use-before-set */

    /* Move tail of buffer to make space. */
    if ((tl = llen - lpos) > 0) {
	if ((tbuf = enif_alloc(tl * sizeof(Uint32))) == NULL)
	    return FALSE;
	memcpy(tbuf, lbuf + lpos, tl * sizeof(Uint32));
    }
    n = insert_buf(s, l);
    if (tl > 0) {
	memcpy(lbuf + lpos, tbuf, tl * sizeof(Uint32));
	enif_free(tbuf);
    }
    llen += n;
    write_buf(lbuf + (lpos - n), llen - (lpos - n));
    move_cursor(llen, lpos);
    return TRUE;
}

/*
 * Delete characters in the buffer. Can delete characters before (n < 0)
 * and after (n > 0) the current position. Cursor left at beginning of
 * deleted block.
 */
static int del_chars(int n)
{
    int i, l, r;
    int pos;

    update_cols();

    /* Step forward or backwards over n logical characters. */
    pos = step_over_chars(n);

    if (pos > lpos) {
	l = pos - lpos;		/* Buffer characters to delete */
	r = llen - lpos - l;	/* Characters after deleted */
	/* Fix up buffer and buffer pointers. */
	if (r > 0)
	    memmove(lbuf + lpos, lbuf + pos, r * sizeof(Uint32));
	llen -= l;
	/* Write out characters after, blank the tail and jump back to lpos. */
	write_buf(lbuf + lpos, r);
	for (i = l ; i > 0; --i)
	  outc(' ');
	if (COL(llen+l) == 0 && xn)
	{
	   outc(' ');
	   move_left(1);
	}
	move_cursor(llen + l, lpos);
    }
    else if (pos < lpos) {
	l = lpos - pos;		/* Buffer characters */
	r = llen - lpos;	/* Characters after deleted */
	move_cursor(lpos, lpos-l);	/* Move back */
	/* Fix up buffer and buffer pointers. */
	if (r > 0)
	    memmove(lbuf + pos, lbuf + lpos, r * sizeof(Uint32));
	lpos -= l;
	llen -= l;
	/* Write out characters after, blank the tail and jump back to lpos. */
	write_buf(lbuf + lpos, r);
	for (i = l ; i > 0; --i)
	  outc(' ');
	if (COL(llen+l) == 0 && xn)
	{
	   outc(' ');
	   move_left(1);
	}
	move_cursor(llen + l, lpos);
    }
    return TRUE;
}

/* Step over n logical characters, check for overflow. */
static int step_over_chars(int n)
{
    Uint32 *c, *beg, *end;

    beg = lbuf;
    end = lbuf + llen;
    c = lbuf + lpos;
    for ( ; n > 0 && c < end; --n) {
#ifdef HAVE_WCWIDTH
	while (*c & WIDE_TAG) {
	    c++;
	}
#endif
	c++;
	while (c < end && (*c & TAG_MASK) && ((*c & ~TAG_MASK) == 0))
	    c++;
    }
    for ( ; n < 0 && c > beg; n++) {
	--c;
#ifdef HAVE_WCWIDTH
	while (c > beg + 1 && (c[-1] & WIDE_TAG)) {
	    --c;
	}
#endif
	while (c > beg && (*c & TAG_MASK) && ((*c & ~TAG_MASK) == 0))
	    --c;
    }
    return c - lbuf;
}

/*
 * Insert n characters into the buffer at lpos.
 * Know about pad characters and treat \n specially.
 */

static int insert_buf(byte *s, int n)
{
    int pos = 0;
    int buffpos = lpos;
    int ch;

    while (pos < n) {
	if ((ch = pick_utf8(s,n,&pos)) < 0) {
	    /* XXX temporary allow invalid chars */
	    ch = (int) s[pos];
	    DEBUGLOG(("insert_buf: Invalid UTF8:%d",ch));
	    ++pos;
	}
	if ((utf8_mode && (ch >= 128 || isprint(ch))) || (ch <= 255 && isprint(ch))) {
#ifdef HAVE_WCWIDTH
	    int width;
	    if ((width = wcwidth(ch)) > 1) {
		while (--width) {
		    DEBUGLOG(("insert_buf: Wide(UTF-8):%d,%d",width,ch));
		    lbuf[lpos++] = (WIDE_TAG | ((Uint32) ch));
		}
	    }
#endif
	    DEBUGLOG(("insert_buf: Printable(UTF-8):%d",ch));
	    lbuf[lpos++] = (Uint32) ch;
	} else if (ch >= 128) { /* not utf8 mode */
	    int nc = octal_or_hex_positions(ch);
	    lbuf[lpos++] = ((Uint32) ch) | ESCAPED_TAG;
	    while (nc--) {
		lbuf[lpos++] = ESCAPED_TAG;
	    }
	} else if (ch == '\t') {
	    do {
		lbuf[lpos++] = (CONTROL_TAG | ((Uint32) ch));
		ch = 0;
	    } while (lpos % 8);
	} else if (ch == '\e' || ch == '\n' || ch == '\r') {
	    write_buf(lbuf + buffpos, lpos - buffpos);
            if (ch == '\e') {
                outc('\e');
            } else {
                outc('\r');
                if (ch == '\n')
                    outc('\n');
            }
	    if (llen > lpos) {
		memcpy(lbuf, lbuf + lpos, llen - lpos);
	    }
	    llen -= lpos;
	    lpos = buffpos = 0;
	} else {
	    DEBUGLOG(("insert_buf: Magic(UTF-8):%d",ch));
	    lbuf[lpos++] = ch | CONTROL_TAG;
	    lbuf[lpos++] = CONTROL_TAG;
	}
    }
    return lpos - buffpos; /* characters "written" into 
			      current buffer (may be less due to newline) */
}
	


/*
 * Write n characters in line buffer starting at s. Be smart about
 * non-printables. Know about pad characters and that \n can never
 * occur normally.
 */

static int write_buf(Uint32 *s, int n)
{
    byte ubuf[4];
    int ubytes = 0, i;
    byte lastput = ' ';

    update_cols();

    while (n > 0) {
	if (!(*s & TAG_MASK) ) {
	    if (utf8_mode) {
		ubytes = 0;
		if (put_utf8((int) *s, ubuf, 4, &ubytes) == 0) {
		    for (i = 0; i < ubytes; ++i) {
			outc(ubuf[i]);
		    }
		    lastput = 0; /* Means the last written character was multibyte UTF8 */
		}
	    } else {
		outc((byte) *s);
		lastput = (byte) *s;
	    }
	    --n;
	    ++s;
	}
	else if (*s == (CONTROL_TAG | ((Uint32) '\t'))) {
	    outc(lastput = ' ');
	    --n; s++;
	    while (n > 0 && *s == CONTROL_TAG) {
		outc(lastput = ' ');
		--n; s++;
	    }
	} else if (*s & CONTROL_TAG) {
	    outc('^');
	    outc(lastput = ((byte) ((*s == 0177) ? '?' : *s | 0x40)));
	    n -= 2;
	    s += 2;
	} else if (*s & ESCAPED_TAG) {
	    Uint32 ch = *s & ~(TAG_MASK);
	    byte *octbuff;
	    byte octtmp[256];
	    int octbytes;
	    DEBUGLOG(("write_buf: Escaped: %d", ch));
	    octbytes = octal_or_hex_positions(ch);
	    if (octbytes > 256) {
		octbuff = enif_alloc(octbytes);
	    } else {
		octbuff = octtmp;
	    }
	    octbytes = 0;
	    octal_or_hex_format(ch, octbuff, &octbytes);
            DEBUGLOG(("write_buf: octbytes: %d", octbytes));
	    outc('\\');
	    for (i = 0; i < octbytes; ++i) {
		outc(lastput = octbuff[i]);
		DEBUGLOG(("write_buf: outc: %d", (int) lastput));
	    }
	    n -= octbytes+1;
	    s += octbytes+1;
	    if (octbuff != octtmp) {
		enif_free(octbuff);
	    }
#ifdef HAVE_WCWIDTH
	} else if (*s & WIDE_TAG) {
	    --n; s++;
#endif
	} else {
	    DEBUGLOG(("write_buf: Very unexpected character %d",(int) *s));
	    ++n;
	    --s;
	}
    }
    /* Check landed in first column of new line and have 'xn' bug. */
    n = s - lbuf;
    if (COL(n) == 0 && xn && n != 0) {
	if (n >= llen) {
	    outc(' ');
	} else if (lastput == 0) { /* A multibyte UTF8 character */
	    for (i = 0; i < ubytes; ++i) {
		outc(ubuf[i]);
	    }
	} else {
	    outc(lastput);
	}
	move_left(1);
    }
    return TRUE;
}


/* The basic procedure for outputting one character. */
static int outc(int c)
{
    putcbuf->buf[putcpos++] = c;
    if (putcpos == putcbuf->nbytes) {
        enqueue_buf();
        alloc_buf(TTY_BUFFSIZE);
    }
    return 1;
}

static int move_cursor(int from, int to)
{
    int dc, dl;

    update_cols();

    dc = COL(to) - COL(from);
    dl = LINE(to) - LINE(from);
    if (dl > 0)
      move_down(dl);
    else if (dl < 0)
      move_up(-dl);
    if (dc > 0)
      move_right(dc);
    else if (dc < 0)
      move_left(-dc);
    return TRUE;
}

static int start_termcap(void)
{
    int eres;
    size_t envsz = 1024;
    char *env = NULL;
    char *c;
    int tres;

    DEBUGLOG(("start_termcap: .."));

    capbuf = enif_alloc(1024);
    if (!capbuf)
	goto false;
    eres = enif_getenv("TERM", capbuf, &envsz);
    if (eres == 0)
	env = capbuf;
    else if (eres < 0) {
        DEBUGLOG(("start_termcap: failure in erl_drv_getenv(\"TERM\", ..) = %d\n", eres));
	goto false;
    } else /* if (eres > 1) */ {
      char *envbuf = enif_alloc(envsz);
      if (!envbuf)
	  goto false;
      while (1) {
	  char *newenvbuf;
	  eres = enif_getenv("TERM", envbuf, &envsz);
	  if (eres == 0)
	      break;
	  newenvbuf = enif_realloc(envbuf, envsz);
          if (eres < 0 || !newenvbuf) {
              DEBUGLOG(("start_termcap: failure in erl_drv_getenv(\"TERM\", ..) = %d or realloc buf == %p\n", eres, newenvbuf));
	      env = newenvbuf ? newenvbuf : envbuf;
	      goto false;
	  }
	  envbuf = newenvbuf;
      }
      env = envbuf;
    }
    if ((tres = tgetent((char*)lbuf, env)) <= 0) {
        DEBUGLOG(("start_termcap: failure in tgetent(..) = %d\n", tres));
        goto false;
    }
    if (env != capbuf) {
	env = NULL;
	enif_free(env);
    }
    c = capbuf;
    cols = tgetnum("co");
    if (cols <= 0)
	cols = DEF_WIDTH;
    xn = tgetflag("xn");
    up = tgetstr("up", &c);
    if (!(down = tgetstr("do", &c)))
      down = "\n";
    if (!(left = tgetflag("bs") ? "\b" : tgetstr("bc", &c)))
      left = "\b";		/* Can't happen - but does on Solaris 2 */
    right = tgetstr("nd", &c);
    if (up && down && left && right) {
        DEBUGLOG(("start_termcap: successful start\n"));
        return TRUE;
    }
    DEBUGLOG(("start_termcap: failed start\n"));
 false:
    if (env && env != capbuf)
	enif_free(env);
    if (capbuf)
	enif_free(capbuf);
    capbuf = NULL;
    return FALSE;
}

/*
static int stop_termcap(void)
{
    if (capbuf) enif_free(capbuf);
    capbuf = NULL;
    return TRUE;
}
*/

static int move_left(int n)
{
    while (n-- > 0)
      tputs(left, 1, outc);
    return TRUE;
}

static int move_right(int n)
{
    while (n-- > 0)
      tputs(right, 1, outc);
    return TRUE;
}

static int move_up(int n)
{
    while (n-- > 0)
      tputs(up, 1, outc);
    return TRUE;
}

static int move_down(int n)
{
    while (n-- > 0)
      tputs(down, 1, outc);
    return TRUE;
}


/*
 * Updates cols if terminal has resized (SIGWINCH). Should be called
 * at the start of any function that uses the COL or LINE macros. If
 * the terminal is resized after calling this function but before use
 * of the macros, then we may write to the wrong screen location.
 *
 * We cannot call this from the SIGWINCH handler because it uses
 * ioctl() which is not a safe function as listed in the signal(7)
 * man page.
 */
static void update_cols(void)
{
    Uint32 width, height;
 
    if (cols_needs_update) {
	cols_needs_update = FALSE;
	ttysl_get_window_size(&width, &height);
	cols = width;
    }
}


/*
 * Put a terminal device into non-canonical mode with ECHO off.
 * Before doing so we first save the terminal's current mode,
 * assuming the caller will call the tty_reset() function
 * (also in this file) when it's done with raw mode.
 */

static struct termios tty_smode, tty_rmode;

static int tty_init(int fd, int canon, int echo, int sig) {
    int tres;
    DEBUGLOG(("tty_init: fd = %d, canon = %d, echo = %d, sig = %d", fd, canon, echo, sig));
    if ((tres = tcgetattr(fd, &tty_rmode)) < 0) {
        DEBUGLOG(("tty_init: failure in tcgetattr(%d,..) = %d\n", fd, tres));
        return -1;
    }
    tty_smode = tty_rmode;

    /* Default characteristics for all usage including termcap output. */
    tty_smode.c_iflag &= ~ISTRIP;

    /* Turn canonical (line mode) on off. */
    if (canon > 0) {
	tty_smode.c_iflag |= ICRNL;
	tty_smode.c_lflag |= ICANON;
	tty_smode.c_oflag |= OPOST;
	tty_smode.c_cc[VEOF] = tty_rmode.c_cc[VEOF];
#ifdef VDSUSP
	tty_smode.c_cc[VDSUSP] = tty_rmode.c_cc[VDSUSP];
#endif
    }
    if (canon < 0) {
	tty_smode.c_iflag &= ~ICRNL;
	tty_smode.c_lflag &= ~ICANON;
	tty_smode.c_oflag &= ~OPOST;
	/* Must get these really right or funny effects can occur. */
	tty_smode.c_cc[VMIN] = 1;
	tty_smode.c_cc[VTIME] = 0;
#ifdef VDSUSP
	tty_smode.c_cc[VDSUSP] = 0;
#endif
    }

    /* Turn echo on or off. */
    if (echo > 0)
      tty_smode.c_lflag |= ECHO;
    if (echo < 0)
      tty_smode.c_lflag &= ~ECHO;

    /* Set extra characteristics for "RAW" mode, no signals. */
    if (sig > 0) {
	/* Ignore IMAXBEL as not POSIX. */
#ifndef QNX
	tty_smode.c_iflag |= (BRKINT|IGNPAR|ICRNL|IXON|IXANY);
#else
	tty_smode.c_iflag |= (BRKINT|IGNPAR|ICRNL|IXON);
#endif
	tty_smode.c_lflag |= (ISIG|IEXTEN);
    }
    if (sig < 0) {
	/* Ignore IMAXBEL as not POSIX. */
#ifndef QNX
	tty_smode.c_iflag &= ~(BRKINT|IGNPAR|ICRNL|IXON|IXANY);
#else
	tty_smode.c_iflag &= ~(BRKINT|IGNPAR|ICRNL|IXON);
#endif	
	tty_smode.c_lflag &= ~(ISIG|IEXTEN);
    }
    DEBUGLOG(("tty_init: successful init\n"));
    return 0;
}

/*
 * Set/restore a terminal's mode to whatever it was on the most
 * recent call to the tty_init() function above.
 */

static int tty_set(int fd)
{
    int tres;
    DEBUGF(("tty_set: Setting tty...\n"));

    if ((tres = tcsetattr(fd, TCSANOW, &tty_smode)) < 0) {
        DEBUGLOG(("tty_set: failure in tcgetattr(%d,..) = %d\n", fd, tres));
	return(-1);
    }
    return(0);
}

static int tty_reset(int fd)         /* of terminal device */
{
    int tres;
    DEBUGF(("tty_reset: Resetting tty...\n"));

    if ((tres = tcsetattr(fd, TCSANOW, &tty_rmode)) < 0) {
        DEBUGLOG(("tty_reset: failure in tcsetattr(%d,..) = %d\n", fd, tres));
	return(-1);
    }
    return(0);
}

/* 
 * Signal handler to cope with signals so that we can reset the tty
 * to the orignal settings
 */

#ifdef ERTS_NOT_USED
/* XXX: A mistake that it isn't used, or should it be removed? */

static RETSIGTYPE suspend(int sig)
{
    if (tty_reset(ttysl_fd) < 0) {
        DEBUGLOG(("signal: failure in suspend(%d), can't reset tty %d\n", sig, ttysl_fd));
	fprintf(stderr,"Can't reset tty \n");
	exit(1);
    }

    sys_signal(sig, SIG_DFL);	/* Set signal handler to default */
    sys_sigrelease(sig);	/* Allow 'sig' to come through */
    kill(getpid(), sig);	/* Send ourselves the signal */
    sys_sigblock(sig);		/* Reset to old mask */
    sys_signal(sig, suspend);	/* Reset signal handler */ 

    if (tty_set(ttysl_fd) < 0) {
        DEBUGLOG(("signal: failure in suspend(%d), can't set tty %d\n", sig, ttysl_fd));
	fprintf(stderr,"Can't set tty raw \n");
	exit(1);
    }
}

#endif

static RETSIGTYPE cont(int sig)
{
    if (tty_set(ttysl_fd) < 0) {
        DEBUGLOG(("signal: failure in cont(%d), can't set tty raw %d\n", sig, ttysl_fd));
	fprintf(stderr,"Can't set tty raw\n");
	exit(1);
    }
}

static RETSIGTYPE winch(int sig)
{
    cols_needs_update = TRUE;
}

ErlNifFunc ttsl_nif_funcs[] = {
    {"start", 1, start_nif},
    {"get_window_size", 0, get_window_size_nif},
    {"get_unicode_state", 0, get_unicode_state_nif},
    {"set_unicode_state", 1, set_unicode_state_nif},
    {"putc", 1, putc_nif},
    {"move_rel", 1, move_rel_nif},
    {"insc", 1, insc_nif},
    {"delc", 1, delc_nif},
    {"beep", 0, beep_nif},
    {"continue_write", 0, continue_write_nif},
    {"read", 0, read_nif}
};

ERL_NIF_INIT(ttsl, ttsl_nif_funcs, ttsl_load, NULL, NULL, NULL)

#endif /* HAVE_TERMCAP */
