/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2012. All Rights Reserved.
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

/* This is a TCP server used for network benchmarking.
 * The server accepts connections and echoes back all received data
 * until the client closes a connection.
 *
 * The server can act as an Erlang port program sending erlang terms
 * on stdout in a line oriented fashion.
 * Server will terminate when EOF or '.' is received on stdin.
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>
#include <netdb.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <fcntl.h>
#include <sys/time.h>

#define PRINT_PREFIX "netmarks server.c: "

/*#define DEBUG*/
#ifdef DEBUG
#define DBG_TRACE(STR) fprintf(stderr, PRINT_PREFIX STR "\r\n")
#define DBG_TRACE1(STR,ARG1) fprintf(stderr, PRINT_PREFIX STR "\r\n", ARG1)
#else
#define DBG_TRACE(STR)
#define DBG_TRACE1(STR,ARG1)
#endif


#define CHK(X) check((X),__LINE__)

void check(int x,int line)
{
    if (!x) {
        fprintf(stderr,PRINT_PREFIX "Error at line %d, errno = %d\r\n",
                line,errno);
        exit(1);
    }
}

void
send_term(const char *format, ...)
{
    va_list arglist;
    va_start(arglist, format);
    vprintf(format, arglist);
    printf(".\n");
    fflush(stdout);
    va_end(arglist);
}


struct buffer_t
{
    int ix;
    char* start;
    char* in;  /* read from socket to here */
    char* out; /* write to socket from here */
    char* in_limit;
    struct buffer_t* next;
};

void print_buf(const char* name, struct buffer_t* buf)
{
    fprintf(stderr, "%s[%d]: out=%ld in=%ld in_limit=%ld\r\n",
	    name,
	    buf->ix,
	    buf->out - buf->start,
	    buf->in - buf->start,
	    buf->in_limit - buf->start);
}

#define BUFFER_CNT 2
#define IDLE_TIMEOUT_SEC 2

void loop(int csock)
{
    struct buffer_t bufs[BUFFER_CNT];
    fd_set fdsr, fdsw;
    long int tot_written = 0, tot_read = 0;
    int i, readlen, flags;
    struct buffer_t* rbuf;
    struct buffer_t* wbuf;
    socklen_t sizeof_readlen = sizeof(readlen);

    /* Make connnection socket nonblocking
     */
    CHK((flags = fcntl(csock, F_GETFL, 0)) >= 0);
    CHK(fcntl(csock, F_SETFL, flags | O_NONBLOCK) >= 0);

    /* We use socket default recv buffer size as the maximum read chunk.
     */
    CHK(getsockopt(csock, SOL_SOCKET, SO_RCVBUF, &readlen, &sizeof_readlen) == 0);

    for (i = 0; i<BUFFER_CNT; i++) {
	int buf_sz = readlen*2;
	char* p = (char*) malloc(buf_sz);
	bufs[i].ix = i;
	bufs[i].start = p;
	bufs[i].in = p;
	bufs[i].out = p;
	bufs[i].in_limit = p + (buf_sz - readlen) + 1;
	bufs[i].next = &bufs[i+1];
    }
    bufs[BUFFER_CNT-1].next = &bufs[0]; /* circular list of buffers */

    rbuf = &bufs[0];
    wbuf = &bufs[0];

    for (;;) {
	struct timeval tv;
	int sel_ret;
	/*print_buf("IN", rbuf);*/
	/*print_buf("OUT", wbuf);*/
	FD_ZERO(&fdsr);
	FD_ZERO(&fdsw);
	if (wbuf->out < wbuf->in) {
	    FD_SET(csock,&fdsw);
	}
	if (rbuf->in < rbuf->in_limit) {
	    FD_SET(csock,&fdsr);
	}
	CHK(FD_ISSET(csock,&fdsw) || FD_ISSET(csock,&fdsr));
	tv.tv_sec = IDLE_TIMEOUT_SEC;
	tv.tv_usec = 0;
	sel_ret = select(csock+1,&fdsr,&fdsw,NULL, &tv);
	if (sel_ret <= 0) {
	    if (sel_ret == 0) {
		fprintf(stderr, PRINT_PREFIX "idle timeout (%ds): ", IDLE_TIMEOUT_SEC);
		if (tot_read == tot_written)
		    fprintf(stderr, "read and written %ld bytes\r\n", tot_read);
		else
		    fprintf(stderr, "read %ld and written %ld bytes\r\n", tot_read, tot_written);
		continue;
	    }
	    if (errno == EINTR) {
		continue;
	    } else {
		CHK(0);
	    }
	}
	if (FD_ISSET(csock,&fdsr)) {
	    int n;
	    CHK(rbuf->in < rbuf->in_limit);

	    n = read(csock, rbuf->in, readlen);
	    if (n > 0) {
		tot_read += n;
		rbuf->in += n;
		if (rbuf->in >= rbuf->in_limit) {
		    rbuf = rbuf->next;
		    /*fprintf(stderr, "Switching to in-buffer %d.\r\n", rbuf->ix);*/
		}
	    }
	    else {
		CHK(n == 0);
		rbuf->in_limit = rbuf->start; /* eof */
		if (wbuf == rbuf && wbuf->out == wbuf->in) {
		    DBG_TRACE("Got EOF, no pending output\r\n");
		    break;
		}
		DBG_TRACE1("Got EOF, but got %ld bytes to write\r\n", tot_read-tot_written);
	    }
	    /*fprintf(stderr,"tot_read = %ld\r\n",tot_read);*/
	}
	if (FD_ISSET(csock,&fdsw)) {
	    int n = write(csock, wbuf->out, wbuf->in - wbuf->out);
	    CHK(n > 0);
	    wbuf->out += n;
	    tot_written += n;
	    if (wbuf->out == wbuf->in) {
		if (wbuf->in >= wbuf->in_limit) {
		    if (wbuf->in_limit == wbuf->start) {
			break; /* eof */
		    }
		    wbuf->in = wbuf->start;
		    wbuf->out = wbuf->start;
		    wbuf = wbuf->next;
		    /*fprintf(stderr, "Switching to out-buffer %d\r\n", wbuf->ix);*/
		}
	    }
	    /*fprintf(stderr,"tot_written = %ld, (errno = %d)\r\n",tot_written,errno);*/
	}
    }

    DBG_TRACE1("read bytes   : %ld", tot_read);
    DBG_TRACE1("written bytes: %ld", tot_written);
}


#define DEFAULT_PORT 4999
#define LISTEN_BACKLOG 5

#define STRINGIFY(X) #X
#define S(X) STRINGIFY(X)

void usage(char *a0)
{
    fprintf(stderr,"Usage: %s [-p <port number>]\n\n"
	    " - Default: %s -p " S(DEFAULT_PORT) "\n\n", a0,a0);
    exit(1);
}

void handle_sigchld(int signo)
{
    int stat;
    pid_t pid = wait(&stat);
    return;
}

int main(int argc, char **argv)
{
    struct sockaddr_in sin;
    fd_set fdset;
    int lsock, csock;
    int port_no = DEFAULT_PORT;
    pid_t chld;
    int i = 1;

    while(i < argc) {
	if (*argv[i] == '-') {
	    if (i == argc - 1) {
		usage(argv[0]);
	    }
	    switch (argv[i][1]) {
	    case 'p':
		port_no = atoi(argv[++i]);
		break;
	    default:
		usage(argv[0]);
	    }
	}
	++i;
    }

    signal(SIGCHLD, handle_sigchld);

    CHK((lsock = socket(AF_INET, SOCK_STREAM, 0)) >= 0);

    { /* Stevens says listen socket should be nonblocking when used in select */
	int flags;
	CHK((flags = fcntl(lsock, F_GETFL, 0)) >= 0);
	CHK(fcntl(lsock, F_SETFL, flags | O_NONBLOCK) >= 0);
    }

    memset(&sin, 0, sizeof(sin));
    sin.sin_family = AF_INET;
    sin.sin_addr.s_addr = htonl(INADDR_ANY);
    sin.sin_port = htons((unsigned short) port_no);

    if (bind(lsock, (struct sockaddr*)&sin, sizeof(sin)) != 0) {
	if (errno == EADDRINUSE) {
	    send_term("{error,\"C server failed, port %d already in use\"}", port_no);
	    exit(1);
	}
	CHK(0);
    }
    CHK(listen(lsock, LISTEN_BACKLOG) == 0);

    send_term("ready_to_serve");
    for (;;) {
	FD_ZERO(&fdset);
	FD_SET(STDIN_FILENO, &fdset);
	FD_SET(lsock, &fdset);

	DBG_TRACE("calling select");
	if (select(lsock+1,&fdset,NULL,NULL,NULL) < 0) {
	    if (errno == EINTR) {
		continue;
	    } else {
		CHK(0);
	    }
	}
	else DBG_TRACE("select returned ok");

	if (FD_ISSET(lsock, &fdset)) {
	    DBG_TRACE("calling accept");
	    csock = accept(lsock, NULL, NULL);
	    DBG_TRACE1("accept returned %d", csock);
	    if (csock < 0) {
		if (errno == EINTR ||
		    errno == EWOULDBLOCK ||
		    errno == ECONNABORTED ||
		    errno == EPROTO) {
		    continue;
		}
		CHK(0);
	    }
	    if ((chld = fork()) == 0) {
		loop(csock);
		return 0;
	    }
	    close(csock);
	}
	if (FD_ISSET(STDIN_FILENO, &fdset)) {
	    char input = 0;
	    DBG_TRACE("reading stdin");
	    if (read(STDIN_FILENO, &input, 1) == 0 || input == '.') {
		break; /* EOF or '.' on STDIN, Master wants me to die */
	    }
	    DBG_TRACE1("Ignored character %d on stdin", (int)input);
	}
    }
    DBG_TRACE("server terminating");
    return 0;
}
