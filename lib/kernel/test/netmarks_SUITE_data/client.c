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

/* This is a TCP client used for network benchmarking.
 * The client sets up connections and writes a fixed amount of data
 * that is expected to be echoed back by the server.
 *
 * The client can act as an Erlang port program sending erlang terms
 * on stdout in a line oriented fashion.
 * Client will terminate when all data has been sent and received.
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
#include <sys/time.h>

#define CHK(X) check((X),__LINE__)

void check(int x,int line)
{
    if (!x) {
        fprintf(stderr,"Error at line %d, errno = %d\n",
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

long tv_diff(struct timeval* t1, struct timeval* t2)
{
    return (t2->tv_sec - t1->tv_sec) * 1000 * 1000
	+ (t2->tv_usec - t1->tv_usec);
}

#define READ_BUF_SIZE 1024

char *build_packet(unsigned sz,int packet_type,int *packet_bytes)
{
    char *res;
    char *ptr;
    char x = 'a';
    CHK(packet_type == 0 || packet_type == 1 ||
	packet_type == 2 || packet_type == 4);
    CHK(packet_type == 0 || packet_type == sizeof(sz) ||
	(sz >> (8*packet_type)) == 0);
    ptr = res = malloc(sz + packet_type);
    *packet_bytes = sz+packet_type;
    switch (packet_type) {
    case 4:
	*ptr++ = (sz >> 24) & 0xFF;
	*ptr++ = (sz >> 16) & 0xFF;
    case 2:
	*ptr++ = (sz >> 8) & 0xFF;
    case 1:
	*ptr++ = sz  & 0xFF;
    default:
	break;
    }
    while(sz-- > 0) {
	*ptr++ = x;
	if (++x > 'z')
	    x = 'a';
    }
    return res;
}

void loop(int sock, unsigned sz, int packet_type, int npackets, int sync)
{
    char *packet;
    int packet_bytes;
    fd_set fdsr, fdsw;
    int bytes_to_write;
    int bytes_to_read;
    int packets_to_send;
    int tot_written = 0, tot_read = 0;
    char throw_away[READ_BUF_SIZE];

    CHK((packet = build_packet(sz,packet_type,&packet_bytes)) != NULL);

    packets_to_send = npackets;
    bytes_to_write = packet_bytes;
    bytes_to_read = packet_bytes;

    for(;;) {
	FD_ZERO(&fdsr);
	FD_ZERO(&fdsw);
	FD_SET(sock,&fdsr);
	if (bytes_to_write > 0)
	    FD_SET(sock,&fdsw);
	CHK(FD_ISSET(sock,&fdsw) || FD_ISSET(sock,&fdsr));
	if (select(sock+1,&fdsr,&fdsw,NULL,NULL)<  0) {
	    if (errno == EINTR) {
		continue;
	    } else {
		CHK(0);
	    }
	}
	if (FD_ISSET(sock,&fdsr)) {
	    int x = read(sock,throw_away,READ_BUF_SIZE);
	    tot_read += x;
	    /*fprintf(stderr,"tot_read = %d\n",tot_read);*/
	    if (sync) {
		bytes_to_read -= x;
		if (bytes_to_read == 0) {
		    if (!packets_to_send ) {
			break;
		    }
		    bytes_to_write = packet_bytes;
		    bytes_to_read  = packet_bytes;
		}
	    }
	    else if (!packets_to_send && tot_read == tot_written) {
		break;
	    }
	}
	if (FD_ISSET(sock,&fdsw)) {
	    int x = write(sock, packet+packet_bytes-bytes_to_write, bytes_to_write);
	    bytes_to_write -= x;
	    tot_written += x;
	    if (bytes_to_write == 0) {
		if (--packets_to_send > 0 && !sync) {
		    bytes_to_write = packet_bytes;
		}
		/*fprintf(stderr, "Sent packet #%d of %d (sync=%d)\n",
		          (npackets-packets_to_send), npackets, sync);*/
	    }
	    /*fprintf(stderr,"tot_written = %d, (errno = %d)\n",tot_written,errno);*/
	}
    }
    free(packet);
}


#define DEFAULT_ITERATIONS 1000000
#define DEFAULT_HEADER_SZ 0
#define DEFAULT_PACKET_SZ 1000
#define DEFAULT_PORT 4999
#define DEFAULT_PARALLEL 1
#define DEFAULT_HOST "localhost"
#define DEFAULT_SYNC 0
#define DEFAULT_BUFFER_SIZE 0

#define STRINGIFY(X) #X
#define S(X) STRINGIFY(X)

void usage(char *a0)
{
    fprintf(stderr,"Usage: %s [-n <num iterations>} [-h <packet header size>]\n"
	           "        [-b <packet data bytes>] [-p <port number>]"
	                  " [-x <num parallell>]"
	                  " [-s <synchronous sending true/false>]"
                          " [-t <send/recv buffer size (0 is none)>]"
                          " [<hostname>]\n\n"
	    " - Default: %s -n " S(DEFAULT_ITERATIONS) " -h " S(DEFAULT_HEADER_SZ)
	    " -b " S(DEFAULT_PACKET_SZ) " -p " S(DEFAULT_PORT) " -x " S(DEFAULT_PARALLEL)
	    " -s %s -t " S(DEFAULT_BUFFER_SIZE) " " DEFAULT_HOST "\n\n", a0,a0,(DEFAULT_SYNC?"true":"false"));
    exit(1);
}

int main(int argc, char **argv)
{
    struct sockaddr_in sin;
    struct hostent *he;
    int sock;

    int port_no     = DEFAULT_PORT;
    int iterations  = DEFAULT_ITERATIONS;
    int packet_size = DEFAULT_PACKET_SZ;
    int header_size = DEFAULT_HEADER_SZ;
    int num_p = DEFAULT_PARALLEL;
    pid_t *wp;
    pid_t chld;
    char *hostname = DEFAULT_HOST;
    int sync = DEFAULT_SYNC;
    int i = 1;
    struct timeval tv_before, tv_after;
    long micros;
    int bufval = DEFAULT_BUFFER_SIZE;


    while(i < argc) {
	if (*argv[i] == '-') {
	    if (i == argc - 1) {
		usage(argv[0]);
	    }
	    switch (argv[i][1]) {
	    case 'n':
		iterations = atoi(argv[++i]);
		break;
	    case 'h':
		header_size = atoi(argv[++i]);
		break;
	    case 'b':
		packet_size = atoi(argv[++i]);
		break;
	    case 'p':
		port_no = atoi(argv[++i]);
		break;
	    case 'x':
		num_p = atoi(argv[++i]);
		break;
	    case 's':
		sync = strcmp(argv[++i], "true") == 0;
		break;
	    case 't':
	        bufval = atoi(argv[++i]);
	        break;
	    default:
		usage(argv[0]);
	    }
	} else {
	    hostname = argv[i];
	}
	++i;
    }

    CHK(((he = gethostbyname(hostname)) != NULL));


    memset(&sin, 0, sizeof(sin));
    memcpy(&(sin.sin_addr.s_addr), *(he->h_addr_list),
	   sizeof(sin.sin_addr.s_addr));
    sin.sin_family = AF_INET;
    sin.sin_port = htons((unsigned short) port_no);
    wp = malloc(sizeof(pid_t)*num_p);

    gettimeofday(&tv_before, NULL);

    for (i = 0; i < num_p; ++i) {
	if ((chld = fork()) == 0) {
	    CHK(( sock = socket(AF_INET, SOCK_STREAM, 0)) >= 0);
	    CHK(connect(sock, (struct sockaddr *) &sin, sizeof(sin)) == 0);
	    if (bufval != 0) {
	      CHK(setsockopt(sock, SOL_SOCKET, SO_SNDBUF, (char *) &bufval,
			     sizeof(bufval)) == 0);
	      CHK(setsockopt(sock, SOL_SOCKET, SO_RCVBUF, (char *) &bufval,
			     sizeof(bufval)) == 0);
	    }
	    loop(sock,packet_size,header_size,iterations,sync);
	    return 0;
	} else {
	    wp[i] = chld;
	}
    }
    i = 0;
    while (i < num_p) {
	int stat,j;
	chld = wait(&stat);
	for(j = 0; j < num_p; ++j) {
	    if (wp[j] == chld) {
		if (stat != 0) {
		    fprintf(stderr,"Child %d returned %d!\n",
			    (int) chld, stat);
		}
		++i;
		break;
	    }
	}
    }
    gettimeofday(&tv_after, NULL);
    micros = tv_diff(&tv_before, &tv_after);
    send_term("{micros,%ld}", micros);
    return 0;
}
