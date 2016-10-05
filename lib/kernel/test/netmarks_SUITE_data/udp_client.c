#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>
#include <netdb.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>


#ifdef HARDDEBUG
# define HARDTRACE0(FMT) fprintf(stderr, FMT"\n")
# define HARDTRACE1(FMT,A) fprintf(stderr, FMT"\n", A)
# define HARDTRACE2(FMT,A,B) fprintf(stderr, FMT"\n", A, B)
#else
# define HARDTRACE0(FMT)
# define HARDTRACE1(FMT,A)
# define HARDTRACE2(FMT,A,B)
#endif


int be_verbose = 0;

#define CHK(X) check((X),__LINE__)

void check(int x,int line)
{
    if (!x) {
        fprintf(stderr,"udp_client: Error at line %d, errno = %d\n",
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

char *build_packet(unsigned sz,int *packet_bytes)
{
    char *res;
    char *ptr;
    char x = 'a';
    ptr = res = malloc(sz + sizeof(unsigned));
    *packet_bytes = sz+sizeof(unsigned);
    while ((ptr - res) < sizeof(unsigned)) {
	*ptr++ = 0;
    }
    while(sz-- > 0) {
	*ptr++ = x;
	if (++x > 'z')
	    x = 'a';
    }
    return res;
}

void put_serial(char *packet, unsigned serial)
{
    unsigned tmp = htonl(serial);
    memcpy(packet,&tmp, sizeof(unsigned));
}
unsigned get_serial(char *packet)
{
    unsigned tmp;
    memcpy(&tmp, packet, sizeof(unsigned));
    return (unsigned) ntohl(tmp);
}

static unsigned now_us()
{
    struct timeval t;
    gettimeofday(&t,NULL);
    return t.tv_sec*1000*1000 + t.tv_usec;
}

float do_loop(int sock, unsigned sz, int n, int port, struct hostent *he,
	      unsigned max_outstanding)
{
    char *packet;
    unsigned serial = 1;
    unsigned max_serial = (unsigned) n;
    int packet_bytes;
    fd_set fdsr, fdsw;
    int tot_written = 0, tot_read = 0;
    char *throw_away;
    int do_resend = 0;
    struct timeval tmo;
    unsigned start_us, end_us, diff_us;
    struct sockaddr_in target;
    const unsigned receive_timeout_us = 1000*1000;
    int timeout_triggered = 0;
    int send_space = max_outstanding;
    int sret;
    float thrput;

    memset(&target, 0, sizeof(target));
    memcpy(&(target.sin_addr.s_addr), *(he->h_addr_list),
	   sizeof(target.sin_addr.s_addr));
    target.sin_family = AF_INET;
    target.sin_port = htons((unsigned short) port);

    CHK((packet = build_packet(sz,&packet_bytes)) != NULL);
    CHK((throw_away = build_packet(sz,&packet_bytes)) != NULL);

    start_us = now_us();
    for(;;) {
	FD_ZERO(&fdsr);
	FD_ZERO(&fdsw);
	FD_SET(sock,&fdsr);
	if (n > 0 && send_space > 0) {
	    FD_SET(sock,&fdsw);
	}
	tmo.tv_sec = 0;
	tmo.tv_usec = receive_timeout_us;
	sret = select(sock+1,&fdsr,&fdsw,NULL,&tmo);
	if (sret <= 0) {
	    if (sret == 0) { /* timeout */
		timeout_triggered = 1;
		break;
	    }
	    if (errno == EINTR) {
		continue;
	    } else {
		CHK(0);
	    }
	}
	if (FD_ISSET(sock,&fdsr)) {
	    CHK(recvfrom(sock, throw_away, packet_bytes, 0, NULL, NULL) == packet_bytes);
	    HARDTRACE1("received %d\n", (int) get_serial(throw_away));
	    ++send_space;
	    tot_read += packet_bytes;
	}
	if (FD_ISSET(sock,&fdsw)) {
	    put_serial(packet,serial);
	    CHK(sendto(sock,packet, packet_bytes, 0, (struct sockaddr *) &target, sizeof(target)) ==
		packet_bytes);
	    tot_written += packet_bytes;
	    HARDTRACE2("sending #%d of %d\n", (int) serial, max_serial);
	    ++serial;
	    --n;
	    --send_space;
	}
	if (!n && tot_written == tot_read) {
	    break;
	}
    }
    end_us = now_us();
    if (timeout_triggered) {
	end_us -= receive_timeout_us;
    }
    diff_us = end_us - start_us;
    thrput = ((float)tot_read / diff_us)*1000*1000;
    if (be_verbose) {
	fprintf(stderr, "Wrote %d bytes, read %d bytes in %u us with windows size %u: throughput %f bytes/s\r\n",
	       tot_written, tot_read, diff_us, max_outstanding, thrput);
    }
    free(packet);
    free(throw_away);
    return thrput;
}

static unsigned between(unsigned a, unsigned b)
{
    return (a + b) / 2;
}

float loop(unsigned sz, int n, int port, struct hostent *he)
{
    struct point {
	unsigned wsz;  /* send window size */
	float thrput;  /* meassured throughput */
    };
    struct point max;   /* max throughput found */
    struct point left, right;  /* the two points with wsz closest to max.wsz */
    unsigned nr;
    int optval;
    int sock;
    unsigned tries;
    unsigned redo = 0; /* Used to trigger a re-rampup if thrput ends at 0 */

    CHK((sock = socket(AF_INET, SOCK_DGRAM, 0)) >= 0);
    optval = 1000000;
    CHK(setsockopt(sock, SOL_SOCKET, SO_SNDBUF, (char *) &optval,
		   sizeof(optval)) == 0);
    CHK(setsockopt(sock, SOL_SOCKET, SO_RCVBUF, (char *) &optval,
		   sizeof(optval)) == 0);

    /* Ramp up wsz until throughput decreases
     */
 rampup:
    max.thrput = 0;
    max.wsz = 0;
    left.wsz = 0;
    tries = 0;
    for (right.wsz = 4; ; right.wsz *= 2) {
	++tries;
	right.thrput = do_loop(sock, sz, n, port, he, right.wsz);
	if (right.thrput > max.thrput) {
	    left = max;
	    max = right;
	}
	else break;
    }
    if (right.thrput == 0 && left.thrput == 0) {
      if (redo != 0) {
	printf("Ramp-up failed!\r\n");
	fprintf(stderr,"Ramp-up failed!\r\n");
	exit(1);
      }
      redo++;
      sleep(2);
      goto rampup;
    }

    if (be_verbose) {
	fprintf(stderr, "After ramp-up:\r\n");
	fprintf(stderr, "left  = %u, %f\r\n", left.wsz, left.thrput);
	fprintf(stderr, "max   = %u, %f\r\n", max.wsz, max.thrput);
	fprintf(stderr, "right = %u, %f\r\n", right.wsz, right.thrput);
    }

    /* Do "bisect" to find the sweet spot
    */
    nr = 0;
    while (right.wsz - left.wsz > 2) {
	struct point mid;
	++tries;
	if ((max.wsz - left.wsz) > (right.wsz - max.wsz)) {
	    mid.wsz = between(left.wsz, max.wsz);
	    mid.thrput = do_loop(sock,sz,n,port,he, mid.wsz);
	    if (mid.thrput > max.thrput) {
		right = max;
		max = mid;
	    }
	    else {
		left = mid;
	    }
	}
	else {
	    mid.wsz = between(max.wsz, right.wsz);
	    mid.thrput = do_loop(sock,sz,n,port,he, mid.wsz);
	    if (mid.thrput > max.thrput) {
		left = max;
		max = mid;
	    }
	    else {
		right = mid;
	    }
	}
	if (be_verbose) {
	    fprintf(stderr, "After bisect #%u:\r\n", ++nr);
	    fprintf(stderr, "left  = %u, %f\r\n", left.wsz, left.thrput);
	    fprintf(stderr, "max   = %u, %f\r\n", max.wsz, max.thrput);
	    fprintf(stderr, "right = %u, %f\r\n", right.wsz, right.thrput);
	}
    }
    send_term("{tries,%u}", tries);
    send_term("{send_window,%u}", max.wsz);
    return max.thrput;
}

void usage(char *a0)
{
    fprintf(stderr,"Usage: %s [-n <num iterations>]  [-b <packet data bytes>]\n"
	           "        [-p <first port number>] [-x <num parallell>] [-v(erbose)] [<hostname>]\n\n"
	           " - Default: %s -n 1000000 -b 1000 -p 4999 -x 1 localhost\n\n",a0,a0);
    exit(1);
}

int main(int argc, char **argv)
{
    struct sockaddr_in sin;
    struct hostent *he;
    float thrput;

    int port_no = 4999;
    int iterations = 100000; //1000000;
    int packet_size = 1000;
    int num_p = 1;
    pid_t *wp;
    pid_t chld;
    const char *hostname = "localhost";
    int i = 1;

    while(i < argc) {
	if (*argv[i] == '-') {
	    if (i == argc - 1) {
		usage(argv[0]);
	    }
	    switch (argv[i][1]) {
	    case 'n':
		iterations = atoi(argv[++i]);
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
	    case 'v':
		be_verbose = 1;
		break;
	    default:
		usage(argv[0]);
	    }
	} else {
	    hostname = argv[i];
	}
	++i;
    }

    if ((he = gethostbyname(hostname)) == NULL) {
	fprintf(stderr, "udp_client: Failed to resolve hostname %s\n", hostname);
	abort();
    }

    memset(&sin, 0, sizeof(sin));
    sin.sin_addr.s_addr = INADDR_ANY;
    sin.sin_family = AF_INET;
    wp = malloc(sizeof(pid_t)*num_p);
    if (num_p == 1) {
	thrput = loop(packet_size, iterations, port_no, he);
    }
    else {
	fprintf(stderr,"udp_client: Parallel UDP clients not supported\n");
	abort();
	/*
	for (i = 0; i < num_p; ++i) {
	    if ((chld = fork()) == 0) {
		loop(packet_size, iterations, port_no+i, he);
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
	*/
    }
    send_term("{throughput,%.0f}", thrput);
    return 0;
}
