#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <poll.h>
#include <errno.h>
#include <assert.h>
#include <limits.h>

/*
 * Test when a pipe is ready for writing
 *
 * Conclusion: PIPE_BUF bytes must be writable.
 *
 */

static void ABORT(const char* txt)
{
    fprintf(stderr, "ERROR: %s\nABORTING...\n", txt);
    abort();
}

static int write_full(int fd)
{
    int wr = 0;

    while(1) {
        int n = write(fd, "x", 1);

        if (n < 0) {
            if (errno == EAGAIN)
                return wr;
            if (errno == EINTR)
                continue;
            ABORT("write");
        }

        assert(n == 1);
        wr++;
    }
}


static void do_poll(int fd)
{
    struct pollfd pollv[1];
    int pr;

    pollv[0].fd = fd;
    pollv[0].events = POLLOUT;
    pollv[0].revents = 0;
    pr = poll(pollv, 1, 0);

    printf("poll -> %d, revents=%d\n", pr, pollv[0].revents);
}

int main()
{
    int fds[2], pr, n, n2, flags, written;
    char* buf = NULL;

    if (pipe(fds) < 0)
        ABORT("pipe");

    if ((flags = fcntl(fds[0], F_GETFL, 0)) < 0
        || fcntl(fds[0], F_SETFL, flags|O_NONBLOCK) < 0
        || (flags = fcntl(fds[1], F_GETFL, 0)) < 0
        || fcntl(fds[1], F_SETFL, flags|O_NONBLOCK) < 0) {
        ABORT("fcntl");
    }

    printf("PIPE_BUF=%d\n", PIPE_BUF);

    do_poll(fds[1]);

    written = write_full(fds[1]);

    printf("written=%d\n", written);

    do_poll(fds[1]);

    buf = malloc(written);

    n = read(fds[0], buf, 1);
    assert(n == 1);

    printf("did read=%d\n", n);
    written -= n;

    do_poll(fds[1]);

    n = read(fds[0], buf, written/2);
    assert(n == written/2);

    printf("did read=%d\n", n);
    written -= n;

    do_poll(fds[1]);

    n2 = read(fds[0], buf, written - 1);
    assert(n2 == written - 1);

    printf("did read=%d\n", n2);
    written -= n2;

    do_poll(fds[1]);

    n2 = read(fds[0], buf, 1);
    assert(n2 == 1);

    printf("did read=%d\n", n2);
    written -= n2;

    do_poll(fds[1]);

    printf("written=%d\n", written);

    return 0;
}
