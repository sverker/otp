#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <unistd.h>

#define MAX_VALUES (2*60*60 + 2*10*10)
struct timespec real[MAX_VALUES];
struct timespec mono[MAX_VALUES];

#if 1
#define LEAP_SEC 1435708800

#define START_SEC   (LEAP_SEC-(1*60*60))
#define LEAP_START_SEC (LEAP_SEC-10)
#define LEAP_END_SEC   (LEAP_SEC+10)
#define STOP_SEC   (LEAP_SEC+(1*60*60))
#else
#define LEAP_SEC (1435691175 + 2*60)

#define START_SEC   (LEAP_SEC-(1*60))
#define LEAP_START_SEC (LEAP_SEC-10)
#define LEAP_END_SEC   (LEAP_SEC+10)
#define STOP_SEC   (LEAP_SEC+(1*60))
#endif


#if defined(__APPLE__) || defined(__MACH__)
#include <mach/clock.h>
#include <mach/mach.h>

#define CLOCK_MONOTONIC 0
#define CLOCK_REALTIME 1

void
clock_gettime(int type, struct timespec* ts)
{
    ethr_sint64_t time;
    kern_return_t res;
    clock_serv_t clk_srv;
    mach_timespec_t time_spec;

    errno = EFAULT;
    switch (type) {
    case CLOCK_MONOTONIC:
	host_get_clock_service(mach_host_self(),
			       SYSTEM_CLOCK,
			       &clk_srv);
	break;

    case CLOCK_REALTIME:
	host_get_clock_service(mach_host_self(),
			       CALENDAR_CLOCK,
			       &clk_srv);
	break;
    default:
	abort();
	break;
    }

    res = clock_get_time(clk_srv, &time_spec);
    if (res != KERN_SUCCESS)
	ETHR_FATAL_ERROR__(errno);
    mach_port_deallocate(mach_task_self(), clk_srv);

    ts->tv_sec = time_spec.tv_sec;
    ts->tv_nsec = time_spec.tv_nsec;
}
#endif

int main()
{
    int nvalues;
    int i = 0;

    for (;;) {
	clock_gettime(CLOCK_REALTIME, &real[i]);
	if (real[i].tv_sec >= START_SEC)
	    break;
	usleep(1000*1000);
    }

    for (i = 0; i < MAX_VALUES; i++) {
	clock_gettime(CLOCK_REALTIME, &real[i]);
	clock_gettime(CLOCK_MONOTONIC, &mono[i]);
	if (real[i].tv_sec >= LEAP_START_SEC)
	    break;
	usleep(1000*1000);
    }

    for (i++; i < MAX_VALUES; i++) {
	usleep(100*1000);
	clock_gettime(CLOCK_REALTIME, &real[i]);
	clock_gettime(CLOCK_MONOTONIC, &mono[i]);
	if (real[i].tv_sec >= LEAP_END_SEC)
	    break;
    }

    usleep(100*1000);
    for (i++; i < MAX_VALUES; i++) {
	clock_gettime(CLOCK_REALTIME, &real[i]);
	clock_gettime(CLOCK_MONOTONIC, &mono[i]);
	if (real[i].tv_sec >= STOP_SEC)
	    break;
	usleep(1000*1000);
    }

    nvalues = i + 1;
    for (i = 0; i < nvalues; i++) {
	long long rt = real[i].tv_nsec + ((long long)real[i].tv_sec)*(1000*1000*1000);
	long long mt = mono[i].tv_nsec + ((long long)mono[i].tv_sec)*(1000*1000*1000);
	printf("%lld %lld\n", mt, rt);
    }
    return 0;
}
