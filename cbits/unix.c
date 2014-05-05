/*
 * lowlevel binder for macosx and 
 */
#include <time.h>
#ifdef __MACH__
#include <mach/clock.h>
#include <mach/mach.h>
#endif

/* on mac os X, clock_gettime doesn't exists, but
 * http://stackoverflow.com/questions/5167269/clock-gettime-alternative-in-mac-os-x
 *
 * we ignore errors as it's very very unlikely considering the hardcoded ID
 * and the fact that haskell should call this code.
 */
void hourglass_clock_calendar(struct timespec *timespec)
{
#ifdef __MACH__
	clock_serv_t cclock;
	mach_timespec_t mts;

	host_get_clock_service(mach_host_self(), CALENDAR_CLOCK, &cclock);
	clock_get_time(cclock, &mts);
	mach_port_deallocate(mach_task_self(), cclock);

	timespec->tv_sec = mts.tv_sec;
	timespec->tv_nsec = mts.tv_nsec;
#else
	clock_gettime(CLOCK_REALTIME, timespec);
#endif
}
