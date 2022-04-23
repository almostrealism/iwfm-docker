#include <time.h>
 
#ifdef _MSC_VER
	#define tzset    _tzset
	#define timezone _timezone
	typedef __int32 int32_t;
#else
	#include <stdint.h>
#endif
#define _1900_to_1970_  25568  // days
 
void systim_(int32_t *julian, int32_t *sec) {
	
	time_t seconds;

	tzset();
	time (&seconds);
	seconds -= timezone;
	if ((localtime(&seconds))->tm_isdst) seconds += 3600;

	*julian = (int32_t)seconds / 86400 + _1900_to_1970_;
	*sec = (int32_t)seconds % 86400;
}
