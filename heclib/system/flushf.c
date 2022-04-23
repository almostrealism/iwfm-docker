#ifdef _MSC_VER
	#include <windows.h>
	typedef __int32 int32_t;
#else
	#include <unistd.h>
	#include <stdint.h>
#endif
 
void flushf_(int32_t *ihandle, int32_t *istat) {
#ifdef _MSC_VER
	*istat = FlushFileBuffers((HANDLE)*ihandle) ? 0 : -1;
#else
	*istat = fsync(*ihandle);
#endif
}
