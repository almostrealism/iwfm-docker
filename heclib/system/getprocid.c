#if defined (_MSC_VER)
	#include <process.h>
	#define getpid _getpid
	typedef __int32 int32_t;
#else
	#include <unistd.h>
	#include <stdint.h>
#endif

void getprocid_(int32_t *pid) {*pid = getpid();}

void getpid_(int32_t *pid) {*pid = getpid();}

