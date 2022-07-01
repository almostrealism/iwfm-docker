#ifdef _MSC_VER
	/* getppid() is not supported on Windows platforms */
#else
	#include <unistd.h>
	#include <stdint.h>
	void getppid_(int32_t *ppid) {*ppid = getppid();}
#endif 
