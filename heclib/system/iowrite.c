#include <stdio.h>

#ifdef _MSC_VER
	#include <io.h>
	#define write _write
	typedef __int32 int32_t;
#else
	#include <unistd.h>
	#include <stdint.h>
#endif

void iowrite_(int32_t *iport, char *carray, int32_t *narray, int32_t *ierr, int32_t lcarray) {
	
    *ierr = write (*iport, carray, *narray);
    if (*ierr < 0) perror ("Write Error:");
    return;
}
