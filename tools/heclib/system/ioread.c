#include <stdio.h>

#ifdef _MSC_VER
	#include <io.h>
	#define read _read
	typedef __int32 int32_t;
#else
	#include <unistd.h>
	#include <stdint.h>
#endif

void ioread_ (int32_t *iport, char *carray, int32_t *maxary, int32_t *narray) {
    *narray = read(*iport, carray, *maxary);
    if (*narray < 0) {
        perror ("Read Error:");
    }
}
