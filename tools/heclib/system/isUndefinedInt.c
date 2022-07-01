/*
    isUndefinedInt -  Returns 1 (true) if the value passed in is undefined,
                      that has been set by function undefinedInt ().
                      Written by Bill Charley,  HEC, April 1994.
*/
 
#ifdef _MSC_VER
	typedef __int32 int32_t;
#else
	#include <stdint.h>
#endif

int32_t undefinedInt(void);
 
int32_t isUndefinedInt(int32_t value) {return value == undefinedInt() ? 1 : 0;}
 
/*   FORTRAN Interface  */
int32_t isundefinedint_(int32_t *value) {return *value == undefinedInt() ? 1 : 0;}

