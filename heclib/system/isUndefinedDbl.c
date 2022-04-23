/*
    isUndefinedDbl -  Returns 1 (true) if the value passed in is undefined,
                      that has been set by function undefinedDbl ().
                      Written by Bill Charley,  HEC, April 1994.
*/
#ifdef _MSC_VER
	typedef __int32 int32_t;
#else
	#include <stdint.h>
#endif

double undefinedDbl(void); 

int32_t isUndefinedDbl(double value) {return value == undefinedDbl() ? 1 : 0;}
 
 
/*   FORTRAN Interface  */
int32_t isundefineddbl_(double *value) {return *value == undefinedDbl() ? 1 : 0;}

