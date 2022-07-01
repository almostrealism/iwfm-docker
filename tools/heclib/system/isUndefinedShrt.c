/*
   isUndefinedShrt -  Returns 1 (true) if the value passed in is undefined,
                      that has been set by function undefinedShrt ().
                      Written by Bill Charley,  HEC, April 1994.
*/
#ifdef _MSC_VER
	typedef __int32 int32_t;
#else
	#include <stdint.h>
#endif
 
short undefinedShrt(void);
 
int32_t isUndefinedShrt(short value) {return value == undefinedShrt() ? 1 : 0;}
 
 
/*   FORTRAN Interface  */
int32_t isundefinedshrt_(short *value) {return *value == undefinedShrt() ? 1 : 0;}

