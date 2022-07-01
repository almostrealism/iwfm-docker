/*
    isUndefinedFlt -  Returns 1 (true) if the value passed in is undefined,
                      that has been set by function undefinedFlt ().
                      Written by Bill Charley,  HEC, April 1994.
*/
#ifdef _MSC_VER
	typedef __int32 int32_t;
#else
	#include <stdint.h>
#endif
 
float undefinedFlt(void);
 
int32_t isUndefinedFlt(float value) {return value == undefinedFlt() ? 1 : 0;}
 
/*   FORTRAN Interface   */
int32_t isundefinedflt_(float *value) {return *value == undefinedFlt() ? 1 : 0;}

