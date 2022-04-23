/*
    undefinedInt -  Returns the value assigned to an undefined
                    int32_t variable.
                    Check with isUndefinedInt ().
                    Written by Bill Charley,  HEC, April 1994.
*/
 
#include <limits.h>
#ifdef _MSC_VER
	typedef __int32 int32_t;
#else
	#include <stdint.h>
#endif

 
int32_t undefinedInt() {return INT_MAX;}
 
 
/*   FORTRAN Interface  */
int32_t undefinedint_() {return INT_MAX;}
