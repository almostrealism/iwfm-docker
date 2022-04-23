/*
    undefinedShrt -  Returns the value assigned to an undefined
                     short integer variable.
                     Check with isUndefinedShrt ().
                     Written by Bill Charley,  HEC, April 1994.
*/
 
#include <limits.h>
 
short undefinedShrt() {return SHRT_MAX;}
 
 
/*   FORTRAN Interface  */
short undefinedshrt_() {return SHRT_MAX;}
