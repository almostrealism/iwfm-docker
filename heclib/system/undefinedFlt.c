/*
    undefinedFlt -  Returns the value assigned to an undefined
                    float variable.
                    Check with isUndefinedFlt ().
                    Written by Bill Charley,  HEC, April 1994.
*/
 
#include <float.h>					
float undefinedFlt() {return FLT_MAX;}
 
/*   FORTRAN Interface  */
float undefinedflt_() {return FLT_MAX;}

