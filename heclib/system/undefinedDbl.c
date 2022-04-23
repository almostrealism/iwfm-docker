/*
    undefinedDbl -  Returns the value assigned to an undefined
                    double float variable.
                    Check with isUndefinedDbl ().
                    Written by Bill Charley,  HEC, April 1994.
*/

#include <float.h> 
double undefinedDbl() {return (double)FLT_MAX;}
 
 
/*   FORTRAN Interface  */
double undefineddbl_() {return (double)FLT_MAX;}

