/*
 * This file is required because the system function in the Intel and
 * Digital fortran libraries is hardwired to "SYSTEM" and cannot be 
 * called with the /names:lowercase and /assume:underscore compiler flags
 *
 */
#include <stdlib.h>

int system_(char *cmd) {return system(cmd);}
