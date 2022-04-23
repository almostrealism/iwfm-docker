#include <stdio.h>
void heclibBuildInfo() {
	#include "../buildInfo.h"
	printf("%s\n", (char*)buildInfo); // so compilers won't optimize this away"
}
