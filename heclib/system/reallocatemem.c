#include <stdio.h>
#include <malloc.h>
#include <stdlib.h>
#ifdef _MSC_VER
	typedef __int32 int32_t;
#else
	#include <stdint.h>
#endif


void
reallocatemem_ (int32_t **buff, int32_t *nbytes)
{
	void *b;
	b = (int32_t *)realloc (*buff, *nbytes);
	*buff = b;
fprintf (stderr, "\nEnter reallocatememc, address = %Xh, nbytes = %d\n\n", b, *nbytes); 
}
