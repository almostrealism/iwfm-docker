#include <stdio.h>
#include <stdlib.h>
#ifdef _MSC_VER
	typedef __int32 int32_t;
#else
	#include <stdint.h>
#endif


void
writemem_(int32_t *mbuff, int32_t *wordpos, int32_t *intbuff, int32_t *nwords)
{
	int32_t  end;
	int32_t  i, k;
	int32_t *membuff;

	membuff = (int32_t *)*mbuff;
	end = wordpos[0] + nwords[0];
	k = 0;
	for (i=wordpos[0]; i<end; i++) {
		membuff[i] = intbuff[k++];
	}
}

