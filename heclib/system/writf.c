#ifdef _MSC_VER
	#include <windows.h>
	typedef __int32 int32_t;
#else
	#include <unistd.h>
	#include <stdint.h>
#endif
 
void writf_(int32_t *ihandle, void *buff, int32_t *nbytes, int32_t *istat, int32_t *ntrans)
{
#ifdef _MSC_VER
	WriteFile((HANDLE)*ihandle, buff, *nbytes, ntrans, NULL);
#else	
	*ntrans = write(*ihandle, buff, *nbytes);
#endif	
	*istat  = *ntrans >= 0 ? 0 : -1;

}
