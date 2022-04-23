#ifdef _MSC_VER
	#include <windows.h>
	typedef __int32 int32_t;
#else
	#include <unistd.h>
	#include <errno.h>
	#include <stdint.h>
#endif
 
void readf_(int32_t *ihandl, void *buff, int32_t *nbytes, int32_t *istat, int32_t *ntrans) {

#ifdef _MSC_VER	
	ReadFile((HANDLE)*ihandl, 
		 buff,
		 (DWORD)*nbytes,
		 (LPDWORD)ntrans,
		 NULL);

	*istat  = *ntrans >= 0 ? 0 : GetLastError();
#else
	*ntrans = read(*ihandl, buff, *nbytes);
	*istat  = *ntrans >= 0 ? 0 : errno;
#endif	
	
}
