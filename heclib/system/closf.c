#ifdef _MSC_VER
#include <windows.h>
typedef __int32 int32_t;
void closf_ (int32_t *ihandle, int32_t *istat)
{
	*istat = (CloseHandle((HANDLE)*ihandle) != -1 ? 0 : -1);
}
#else
#include <unistd.h>
#include <stdint.h>
void closf_ (int32_t *ihandle, int32_t *istat)
{
	*istat = (close(*ihandle) != -1 ? 0 : -1);
}
#endif
