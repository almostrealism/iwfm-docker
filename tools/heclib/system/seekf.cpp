#ifdef _MSC_VER
	#include <windows.h>
	typedef __int32 int32_t;
	typedef __int64 int64_t;
#else
	#include <unistd.h>
	#include <stdint.h>
#endif
#include <limits.h>

// Currently:
// 
//  Unix       Windows        Value
//  --------   ------------   ------
//  SEEK_SET = FILE_BEGIN   = 0
//  SEEK_CUR = FILE_CURRENT = 1
//  SEEK_END = FILE_END     = 2
// 
// This probably won't change since it would break
// LOTS of code, so we will count on it here.
// 
extern "C" {
	
#if (defined(__SUNPRO_CC) && defined(__linux))
	#if (!(defined(__x86_64) || defined(__amd64)))
		//
		// Sun Studio 12 C++ on 32-bit Linux doesn't support large file operations
		// 
		int64_t lseek64(int32_t ihandl, int64_t iofset, int32_t iorigin) {

			if (iofset > 2147483647 or iofset < -2147483647) return -1;
			return lseek(ihandl, static_cast<int32_t>(iofset), iorigin);
		}
	#else
		//
		// Sun Studio 12 C++ on 64-bit Linux doesn't use special 64-bit names
		// 
		#define lseek64 lseek
	#endif
#endif

void seekf64_(int32_t *ihandl, int32_t *iorigin, int64_t *iofset, int64_t *ipos, int32_t *istat) {

	#ifdef _MSC_VER	
	LARGE_INTEGER ofset, pos;
	ofset.QuadPart = (int64_t)*iofset;
	pos.QuadPart = 0;
	*istat = SetFilePointerEx(
		reinterpret_cast<HANDLE>(*ihandl), 
		ofset, 
		&pos, 
		*iorigin) == 0 ? -1 : 0;
	*ipos = pos.QuadPart;
	if (*istat) *ipos = -1;
	#else
	*ipos = lseek64(*ihandl, *iofset, *iorigin);
	*istat = *ipos == -1 ? -1 : 0;
	#endif
}

void seekf_(int32_t *ihandl, int32_t *iorigin, int32_t *iofset, int32_t *ipos, int32_t *istat) {

	// It's possible to successfully seek to a position in a large
	// file that can't be reported back through the ipos variable.
	// In this case, we leave istat = 0, but set the ipos variable
	// to -1.

	int64_t iofset64 = *iofset;
	int64_t ipos64;

	seekf64_(ihandl, iorigin, &iofset64, &ipos64, istat);
	if (!*istat) *ipos = (ipos64 > LONG_MAX) ? -1 : ipos64;
}

}
