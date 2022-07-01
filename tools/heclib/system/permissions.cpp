#include <sys/types.h>
#include <sys/stat.h>
#include <string.h>
#include <stdlib.h>

#ifdef _MSC_VER
	#define stat _stat
	typedef __int32 int32_t;
#else
	#include <stdint.h>
#endif

extern "C" {

//
// This source file has to be C++ to allow stat64 of large files on Unix
// 	
	
void permissions_(char *path, int32_t *mode, int32_t *istat, int32_t l_path) {
	
	struct stat buff;
	char *pathname = new char[l_path+1];
	strncpy(pathname, path, l_path);
	pathname[l_path] = '\0';
	for (int32_t i = l_path - 1; i >= 0; --i) {
		if (pathname[i] != ' ') break;
		pathname[i] = '\0';
	}
	if (!(*istat = stat (pathname, &buff))) *mode = buff.st_mode & 0777;
	delete[] pathname;
}

}

