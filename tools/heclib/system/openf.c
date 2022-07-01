#ifdef _MSC_VER
	#include <windows.h>
	typedef __int32 int32_t;
#else
	#include <sys/types.h>
	#include <sys/stat.h>
	#include <fcntl.h>
	#include <stdint.h>
#endif

#include <stdlib.h>
#include <string.h>
 
#ifdef _MSC_VER
	HANDLE hFile;
	DWORD accessMode;
	DWORD createMode;
#else
	int32_t accessMode;
#endif
	
/* open a file using "c" i/o, typically from fortran
   set iaccess = 10 (decimal) for most apps.
   (HECLIB manual says 0=read, 1=write, 2=read/write, the 8 is for creation)
   
   istat is returned -1 if error, otherwise 0 */

void openf_(char *cname, int32_t *iaccess, int32_t *ihandle, int32_t *istat, int32_t len_cname) {

	char *_cname = (char *)malloc(len_cname + 1);
	int32_t i;
	strncpy(_cname, cname, len_cname);
	_cname[len_cname] = '\0';
	for (i = len_cname-1; i >= 0; --i) {
		if (_cname[i] != ' ') break;
		_cname[i] = '\0';
	}
#ifdef _MSC_VER
	accessMode = 0;
	createMode = *iaccess & 0x0008  ? OPEN_ALWAYS : OPEN_EXISTING;

	switch (*iaccess & 0x0003) {
		case 0 : 
			accessMode = GENERIC_READ;
			break;
		case 1 :
			accessMode = GENERIC_WRITE;
			break;
		case 2 :
			accessMode = GENERIC_READ | GENERIC_WRITE;
	}

	hFile = CreateFile (_cname, 
			    accessMode,
			    FILE_SHARE_READ | FILE_SHARE_WRITE,
			    NULL,
			    createMode,
			    FILE_ATTRIBUTE_NORMAL,
			    NULL);
      	  
	*ihandle = (int32_t)hFile;
	*istat  = hFile == INVALID_HANDLE_VALUE ? 1 : 0;
#else
	accessMode = 0 ;
	switch (*iaccess & 0x0003) {
		case 0 : 
			accessMode = O_RDONLY;
			break;
		case 1 :
			accessMode = O_WRONLY;
			break;
		case 2 :
			accessMode = O_RDWR;
	}
	if (*iaccess & 0x0008) accessMode |= O_CREAT;
	*ihandle = open(_cname, accessMode, 0666);
	*istat = *ihandle == -1 ? 1 : 0;
#endif
	free(_cname);
}
