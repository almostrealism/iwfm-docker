#include <sys/types.h>
#include <sys/stat.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <time.h>

#ifdef _MSC_VER
	#define stat _stat
#elif defined(__GNUC__)
	#define stat stat64
#endif

#ifdef _MSC_VER
	typedef __int32 int32_t;
#else
	#include <stdint.h>
#endif

extern "C" {

//
// This source file has to be C++ to allow stat of large files on Unix
// 	
	
void fildat_(char *cname, char *cfdate, char *cftime, int32_t *istat, int32_t l_cname, int32_t l_cfdate, int32_t l_cftime) {

	char *filename = NULL;
	int32_t i;
	struct stat statbuf;
	
	*istat = -1;
	memset(cfdate, '*', l_cfdate);
	memset(cftime, '*', l_cftime);

	if (l_cfdate < 8 || l_cftime < 8) return;

	filename = (char *)malloc((l_cname+1) * sizeof(char));
	strncpy(filename, cname, l_cname);
	filename[l_cname] = '\0';
	for (i = l_cname - 1; i >= 0; --i) {
		if (filename[i] != ' ') break;
		filename[i] = '\0';
	}

	if (stat(filename, &statbuf) == 0) {

		char chbuf[9];
		struct tm timebuf;

		memcpy(&timebuf, localtime(&statbuf.st_mtime), sizeof(timebuf));
		
		sprintf(chbuf, "%2.2d-%2.2d-%2.2d", timebuf.tm_mon + 1, timebuf.tm_mday, timebuf.tm_year % 100);
		strncpy(cfdate, chbuf, 8);
		memset(cfdate + 8, ' ', l_cfdate - 8);

		sprintf(chbuf, "%2.2d:%2.2d:%2.2d", timebuf.tm_hour, timebuf.tm_min, timebuf.tm_sec);
		strncpy(cftime, chbuf, 8);
		memset(cftime + 8, ' ', l_cftime - 8);

		*istat = 0;
	}

	free(filename);
	
}

}
