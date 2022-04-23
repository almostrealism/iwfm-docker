#ifdef _MSC_VER
	#include <io.h>
	typedef __int32 int32_t;
#else
	#include <fcntl.h>
	#include <stdint.h>
#endif
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>

void chmodf_(char *path, int32_t *mode, int32_t *istat, int32_t l_path) {

	char *filename = NULL;
	int32_t i;

	filename = (char *)malloc(l_path+1);
	strncpy(filename, path, l_path);
	filename[l_path] = '\0';
	for (i = l_path - 1; i >= 0; --i) {
		if (filename[i] != ' ') break;
		filename[i] = '\0';
	}
#ifdef _MSC_VER
	i = 0;
	if (*mode & 0555) i |= _S_IREAD;
	if (*mode & 0444) i |= _S_IWRITE;
	*istat = chmod(filename, i);
#else
	*istat = chmod(filename, *mode);
#endif
	free(filename);
}
