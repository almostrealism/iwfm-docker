#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <stdio.h>
#ifdef _MSC_VER
	#include <io.h>
#else
	#include <dirent.h>
	#include <sys/stat.h>
	#include <fnmatch.h>
	#include <stdint.h>
#endif

#if !defined(TRUE)
	#define FALSE 0
	#define TRUE  !FALSE
#endif

#if !defined(min)
	#define min(a,b) (a < b ? a : b)
#endif
extern "C" {
	
#ifdef _MSC_VER
	typedef __int32 int32_t;
	typedef __int64 int64_t;
	static intptr_t h_find = -1L;
#else
	static DIR *dir = NULL;
#endif	
static int32_t attr = 0;
static char dirname[260];

void filesize64_(char *filename, int64_t *nbytes, int32_t *status, int32_t filename_len);
void fildat_(char *cname, char *cfdate, char *cftime, int32_t *istat, int32_t l_cname, int32_t l_cfdate, int32_t l_cftime);

void filen64_(
	char      *cmask,    // Mask to match             
	int32_t   *ifatt,    // Attributes to match       
	char      *cmode,    // 'F' = first, 'N' = next   
	char      *cfname,   // Name of findbuf file      
	int64_t   *ifsize,   // Size of findbuf file
	char      *cfdate,   // Date of findbuf file      
	char      *cftime,   // Time of findbuf file      
	int32_t   *iatt,     // Attributes of findbuf file
	int32_t   *istat,    // 0 = match, 1 = no match   
	int32_t    l_cmask, 
	int32_t    l_cmode, 
	int32_t    l_cfname, 
	int32_t    l_cfdate, 
	int32_t    l_cftime) {

#ifdef _MSC_VER
	struct _finddata_t findbuf;
#else
	struct dirent *dirbuf;
	struct stat statbuf;
#endif	
	char    pathname[260];
	char    fdate[8];
	int32_t new_search = 0;
	int32_t i;
	int32_t namelen;
	int32_t matched = 0;

	switch (*cmode) {
		case 'N' : case 'n' : new_search = 0; break;
		case 'F' : case 'f' : new_search = 1; break;
		default  : *istat = 1; return;
	}

	if (new_search) {
		char *mask = new char[l_cmask+1];
		strncpy(mask, cmask, l_cmask);
		mask[l_cmask] = '\0';
		for (i = l_cmask - 1; i >= 0; --i) {
			if (mask[i] != ' ') break;
			mask[i] = '\0';
		}
		attr = *ifatt;
#ifdef _MSC_VER
		char drive[3];
		char dname[260];
		char fname[260];
		char ext[260];
		_splitpath(mask, drive, dname, fname, ext);
		_makepath(dirname, drive, dname, "", "");
		if (h_find != -1L) _findclose(h_find);
		h_find = _findfirst(mask, &findbuf);
		matched = h_find != -1L;
		while (matched && ((findbuf.attrib & _A_SUBDIR) || (findbuf.attrib & attr) != attr)) {
			if (_findnext(h_find, &findbuf) != 0) matched = FALSE;
		}
#else
		strcpy(dirname, mask);
		char *cp = strrchr(dirname, '/');
		if (cp) {
			*cp = '\0';
		}
		else {
			strcpy(dirname, ".");
			strcpy(&dirname[2], mask);
		}
		if (dir != NULL) closedir(dir);
		dir = opendir(dirname);
		matched = dir != NULL;
		while ((dirbuf = readdir(dir)) != NULL) {
			if (fnmatch(&dirname[strlen(dirname)+1], dirbuf->d_name, 0)) continue;
			sprintf(pathname, "%s/%s", dirname, dirbuf->d_name);
			stat(pathname, &statbuf);
			if (statbuf.st_mode & S_IFDIR) continue;
			if ((statbuf.st_mode & attr) != attr) continue;
			break;
		}
		matched = dirbuf != NULL;
#endif
		delete[] mask;
	}
	else {
#ifdef _MSC_VER
		matched = h_find != -1L; // from previous search
		if (matched) {
			do {
				if (_findnext(h_find, &findbuf) != 0) matched = FALSE;
			} while (matched && ((findbuf.attrib & _A_SUBDIR) || (findbuf.attrib & attr) != attr));
		}
#else
		matched = dir != NULL; // from previous search
		while ((dirbuf = readdir(dir)) != NULL) {
			if (fnmatch(&dirname[strlen(dirname)+1], dirbuf->d_name, 0)) continue;
			sprintf(pathname, "%s/%s", dirname, dirbuf->d_name);
			stat(pathname, &statbuf);
			if (statbuf.st_mode & S_IFDIR) continue;
			if ((statbuf.st_mode & attr) != attr) continue;
			break;
		}
		matched = dirbuf != NULL;
#endif		
	}
	if (matched) {
#ifdef _MSC_VER
		sprintf(pathname, "%s%s", dirname, findbuf.name);
		namelen = strlen(findbuf.name);
		strncpy(cfname, findbuf.name, min(namelen, l_cfname));
		*iatt = findbuf.attrib;
#else
		// pathname already set
		namelen = strlen(dirbuf->d_name);
		strncpy(cfname, dirbuf->d_name, min(namelen, l_cfname));
		*iatt = statbuf.st_mode;
#endif		
		if (namelen < l_cfname) memset(cfname + namelen, ' ', l_cfname - namelen);
		memset(pathname + strlen(pathname), ' ', sizeof(pathname) - strlen(pathname));
		filesize64_(pathname, ifsize, &i, sizeof(pathname));
		fildat_(pathname, fdate, cftime, &i, sizeof(pathname), sizeof(fdate), l_cftime);
		if (l_cfdate < 6) {
			memset(cfdate, '*', l_cfdate);
		}
		else {
			strncpy(&cfdate[0], &fdate[6], 2);
			strncpy(&cfdate[2], &fdate[0], 2);
			strncpy(&cfdate[4], &fdate[3], 2);
		}
		*istat = 0;
	}
	else {
#ifdef _MSC_VER
		_findclose(h_find);
		h_find = -1L;
#else
		closedir(dir);
		dir = NULL;
#endif		
		*istat = 1;
	}
}

void filen_(
	char     *cmask,    // Mask to match             
	int32_t  *ifatt,    // Attributes to match       
	char     *cmode,    // 'F' = first, 'N' = next   
	char     *cfname,   // Name of findbuf file      
	int32_t  *ifsize,   // Size of findbuf file      
	char     *cfdate,   // Date of findbuf file      
	char     *cftime,   // Time of findbuf file      
	int32_t  *iatt,     // Attributes of findbuf file
	int32_t  *istat,    // 0 = match, 1 = no match   
	int32_t   l_cmask, 
	int32_t   l_cmode, 
	int32_t   l_cfname, 
	int32_t   l_cfdate, 
	int32_t   l_cftime) {

	int64_t fsize;
	filen64_(cmask,ifatt,cmode,cfname,&fsize,cfdate,cftime,iatt,istat,l_cmask,l_cmode,l_cfname,l_cfdate,l_cftime);
	if (*istat == 0) *ifsize = fsize < LONG_MAX ? fsize : -1;
}

}

