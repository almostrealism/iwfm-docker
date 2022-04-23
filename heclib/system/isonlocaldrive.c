#include <stdlib.h>
#ifdef _MSC_VER
	#include <windows.h>
	typedef __int32 int32_t;
	
	int32_t windows_isonlocaldrive(char *pathname) {
	
		char abspath[_MAX_PATH];
		char *filePart;
		GetFullPathName(pathname, _MAX_PATH, abspath, &filePart);
		abspath[3] = '\0';
		switch (GetDriveType(abspath)) {
			case DRIVE_FIXED     :
			case DRIVE_REMOVABLE :
			case DRIVE_CDROM     :
			case DRIVE_RAMDISK   : return 1;
				
			default              : return 0;
	
		}
	}
#else
	#include <stdio.h>
	#include <string.h>
	#include <stdint.h>

	int32_t posix_isonlocaldrive(char *pathname) {
		
		char buf[1024];
		char abspath[1024];
		char fstype[32];
		char mountpoint[256];
		char *cp;
		FILE *pipe;

		//---------------------------//
		// get the absolute pathname //
		//---------------------------//
		if (!(realpath(pathname, abspath))) return -1;

		//---------------------------------------------//
		// get the file system type of the mount point //
		// containing the file                         //
		//---------------------------------------------//
		if (!(pipe = popen("mount -v", "r"))) return -1;
		mountpoint[0] = '\0';
		strcpy(fstype, "unk");
		
		//--------------------------------------------------//
		// the output of the "mount -v" command should be   // 
		// some form of:                                    // 
		//     device "on" directory "type" fstype other... // 
		//--------------------------------------------------//
		while (fgets(buf, sizeof(buf), pipe)) {
			cp  = strtok(buf, " \t\n");
			while (cp) {
				if (!strcmp(cp, "on")) {
					cp = strtok(NULL, " \t\n");
					if (!strncmp(cp, abspath, strlen(cp))) {
						if (!mountpoint[0] || strlen(cp) > strlen(mountpoint)) {
							strcpy(mountpoint, cp);
							fstype[0] = '\0';
						}
					}
				}
				else if (!strcmp(cp, "type")) {
					cp = strtok(NULL, " \t\n");
					if (!fstype[0]) strcpy(fstype, cp);
					break;
				}
				else {
					cp = strtok(NULL, " \t\n");
				}
			}
		}
		pclose(pipe);
		
		if (!strncmp(fstype, "nfs",   3)) return 0;  // nfs
		if (!strncmp(fstype, "smb",   3)) return 0;  // samba
		if (!strncmp(fstype, "samba", 5)) return 0;  // samba
							     // 
		return 1;
	}
#endif

void isonlocaldrive_ (char *pathname, int32_t *islocal, int32_t len_pathname) {

	int32_t i;
	char *path = (char *)malloc((len_pathname+1) * sizeof(char));
	strncpy(path, pathname, len_pathname);
	path[len_pathname] = '\0';
	for (i = len_pathname - 1; i >= 0; --i) {
		if (path[i] != ' ') break;
		path[i] = '\0';
	}
#ifdef _MSC_VER
	*islocal = windows_isonlocaldrive(path);
#else
	*islocal = posix_isonlocaldrive(path);
#endif
	free(path);
}

