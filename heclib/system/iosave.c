#ifdef _MSC_VER
	/*
	 * iosave() is not supported in Windows
         */
#else
	#include <termio.h>
	#include <unistd.h>
	#include <stdio.h>
	#include <stdint.h>
	 
	static struct termio sdev1;
	static int32_t sdev_set = 0;
	
	void iosave_(int32_t *iunit, char *cset, int32_t *istat, int32_t l_cset) {
		
		int32_t errflag;
	
		 *istat = 0;
		 if (*cset == 'Y') {
			if ((errflag = ioctl(*iunit, TCGETA, &sdev1)) == -1) {
				*istat = errflag;
			}
			else {
				sdev_set = 1;
			}
		}
		else  {
			if (sdev_set) {
				*istat = ioctl (*iunit, TCSETAW, &sdev1);
			}
			else {
				*istat = -1;
				perror("I/O Attributes have not been saved.");
			}
		}
	}
#endif

