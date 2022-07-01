#ifdef _MSC_VER
	/*
	 *ioset() is not supported on Windows.
         */
#else
	
	#include <stdio.h>
	#include <termio.h>
	#include <unistd.h>
	#include <stdint.h>
	 
	/*  Written by Bill Charley, HEC, 1992  */
	 
	static struct termio rdev1;
	 
	void ioset_(int32_t *iunit, int32_t *iwait, int32_t *nchar, int32_t *istat) {
		
		int32_t errflag, set_mode;
		int32_t ier, ieof, ifile;
	 
		*istat = 0;
	 
		/*
		 * Get ioctl set-up
		 */
		if ((errflag = ioctl(*iunit, TCGETA, &rdev1)) == -1) {
			perror ("Device failure (terminal)");
			*istat = errflag;
		}
	 
		rdev1.c_lflag &= ~(ICANON | ECHO);
		rdev1.c_oflag &= ~OPOST;
		rdev1.c_iflag &= ~(INLCR | ICRNL | IUCLC | ISTRIP );
	 
	 
		/*
		nchar = 0     will accept zero or more characters minmum
		nchar > 0     will accept nchar or more characters minimum
	
		iwait = 0     timer is in-active
		iwait > 0     timer waits up iwait*0.1 seconds or until at least
			      nchar characters are available
	
		use:  nchar = 0, iwait = 100,  to wait for characters, one at a time
					       for up to 10 seconds.
					       (like PREAD screen with inactivity)
	
		use:  nchar = 80, iwait = 0,   to wait for characters
					       (like coed would do)
	
		use:  nchar = 0, iwait = 0,    to check for anything in input buffer
					       without waiting for anything
	
		see manpages for termio for the details!
		*/
	 
	 
		/* How many characters to wait for */
		rdev1.c_cc[VMIN] = (char) *nchar;
	 
		/* Set the time to wait in 10ths of a second */
		rdev1.c_cc[VTIME] = (char) *iwait;
	 
		/*
		 * Add new change to device
		 */
		if ((errflag = ioctl(*iunit, TCSETAW, &rdev1)) == -1) {
			perror ("Device failure (terminal)");
			*istat = errflag;
		}
	}
#endif
