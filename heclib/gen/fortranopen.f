      INTEGER FUNCTION fortranopen (IUNIT, FILENAME)
C
C     Interface routine for C/C++ functions to open a file
C     under FORTRAN
C
      INTEGER IUNIT
      CHARACTER FILENAME*(*)
      INTEGER ISTAT
C
      OPEN (UNIT=IUNIT, FILE=FILENAME, IOSTAT=ISTAT)                    u
C     OPEN (UNIT=IUNIT, FILE=FILENAME, SHARE="DENYNONE", IOSTAT=ISTAT)  M
      fortranopen = ISTAT
C
      RETURN
      END


