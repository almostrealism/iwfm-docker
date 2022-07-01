! Last change: CFB 03 Aug 2009 
! Prev change: SS  06 Apr 2005    
!
! This file contains modules and subroutines from John Doherty. ALL subroutines
! were written by John. The new code is the program IWFM2OBS itself, which reads
! IWFM output (groundwater, stream, and tile drain hydrographs) and converts it
! to SMP format. John's utility SMP2SMP is called to time-interpolate model
! simulated values to times of observed values. Also added is functionality
! to write PEST instruction files. 
! CFBrush May 2007 - created subroutine readMain
!                  - extracted from IWFM2OBS program to a subroutine
! CFBrush Aug 2009 - increase obs name size to 20 characters
!

program iwfm2obs_2015

! -- Program iwfm2obs_2015 converts iwfm hydrograph output data to pest's well sample file
!   format.

    use defn
    use inter
    use ifport  ! for getcwd() for debugging
    implicit none

    integer(4)         :: istat = 0
    character*80          workdir /' '/
 
    integer, parameter :: MAXHYD   = 60000
    integer, parameter :: MAXTIME  =  1500
    integer, parameter :: iitimeopt =     8
    
    real               :: rtime(MAXTIME)
    real               :: rval(INUMHYD,MAXHYD,MAXTIME)
    character*25       :: hydid(INUMHYD,MAXHYD)                              !CFB
    character*1000     :: adate

    integer i,j,k,ifail,ls(6),rs(6),iline,ntime,iyr,imon,iday,ihydtype,ntime2
    integer startyr,startmon,startday,endyr,endmon,endday,iitime, ipos
    integer countfile,iidate,iheader
    integer iiwfmfile(inumhyd),iobsfile(inumhyd)
    integer ioutfile(inumhyd), itempfile(inumhyd)
    integer iinsfile(inumhyd),ipcffile(inumhyd)
    integer bprocess(inumhyd),bwriteins(inumhyd)
    integer nout(inumhyd)

    integer iinfile, iprntfile
    real gwfact,sbfact
    real rthresh(inumhyd)
    character*120 junk
    character*500 filenames(inumhyd)            ! names of files with print information
    character*500 iwfmfile(inumhyd)             ! names of files with output hydrographs
    character*500 obsfile(inumhyd)              ! names of files with observed values (smp)
    character*500 outfile(inumhyd)              ! names of output files with simulated valuse (smp)
    character*500 insfile(inumhyd)              ! names of instruction files
    character*500 pcffile(inumhyd)              ! names of pcs files
    character*500 infile /' '/
    character*500 prntfile
    character*500 hdiffile                                             !MJT
    character*1 tunit
    character*5 anum
    logical  :: headdiffs                                             !MJT
    character(len=1) :: YN                                            !MJT

    integer :: nsi
    
    integer filenumber(inumhyd)

    character*80 filetype(inumhyd)
    data filetype  /'subsidence','tiledrain','stream','groundwater'/   !MJT groundwater is 3, CFB subsidence is 4

    character*1 cvalidtim(iitimeopt)
    data cvalidtim /'d','D','w','W','m','M','y','Y'/

    character*80 namenout(inumhyd)
    data namenout  /'NOUTS','NOUTTD','NOUTR','NOUTH'/

    integer colid(inumhyd)                     ! column of print format holding observation ID
    data colid     /4,3,2,6/

    character*80 tempfile(inumhyd)             ! names of temp files for raw simulated values
    data tempfile  /'sb_temp.smp','td_temp.smp','st_temp.smp','gw_temp.smp'/

    integer mondays(12)
    data mondays   /31,28,31,30,31,30,31,31,30,31,30,31/

    !introduce unit numbers for all files. 
    iinfile      = 30 
    iprntfile    = 40
    data iiwfmfile / 11,12,13,14 /
    data iobsfile  / 15,16,17,18 /
    data ioutfile  / 20,21,22,23 /
    data itempfile / 24,25,26,27 /
    data iinsfile  / 28,29,30,31 /
    data ipcffile  / 33,34,35,36 /
    data filenumber  /51,52,53,54/
    rval=0.
    
    ! write program name -----------------------------------------------
    write(amessage,115)
115 format(' Program IWFM2OBS_2015 interpolates model output to match ',  &
           'the times and locations of calibration observations and put ', &
           'them into a PEST-compatible output file.')
    call write_message(leadspace='yes',endspace='yes')

     ! read settings.fig ------------------------------------------------
    call read_settings(ifail,iidate,iheader)
    if(ifail.eq.1) then
      write(amessage,117)
117   format(' A settings file (settings.fig) was not found in the ', &
      'current directory.')
      call write_message
      go to 9900
    else if(ifail.eq.2) then
      write(amessage,118)
118   format(' Error encountered while reading settings file settings.fig')
      call write_message
      go to 9900
    endif
    if((iidate.ne.0).or.(datespec.eq.0)) then
      write(amessage,119)
119   format(' Cannot read date format from settings file ', &
      'settings.fig')
      call write_message
      go to 9900
    end if

    !--- get name of IWFM main input file ----------------------------------
120 write(6,130)
130 format(' Enter name of IWFM main input file: ',$)
    open(5,file='iwfm2obs_2015.in')
    read(5,'(a)', err=120) infile
    filenames = ''
    iwfmfile  = ''
    
    !====================================================================
    ! Get file names and time-step info from the main IWFM Simulation input file
    open(unit=iinfile,file=infile,status='old',err=120)

    do i = 1,4    ! skip to the line with the name of the groundwater file
      call readOneLine(infile,iinfile,cline)
    end do

    ! read to the name of the groundwater component main file: heads, subsidence, tile drains
    call readOneLine(infile,iinfile,cline)
    ! extract the file name
    prntfile = adjustl(trim(cline))
    IPOS=SCAN(prntfile,'/')
    IF (IPOS.GT.0) prntfile=prntfile(1:IPOS-1)
    IPOS=SCAN(cline,'/')
    call tabrem(prntfile)
    filenames(GWHEAD)=ADJUSTL(TRIM(prntfile))
    
    ! read to the name of the stream component main file
    call readOneLine(infile,iinfile,cline)
    ! extract the file name
    prntfile = adjustl(trim(cline))
    IPOS=SCAN(prntfile,'/')
    IF (IPOS.GT.0) prntfile=prntfile(1:IPOS-1)
    IPOS=SCAN(cline,'/')
    call tabrem(prntfile)
    filenames(STREAM)=ADJUSTL(TRIM(prntfile))

    ! skip to the starting date
    do i = 1,8
      call readOneLine(infile,iinfile,cline)
    end do
    
    ! read the starting date
    call readOneLine(infile,iinfile,cline)
    ! extract the date from the string
    call tabrem(cline)
    IPOS=SCAN(cline,'_')
    adate=adjustl(cline(1:IPOS-1))
    call char2date(ifail,adate,startday,startmon,startyr)

    ! skip to the time unit in DSS format
    call readOneLine(infile,iinfile,cline)

    ! read the time unit in DSS format
    call readOneLine(infile,iinfile,cline)
    ! DSS can have a range of time units but only three are valid for PEST
    ! convert time unit to 'd', 'm', 'w' or 'y'
    call tabrem(cline)
    IPOS=SCAN(cline,'/')
    adate = adjustl(trim(cline(1:IPOS-1)))

    call casetrans(adate,'hi')
    if( adjustl(trim(adate)) .eq. "1DAY" ) then 
        tunit = 'd'
    elseif( adjustl(trim(adate)) .eq. "1WEEK" ) then 
        tunit = 'w'
    elseif( adjustl(trim(adate)) .eq. "1MON" ) then 
        tunit = 'm'
    elseif( adjustl(trim(adate)) .eq. "1YEAR" ) then 
        tunit = 'y'
    else  !--- date units not supported by PEST ---
        write (6,140) adate
140     format(' ',a,' is not a valid time unit for PEST.')
        write (6,'(A)') 'Please re-run the model with time units 1DAY, 1WEEK, 1MON or 1YEAR'
        ifail = 1
        stop
    endif

    ! read the ending date
    call readOneLine(infile,iinfile,cline)
    ! extract the date from the string
    call tabrem(cline)
    IPOS=SCAN(cline,'_')
    adate=adjustl(cline(1:IPOS-1))
    call char2date(ifail,adate,endday,endmon,endyr)

    close(unit=iinfile)

    !=============================================================
    ! go into the groundwater main file and get names of subsidence and tile drain files
    open(unit=filenumber(GWHEAD),file=filenames(GWHEAD),status='old',err=9300)
    
    ! read and skip the first non-comment line
    call readOneLine(filenames(GWHEAD),filenumber(GWHEAD),cline)

    ! Tile drain file name
    call readOneLine(filenames(GWHEAD),filenumber(GWHEAD),cline)
    ! extract the file name
    prntfile = adjustl(trim(cline))
    IPOS=SCAN(prntfile,'/')
    IF (IPOS.GT.0) prntfile=prntfile(1:IPOS-1)
    IPOS=SCAN(cline,'/')
    call tabrem(prntfile)
    filenames(TILEDR)=ADJUSTL(TRIM(prntfile))
    
    ! Pumping file name - skip
    call readOneLine(filenames(GWHEAD),filenumber(GWHEAD),cline)

    ! Subsidence file name
    call readOneLine(filenames(GWHEAD),filenumber(GWHEAD),cline)
    ! extract the file name
    prntfile = adjustl(trim(cline))
    IPOS=SCAN(prntfile,'/')
    IF (IPOS.GT.0) prntfile=prntfile(1:IPOS-1)
    IPOS=SCAN(cline,'/')
    call tabrem(prntfile)
    filenames(SUBSID)=ADJUSTL(TRIM(prntfile))

    ! skip 16 non-comment lines and read line with NOUTH
    do i=1,17
      call readOneLine(filenames(GWHEAD),filenumber(GWHEAD),cline)
    end do

    ! get NOUTH from cline
    call multisplit(ifail,1,ls,rs,cline)
    if(ifail.ne.0) go to 9600
    call intread(ifail,cline(ls(1):rs(1)),nout(GWHEAD))
    if(ifail.ne.0) go to 9600

    ! read FACTXY
    call readOneLine(filenames(GWHEAD),filenumber(GWHEAD),cline)

    ! read name of groundwater hydrograph file iwfmfile(GWHEAD=4)
    call readOneLine(filenames(GWHEAD),filenumber(GWHEAD),cline)
    ! extract the file name
    prntfile = adjustl(trim(cline))
    IPOS=SCAN(prntfile,'/')
    IF (IPOS.GT.0) prntfile=prntfile(1:IPOS-1)
    IPOS=SCAN(cline,'/')
    call tabrem(prntfile)
    iwfmfile(GWHEAD)=ADJUSTL(TRIM(prntfile))

   ! move to the start of the hydrograph descriptions
    call skip(filenumber(GWHEAD))
    
    !------------------------------------------------------------
    ! open tile drain main file
    
    if(len(ADJUSTL(TRIM(filenames(TILEDR)))).gt.0) then
    open(unit=filenumber(TILEDR),file=filenames(TILEDR),status='old',err=9300)
    
    ! skip to NTD
    call readOneLine(filenames(TILEDR),filenumber(TILEDR),cline)
    ! get NTD from cline
    call multisplit(ifail,1,ls,rs,cline)
    if(ifail.ne.0) go to 9600
    call intread(ifail,cline(ls(1):rs(1)),nout(TILEDR))
    if(ifail.ne.0) go to 9600

    ! skip 3+NTD lines and comment blocks and get the next line
    do i = 1,nout(TILEDR) + 4
       call readOneLine(filenames(TILEDR),filenumber(TILEDR),cline)
    end do
       
    ! get NSI from cline
    call multisplit(ifail,1,ls,rs,cline)
    if(ifail.ne.0) go to 9600
    call intread(ifail,cline(ls(1):rs(1)),nsi)
    if(ifail.ne.0) go to 9600
    
    ! skip 6+NSI lines  and get the next line
    do i = 1,nsi + 7
       call readOneLine(filenames(TILEDR),filenumber(TILEDR),cline)
    end do
       
    ! read name of tile drain hydrograph file iwfmfile(TILEDR=2)
    ! extract the file name
    prntfile = adjustl(trim(cline))
    IPOS=SCAN(prntfile,'/')
    IF (IPOS.GT.0) prntfile=prntfile(1:IPOS-1)
    IPOS=SCAN(cline,'/')
    call tabrem(prntfile)
    iwfmfile(TILEDR)=ADJUSTL(TRIM(prntfile))

   ! move to the start of the hydrograph descriptions
    call skip(filenumber(TILEDR))

    end if
    !------------------------------------------------------------
    ! open subsidence main file
    
     if(len(ADJUSTL(TRIM(filenames(SUBSID)))).gt.0) then
      open(unit=filenumber(SUBSID),file=filenames(SUBSID),status='old',err=9300)
    
      ! skip to NOUTS
      do i=1,6
         call readOneLine(filenames(SUBSID),filenumber(SUBSID),cline)
      end do
    
      ! get NOUTS from cline
      call multisplit(ifail,1,ls,rs,cline)
      if(ifail.ne.0) go to 9600
      call intread(ifail,cline(ls(1):rs(1)),nout(SUBSID))
      if(ifail.ne.0) go to 9600

      ! read FACTXY
      call readOneLine(filenames(SUBSID),filenumber(SUBSID),cline)

      ! skip 1 line
      call readOneLine(filenames(SUBSID),filenumber(SUBSID),cline)
       
      ! read name of subsidence hydrograph file iwfmfile(SUBSID)
      ! extract the file name
      prntfile = adjustl(trim(cline))
      IPOS=SCAN(prntfile,'/')
      IF (IPOS.GT.0) prntfile=prntfile(1:IPOS-1)
      IPOS=SCAN(cline,'/')
      call tabrem(prntfile)
      iwfmfile(SUBSID)=ADJUSTL(TRIM(prntfile))

      ! move to the start of the hydrograph descriptions
      call skip(filenumber(SUBSID))
    end if
    !=============================================================
    ! go into the stream file and get number of hydrographs and hydrograph file name
    open(unit=filenumber(STREAM),file=filenames(STREAM),status='old',err=9300)
    
    ! read and skip to NOUTR
    do i = 1,7
       call readOneLine(filenames(STREAM),filenumber(STREAM),cline)
    end do
       
    ! get NOURT from cline
    call multisplit(ifail,1,ls,rs,cline)
    if(ifail.ne.0) go to 9600
    call intread(ifail,cline(ls(1):rs(1)),nout(STREAM))
    if(ifail.ne.0) go to 9600

    ! skip 5 lines & read output file name
    do i = 1,6
       call readOneLine(filenames(STREAM),filenumber(STREAM),cline)
    end do
       
    ! extract the file name
    prntfile = adjustl(trim(cline))
    IPOS=SCAN(prntfile,'/')
    IF (IPOS.GT.0) prntfile=prntfile(1:IPOS-1)
    IPOS=SCAN(cline,'/')
    call tabrem(prntfile)
    iwfmfile(STREAM)=ADJUSTL(TRIM(prntfile))

    ! move to the start of the hydrograph descriptions
    call skip(filenumber(STREAM))

    !==================================================================
    
    !--- count the number of hydrographs
    countfile = 0
    do i=1,inumhyd
        if (trim(iwfmfile(i)).eq.'') then
            bprocess(i) = 0 
        else
            bprocess(i) = 1
            countfile = countfile+1
        end if
    end do

    ! Exit if there is nothing to do
    if (countfile.eq.0) go to 9500

    ! Inform user how many output files exist. 
    write(6,230) countfile
230 format(//,' ',i1,' hydrograph file(s) are output by IWFM')
    write(6,*)

    !==================================================================
    
    !--- Prompt user for names of input files. 
    do i=1,inumhyd
        bwriteins(i) = 0
        rthresh(i)   = 0
        insfile(i)   = "error.txt"

        
        if (bprocess(i).eq.1) then
235         write(6,240) trim(filetype(i))
240         format(' Enter name for ',a,' observation smp file (ENTER for none): ',$)
            read(5,'(a)') obsfile(i)
            write(6,*)
            if (trim(obsfile(i)).eq.'') then
                bprocess(i) = 0
                cycle
            else
!                here we open then close the file - just to check to make sure it exists
!                it is later opened by SMP2SMP
                open(unit=iobsfile(i),file=obsfile(i),status='old',err=235)
                close(unit = iobsfile(i))
245             write(6,250,advance='no') trim(filetype(i))
250             format(' Enter (',a,') extrapolation threshold in days ',                  &
                '(fractional if necessary): ')
                read(5,'(a)') anum
                if(anum.eq.' ') go to 245
                anum=adjustl(anum)
                call char2num(ifail,anum,rthresh(i))
                if(ifail.ne.0) go to 245
                if(rthresh(i).lt.0.0) go to 245
                  !  Check if groundwater head differences will be calculated
                  if (i.eq.GWHEAD) then          !MJT - groundwater head differences
                    write(6,*)
251                 write(6,252,advance='no')
252                 format(' Will head differences be calculated? ',$)
                    read(5,'(a)') yn
                    if(yn.eq.'Y'.or.yn.eq.'y')then
                      headdiffs=.true.
                      write(6,*)
                      write(6,*)
253                   write(6,254,advance='no')
254                   format('   Enter name of file listing well pairs: ')
                      read(5,'(a)') hdiffile
                      if(hdiffile.eq.' ') go to 253
                    end if
                  end if
            end if
            write(6,*)
! -- now prompt for instruction file
260         write(6,265) trim(filetype(i))
265         format(' OPTIONAL Enter name for ',a,' PEST instruction file (ENTER for none): ',$)
            read(5,'(a)') insfile(i)
            if (trim(insfile(i)).eq.'') then
                bwriteins(i) = 0
            else
                bwriteins(i) = 1
                open(unit=iinsfile(i),file=insfile(i))
                pcffile(i) = "pcf_"//insfile(i)
                open(unit=ipcffile(i),file=pcffile(i))
            end if
            write(6,*)
            write(6,270) trim(filetype(i))
270            format(' Enter name for ',a,' output: ',$)
            read(5,'(a)') outfile(i)
            open(unit=ioutfile(i),file=outfile(i))
            write(6,*)
        end if
    end do
    write(6,*) ''
    
    !===============================================================================
    ! loop through and read hydrograph IDs
    do ihydtype = 4,1,-1
        do i=1,nout(ihydtype)
            call readOneLine(filenames(ihydtype),filenumber(ihydtype),cline)
            if(ihydtype.eq.SUBSID .or. ihydtype.eq.GWHEAD) then ! need to determine colid from SUBTYP or HYDTYP
                call multisplit(ifail,2,ls,rs,cline)
                if(ifail.lt.0) cycle
                if(ifail.gt.0) go to 9700
                call intread(ifail,cline(ls(2):rs(2)),j)
                if(ifail.gt.0) go to 9600
                if (j.eq.1) then
                    colid(ihydtype) = 5
                else
                    colid(ihydtype) = 6
                end if
            end if
            call multisplit(ifail,colid(ihydtype),ls,rs,cline)
            if(ifail.lt.0) cycle
            if(ifail.gt.0) go to 9700
            HYDID(ihydtype,i)=adjustl(trim(cline(ls(colid(ihydtype)):rs(colid(ihydtype)))))
            if (hydid(ihydtype,i)(1:1).eq.'/') then
                  hydid(ihydtype,i)(1:1) = ' '
            end if
         end do
         continue
         write(6,275) nout(ihydtype),trim(filetype(ihydtype)),trim(filenames(ihydtype))
275      format(' - names of ',i5,' ',a,' hydrograph ids read from file ',a)

         close(filenumber(ihydtype))
    end do


! -------------------------------------------------------------------------
! this is the start of the loop that converts IWFM output into a SMP format
! note that this entire section should be replaced if smp2smp is stitched in
! directly rather than called as a subroutine
    
    do ihydtype = 1,inumhyd
        if (bprocess(ihydtype).eq.1) then
            open(unit=iiwfmfile(ihydtype),file=iwfmfile(ihydtype),status='old',err=9320)        
            call skip(iiwfmfile(ihydtype))
            ntime=0
            do
                ntime=ntime+1
                if(ntime.gt.MAXTIME)then
                    write(6,610) trim(iwfmfile(ihydtype))
610                 format(/,' *** Too many times in file ',a,' ***',/,&
                    ' *** Increase MAXTIME and re-compile program ***')
                    stop
                end if
! -------reading times - need to have error checks here too. 
!                read(iiwfmfile(ihydtype),*,end=800) rtime(ntime),junk,&
!                  (rval(ihydtype,j,ntime),j=1,nout(ihydtype))
                rtime(ntime) = ntime
                if(ihydtype.eq.SUBSID) then          ! subsidence
                    read(iiwfmfile(ihydtype),'(A22,60000F12.2)',end=800) junk,&
                      (rval(ihydtype,j,ntime),j=1,nout(ihydtype))
                else if(ihydtype.eq.TILEDR) then     ! tile drain
                    read(iiwfmfile(ihydtype),'(A22,60000F12.2)',end=800) junk,&
                      (rval(ihydtype,j,ntime),j=1,nout(ihydtype))
                else if(ihydtype.eq.STREAM) then     ! stream 
                    ntime2=ntime+1
                    read(iiwfmfile(ihydtype),'(A22,60000F14.2)',end=800) junk,&
                      (rval(ihydtype,j,ntime2),j=1,nout(ihydtype))
                else  !(ihydtype.eq.GWHEAD)          ! groundwater
                    read(iiwfmfile(ihydtype),'(A22,60000F12.4)',end=800) junk,&
                      (rval(ihydtype,j,ntime),j=1,nout(ihydtype))
                end if
            end do
800         ntime=ntime-1
            close(unit=iiwfmfile(ihydtype))
            write(6,*) ''
            write(6,810) ntime,trim(iwfmfile(ihydtype))
810         format(' - data for ',i6,' times read from file ',a)


! -- The the output file is written

            open(unit=itempfile(ihydtype),file=tempfile(ihydtype))
            do j=1,nout(ihydtype)
                do k=1,ntime
                    IF ((tunit.eq.'d').or.(tunit.eq.'D')) then
                        iitime = rtime(k)
                        call newdate(iitime,startday,startmon,startyr,iday,imon,iyr)
                    end if
                    IF ((tunit.eq.'w').or.(tunit.eq.'W')) then
                        iitime = rtime(k)*7
                        call newdate(iitime,startday,startmon,startyr,iday,imon,iyr)
                    end if
                    if ((tunit.eq.'m').or.(tunit.eq.'M')) then
                        iyr=startyr + rtime(k)/12
                        imon=startmon + mod(rtime(k),12.0) -1
                        if(imon.gt.12)then
                            imon=imon-12
                            iyr=iyr+1
                        end if
                        if (leap(iyr) .and. imon .eq. 2) then
                           iday = 29
                        else
                           iday = mondays(imon)
                        endif
                    end if
                    if ((tunit.eq.'y').or.(tunit.eq.'Y')) then
                        iyr = startyr + rtime(k)
                    end if
                    if (datespec.eq.1) then 
                        write(itempfile(ihydtype),850) trim(hydid(ihydtype,j)),&
                            iday,imon,iyr,rval(ihydtype,j,k)
                    else
                        write(itempfile(ihydtype),850) trim(hydid(ihydtype,j)),&
                            imon,iday,iyr,rval(ihydtype,j,k)
                    end if
850                    format(1x,a,10x,i2.2,'/',i2.2,'/',i4.4,'   00:00:00 ',1pg14.7)
                end do
            end do
            close(unit=itempfile(ihydtype))

!************************
! should use a variable for threshold, based on model units. fixed now
!************************

! SMP2SMP, written by John Doherty, is called here.
! note that it would be slightly more efficient, and arguably cleaner, 
! to do the time interpolation in iwfm2obs (it would elimnate
! the inefficiency of writing a *.smp file only to be re-read by subroutine. 
          call smp2smpD(obsfile(ihydtype),iobsfile(ihydtype),tempfile(ihydtype), &
                        itempfile(ihydtype),outfile(ihydtype),ioutfile(ihydtype), &
                        rthresh(ihydtype),bwriteins(ihydtype),insfile(ihydtype), &
                        iinsfile(ihydtype),pcffile(ihydtype),ipcffile(ihydtype), &
                        headdiffs,hdiffile,ihydtype)

!            call smp2smp(obsfile(ihydtype),iobsfile(ihydtype),&
!                 tempfile(ihydtype),itempfile(ihydtype),&
!                 outfile(ihydtype),ioutfile(ihydtype),&
!                 rthresh(ihydtype),bwriteins(ihydtype),&
!                 insfile(ihydtype),iinsfile(ihydtype),&
!                 pcffile(ihydtype),ipcffile(ihydtype))
!            close(unit=iobsfile(ihydtype))
!            close(unit=itempfile(ihydtype))
!            close(unit=ioutfile(ihydtype))
!            close(unit=iinsfile(ihydtype))
!            close(unit=ipcffile(ihydtype))
        end if

   end do

!
! erase the tempfiles??
!
    go to 9999


9000  write(6,9010) trim(prntfile)
9010  format(/,' *** Unexpected end to file ',a,' ***',/)
      stop
9100  write(6,9110) trim(prntfile)
9110  format(/,' *** Less than NOUTH wells in file ',a,' ***',/)
      stop
!----UPDATE THIS ERROR STATEMENT FOR END OF FILE FOR EACH OUTPUT
9150  write(6,9160) trim(infile)
9160  format(/,' *** IWFM version was not determined for main file ',a,' ***',/)
      stop
!----UPDATE THIS ERROR STATEMENT FOR END OF FILE FOR EACH OUTPUT
9200  write(6,9210) 
9210  format(/,' *** Unexpected end encountered to file  ***',/)
      stop
9250  write (6,9260) trim(infile)
9260  format(/' *** Error reading print file from ',a)
      stop
9300  write(6,9310) trim(prntfile)
      stop
9310  format(/,' *** Print control file: ',a,' not found ***',/)
      stop
9320  write(6,9330) trim(filetype(ihydtype)), trim(iwfmfile(ihydtype))
9330  format(/,' *** ',a,' output: ',a,' not found ***',/)
      stop
9400  write(6,9410) trim(infile)
9410  format(/,' *** Unexpected end to file ',a,' ***',/)
      stop
9500  write(6,9510) trim(prntfile)
9510  format(/,' *** No appropriate output files specified in file ',a,' ***',/)
      stop
9600  write(6,9610) trim(namenout(ihydtype)), trim(prntfile)
9610  format(/,' *** Cannot read ',a,' from file ',a,' ***',/)
      stop
9620  write(6,9630) trim(namenout(ihydtype)), trim(prntfile)
9630  format(/,' *** ',a,' cannot be less than or equal to zero ',&
         'in file ',a,' ***',/)
      stop
9640  write(6,9650) trim(filetype(ihydtype)), trim(prntfile)
9650      format(/,' *** Too many ',a,' hydrographs - increase MAXHYD and ',&
            're-compile program ***',/)
      stop
9700  write(6,9710) trim(filetype(ihydtype)), trim(prntfile)
9710  format(/,' *** Error reading ',a' hydrograph id from file ',a,&
                ' ***',/)
      write(6,9720) trim(cline)
9720  format(/,' Line follows:-',/,' ',a)
      stop



9900  go to 9999

9999 write(6,'(a)') 'NORMAL TERMINATION - IWFM2OBS_2015'
     end program iwfm2obs_2015
  

!================================================================================

subroutine open_output_file(ifail,aprompt,outfile,outunit)

! -- Subroutine open_output_file opens a file for data output.

! -- Subroutine arguments are as follows:-
!       ifail:    returned as non-zero in case of failure
!       aprompt:  user prompt for filename
!       outfile:  name of output file
!       outunit:  unit number of output file

! -- Revision history:-
!       June-November, 1995: version 1.

    use defn
    use inter

    integer, intent(out)            :: ifail
    character (len=*)               :: aprompt,outfile
    integer, intent(out)            :: outunit
    integer                         :: ierr,nbb,ifail1
    logical                         :: lexist,lopened
    character (len=1)               :: aa
        character (len=200)             :: atempf

! -- The user is prompted for the name of the output file.

    imessage=0
    ifail=0
10      write(6,'(a)',advance='no') aprompt(1:len_trim(aprompt)+1)
    read(5,'(a)')outfile
    if(outfile.eq.' ') go to 10
    outfile=adjustl(outfile)
    if(index(eschar,outfile(1:2)).ne.0) then
      escset=1
      return
    end if
        nbb=len_trim(outfile)
        call getfile(ifail1,outfile,atempf,1,nbb)
        if(ifail1.ne.0) go to 10
        outfile=atempf
    inquire(file=outfile,opened=lopened)
    if(lopened)then
      write(amessage,30) trim(outfile)
30        format(' File ',a,' is already open  - try again.')
      call write_message(increment=1)
      go to 10
    end if
    inquire(file=outfile,exist=lexist)
!       if(lexist) then
!40        write(6,50,advance='no')
!50        format(' File already exists: overwrite it?  [y/n] ')
!         read(5,'(a)') aa
!         call casetrans(aa,'lo')
!         if((aa.eq.'e').or.(aa.eq.'n'))then
!           write(6,*)
!           go to 10
!         end if
!         if(aa.ne.'y') go to 40
!       end if

! -- The file is opened.

    outunit=nextunit()
    open(unit=outunit,file=outfile,status='new',iostat=ierr)
    if(ierr.ne.0) then
     open(unit=outunit,file=outfile,status='replace',iostat=ierr)
    end if
    if(ierr.ne.0) then
      if(imessage.gt.5) then
        ifail=1
        return
      end if
      write(amessage,100) trim(outfile)
100       format(' Unable to open file ',a,' - try again.')
      call write_message(increment=1)
      go to 10
    end if

    return

end subroutine open_output_file

! ***********************************************************************
! ***** SUBROUTINE TO SKIP COMMENT LINES IN AN INPUT FILE
! ***********************************************************************
subroutine SKIP(IFILE)
! -- Subroutine SKIP moves past lines that should be ignored

! -- Subroutine arguments are as follows:-
!       ifail:    number of open file to read

!    Revision history:-
!       June-November, 1995: version 1.
!       April 2015: Added '#' 

          IMPLICIT NONE
          !Transferred variables
          INTEGER::IFILE

          !Local variables
          CHARACTER(LEN=1) JUNK
          INTEGER::ERR

          JUNK='C'
          DO 
            READ (IFILE,'(A)',IOSTAT=ERR) JUNK
            IF (ERR.NE.0) RETURN
            IF (.NOT.(JUNK.EQ.'*'.OR.JUNK.EQ.'C'.OR.JUNK.EQ.'c'.OR.JUNK.EQ.'#')) EXIT
          END DO
          BACKSPACE(IFILE)
end subroutine SKIP


subroutine linesplit(ifail,num)

! -- subroutine linesplit splits a line into whitespace-delimited words

! -- Arguments are as follows:-
!       ifail:   returned as -1 if line is blank
!                returned as  1 if less than num segments
!       num:     number of words to be extracted

!    Revision history:-
!       June-November, 1995: version 1.

    use defn
    use inter

    integer, intent(out)            :: ifail
    integer, intent(in)             :: num
    integer                         :: nblc,j,i,nw
    character (len=3)               :: aspace

    ifail=0; nw=0; j=0
    aspace=' ,'//achar(9)
    if(num.gt.NUM_WORD_DIM) call sub_error('LINESPLIT')
    nblc=len_trim(cline)
    if(nblc.eq.0) then
      ifail=-1
      return
    end if

5       if(nw.eq.num) return
    do i=j+1,nblc
      if(index(aspace,cline(i:i)).eq.0) go to 20
    end do
    ifail=1
    return
20      nw=nw+1
    left_word(nw)=i
    do i=left_word(nw)+1,nblc
      if(index(aspace,cline(i:i)).ne.0) go to 40
    end do
    right_word(nw)=nblc
    if(nw.lt.num) ifail=1
    return
40      right_word(nw)=i-1
    j=right_word(nw)
    go to 5

end subroutine linesplit

SUBROUTINE multisplit(IFAIL,NUM,LW,RW,CLINE)
 
! -- Subroutine multisplit splits a string into blank-delimited fragments.

! -- Subroutine arguments are as follows:-
!       ifail:    returned as non-zero in case of failure
!       num:      number of ...
!       lw:       number of ...
!       rw:       number of ...
!       cline:    character string

! -- Author:-
!       John Doherty
 
       INTEGER IFAIL,NW,NBLC,J,I
       INTEGER NUM,NBLNK
       INTEGER LW(NUM),RW(NUM)
       CHARACTER*(*) CLINE
       IFAIL=0
       NW=0
       NBLC=LEN_TRIM(CLINE)
       IF((NBLC.NE.0).AND.(INDEX(CLINE,CHAR(9)).NE.0)) THEN
         CALL TABREM(CLINE)
         NBLC=LEN_TRIM(CLINE)
       ENDIF
       IF(NBLC.EQ.0) THEN
         IFAIL=-1
         RETURN
       END IF
       J=0
5      IF(NW.EQ.NUM) RETURN
       DO 10 I=J+1,NBLC
         IF((CLINE(I:I).NE.' ').AND.(CLINE(I:I).NE.',').AND.&
         (ICHAR(CLINE(I:I)).NE.9)) GO TO 20
10     CONTINUE
       IFAIL=1
       RETURN
20     NW=NW+1
       LW(NW)=I
       DO 30 I=LW(NW)+1,NBLC
         IF((CLINE(I:I).EQ.' ').OR.(CLINE(I:I).EQ.',').OR.&
         (ICHAR(CLINE(I:I)).EQ.9)) GO TO 40
30     CONTINUE
       RW(NW)=NBLC
       IF(NW.LT.NUM) IFAIL=1
       RETURN
40     RW(NW)=I-1
       J=RW(NW)
       GO TO 5
 
END subroutine multisplit

subroutine TABREM(CLINE)
 
! -- Subroutine TABREM removes tabs from a string.

! -- Subroutine arguments are as follows:-
!       cline:    character string


       INTEGER I
       CHARACTER*(*) CLINE
 
       DO 10 I=1,LEN(CLINE)
10     IF(ICHAR(CLINE(I:I)).EQ.9) CLINE(I:I)=' '
 
       RETURN
end subroutine tabrem

subroutine intread(IFAIL,CLINE,iTEMP)
      integer, intent(out)            ::IFAIL
      character (len=*), intent(in)   ::cline
      integer, intent(out)            ::iTEMP
 
! -- Subroutine intREAD reads an integer number from a string.


! -- Subroutine arguments are as follows:-
!       ifail:    returned as non-zero in case of failure
!       cline:    character string
!       itemp:    return integer

       CHARACTER*6 AFMT
 
       IFAIL=0
       AFMT='(i   )'
       WRITE(AFMT(3:5),'(I3)') LEN(CLINE)
       READ(CLINE,AFMT,ERR=100) iTEMP

       RETURN
 
100    IFAIL=1
       RETURN
end subroutine intread

subroutine open_input_file(ifail,aprompt,infile,inunit,file_format,form_prompt,fformat)

! -- Subroutine open_input_file opens an input file.

! -- Arguments are as follows:-
!       ifail:        returned as non-zero if failure
!       aprompt:      user prompt for filename
!       infile:       name of input file
!       inunit:       unit number to communicate with file
!       file_format:  supplied as either "formatted" or "unformatted" to open
!                     formatted or unformatted file
!       form_prompt:  supplied as either "yes" or "no" to prompt for file format
!       fformat:      returns file format to main program if form_prompt is supplied

! -- Revision history:-
!       June-November, 1995: version 1.

    use defn
    use inter

    integer, intent(out)                       :: ifail
    character (len=*), intent(in)              :: aprompt
    character (len=*), intent(out)             :: infile
    integer, intent(out)                       :: inunit
    character (len=*), intent(in), optional    :: file_format
        character (len=*), intent(in), optional    :: form_prompt
        character (len=*), intent(out),optional    :: fformat
    integer                                    :: ierr,nbb,ifail1
    character (len=1)                          :: aformat,aa
    character (len=10)                         :: aaformat
        character (len=120)                        :: bfile
    logical                                    :: lopened

! -- The user is prompted for the name of the file to open.

    imessage=0
    ifail=0
10      write(6,'(a)',advance='no') aprompt(1:len_trim(aprompt)+1)
    read(5,'(a)') infile
    if(infile.eq.' ') go to 10
    infile=adjustl(infile)
    if(index(eschar,infile(1:2)).ne.0) then
      escset=1
      return
    end if
        nbb=len_trim(infile)
        call getfile(ifail1,infile,bfile,1,nbb)
        if(ifail1.ne.0) go to 10
        infile=bfile


! -- Is the file already open?

    inquire(file=infile,opened=lopened)
    if(lopened)then
      write(amessage,30) trim(infile)
30        format(' File ',a,' is already open  - try again.')
      call write_message(increment=1)
      go to 10
    end if

! -- The file is opened (after the format is established).

    aformat='f'
        if(present(form_prompt))then
          if(form_prompt.eq.'yes')then
            if(.not.(present(fformat))) call sub_error('OPEN_INPUT_FILE')
31          write(6,32,advance='no')
32          format(' Is this a formatted or unformatted file? [f/u]: ')
            read(5,'(a)') aa
            call casetrans(aa,'lo')
            if(aa.eq.'e') go to 10
            if(aa.eq.'f')then
              aformat='f'
              fformat='f'
            else if(aa.eq.'u')then
              aformat='u'
              fformat='u'
            else
              go to 31
            end if
          end if
        else if(present(file_format)) then
      if(file_format.eq.'formatted') then
        aformat='f'
      else if(file_format.eq.'unformatted')then
        aformat='u'
      else
        call sub_error('OPEN_INPUT_FILE')
      end if
    end if
    inunit=nextunit()
    if(aformat.eq.'f')then
      open(unit=inunit,file=infile,status='old',iostat=ierr)
    else
      open(unit=inunit,file=infile,status='old',form='binary',  &
          iostat=ierr)
      if(ierr.ne.0)then
              open(unit=inunit,file=infile,status='old',            &
                  form='unformatted',iostat=ierr)
      end if
    end if

! -- If the file could not be opened a test is made as to whether the file
!    is formatted or unformatted when the opposite was expected.

    if(ierr.ne.0)then
      inquire(file=infile,unformatted=aaformat,iostat=ierr)
      if(ierr.ne.0)go to 40
      call casetrans(aaformat,'lo')
      if(aaformat.eq.' ') go to 40
      if(aformat.eq.'f')then
        if(aaformat.eq.'yes') then
          if(imessage.gt.5) then
        ifail=1
        return
          end if
          write(amessage,35) trim(infile)
35            format(' File ',a,' is an unformatted file  - try again.')
          call write_message(increment=1)
          go to 10
        else
          go to 40
        end if
      else
        if(aaformat.eq.'no') then
          if(imessage.gt.5) then
        ifail=1
        return
          end if
          write(amessage,36) trim(infile)
36            format(' File ',a,' is a formatted file  - try again.')
          call write_message(increment=1)
          go to 10
        else
          go to 40
        end if
      end if
40        if(imessage.gt.5) then
        ifail=1
        return
      end if
      write(amessage,50) trim(infile)
50        format(' Cannot open file ',a,'  - try again.')
      call write_message(increment=1)
      go to 10
    end if

    return

end subroutine open_input_file

subroutine write_message(increment,iunit,error,leadspace,endspace)

! -- Subroutine write_message formats and writes a message.

! -- Arguments are as follows:-
!       increment:  the increment to the message counter
!       iunit:      the unit number to which the message is written
!       error:      if "yes" precede message with "Error"
!       leadspace   if "yes" precede message with blank line
!       endspace    if "yes" follow message by blank line

! -- Revision history:-
!       June-November, 1995: version 1.

    use defn
    use inter

    integer, intent(in), optional           ::increment,iunit
    integer                                 ::jend,i,nblc,junit,leadblank
    integer                                 ::itake,j
    character (len=*), intent(in), optional ::error,leadspace,endspace
    character (len=25) ablank

    ablank=' '
    itake=0
    j=0
    if(present(increment)) imessage=imessage+increment
    if(present(iunit))then
      junit=iunit
    else
      junit=6
    end if
    if(present(leadspace))then
      if(leadspace.eq.'yes') write(junit,*)
    endif
    if(present(error))then
      if(index(error,'yes').ne.0)then
        nblc=len_trim(amessage)
        amessage=adjustr(amessage(1:nblc+8))
        if(nblc+8.lt.len(amessage)) amessage(nblc+9:)=' '
        amessage(1:8)=' Error: '
      end if
    end if

    do i=1,20
      if(amessage(i:i).ne.' ')exit
20      end do
    leadblank=i-1
    nblc=len_trim(amessage)
5       jend=j+78-itake
    if(jend.ge.nblc) go to 100
    do i=jend,j+1,-1
    if(amessage(i:i).eq.' ') then
      if(itake.eq.0) then
         write(junit,'(a)',err=200) amessage(j+1:i)
         itake=2+leadblank
      else
         write(junit,'(a)',err=200) ablank(1:leadblank+2)//amessage(j+1:i)
      end if
      j=i
      go to 5
    end if
    end do
    if(itake.eq.0)then
      write(junit,'(a)',err=200) amessage(j+1:jend)
      itake=2+leadblank
    else
      write(junit,'(a)',err=200) ablank(1:leadblank+2)//amessage(j+1:jend)
    end if
    j=jend
    go to 5
100     jend=nblc
    if(itake.eq.0)then
    write(junit,'(a)',err=200) amessage(j+1:jend)
      else
    write(junit,'(a)',err=200) ablank(1:leadblank+2)//amessage(j+1:jend)
      end if
    if(present(endspace))then
      if(endspace.eq.'yes') write(junit,*)
    end if
    return

200     call exit(100)

end subroutine write_message

subroutine casetrans(string,hi_or_lo)

! -- Subroutine casetrans converts a string to upper or lower case.

! -- Arguments are as follows:-
!      string:      contains the string whose case must be changed
!      hi_or_lo:  must be either 'lo' or 'hi' to indicate
!                 change of case direction.

! -- Revision history:-
!       June-November, 1995: version 1.

    use inter

    character (len=*), intent(inout)        :: string
    character (len=*), intent(in)           :: hi_or_lo
    character                               :: alo, ahi
    integer                                 :: inc,i

    if(hi_or_lo.eq.'lo') then
      alo='A'; ahi='Z'; inc=iachar('a')-iachar('A')
    else if(hi_or_lo.eq.'hi') then
      alo='a'; ahi='z'; inc=iachar('A')-iachar('a')
    else
      call sub_error('CASETRANS')
    endif

    do i=1,len_trim(string)
      if((string(i:i).ge.alo).and.(string(i:i).le.ahi)) &
      string(i:i)=achar(iachar(string(i:i))+inc)
    end do

    return

end subroutine casetrans

subroutine sub_error(subname)

! -- Subroutine sub_error names the subroutine causing a run-time error.

! -- Arguments are as follows:-
!       subname:  name of offending subroutine

! -- Revision history:-
!       June-November, 1995: version 1.

    character (len=*)               ::subname

    write(6,10) trim(subname)
10      format(/,' *** PROGRAMMING ERROR CALLING SUBROUTINE ',a,' ***')
    stop

end subroutine sub_error

subroutine getfile(ifail,cline,filename,ibeg,iend)

! Subroutine getfile extracts a filename from a string.

! -- Arguments are as follows:-
!       ifail: returned as zero if filename successfully read
!       cline: a character string containing the file name
!       filename: the name of the file read from the string
!       ibeg: character position at which to begin search for filename
!       iend: on input  - character position at which to end search for filename
!             on output - character postion at which filename ends


        integer, intent(out)               :: ifail
        integer, intent(in)                :: ibeg
        integer, intent(inout)             :: iend
        character (len=*), intent(in)      :: cline
        character (len=*), intent(out)     :: filename

        integer                            :: i,j,k
        character (len=1)                  :: aa

        ifail=0
        do i=ibeg,iend
          aa=cline(i:i)
          if((aa.ne.' ').and.(aa.ne.',').and.(aa.ne.char(9)))go to 50
        end do
        ifail=1
        return

50      if((aa.eq.'"').or.(aa.eq.''''))then
          do j=i+1,iend
            if(cline(j:j).eq.aa) go to 60
          end do
          ifail=1
          return
60        iend=j
          if(i+1.gt.j-1)then
            ifail=1
            return
          else
            filename=cline(i+1:j-1)
          end if
        else
          do j=i+1,iend
            if((cline(j:j).eq.' ').or.(cline(j:j).eq.',').or.(cline(j:j).eq.char(9)))then
              k=j-1
              go to 100
            end if
          end do
          k=iend
100       filename=cline(i:k)
          if(cline(k:k).eq.'"')then
            ifail=1
            return
          else if(cline(k:k).eq.'''')then
            ifail=1
            return
          end if

          iend=k
        end if
        filename=adjustl(filename)
        return

end subroutine getfile

!*****************************************************************************
! subroutines comprising the generic subroutine NUM2CHAR ------->
!*****************************************************************************

! -- Subroutine num2char writes the character equivalent of a number.

! -- Arguments are as follows:-
!       value:   the number to be expressed in character form
!       string:  the number expressed in character form
!       nchar:   the maximum number of characters in which to express number

! -- Revision history:-
!       June-November, 1995: version 1.

subroutine i2a(value,string,nchar)
! -- Subroutine i2a writes the character equivalent of an integer.

! -- Arguments are as follows:-
!       value:   the number to be expressed in character form
!       string:  the number expressed in character form
!       nchar:   the maximum number of characters in which to express number


    use inter

    integer, intent(in)             :: value
    character (len=*), intent(out)  :: string
    integer, intent(in), optional   :: nchar
    character (len=12)              :: afmt
    integer                         :: llen

    string=' '
    afmt='(i    )'
    llen=min(30,len(string))
    if(present(nchar)) llen=min(llen,nchar)
    write(afmt(3:6),'(i4)') llen
    write(string(1:llen),afmt,err=100) value
    string=adjustl(string)
    if(string(1:1).eq.'*') go to 100
    return

100     string(1:llen)=repeat('#',llen)
    return

end subroutine i2a


subroutine d2a(value,string,nchar)
! -- Subroutine d2a writes the character equivalent of a double.

! -- Arguments are as follows:-
!       value:   the number to be expressed in character form
!       string:  the number expressed in character form
!       nchar:   the maximum number of characters in which to express number

    use inter

    double precision, intent(in)    :: value
    character (len=*), intent(out)  :: string
    integer, intent(in), optional   :: nchar
    integer                         :: llen, ifail
    double precision                :: value_check
    character (len=32)              :: word

    string=' '
    llen=min(29,len(string))
    if(present(nchar)) llen=min(llen,nchar)
    call wrtsig(ifail,value,word,llen,1,value_check,0)
    if(ifail.lt.0) then
      call sub_error('D2A')
    else if(ifail.gt.0) then
      string(1:llen)=repeat('#',llen)
    else
      string=adjustl(word)
    end if
    return

end subroutine d2a


subroutine r2a(value,string,nchar)
! -- Subroutine r2a writes the character equivalent of a real.

! -- Arguments are as follows:-
!       value:   the number to be expressed in character form
!       string:  the number expressed in character form
!       nchar:   the maximum number of characters in which to express number

    use inter

    real,intent(in)                 :: value
    character (len=*), intent(out)  :: string
    integer, intent(in), optional   :: nchar
    integer                         :: llen,ifail
    double precision                :: dvalue,dvalue_check
    character (len=32)              :: word

    string=' '
    llen=min(29,len(string))
    if(present(nchar)) llen=min(llen,nchar)
    dvalue=value
    call wrtsig(ifail,dvalue,word,llen,0,dvalue_check,0)
    if(ifail.lt.0) then
      call sub_error('R2A')
    else if(ifail.gt.0) then
      string(1:llen)=repeat('#',llen)
    else
      string=adjustl(word)
    end if
    return

end subroutine r2a

!*****************************************************************************
! subroutines comprising the generic subroutine  ------->
!*****************************************************************************


! -- The subroutines comprising char2num convert a string to either an integer,
!    a real number, or a double precision number.

! -- Arguments are as follows:-
!      ifail:   indicates failure if returned as non-zero
!      string:  a character string containing a number
!      num:     an integer (for a2i), real (for a2r), or double precision (for
!               a2d) number extracted from the string.

! -- Revision history:-
!       June-November, 1995: version 1.


subroutine a2i(ifail,string,num)
! -- Subroutine a2i writes the integer equivalent of a string.

! -- Arguments are as follows:-
!      ifail:   indicates failure if returned as non-zero
!      string:  a character string containing a number
!      num:     an integer number extracted from the string.

    integer, intent(out)            :: ifail
    character (len=*), intent(in)   :: string
    integer, intent(out)            :: num
    character (len=10)              :: afmt

    ifail=0
    afmt='(i    )'
    write(afmt(3:6),'(i4)')len(string)
    read(string,afmt,err=10) num
    return

10      ifail=1
    return

end subroutine a2i


subroutine a2r(ifail,string,num)
! -- Subroutine a2r writes the real equivalent of a string.

! -- Arguments are as follows:-
!      ifail:   indicates failure if returned as non-zero
!      string:  a character string containing a number
!      num:     a real number extracted from the string.

    integer, intent(out)            :: ifail
    character (len=*), intent(in)   :: string
    real, intent(out)               :: num
    character (len=10)              :: afmt

    ifail=0
    afmt='(f    .0)'
    write(afmt(3:6),'(i4)')len(string)
    read(string,afmt,err=10) num
    return

10      ifail=1
    return

end subroutine a2r


subroutine a2d(ifail,string,num)
! -- Subroutine a2d writes the double equivalent of a string.

! -- Arguments are as follows:-
!      ifail:   indicates failure if returned as non-zero
!      string:  a character string containing a number
!      num:     a double precision number extracted from the string.

    integer, intent(out)            :: ifail
    character (len=*), intent(in)   :: string
    double precision, intent(out)   :: num
    character (len=10)              :: afmt

    ifail=0
    afmt='(f    .0)'
    write(afmt(3:6),'(i4)')len(string)
    read(string,afmt,err=10) num
    return

10      ifail=1
    return

end subroutine a2d

SUBROUTINE WRTSIG(IFAIL,VAL,WORD,NW,PRECIS,TVAL,NOPNT)
! --
! -- SUBROUTINE WRTSIG WRITES A NUMBER INTO A CONFINED SPACE WITH MAXIMUM
! -- PRECISION
! --

! -- Arguments are as follows:-
!      ifail:   indicates failure if returned as non-zero
!      val:     
!      word:    
!      nw:      
!      precis:  
!      tval:    
!      nopnt:   

!       failure criteria:
!           ifail= 1 ...... number too large or small for single precision type
!           ifail= 2 ...... number too large or small for double precision type
!           ifail= 3 ...... field width too small to represent number
!           ifail=-1 ...... internal error type 1
!           ifail=-2 ...... internal error type 2
!           ifail=-3 ...... internal error type 3

! -- Revision history:-
!       July, 1993: version 1.
!       August 1994: modified for unix version (#ifdef's added)
!       August, 1995: #ifdefs commented out for inclusion in Groundwater
!                     Data Utilities


    INTEGER PRECIS,LW,POS,INC,D,P,W,J,JJ,K,JEXP,N,JFAIL,NW, &
    EPOS,PP,NOPNT,KEXP,IFLAG,LEXP
    INTEGER IFAIL
    DOUBLE PRECISION VAL,TVAL
    CHARACTER*29 TWORD,TTWORD,FMT*14
    CHARACTER*(*) WORD

    LEXP=0
    IFLAG=0
    WORD=' '
    POS=1
    IF(VAL.LT.0.0D0)POS=0
!#ifdef USE_D_FORMAT
!        WRITE(TWORD,'(1PD23.15D3)') VAL
!#else
    WRITE(TWORD,'(1PE23.15E3)') VAL
!#endif
    READ(TWORD(20:23),'(I4)') JEXP
    EPOS=1
    IF(JEXP.LT.0)EPOS=0

    JFAIL=0
    IFAIL=0
    IF(PRECIS.EQ.0)THEN
      LW=MIN(15,NW)
    ELSE
      LW=MIN(23,NW)
    END IF

    N=0
    IF(NOPNT.EQ.1)N=N+1
    IF(POS.EQ.1)N=N+1
    IF(PRECIS.EQ.0)THEN
      IF(ABS(JEXP).GT.38)THEN
        IFAIL=1
        RETURN
      END IF
      IF(POS.EQ.1) THEN
        IF(LW.GE.13) THEN
          WRITE(WORD,'(1PE13.7E3)',ERR=80) VAL
          GO TO 200
        END IF
      ELSE
        IF(LW.GE.14)THEN
          WRITE(WORD,'(1PE14.7E3)',ERR=80) VAL
          GO TO 200
        END IF
      END IF
      IF(LW.GE.14-N) THEN
        LW=14-N
        GO TO 80
      END IF
    ELSE
      IF(ABS(JEXP).GT.275)THEN
        IFAIL=2
        RETURN
      END IF
      IF(POS.EQ.1) THEN
        IF(LW.GE.22) THEN
!#ifdef USE_D_FORMAT
!              WRITE(WORD,'(1PD22.15D3)',ERR=80) VAL
!#else
          WRITE(WORD,'(1PE23.15E3)',ERR=80) VAL
!#endif
          GO TO 200
        END IF
      ELSE
        IF(LW.GE.23) THEN
!#ifdef USE_D_FORMAT
!              WRITE(WORD,'(1PD23.15D3)',ERR=80) VAL
!#else
          WRITE(WORD,'(1PE23.15E3)',ERR=80) VAL
!#endif
          GO TO 200
        END IF
      END IF
      IF(LW.GE.23-N)THEN
        LW=23-N
        GO TO 80
      END IF
    END IF

    IF(NOPNT.EQ.1)THEN
      IF((JEXP.EQ.LW-2+POS).OR.(JEXP.EQ.LW-3+POS))THEN
        WRITE(FMT,15)LW+1
15          FORMAT('(F',I2,'.0)')
        WRITE(WORD,FMT,ERR=19) VAL
        IF(INDEX(WORD,'*').NE.0) GO TO 19
        IF(WORD(1:1).EQ.' ') GO TO 19
        WORD(LW+1:LW+1)=' '
        GO TO 200
      END IF
    END IF
19      D=MIN(LW-2+POS,LW-JEXP-3+POS)
20      IF(D.LT.0) GO TO 80
    WRITE(FMT,30) LW,D
30      FORMAT('(F',I2,'.',I2,')')
    WRITE(WORD,FMT,ERR=80) VAL
    IF(INDEX(WORD,'*').NE.0) THEN
      D=D-1
      GO TO 20
    END IF
    K=INDEX(WORD,'.')
    IF(K.EQ.0)THEN
      IFAIL=-1
      RETURN
    END IF
    IF((K.EQ.1).OR.((POS.EQ.0).AND.(K.EQ.2)))THEN
      DO 70 J=1,3
      IF(K+J.GT.LW) GO TO 75
      IF(WORD(K+J:K+J).NE.'0') GO TO 200
70        CONTINUE
      GO TO 80
75        IFAIL=3
      RETURN
    END IF
    GO TO 200

80      WORD=' '
    IF(NOPNT.EQ.0)THEN
      D=LW-7
      IF(POS.EQ.1) D=D+1
      IF(EPOS.EQ.1) D=D+1
      IF(ABS(JEXP).LT.100) D=D+1
      IF(ABS(JEXP).LT.10) D=D+1
      IF((JEXP.GE.100).AND.(JEXP-(D-1).LT.100))THEN
        P=1+(JEXP-99)
        D=D+1
        LEXP=99
      ELSE IF((JEXP.GE.10).AND.(JEXP-(D-1).LT.10))THEN
        P=1+(JEXP-9)
        D=D+1
        LEXP=9
      ELSE IF((JEXP.EQ.-10).OR.(JEXP.EQ.-100)) THEN
        IFLAG=1
        D=D+1
      ELSE
        P=1
      END IF
      INC=0
85        IF(D.LE.0) GO TO 300
      IF(IFLAG.EQ.0)THEN
        WRITE(FMT,100,ERR=300) P,D+7,D-1
      ELSE
        WRITE(FMT,100,ERR=300) 0,D+8,D
      END IF
      WRITE(TWORD,FMT) VAL
      IF(IFLAG.EQ.1) GO TO 87
      READ(TWORD(D+4:D+7),'(I4)',ERR=500) KEXP
      IF(((KEXP.EQ.10).AND.((JEXP.EQ.9).OR.(LEXP.EQ.9))).OR. &
      ((KEXP.EQ.100).AND.((JEXP.EQ.99).OR.LEXP.EQ.99))) THEN
        IF(INC.EQ.0)THEN
          IF(LEXP.EQ.0)THEN
        IF(D-1.EQ.0) THEN
          D=D-1
        ELSE
          P=P+1
        END IF
          ELSE IF(LEXP.EQ.9)THEN
        IF(JEXP-(D-2).LT.10) THEN
          P=P+1
        ELSE
          D=D-1
        END IF
          ELSE IF(LEXP.EQ.99)THEN
        IF(JEXP-(D-2).LT.100)THEN
          P=P+1
        ELSE
          D=D-1
        END IF
          END IF
          INC=INC+1
          GO TO 85
        END IF
      END IF
!#ifdef USE_D_FORMAT
!87        J=INDEX(TWORD,'D')
!#else
87        J=INDEX(TWORD,'E')
!#endif
      GO TO 151
    END IF
    INC=0
    P=LW-2
    PP=JEXP-(P-1)
    IF(PP.GE.10)THEN
      P=P-1
      IF(PP.GE.100)P=P-1
    ELSE IF(PP.LT.0)THEN
      P=P-1
      IF(PP.LE.-10)THEN
        P=P-1
        IF(PP.LE.-100)P=P-1
      END IF
    END IF
    IF(POS.EQ.0)P=P-1
90      CONTINUE
    D=P-1
    W=D+8
    WRITE(FMT,100) P,W,D
    IF(D.LT.0)THEN
      IF(JFAIL.EQ.1) GO TO 300
      JFAIL=1
      P=P+1
      GO TO 90
    END IF
!#ifdef USE_D_FORMAT
!100     FORMAT('(',I2,'pD',I2,'.',I2,'D3)')
!#else
100     FORMAT('(',I2,'pE',I2,'.',I2,'E3)')
!#endif
    WRITE(TWORD,FMT) VAL
!#ifdef USE_D_FORMAT
!        J=INDEX(TWORD,'D')
!#else
    J=INDEX(TWORD,'E')
!#endif
    IF(TWORD(J-1:J-1).NE.'.')THEN
      IFAIL=-1
      RETURN
    END IF
    N=1
    IF(TWORD(J+1:J+1).EQ.'-') N=N+1
    IF(TWORD(J+2:J+2).NE.'0') THEN
      N=N+2
      GO TO 120
    END IF
    IF(TWORD(J+3:J+3).NE.'0') N=N+1
120     N=N+1
    IF(J+N-2-POS.LT.LW)THEN
      IF(INC.EQ.-1) GO TO 150
      TTWORD=TWORD
      P=P+1
      INC=1
      GO TO 90
    ELSE IF(J+N-2-POS.EQ.LW) THEN
      GO TO 150
    ELSE
      IF(INC.EQ.1)THEN
        TWORD=TTWORD
        GO TO 150
      END IF
      IF(JFAIL.EQ.1) GO TO 300
      P=P-1
      INC=-1
      GO TO 90
    END IF

150     J=INDEX(TWORD,'.')
151     IF(POS.EQ.0)THEN
      K=1
    ELSE
     K=2
    END IF
    WORD(1:J-K)=TWORD(K:J-1)
    JJ=J
    J=J-K+1
    IF(PRECIS.EQ.0)THEN
      WORD(J:J)='E'
    ELSE
      WORD(J:J)='D'
    END IF
    JJ=JJ+2
    IF(NOPNT.EQ.0) JJ=JJ-1
    IF(TWORD(JJ:JJ).EQ.'-')THEN
      J=J+1
      WORD(J:J)='-'
    END IF
    IF(TWORD(JJ+1:JJ+1).NE.'0')THEN
      J=J+2
      WORD(J-1:J)=TWORD(JJ+1:JJ+2)
      GO TO 180
    END IF
    IF(TWORD(JJ+2:JJ+2).NE.'0')THEN
      J=J+1
      WORD(J:J)=TWORD(JJ+2:JJ+2)
    END IF
180     J=J+1
    WORD(J:J)=TWORD(JJ+3:JJ+3)
    IF(IFLAG.EQ.1)THEN
      IF(POS.EQ.1)THEN
        JJ=1
      ELSE
        JJ=2
      END IF
      N=len_trim(WORD)
      DO 190 J=JJ,N-1
190       WORD(J:J)=WORD(J+1:J+1)
      WORD(N:N)=' '
    END IF

200     IF(len_trim(WORD).GT.LW)THEN
      IFAIL=-2
      RETURN
    END IF
    WRITE(FMT,30) LW,0
    READ(WORD,FMT,ERR=400) TVAL
    RETURN
300     IFAIL=3
    RETURN
400     IFAIL=-3
    RETURN
500     IFAIL=-2
    RETURN
    END SUBROUTINE WRTSIG

integer function nextunit()

! -- Function nextunit determines the lowest unit number available for
! -- opening.

! -- Revision history:-
!       June-November, 1995: version 1.

    logical::lopen

    do nextunit=10,100
      inquire(unit=nextunit,opened=lopen)
      if(.not.lopen) return
    end do
    write(6,10)
10      format(' *** No more unit numbers to open files ***')
    stop

end function nextunit

subroutine close_files

! -- Subroutine close_files closes all open files.

! -- Revision history:-
!       June-November, 1995: version 1.

    integer         :: i,ierr

    do i=10,100
      close(unit=i,iostat=ierr)
    end do
    return

end subroutine close_files

subroutine read_settings(ifail,iidate,iheader)

! -- Subroutine read_settings reads a settings file located in the current
! -- directory.

! -- Arguments are as follows:-
!      ifail:    returned as zero unless settings file cannot be read
!      iidate:    returned as zero unless date format is incorrect in settings
!                file
!      iheader:  returned as zero unless header specifier is incorrect


    use defn
    use inter

    integer, intent(out)            :: ifail,iidate,iheader
    integer                                 :: iunit,ierr,iequals,i
    character (len=40)                      :: aline

    ifail=0
    iidate=0
        iheader=0
    datespec=0
        headerspec=' '

    iunit=nextunit()
    open(unit=iunit,file='settings.fig',status='old',iostat=ierr)
    if(ierr.ne.0) then
      ifail=1
      return
    end if

    do
      read(iunit,'(a)',err=100,end=200) cline
      call casetrans(cline,'lo')
      iequals=index(cline,'=')
      if(iequals.le.1) cycle
      aline=cline(1:iequals-1)
      aline=adjustl(aline)
      if(aline(1:4).eq.'date') then
        aline=cline(iequals+1:)
        aline=adjustl(aline)
        if((aline(1:2).eq.'dd').and.(aline(4:5).eq.'mm')) then
          datespec=1
        else if((aline(1:2).eq.'mm').and.(aline(4:5).eq.'dd')) then
          datespec=2
        else
          iidate=1
        end if
      else if(aline(1:6).eq.'colrow') then
        aline=cline(iequals+1:)
            do i=1,len_trim(aline)
              if(aline(i:i).eq.'''')aline(i:i)=' '
            end do
        aline=adjustl(aline)
            call casetrans(aline,'lo')
            if(aline(1:3).eq.'yes')then
              headerspec='yes'
            else if(aline(1:2).eq.'no')then
              headerspec='no '
            else
              iheader=1
            end if
          end if
    end do
    go to 200

100    ifail=2
200     close(unit=iunit,iostat=ierr)
    return

end subroutine read_settings

subroutine read_rest_of_sample_line(ifail,cols,ndays,nsecs,value,iline,sampfile)

! -- Subroutine read_rest_of_sample_line reads the date, time, value and
!    optional fifth column from a line of a bore sample file.

! -- Arguments are as follows:-
!       ifail:     returned as zero unless an error condition is encountered
!       cols:      number of data columns in the line
!       ndays:     number of days from 1/1/1970 until sample date
!       nsecs:     number of seconds from midnight until sample time
!       value:     sample value
!       iline:     current line number of bore sample file
!       sampfile:  name of bore sample file

! -- Revision history:-
!       June-November, 1995: version 1.

    use defn
    use inter

    integer, intent(out)            :: ifail
    integer, intent(in)             :: cols
    integer, intent(out)            :: ndays,nsecs
    double precision, intent(out)   :: value
    integer, intent(in)             :: iline
    character (len=*), intent(in)   :: sampfile
    integer                         :: dd,mm,yy,hhh,mmm,sss
    character (len=15)              :: aline
    character (len=2)               :: aa

    ifail=0
    call char2date(ifail,cline(left_word(2):right_word(2)),dd,mm,yy)
    if(ifail.ne.0) then
      call num2char(iline,aline)
      write(amessage,150) trim(aline),trim(sampfile)
150       format('illegal date at line ',a,' of bore sample file ',a)
      call write_message(error='yes',leadspace='yes')
      go to 9800
    end if
    ndays=numdays(1,1,1970,dd,mm,yy)

    call char2time(ifail,cline(left_word(3):right_word(3)),hhh,mmm,sss)
    if(ifail.ne.0) then
      call num2char(iline,aline)
      write(amessage,160) trim(aline),trim(sampfile)
160       format('illegal time at line ',a,' of bore sample file ',a)
      call write_message(error='yes',leadspace='yes')
      go to 9800
    end if
    nsecs=numsecs(0,0,0,hhh,mmm,sss)

    value=char2double(ifail,4)
    if(ifail.ne.0)then
      call num2char(iline,aline)
      write(amessage,180) trim(aline),trim(sampfile)
180       format('cannot read sample value at line ',a,' of bore sample file ',a)
      call write_message(error='yes',leadspace='yes')
      go to 9800
    end if
    if(value.lt.-1.0e37) then
      call num2char(iline,aline)
      write(amessage,190) trim(aline),trim(sampfile)
190       format('illegal sample value at line ',a,' of bore sample file ',a, &
      '; lower limit is -1.0E37.')
      call write_message(error='yes',leadspace='yes')
      go to 9800
    end if
    if(cols.eq.5)then
      aa=cline(left_word(5):right_word(5))
      call casetrans(aa,'lo')
      if(aa.eq.'x ') then
        value=-1.1e38
      else
        call num2char(iline,aline)
        write(amessage,210) trim(aline),trim(sampfile)
210         format('illegal optional fifth item on line ',a,' of bore sample ',&
        'file ',a,'; item must be "x" if present.')
        call write_message(error='yes',leadspace='yes')
        go to 9800
      end if
    end if
    return

9800    ifail=1
    return

end subroutine read_rest_of_sample_line

subroutine char2time(ifail,atime,hh,mm,ss)

! -- Subroutine CHAR2TIME extracts the time from a string.

! -- Arguments are as follows:-
!       ifail:     indicates failure if returned as non-zero
!       atime:     a string containing the time in ASCII format
!       hh,mm,ss   hours, minutes and seconds extracted from the atime string.

! -- Revision history:-
!       June-November, 1995: version 1.

    use defn
    use inter

    integer, intent(out)            :: ifail
    character (len=*), intent(in)   :: atime
    integer, intent(out)            :: hh,mm,ss
    integer                         :: lentime,i,j
    character (len=2)               :: asep
    character (len=20)              :: btime

    ifail=0
    asep=':.'
    if(atime.eq.' ') go to 9000
    btime=adjustl(atime)
    lentime=len_trim(btime)
    if(lentime.lt.5) go to 9000

    do i=1,lentime
      if(index(asep,btime(i:i)).ne.0) go to 20
    end do
    go to 9000

! -- The first integer is extracted from the string. This represents hours.

20      if(i.eq.1) go to 9000
    call char2num(ifail,btime(1:i-1),hh)
    if(ifail.ne.0) go to 9000
    if((hh.lt.0).or.(hh.gt.23)) go to 9000

    i=i+1
    if(lentime-i.lt.2) go to 9000
    do j=i,lentime
      if(index(asep,btime(j:j)).ne.0) go to 40
    end do
    go to 9000

! -- The second integer (representing minutes) is extracted from the string.

40      if(j.eq.i) go to 9000
    call char2num(ifail,btime(i:j-1),mm)
    if(ifail.ne.0) go to 9000
    if((mm.lt.0).or.(mm.gt.59)) go to 9000

! -- The third integer (representing seconds) is extracted from the string.

    j=j+1
    if(lentime-j.lt.0) go to 9000
    call char2num(ifail,btime(j:lentime),ss)
    if(ifail.ne.0) go to 9000
    if((ss.lt.0).or.(ss.gt.59)) go to 9000
    ifail=0
    return

9000    ifail=1
    return

end subroutine char2time

subroutine char2date(ifail,adate,dd,mm,yy)

! -- Subroutine CHAR2DATE extracts the date from a string.


! -- Arguments are as follows:-
!      ifail:      returns a non-zero value if an error condition is encountered
!      adate:      the string containing the date
!      dd,mm,yy    the day, month and year read from the date string

! --  Revision history:-
!       June-November, 1995: version 1.

    use defn
    use inter

    integer, intent(out)    :: ifail
    character (len=*), intent(in)   :: adate
    integer, intent(out) :: dd,mm,yy
    integer :: lendate,i,j
    character (len=2)       :: asep
    character (len=20)      :: bdate

    ifail=0
    asep=':/'
    if(adate.eq.' ') go to 9000
    bdate=adjustl(adate)
    lendate=len_trim(bdate)
    if(lendate.lt.8) go to 9000

    do i=1,lendate
      if(index(asep,bdate(i:i)).ne.0) go to 20
    end do
    go to 9000

! -- The first integer is extracted from the date string. This is either days
!    or months depending on the contents of file settings.fig.

20      if(i.eq.1) go to 9000
    if(datespec.ne.1) then
       call char2num(ifail,bdate(1:i-1),mm)
    else
       call char2num(ifail,bdate(1:i-1),dd)
    end if
    if(ifail.ne.0) go to 9000

    i=i+1
    if(lendate-i.lt.5) go to 9000
    do j=i,lendate
      if(index(asep,bdate(j:j)).ne.0) go to 40
    end do
    go to 9000

! -- The second integer is extracted from the date string. This is either months
!    or days depending on the contents of file settings.fig.

40      if(j.eq.i) go to 9000
    if(datespec.ne.1) then
      call char2num(ifail,bdate(i:j-1),dd)
    else
      call char2num(ifail,bdate(i:j-1),mm)
    end if
    if(ifail.ne.0) go to 9000
    if((dd.le.0).or.(dd.gt.31)) go to 9000
    if((mm.le.0).or.(mm.gt.12)) go to 9000
    if(dd.eq.31)then
      if((mm.eq.2).or.(mm.eq.4).or.(mm.eq.6).or.(mm.eq.9).or.&
      (mm.eq.11)) go to 9000
    end if
    if((mm.eq.2).and.(dd.eq.30)) go to 9000

! -- The third integer is extracted from the date string. This is years.

    j=j+1
    if(lendate-j.ne.3) go to 9000
    call char2num(ifail,bdate(j:lendate),yy)
    if(ifail.ne.0) go to 9000
    if(.not.leap(yy))then
      if((mm.eq.2).and.(dd.eq.29)) go to 9000
    end if
    ifail=0
    return

9000    ifail=1
    return

end subroutine char2date

double precision function char2double(ifail,num)

! -- Function char2double extracts a double precision number from a word
!    demarcated by subroutine linesplit.

! -- Arguments are as follows:-
!       ifail:    returned as zero unless an error condition arises
!       num:      the number of the word previously extracted by linesplit
!       returns   value of double precision number read from word

! -- Revision history:-
!       June-November, 1995: version 1.

    use defn

    integer, intent(in)             :: num
    integer, intent(out)            :: ifail
    integer                         :: ierr
    character (len=10)              :: afmt

    if(num.gt.NUM_WORD_DIM) call sub_error('CHAR2DOUBLE')
    if((right_word(num).lt.left_word(num)).or. &
      (left_word(num).le.0)) call sub_error('CHAR2DOUBLE')

    ifail=0
    afmt='(f   .0)'
    write(afmt(3:5),'(i3)') right_word(num)-left_word(num)+1
    read(cline(left_word(num):right_word(num)),afmt, iostat=ierr) char2double
    if(ierr.ne.0) go to 110
    return

110     ifail=1
    return
        end

!     Last change:  JD   23 Dec 2000    8:32 pm
subroutine open_named_input_file(ifail,aprompt,infile,inunit)

! -- Subroutine open_named_input_file opens an input file for which a default
!    name exists.

! -- Arguments are as follows:-
!       ifail:    returned as non-zero in case of failure
!       aprompt:  user prompt for filename
!       infile:   name of input file (carries default name on entry)
!       inunit:   unit number of input file

! -- Revision history:-
!       June-November, 1995: version 1.

    use defn
    use inter

    integer, intent(out)                    :: ifail
    character (len=*), intent(in)           :: aprompt
    character (len=*), intent(inout)        :: infile
    integer, intent(out)                    :: inunit
    integer                                 :: ierr,nbb,ifail1
    logical                                 :: lopened
    character (len=200)                     :: tempfile,atempf

! -- The user is prompted for the name of the file to open.

    imessage=0
    ifail=0
5       if(infile.eq.' ')then
10        write(6,'(a)',advance='no') aprompt(1:len_trim(aprompt)+1)
      read(5,'(a)') tempfile
      if(tempfile.eq.' ') go to 10
      tempfile=adjustl(tempfile)
      if(index(eschar,tempfile(1:2)).ne.0) then
        escset=1
        return
      end if
          nbb=len_trim(tempfile)
          call getfile(ifail1,tempfile,atempf,1,nbb)
          if(ifail1.ne.0) go to 10
          tempfile=atempf
    else
      write(6,'(a)',advance='no') aprompt(1:len_trim(aprompt)-1)//&
      ' ['//trim(infile)//']: '
      read(5,'(a)') tempfile
      if(tempfile.eq.' ')then
        tempfile=infile
      else
        tempfile=adjustl(tempfile)
        if(index(eschar,tempfile(1:2)).ne.0) then
          escset=1
          return
        end if
            nbb=len_trim(tempfile)
            call getfile(ifail1,tempfile,atempf,1,nbb)
            if(ifail1.ne.0) go to 5
            tempfile=atempf
      end if
    end if

! -- Is the file already open?

    inquire(file=tempfile,opened=lopened)
    if(lopened)then
      write(amessage,30) trim(tempfile)
30        format(' File ',a,' is already open  - try again.')
      call write_message(increment=1)
      go to 5
    end if

! -- The file is opened.

    inunit=nextunit()
    open(unit=inunit,file=tempfile,status='old',iostat=ierr)
    if(ierr.ne.0)then
      if(imessage.gt.5) then
        ifail=1
        return
      end if
      write(amessage,50) trim(tempfile)
50        format(' Cannot open file ',a,'  - try again.')
      call write_message(increment=1)
      if(infile.eq.tempfile) infile=' '
      go to 5
    end if
    infile=tempfile

    return

end subroutine open_named_input_file

integer function numdays(DR,MR,YR,D,M,Y)

! -- Function numdays calculates the number of days between dates
!    D-M-Y and DR-MR-YR. If the former preceeds the latter the answer is
!    negative.

! -- Arguments are as follows:-
!       dr,mr,yr:     days, months and years of first date
!       d,m,y:        days, months and years of second date
!       numdays returns the number of elapsed days

! -- Revision history:-
!       22 July 1994:  version 1
!       13 September 1995:  modified for Groundwater Data Utilities


    integer, intent(in)     :: dr,mr,yr,d,m,y

    INTEGER FLAG,I,J,DA(12),YE,ME,DE,YL,ML,DL
    logical leap

    DATA DA /31,28,31,30,31,30,31,31,30,31,30,31/

! --    THE SMALLER OF THE TWO DATES IS NOW CHOSEN TO DO THE COUNTING FROM.

    IF(Y.LT.YR)GO TO 10
    IF((Y.EQ.YR).AND.(M.LT.MR)) GO TO 10
    IF((Y.EQ.YR).AND.(M.EQ.MR).AND.(D.LT.DR)) GO TO 10
    FLAG=0
    YE=YR
    ME=MR
    DE=DR
    YL=Y
    ML=M
    DL=D
    GO TO 20
10      FLAG=1
    YE=Y
    ME=M
    DE=D
    YL=YR
    ML=MR
    DL=DR

! --    IN THE ABOVE THE POSTSCRIPT "E" STANDS FOR EARLIER DATE, WHILE
!       "L" STANDS FOR THE LATER DATE.

20      numdays=0
    IF((ME.EQ.ML).AND.(YL.EQ.YE))THEN
    numdays=DL-DE
    IF(FLAG.EQ.1) numdays=-numdays
    RETURN
    END IF

    DO 30 J=ME,12
    IF((ML.EQ.J).AND.(YE.EQ.YL))GOTO 40
    numdays=numdays+DA(J)
    IF((J.EQ.2).AND.(leap(ye)))numdays=numdays+1
30      CONTINUE
    GO TO 50
40      numdays=numdays+DL-DE
    IF(FLAG.EQ.1)numdays=-numdays
    RETURN

50      DO 60 I=YE+1,YL
    DO 70 J=1,12
    IF((YL.EQ.I).AND.(ML.EQ.J))GO TO 80
    numdays=numdays+DA(J)
    IF((J.EQ.2).AND.(leap(i))) numdays=numdays+1
70      CONTINUE
60      CONTINUE
    call sub_error('NUMDAYS')
    RETURN

80      numdays=numdays+DL-DE
    IF(FLAG.EQ.1) numdays=-numdays

    RETURN
end function numdays

integer function numsecs(h1,m1,s1,h2,m2,s2)

! -- Subroutine NUMSECS calculates the number of seconds between two times.

! -- Arguments are as follows:-
!       h1,m1,s1:   hours, minutes seconds of first time
!       h2,m2,y2:   hours, minutes seconds of second time

! -- Revision history:-
!       June-November 1995: version 1.

    integer, intent(in)             :: h1,m1,s1,h2,m2,s2

    numsecs=(h2-h1)*3600+(m2-m1)*60+s2-s1

end function numsecs

subroutine newdate(ndays,day1,mon1,year1,day2,mon2,year2)

! -- Subroutine NEWDATE evaluates the date after NDAYS days have elapsed from
!    a provided date. NDAYS may be negative.

! -- Arguments are as follows:-
!       ndays:            elapsed number of days
!       day1,mon1,year1:  days, month and year of first date
!       day2,mon2,year2:  days, month and year of second date

! -- Revision history:-
!       June-November, 1995: version 1.

    use inter
    implicit none

    integer, intent(in)     :: ndays,day1,mon1,year1
    integer, intent(out)    :: day2,mon2,year2

    integer  :: yearref,newdays,idays,iyear,jdays,i
    integer, dimension(12) :: monthdays

    data monthdays /31,28,31,30,31,30,31,31,30,31,30,31/

! -- First a reference date is chosen. This is the beginning of the first
! -- year. Alternatively the reference date is the beginning of a year prior
! -- to the likely calculated date if NDAYS is negative.

    if(ndays.ge.0) then
      yearref=year1
    else
      yearref=year1-abs(ndays)/365-1
    end if
    newdays=numdays(31,12,yearref-1,day1,mon1,year1)
    newdays=ndays+newdays
    if(newdays.lt.0) call sub_error('NEWDATE')

! -- Next days are counted, starting at the new reference date.

    idays=0
    iyear=yearref
    do
      jdays=idays+365
      if(leap(iyear)) jdays=jdays+1
      if(jdays.ge.newdays) go to 20
      iyear=iyear+1
      idays=jdays
    end do
    call sub_error('NEWDATE')
20      year2=iyear

    do i=1,12
      jdays=idays+monthdays(i)
      if((i.eq.2).and.(leap(year2))) jdays=jdays+1
      if(jdays.ge.newdays) go to 40
      idays=jdays
    end do
    call sub_error('NEWDATE')
40      mon2=i
    day2=newdays-idays
    if((day2.le.0).or.(mon2.le.0).or.(year2.le.0)) call sub_error('NEWDATE')

    return

end subroutine newdate

logical function leap(year)

! -- Function LEAP returns .true. if a year is a leap year.

! -- Revision history:-
!       June-November, 1995: version 1.

    integer, intent(in)     :: year

        leap = ( mod(year,4).eq.0 .and. mod(year,100).ne.0 ) .or. &
               ( mod(year,400).eq.0 .and. year.ne.0 )

    return
end function leap

!     Last change:  JD   21 Dec 2000    2:33 pm
subroutine readfig(specfile,coordfile,sampfile,pumpfile,pilotfile)

! -- Subroutine readfig reads a filename file.

! -- Arguments are as follows:-
!       specfile:  name of grid specification file
!       coordfile: name of bore coordinates file
!       sampfile:  name of bore sample file
!       pumpfile:  name of bore pumping file


    use defn
    use inter

    character (len=*),intent(out)           :: specfile
    character (len=*),intent(out),optional  :: coordfile,sampfile,pumpfile,pilotfile
    integer                                 :: iunit,ierr,iequals,nb,ifail
    character (len=100)                     :: filename

    specfile=' '
    if(present(coordfile)) coordfile=' '
    if(present(sampfile)) sampfile=' '
    if(present(pumpfile)) pumpfile=' '
    if(present(pilotfile)) pilotfile=' '

    iunit=nextunit()
    open(unit=iunit,file='files.fig',status='old',err=200)
    do
      read(iunit,'(a)',err=100,end=200) cline
          cline=adjustl(cline)
      iequals=index(cline,'=')
      if(iequals.le.1) cycle
          nb=len_trim(cline)
          if(nb.eq.iequals) return
          call getfile(ifail,cline,filename,iequals+1,nb)
          if(ifail.ne.0) cycle
      call casetrans(cline(1:iequals),'lo')
      if(cline(1:10).eq.'grid_speci') then
        specfile=filename
      else if(cline(1:10).eq.'bore_coord') then
        if(present(coordfile)) then
          coordfile=filename
        end if
      else if(cline(1:10).eq.'bore_sampl') then
        if(present(sampfile)) then
          sampfile=filename
        end if
      else if(cline(1:10).eq.'bore_pumpi') then
        if(present(pumpfile)) then
          pumpfile=filename
        end if
          else if(cline(1:12).eq.'pilot_points')then
            if(present(pilotfile))then
              pilotfile=filename
            end if
      end if
100       continue
    end do

200     close(unit=iunit,iostat=ierr)
    return

end subroutine readfig

subroutine time_interp(ifail,nbore,ndays,nsecs,value,intday,intsec, &
rnear,rconst,valinterp,extrap,direction)

! -- Subroutine time_interp interpolates an array of times and values to a
! -- user-supplied time.

! -- Arguments are as follows:-
!       ifail:     returned as zero unless an error condition arises
!       nbore:     number of times and corresponding values to be interpolated
!       ndays:     elapsed days corresponding to each value
!       nsecs:     elpased seconds corresponding to each value
!       value:     array of time-based values to be interpolated
!       intday:    the day to be interpolated to, expressed as elapsed days
!       intsec:    the time to be intepolated to, expressed as elapsed seconds
!       rnear:     maximum permitted days to nearest sample
!       rconst:    maximum days to nearest sample if interpolation cannot take
!                  place
!       valinterp: interpolated value
!       extrap:    'yes' if use linear extrapolation to (within rconst) if
!                  interpolation cannot take place
!       direction: 'lo' if extrapolation from two previous points,
!                  'hi' if extrapolation from two following points,
!                  'med' if interpolation if possible, otherwise extrapolation
!                  (note: 'med' is the default)

! -- Revision history:-
!       June-November, 1995: version 1.
!       Mid Year, 1997: incorporated "extrap" argument for SMPCAL
!    October, 1997: incorporated "direction" input for SMPCAL

    use defn
    use inter

    integer, intent(out)                    :: ifail
    integer, intent(in)                     :: nbore
    integer, intent(in), dimension(nbore)   :: ndays,nsecs
    double precision, intent(in), dimension(nbore)      :: value
    integer, intent(in)                     :: intday,intsec
    real, intent(in)                        :: rnear,rconst
    double precision, intent(out)           :: valinterp
    character (len=*), intent(in), optional :: extrap
    character (len=*), intent(in), optional :: direction

    integer                                 :: i,ie,id
    double precision                        :: secfac,diff,diff1,dentime
    character (len=3)                       :: atemp


    ie=0
    if(present(extrap)) then
      atemp=extrap
      call casetrans(atemp,'lo')
      if(atemp.eq.'yes')then
        ie=1
      else if(atemp.eq.'no') then
        ie=0
      else
        call sub_error('TIME_INTERP')
      end if
    end if

    id=0
    if(present(direction))then
      atemp=direction
      call casetrans(atemp,'lo')
      if(atemp.eq.'lo')then
        id=-1
      else if(atemp.eq.'hi')then
        id=1
      else if(atemp.eq.'med')then
        id=0
      else
        call sub_error('TIME_INTERP')
      end if
    end if

    if((id.ne.0).and.(ie.eq.0))then
      call sub_error('TIME_INTERP')
    end if

    ifail=0
    secfac=1.0d0/86400.0d0
    if(nbore.eq.1) then
      diff=dble(intday-ndays(1))+dble(intsec-nsecs(1))*secfac
      if(abs(diff).le.rconst)then
        valinterp=value(1)
      else
        if(diff.gt.0)then
          valinterp=-9.1e37
        else
          valinterp=-8.1e37
        end if
      end if
      return
    end if

    do i=1,nbore-1
      if((ndays(i).gt.ndays(i+1)).or. &
        ((ndays(i).eq.ndays(i+1)).and.(nsecs(i).ge.nsecs(i+1))))then
        ifail=1
        return
      end if
    end do

    do i=1,nbore
      diff=dble(ndays(i)-intday)+dble(nsecs(i)-intsec)*secfac
      if(diff.ge.0)then
        if(i.eq.1)then
          if(diff.le.rconst)then
            if(ie.eq.1)then
              if((value(1).lt.-1.0e38).or.(value(2).lt.-1.0e38))then
                valinterp=value(1)
              else
                dentime=dble(ndays(i+1)-ndays(i))+ &
                            dble(nsecs(i+1)-nsecs(i))*secfac
                if(dentime.le.0) then
                  ifail=1
                  return
                else
                  valinterp=value(i)-(value(i+1)-value(i))*diff/dentime
                end if
              end if
            else
          valinterp=value(1)
            end if
          else
        valinterp=-8.1e37
          end if
          return
        end if

        if(id.eq.-1)then
          if(i.eq.2)then
            diff1=dble(intday-ndays(1))+dble(intsec-nsecs(1))*secfac
            if(diff1.gt.rnear)then               !note - not rconst
              valinterp=-7.1e37
            else
              valinterp=value(1)
            end if
            if(value(1).lt.-1.0e38) valinterp=-1.1e38
          else
            dentime=dble(ndays(i-1)-ndays(i-2))+ &
                        dble(nsecs(i-1)-nsecs(i-2))*secfac
            if(dentime.lt.0.0d0)then
              ifail=1
              return
            else
              diff1=dble(intday-ndays(i-1))+  &
                  dble(intsec-nsecs(i-1))*secfac
              if(diff1.gt.rnear)then
                valinterp=-7.1e37
              else
                if(value(i-1).lt.-1.0e38)then
                  valinterp=-1.1e38
                else if(value(i-2).lt.-1.0e38) then
                  valinterp=value(i-1)
                else
                  valinterp=value(i-1)+ &
                      (value(i-1)-value(i-2))/dentime*diff1
                end if
              end if
            end if
          end if
          return
        else if(id.eq.1)then
          if(i.eq.nbore)then
            if(diff.gt.rnear)then
              valinterp=-7.1e37
            else
              valinterp=value(i)
            end if
            if(value(i).lt.-1.0e38)valinterp=-1.1e38
          else
            dentime=dble(ndays(i+1)-ndays(i))+      &
                    dble(nsecs(i+1)-nsecs(i))*secfac
            if(dentime.le.0)then
              ifail=1
              return
            else
              if(diff.gt.rnear)then
                valinterp=-7.1e37
              else
                if(value(i).lt.-1.0e38)then
                  valinterp=-1.1e38
                else if(value(i+1).lt.-1.0e38)then
                  valinterp=value(i)
                else
                  valinterp=value(i)-  &
                      (value(i+1)-value(i))/dentime*diff
                end if
              end if
            end if
          end if
          return
        else

          dentime=dble(ndays(i)-ndays(i-1))+ &
          dble(nsecs(i)-nsecs(i-1))*secfac
          if(dentime.le.0)then
            ifail=1
            return
          else
            diff1=dentime-diff
            if((diff1.gt.rnear).and.(diff.gt.rnear))then
          valinterp=-7.1e37
            else
          valinterp=value(i-1)+(value(i)-value(i-1))/dentime*diff1
            end if
            if(value(i).lt.-1.0e38)then
          if(diff1.le.rconst)then
            valinterp=value(i-1)
          else
            valinterp=-1.1e38
          end if
            else if(value(i-1).lt.-1.0e38)then
          if(diff.le.rconst)then
            valinterp=value(i)
          else
            valinterp=-1.1e38
          end if
            end if
          end if
        return
        end if
      end if
    end do

    diff1=dble(intday-ndays(nbore))+dble(intsec-nsecs(nbore))*secfac
    if(diff1.le.rconst)then
      if(ie.eq.1) then
        if((value(nbore).lt.-1.0e38).or.(value(nbore-1).lt.-1.0e38)) then
          valinterp=value(nbore)
        else
          dentime=dble(ndays(nbore)-ndays(nbore-1))    &
                     +dble(nsecs(nbore)-nsecs(nbore-1))*secfac
          if(dentime.le.0) then
            ifail=1
            return
          else
            valinterp=value(nbore)+(value(nbore)-value(nbore-1))*    &
                diff1/dentime
          end if
        end if
      else
        valinterp=value(nbore)
      end if
    else
      valinterp=-9.1e37
    end if

    return
    end


subroutine smp2smp (obsfle,obsunit,modfle,modunit,outfle,outunit,elim, &
                    iwriteins,insfle,insunit,pcffle,pcfunit)

! -- Program SMP2SMP builds a bore sample file by temporally interpolating
!    samples contained in one bore sample file to the dates and times of
!    samples recorded in another bore sample file.

!
! This piece of code, written by John Doherty (part of PEST Utilities)
! has been modified to be a subroutine by Steve Shultz/CH2M HILL
! Modifications include:
! 1. passing file names, units, and extrapolation to the subroutine
! 2. addition of an option to write an instruction file, based on 
!    the EXISTING model output file (i.e., must run model prior to 
!    being able to create ins file)



    use defn
    use inter
    implicit none

    logical              :: active
    integer, parameter   :: MAXID=9000000
    integer              :: ifail,iidate,obsunit,modunit,iline,nobsid,i,  &
                            l,cols,maxmod,ierr,ind,iout,iobs,intdays,  &
                            intsecs,ii,dd,mm,yy,hhh,mmm,sss,j,iflag,     &
                            outunit,iheader,iwriteins,insunit,iid,pcfunit
    integer, dimension (MAXID)                        :: nmod,loc,nout
    integer, allocatable, dimension (:)               :: ndays,nsecs

    real                 :: elim
    double precision     :: intvalue, obsvalue
    double precision, allocatable, dimension (:)      :: value

    character (len=5)    :: anum,aline
    character (len=25)   :: atemp1,atemp1old                                  !CFB
    character (len=25)   :: atemp,atempold                                    !CFB
    character (len=80)   :: obsfle,modfle,outfle,specfle,insfle,pcffle
    character (len=25), dimension(MAXID)              :: obsid                !CFB

!obsfle = observation bore sample file
!modfle = model generated bore sample file
!outfle = output file, model data at obs times
!anum = threshold

! *********** temporary - need to pass more file names
!    pcfunit = 40
!    open(unit=pcfunit,file='testpcf.txt')
!    open(unit=pcfunit,file=pcffle)

!    open(unit=*,action='read',carriagecontrol='list')

!    write(amessage,5)
!5    format(' Program SMP2SMP builds a bore sample file by ',  &
!    'interpolating samples contained in one bore sample file to the ',   &
!    'dates and times recorded in another bore sample file.')
!    call write_message(leadspace='yes',endspace='yes')

    call read_settings(ifail,iidate,iheader)
    if(ifail.eq.1) then
      write(amessage,7)
7      format(' A settings file (settings.fig) was not found in the ', &
      'current directory.')
      call write_message
      go to 9900
    else if(ifail.eq.2) then
      write(amessage,8)
8      format(' Error encountered while reading settings file settings.fig')
      call write_message
      go to 9900
    endif
    if((iidate.ne.0).or.(datespec.eq.0)) then
      write(amessage,9)
9      format(' Cannot read date format from settings file ', &
      'settings.fig')
      call write_message
      go to 9900
    end if

!    call readfig(specfle,sampfile=obsfle)

!50    call open_named_input_file(ifail, &
!    ' Enter name of observation bore sample file: ',obsfle,obsunit)
!    if(ifail.ne.0) go to 9900
!    if(escset.ne.0) go to 9900

!    write(6,*)
!70    call open_input_file(ifail,   &
!    ' Enter name of model-generated bore sample file: ',modfle,modunit)
!    if(ifail.ne.0) go to 9900
!    if(escset.ne.0)then
!      escset=0
!      write(6,*)
!      close(unit=obsunit)
!      go to 50
!    end if



    open(unit=modunit,file=modfle,status='old',err=9020)
    open(unit=obsunit,file=obsfle,status='old',err=9000)
    open(unit=outunit,file=outfle)
    
    if (iwriteins.eq.1) then
        open(unit=insunit,file=insfle)
        open(unit=pcfunit,file=pcffle)
        write(insunit,100) 
100        format('pif #')

    end if


!100    write(6,110,advance='no')
!110    format(' Enter extrapolation threshold in days ',                  &
!    '(fractional if necessary): ')
!    read(5,'(a)') anum
!    if(anum.eq.' ') go to 100
!    anum=adjustl(anum)
!    if(index(eschar,anum(1:2)).ne.0) then
!      close(unit=modunit)
!      write(6,*)
!      go to 70
!    end if
!    call char2num(ifail,anum,elim)
!    if(ifail.ne.0) go to 100
!    if(elim.lt.0.0) go to 100

!140    write(6,*)
!150    call open_output_file(ifail,   &
!    ' Enter name for new bore sample file: ',outfle,outunit)
!    if(ifail.ne.0) go to 9900
!    if(escset.ne.0)then
!      escset=0
!      write(6,*)
!      go to 100
!    end if

!    outunit = 37
!    open(unit=outunit,file=outfle)

! -- First the observation bore sample file is read to obtain all bore_ids.

    atempold=' '
    iline=0
    nobsid=0
    do
      iline=iline+1
      read(obsunit,*,end=210) atemp
      atemp=adjustl(atemp)
      call casetrans(atemp,'hi')
      l=len_trim(atemp)
      if(l.gt.25)then                                                            !CFB
        call num2char(iline,aline)
        write(amessage,160) trim(aline),trim(obsfle)
160        format(' Identifier greater than 20 characters at ',  &               !CFB
        'line ',a,' of file ',a)
        go to 9890
      end if
      if(nobsid.eq.0)then
        nobsid=nobsid+1
        if(nobsid.gt.MAXID)then
          write(amessage,180) trim(obsfle)
180          format(' Too many bores cited in file ',a,                    &
          ': increase MAXID and re-compile program.')
          go to 9890
        end if
        obsid(nobsid)=atemp(1:25)                                                !CFB
        atempold=atemp
      else if(atemp.eq.atempold) then
        continue
      else
        call casetrans(atemp,'hi')
        do i=1,nobsid
          if(atemp.eq.obsid(i))then
            call num2char(iline,aline)
            write(amessage,200) trim(aline),trim(obsfle)
200            format(' Identifier used previously at line ',a,          &
            ' of file ',a)
            go to 9890
          end if
        end do
        nobsid=nobsid+1
        if(nobsid.gt.MAXID)then
          write(amessage,180) trim(obsfle)
          go to 9890
        end if
        obsid(nobsid)=atemp(1:25)                                                !CFB
        atempold=atemp
      end if
    end do
210    rewind(unit=obsunit,iostat=ierr)
    if(ierr.ne.0)then
      write(amessage,245) trim(obsfle)
245      format(' Cannot rewind file ',a)
      go to 9890
    end if


! -- Next the model-generated bore sample file is read to get some
!    specifications.

250    continue
    nmod=0                !nmod is an array
    iline=0
    atemp1old=' '
    read_model_sample_file: do
      iline=iline+1
      read(modunit,'(a)',end=300) cline
      call linesplit(ifail,1)
      if(ifail.lt.0) cycle read_model_sample_file
      if(right_word(1)-left_word(1).gt.24) then                                !CFB
        call num2char(iline,aline)
        write(amessage,280) trim(aline),trim(modfle)
280        format(' Identifier greater than 20 characters in length at ',  &   !CFB
        'line ',a,' of file ',a)
        go to 9890
      end if
      atemp1=cline(left_word(1):right_word(1))
      atemp1=adjustl(atemp1)
      call casetrans(atemp1,'hi')
      if(atemp1.eq.atemp1old)then
        if(active) then
          nmod(iobs)=nmod(iobs)+1
        else
          cycle read_model_sample_file
        end if
      else
        atemp1old=atemp1
        do i=1,nobsid
          if(atemp1.eq.obsid(i))then
            active=.true.
            iobs=i
            if(nmod(iobs).ne.0)then
              call num2char(iline,aline)
              write(amessage,290) trim(aline),trim(modfle)
290              format(' Identifiers with same name not in juxtaposition ',&
              'at line ',a,' of file ',a)
              go to 9890
            end if
        nmod(iobs)=1
            cycle read_model_sample_file
          end if
        end do
        active=.false.
      end if
    end do read_model_sample_file

! -- Space is allocated for the storage of all pertinent values from the
!    model-generated bore sample file.

300    maxmod=0
    do i=1,nobsid
      maxmod=maxmod+nmod(i)
    end do
    if(maxmod.eq.0)then
      write(amessage,310) trim(modfle),trim(obsfle)
310      format(' No identifiers cited in file ',a,' correspond to ',      &
      'any of the identifiers in file ',a)
      go to 9890
    end if
    allocate(ndays(maxmod),nsecs(maxmod),value(maxmod),stat=ierr)
    if(ierr.ne.0)then
      write(amessage,330)
330      format(' Cannot allocate sufficient memory to continue execution.')
      go to 9890
    end if
    rewind(unit=modunit,iostat=ierr)
    if(ierr.ne.0)then
      write(6,340) trim(modfle)
340      format(' Cannot rewind file ',a)
      go to 9890
    end if

! -- The model-generated bore sample file is next re-read and sample values
!    stored.

    iline=0
    loc=0                !loc is an array
    ind=0
    atemp1old=' '
    read_model_sample_file_1: do
      iline=iline+1
      read(modunit,'(a)',end=390) cline
      cols=5
      call linesplit(ifail,5)
      if(ifail.lt.0) cycle read_model_sample_file_1
      if(ifail.gt.0)then
        cols=4
        call linesplit(ifail,4)
        if(ifail.gt.0)then
          call num2char(iline,aline)
          write(amessage,360) trim(aline),trim(modfle)
360          format(' Insufficient entries at line ',a,' of file ',a)
          go to 9890
        end if
      end if
      atemp1=cline(left_word(1):right_word(1))
      call casetrans(atemp1,'hi')
      atemp1=adjustl(atemp1)
      if(atemp1.eq.atemp1old)then
        if(.not.active)then
          cycle read_model_sample_file_1
        end if
        ind=ind+1
      else
        atemp1old=atemp1
        do i=1,nobsid
          if(atemp1.eq.obsid(i))then
            ind=ind+1
            loc(i)=ind
            active=.true.
            go to 370
          end if
        end do
        active=.false.
        cycle read_model_sample_file_1
      end if
370   call read_rest_of_sample_line(ifail,cols,ndays(ind),           &
            nsecs(ind),value(ind),iline,modfle)
      if(ifail.ne.0) go to 9900
      if(value(ind).lt.-1.0e38)then
        ind=ind-1
        nmod(i)=nmod(i)-1
      endif
    end do read_model_sample_file_1
390 close(unit=modunit)

! --  Next the observation bore sample file is read line by line and entries
!     for the output bore sample file written where appropriate.

    iobs=0
    iout=0
    iline=0
    nout=0                    ! nout is an array
    atemp1old=' '
    read_obs_file: do
      iline=iline+1
      read(obsunit,'(a)',end=500) cline
      cols=5
      call linesplit(ifail,5)
      if(ifail.lt.0) cycle read_obs_file
      if(ifail.gt.0)then
        cols=4
        call linesplit(ifail,4)
        if(ifail.ne.0) then
          call num2char(iline,aline)
          write(amessage,360) trim(aline),trim(obsfle)
          go to 9890
        end if
      end if
      atemp1=cline(left_word(1):right_word(1))
      call casetrans(atemp1,'hi')
      atemp1=adjustl(atemp1)
      if(atemp1.ne.atemp1old)then
        iobs=iobs+1
        iid = 1
        atemp1old=atemp1
      end if
      if(nmod(iobs).eq.0) cycle read_obs_file
      call read_rest_of_sample_line(ifail,cols,intdays,intsecs,intvalue, &
            iline,obsfle)
      if(ifail.ne.0) go to 9900
      if(intvalue.lt.-1.0e38) cycle read_obs_file
! shultz enters this here...
      obsvalue = intvalue      
! end shultz
      ii=loc(iobs)
      call time_interp(ifail,nmod(iobs),ndays(ii),nsecs(ii),value(ii),   &
            intdays,intsecs,1.0e30,elim,intvalue)
      if(ifail.ne.0)then
        write(amessage,380) trim(modfle)
380        format(' Problem with sample file ',a,'; run SMPCHEK for ',      &
        'more information.')
        go to 9890
      end if
      if(intvalue.gt.-1.0e30)then
        iout=iout+1
        call newdate(intdays,1,1,1970,dd,mm,yy)
        hhh=intsecs/3600
        mmm=(intsecs-hhh*3600)/60
        sss=intsecs-hhh*3600-mmm*60
        if(datespec.eq.1) then
          write(outunit,400) trim(atemp1),dd,mm,yy,hhh,mmm,sss,intvalue
400          format(1x,a,10x,i2.2,'/',i2.2,'/',i4.4,3x,i2.2,':',i2.2,':',   &
          i2.2,3x,1pg15.8)
        else
          write(outunit,400) trim(atemp1),mm,dd,yy,hhh,mmm,sss,intvalue
        endif
        nout(iobs)=nout(iobs)+1
!**********************************************************
!       this section added by Steve Shultz/CH2M
!        writes the instruction file
!**********************************************************
        if (iwriteins.eq.1) then
            write(insunit,450) trim(atemp1),iid
450            format('l1  [',a,'_',I4.4,']37:56')
            write(pcfunit,470) trim(atemp1),iid,obsvalue
470            format(a,'_',I4.4,'    ',1pg15.8)
            iid = iid + 1
        end if 
      endif
    end do read_obs_file

500 call num2char(iout,anum)
!    write(amessage,510) trim(anum),trim(outfle)
!510 format(' - ',a,' data lines written to new bore sample file ',a)
    write(amessage,510) iout,trim(outfle)
510 format(' - ',i7,' data lines written to new bore sample file ',a)
    call write_message
    close(unit=obsunit)
    close(unit=modunit)

    iflag=0
    do i=1,nobsid
      if(nmod(i).eq.0) then
        iflag=1
        exit
      end if
    end do
    if(iflag.ne.0)then
      write(amessage,540) trim(obsfle),trim(modfle)
540   format(' The following identifiers cited in observaton sample ',   &
      'file ',a,' are either uncited in model observation file ',a,      &
      ' or are all x-affected in that file.')
      call write_message(leadspace='yes')
      amessage=' '
      j=4
      do i=1,nobsid
        if(nmod(i).eq.0)then
          write(amessage(j:),'(a)') trim(obsid(i))
          j=j+11
          if(j.gt.69)then
            call write_message
            amessage=' '
            j=4
          end if
        end if
      end do
      if(j.ne.4) call write_message
    end if

    iflag=0
    do i=1,nobsid
      if((nmod(i).ne.0).and.(nout(i).eq.0)) then
        iflag=1
        exit
      end if
    end do
    if(iflag.ne.0)then
      write(amessage,580) trim(obsfle),trim(modfle)
580   format(' The following identifiers cited in observaton sample ',   &
      'file ',a,' are cited in model sample file ',a,'. However no ',    &
      'observation times are within model simulation time frame.')
      call write_message(leadspace='yes')
      amessage=' '
      j=4
      do i=1,nobsid
        if((nmod(i).ne.0).and.(nout(i).eq.0))then
          write(amessage(j:),'(a)') trim(obsid(i))
          j=j+11
          if(j.gt.69)then
            call write_message
            amessage=' '
            j=4
          end if
        end if
      end do
      if(j.ne.4) call write_message
    end if

    close(unit=modunit)
    close(unit=obsunit)
    close(unit=outunit)
    
    if (iwriteins.eq.1) then
        close(unit=insunit)
        close(unit=pcfunit)
    end if



    go to 9900

9000    write(6,9010) trim(obsfle)
9010    format(/,' Internal error. SMP2SMP Failed: Observation file ',a,' could not be opened',/)
        stop
9020    write(6,9030) trim(modfle)
9030    format(/,' Internal error. SMP2SMP Failed: Model file ',a,' could not be opened',/)
        stop

9890    call write_message(leadspace='yes')
9900    call close_files

        deallocate(ndays,nsecs,value,stat=ierr)
    write(6,*)

end subroutine smp2smp


subroutine smp2smpD(obsfle,obsunit,modfle,modunit,outfle,outunit,elim, &
                    iwriteins,insfle,insunit,pcffle,pcfunit,headdiffs,hdiffile,ihydtype)

! -- Program SMP2SMP builds a bore sample file by temporally interpolating
!    samples contained in one bore sample file to the dates and times of
!    samples recorded in another bore sample file.

!
! This piece of code, written by John Doherty (part of PEST Utilities)
! has been modified to be a subroutine by Steve Shultz/CH2M HILL
! Modifications include:
! 1. passing file names, units, and extrapolation to the subroutine
! 2. addition of an option to write an instruction file, based on
!    the EXISTING model output file (i.e., must run model prior to
!    being able to create ins file)

	use defn
	use inter
	implicit none

	logical              :: active
	integer, parameter   :: MAXID=800000
      integer, parameter   :: maxhdiff=500                              ! MJT
	integer              :: ifail,idate,obsunit,modunit,iline,nobsid,i,  &
                              l,cols,maxmod,ierr,ind,iout,iobs,intdays,    &
	                        intsecs,ii,dd,mm,yy,hhh,mmm,sss,j,iflag,     &
	                        outunit,iheader,iwriteins,insunit,iid,pcfunit
	integer, dimension (MAXID)                        :: nmod,loc,nout
	integer, allocatable, dimension (:)               :: ndays,nsecs
      integer, allocatable,dimension (:,:,:) :: hhhd,sssd,dd_d,mmmd,yy_d,mm_d   ! MJT

	real                 :: elim

        double precision     :: intvalue, obsvalue
        double precision, allocatable, dimension (:)      :: value
        real(kind=4),allocatable, dimension (:,:,:)       ::  hdvalue      !MJT

        character (len=5)    :: anum,aline
        character (len=25)   :: atemp1,atemp1old                             !CFB
        character (len=25)   :: atemp,atempold                               !CFB
        character (len=80)   :: obsfle,modfle,outfle,specfle,insfle,pcffle,hdiffile !MJT
        character (len=25), dimension(MAXID)              :: obsid           !CFB
        logical :: headdiffs,found                                     !MJT
        integer :: hdiffunit,nhdiff,ir,nmaxobs,hline,n,m,ihydtype,m2   !MJT
        character(len=25),allocatable, dimension(:,:)  :: hdiffpairs   !MJT, !CFB
        hdiffunit = 189                                                !MJT

! ** obsfle = observation bore sample file
! ** modfle = model generated bore sample file
! ** outfle = output file, model data at obs times
! ** anum = threshold

	call read_settings(ifail,idate,iheader)
	if(ifail.eq.1) then
	  write(amessage,7)
7	  format(' A settings file (settings.fig) was not found in the ', &
	  'current directory.')
	  call write_message
	  go to 9900
	else if(ifail.eq.2) then
	  write(amessage,8)
8	  format(' Error encountered while reading settings file settings.fig')
	  call write_message
	  go to 9900
    endif

    if((idate.ne.0).or.(datespec.eq.0)) then
	  write(amessage,9)
9	  format(' Cannot read date format from settings file ', &
	  'settings.fig')
	  call write_message
	  go to 9900
	end if

	open(unit=modunit,file=modfle,status='old',err=9020)
	open(unit=obsunit,file=obsfle,status='old',err=9000)
    if(headdiffs)open(unit=hdiffunit,file=hdiffile,status='old',err=9040)  !MJT
	open(unit=outunit,file=outfle)

	if (iwriteins.eq.1) then
		open(unit=insunit,file=insfle)
		open(unit=pcfunit,file=pcffle)
		write(insunit,100)
100		format('pif #')
	end if

! -- First the observation bore sample file is read to obtain all bore_ids.

	atempold=' '
	iline=0
	nobsid=0
	do
	  iline=iline+1
	  read(obsunit,*,end=210) atemp
	  atemp=adjustl(atemp)
	  call casetrans(atemp,'hi')
	  l=len_trim(atemp)
	  if(l.gt.25)then                                                           !CFB
	    call num2char(iline,aline)
	    write(amessage,160) trim(aline),trim(obsfle)
160	    format(' Identifier greater than 20 characters at ',  &                 !CFB
	    'line ',a,' of file ',a)
	    go to 9890
      end if

      if(nobsid.eq.0)then   ! first observation
	    nobsid=nobsid+1
	    if(nobsid.gt.MAXID)then
	      write(amessage,180) trim(obsfle)
180	      format(' Too many bores cited in file ',a,                    &
	      ': increase MAXID and re-compile program.')
	      go to 9890
	    end if
	    obsid(nobsid)=atemp(1:25)                                              !CFB
	    atempold=atemp
	  else if(atemp.eq.atempold) then
	    continue
	  else
	    call casetrans(atemp,'hi')
	    do i=1,nobsid
	      if(atemp.eq.obsid(i))then
	        call num2char(iline,aline)
	        write(amessage,200) trim(aline),trim(obsfle)
200	        format(' Identifier used previously at line ',a,          &
	        ' of file ',a)
	        go to 9890
	      end if
	    end do
	    nobsid=nobsid+1
	    if(nobsid.gt.MAXID)then
	      write(amessage,180) trim(obsfle)
	      go to 9890
	    end if
	    obsid(nobsid)=atemp(1:25)                                           !CFB
	    atempold=atemp
	  end if
	end do
210	rewind(unit=obsunit,iostat=ierr)
	if(ierr.ne.0)then
        write(amessage,220) trim(obsfle)
220     format(' Cannot rewind file ',a)
	  go to 9890
	end if

! -- next the head difference file is read if necessary

     if(ihydtype.eq.GWHEAD) then
        if(headdiffs)then
          hline=0
          do
            read(hdiffunit,*,end=240) atemp
            hline=hline+1
            atemp=adjustl(atemp)
            call casetrans(atemp,'hi')
            l=len_trim(atemp)
            if(l.gt.25)then                                                          !CFB
              call num2char(hline,aline)
              write(amessage,230) trim(aline),trim(hdiffile)
230           format(' Identifier greater than 20 characters at ',  &                !CFB
              'line ',a,' of file ',a)
              go to 9890
            end if
          end do
240       rewind(unit=hdiffunit,iostat=ierr)
          ! now we know hline allocate character arrays
          if(hline.eq.0) stop 'No pairs read from head difference file (SMP2SMP - MJT)'
          nhdiff=hline
          allocate(hdiffpairs(2,nhdiff),hdvalue(maxhdiff,nhdiff,2),  &
                   hhhd(maxhdiff,nhdiff,2),sssd(maxhdiff,nhdiff,2),  &
                   dd_d(maxhdiff,nhdiff,2),mmmd(maxhdiff,nhdiff,2),  &
                   yy_d(maxhdiff,nhdiff,2),mm_d(maxhdiff,nhdiff,2),  &
                   stat=ierr)
          if(ierr.ne.0) stop 'Error allocating head difference arrays (SMP2SMP - MJT)'
          hhhd=0
          sssd=0
          dd_d=0
          mmmd=0
          yy_d=0
          mm_d=0
          ! now read the head pairs
          do n=1,nhdiff
            read(hdiffunit,*,err=9060) hdiffpairs(1,n),hdiffpairs(2,n)
            call casetrans(hdiffpairs(1,n),'hi')
            call casetrans(hdiffpairs(2,n),'hi')
            ! check the IDs are not the same
            if(hdiffpairs(1,n).eq.hdiffpairs(2,n)) then
              write(amessage,245) trim(hdiffpairs(2,n)),trim(hdiffile),nhdiff
245           format(' Identifier ',a,' appears twice in file ',a,' at line ',i)
              go to 9890
            else
              !write(*,*) trim(hdiffpairs(1,n)),trim(hdiffpairs(2,n))
            end if
          end do
          ! check all these IDs are listed in the obs SMP file
          do n=1,nhdiff
            found=.false.
            do i=1,nobsid
              if(trim(obsid(i)).eq.trim(hdiffpairs(1,n))) found=.true.
            end do
            if (.not.found) then
              write(amessage,246) trim(hdiffpairs(1,n)),trim(hdiffile),trim(obsfle)
246           format(' Identifier ',a,' appears in file ',a,' but not file ',a)
              go to 9890
            end if
            found=.false.
            do i=1,nobsid
              if(trim(obsid(i)).eq.trim(hdiffpairs(2,n))) found=.true.
            end do
            if (.not.found) then
              write(amessage,246) trim(hdiffpairs(2,n)),trim(hdiffile),trim(obsfle)
              go to 9890
            end if
          end do
        end if
      end if

! -- Next the model-generated bore sample file is read to get some
!    specifications.

	nmod=0				!nmod is an array
	iline=0
	atemp1old=' '
	read_model_sample_file: do
	  iline=iline+1
	  read(modunit,'(a)',end=300) cline
	  call linesplit(ifail,1)
	  if(ifail.lt.0) cycle read_model_sample_file
	  if(right_word(1)-left_word(1).gt.24) then                              !CFB
	    call num2char(iline,aline)
	    write(amessage,280) trim(aline),trim(modfle)
280	    format(' Identifier greater than 20 characters in length at ',  &    !CFB
	    'line ',a,' of file ',a)
	    go to 9890
	  end if
	  atemp1=cline(left_word(1):right_word(1))
	  atemp1=adjustl(atemp1)
	  call casetrans(atemp1,'hi')
	  if(atemp1.eq.atemp1old)then
	    if(active) then
	      nmod(iobs)=nmod(iobs)+1
	    else
	      cycle read_model_sample_file
	    end if
	  else
	    atemp1old=atemp1
	    do i=1,nobsid
	      if(atemp1.eq.obsid(i))then
	        active=.true.
	        iobs=i
	        if(nmod(iobs).ne.0)then
	          call num2char(iline,aline)
	          write(amessage,290) trim(aline),trim(modfle)
290	          format(' Identifiers with same name not in juxtaposition ',&
	          'at line ',a,' of file ',a)
	          go to 9890
	        end if
		nmod(iobs)=1
	        cycle read_model_sample_file
	      end if
	    end do
	    active=.false.
	  end if
	end do read_model_sample_file

! -- Space is allocated for the storage of all pertinent values from the
!    model-generated bore sample file.

300	maxmod=0
	do i=1,nobsid
	  maxmod=maxmod+nmod(i)
	end do
	if(maxmod.eq.0)then
	  write(amessage,310) trim(modfle),trim(obsfle)
310	  format(' No identifiers cited in file ',a,' correspond to ',      &
	  'any of the identifiers in file ',a)
	  go to 9890
	end if
	allocate(ndays(maxmod),nsecs(maxmod),value(maxmod),stat=ierr)
	if(ierr.ne.0)then
	  write(amessage,330)
330	  format(' Cannot allocate sufficient memory to continue execution.')
	  go to 9890
	end if
	rewind(unit=modunit,iostat=ierr)
	if(ierr.ne.0)then
	  write(6,340) trim(modfle)
340	  format(' Cannot rewind file ',a)
	  go to 9890
	end if

! -- The model-generated bore sample file is next re-read and sample values stored

	iline=0
	loc=0				!loc is an array
	ind=0
	atemp1old=' '
	read_model_sample_file_1: do
	  iline=iline+1
	  read(modunit,'(a)',end=390) cline
	  cols=5
	  call linesplit(ifail,5)
	  if(ifail.lt.0) cycle read_model_sample_file_1
	  if(ifail.gt.0)then
	    cols=4
	    call linesplit(ifail,4)
	    if(ifail.gt.0)then
	      call num2char(iline,aline)
	      write(amessage,360) trim(aline),trim(modfle)
360	      format(' Insufficient entries at line ',a,' of file ',a)
	      go to 9890
	    end if
	  end if
	  atemp1=cline(left_word(1):right_word(1))
	  call casetrans(atemp1,'hi')
	  atemp1=adjustl(atemp1)
	  if(atemp1.eq.atemp1old)then
	    if(.not.active)then
	      cycle read_model_sample_file_1
	    end if
	    ind=ind+1
	  else
	    atemp1old=atemp1
	    do i=1,nobsid
	      if(atemp1.eq.obsid(i))then
	        ind=ind+1
	        loc(i)=ind
	        active=.true.
	        go to 370
	      end if
	    end do
	    active=.false.
	    cycle read_model_sample_file_1
	  end if
370	  call read_rest_of_sample_line(ifail,cols,ndays(ind),           &
	  nsecs(ind),value(ind),iline,modfle)
	  if(ifail.ne.0) go to 9900
	  if(value(ind).lt.-1.0e38)then
	    ind=ind-1
	    nmod(i)=nmod(i)-1
	  endif
	end do read_model_sample_file_1
390	close(unit=modunit)

! --  Next the observation bore sample file is read line by line and entries
!     for the output bore sample file written where appropriate.

	iobs=0
	iout=0
	iline=0
	nout=0					! nout is an array
	atemp1old=' '
	read_obs_file: do
	  iline=iline+1
	  read(obsunit,'(a)',end=500) cline
	  cols=5
	  call linesplit(ifail,5)
	  if(ifail.lt.0) cycle read_obs_file
	  if(ifail.gt.0)then
	    cols=4
	    call linesplit(ifail,4)
	    if(ifail.ne.0) then
	      call num2char(iline,aline)
	      write(amessage,360) trim(aline),trim(obsfle)
	      go to 9890
	    end if
	  end if
	  atemp1=cline(left_word(1):right_word(1))
	  call casetrans(atemp1,'hi')
	  atemp1=adjustl(atemp1)
	  if(atemp1.ne.atemp1old)then
	    iobs=iobs+1
		iid = 1
	    atemp1old=atemp1
	  end if
	  if(nmod(iobs).eq.0) cycle read_obs_file
        call read_rest_of_sample_line(ifail,cols,intdays,intsecs,intvalue,iline,obsfle)
	  if(ifail.ne.0) go to 9900
	  if(intvalue.lt.-1.0e38) cycle read_obs_file
! shultz enters this here...
	  obsvalue = intvalue
! end shultz
	  ii=loc(iobs)
        call time_interp(ifail,nmod(iobs),ndays(ii),nsecs(ii),value(ii),intdays,intsecs,1.0e30,elim,intvalue)
	  if(ifail.ne.0)then
	    write(amessage,380) trim(modfle)
380       format(' Problem with sample file ',a,'; run SMPCHEK for more information.')
	    go to 9890
	  end if
	  if(intvalue.gt.-1.0e30)then
	    iout=iout+1
	    call newdate(intdays,1,1,1970,dd,mm,yy)
	    hhh=intsecs/3600
	    mmm=(intsecs-hhh*3600)/60
	    sss=intsecs-hhh*3600-mmm*60
	    if(datespec.eq.1) then
	      write(outunit,400) trim(atemp1),dd,mm,yy,hhh,mmm,sss,intvalue
400         format(1x,a,10x,i2.2,'/',i2.2,'/',i4.4,3x,i2.2,':',i2.2,':',i2.2,3x,1pg15.8)
	    else
	      write(outunit,400) trim(atemp1),mm,dd,yy,hhh,mmm,sss,intvalue
	    endif
	    nout(iobs)=nout(iobs)+1
          if (iwriteins.eq.1) then                                      ! SS CH2M
              write(insunit,450) trim(atemp1),iid                       ! SS CH2M
450           format('l1  [',a,'_',I4.4,']37:56')                       ! SS CH2M
              write(pcfunit,470) trim(atemp1),iid,obsvalue              ! SS CH2M
470           format(a,'_',I4.4,'    ',1pg15.8)                         ! SS CH2M
              iid = iid + 1                                             ! SS CH2M
          end if                                                        ! SS CH2M
          ! store for head differences?                                 ! MJT
          if(ihydtype.eq.GWHEAD)then
            if(headdiffs)then                                           ! MJT
              do n=1,nhdiff                                             ! MJT
                do ir=1,2                                               ! MJT
                  if(trim(atemp1).eq.trim(hdiffpairs(ir,n))) then       ! MJT
                    !write(*,*)  trim(hdiffpairs(ir,n))
                    hhhd(nout(iobs),n,ir)=hhh                           ! MJT
                    sssd(nout(iobs),n,ir)=sss                           ! MJT
                    dd_d(nout(iobs),n,ir)=dd                            ! MJT
                    mmmd(nout(iobs),n,ir)=mmm                           ! MJT
                    yy_d(nout(iobs),n,ir)=yy                            ! MJT
                    mm_d(nout(iobs),n,ir)=mm                            ! MJT
                    hdvalue(nout(iobs),n,ir)=intvalue
                  end if                                                ! MJT
                end do
              end do                                                    ! MJT
            end if                                                      ! MJT
          end if
        endif                                                           ! MJT
      end do read_obs_file                                              ! MJT
      ! head differences?                                               ! MJT
500   nmaxobs=maxval(nout)                                              ! MJT
      if(ihydtype.eq.GWHEAD) then
          if(headdiffs)then                                             ! MJT
            do n=1,nhdiff                                               ! MJT
              iid=1
              do m=1,nmaxobs                                            ! MJT
                do m2=1,nmaxobs                                         ! MJT
                  if ((dd_d(m,n,1).eq.dd_d(m2,n,2)).and.    &           ! MJT
                      (yy_d(m,n,1).eq.yy_d(m2,n,2)).and.    &           ! MJT
                      (mm_d(m,n,1).eq.mm_d(m2,n,2)).and.    &
                      (mm_d(m,n,1).ne.0)) then                          ! MJT
                      iout=iout+1
                      if(datespec.eq.1) then
                        write(outunit,400) hdiffpairs(1,n)(1:5)//'_'//hdiffpairs(2,n)(1:5), &
                                           dd_d(m,n,1),mm_d(m,n,1),yy_d(m,n,1), &
                                           hhhd(m,n,1),mmmd(m,n,1),sssd(m,n,1),hdvalue(m,n,1)-hdvalue(m2,n,2)
                      else
                        write(outunit,400) hdiffpairs(1,n)(1:5)//'_'//hdiffpairs(2,n)(1:5), &
                                           mm_d(m,n,1),dd_d(m,n,1),yy_d(m,n,1), &
                                           hhhd(m,n,1),mmmd(m,n,1),sssd(m,n,1),hdvalue(m,n,1)-hdvalue(m2,n,2)
                      endif
                      ! write instruction file entries for head differences?  ! MJT
                      if (iwriteins.eq.1) then                                ! SS CH2M
                        write(insunit,450) hdiffpairs(1,n)(1:5)//'_'//hdiffpairs(2,n)(1:5),iid          ! SS CH2M
                        write(pcfunit,470) hdiffpairs(1,n)(1:5)//'_'//hdiffpairs(2,n)(1:5),iid,-999.    ! SS CH2M
                        iid = iid + 1                                         ! SS CH2M
                      end if                                                  ! SS CH2M
                  end if                                                      ! MJT
                end do
              end do                                                          ! MJT
            end do                                                            ! MJT
          end if
      end if
      ! all done
!500
    call num2char(iout,anum)
	write(amessage,510) trim(anum),trim(outfle)
510	format(' - ',a,' data lines written to new bore sample file ',a)
	call write_message
	close(unit=obsunit)
	close(unit=modunit)

	iflag=0
	do i=1,nobsid
	  if(nmod(i).eq.0) then
	    iflag=1
	    exit
	  end if
	end do
	if(iflag.ne.0)then
	  write(amessage,540) trim(obsfle),trim(modfle)
540	  format(' The following identifiers cited in observaton sample ',   &
	  'file ',a,' are either uncited in model observation file ',a,      &
	  ' or are all x-affected in that file.')
	  call write_message(leadspace='yes')
	  amessage=' '
	  j=4
	  do i=1,nobsid
	    if(nmod(i).eq.0)then
	      write(amessage(j:),'(a)') trim(obsid(i))
	      j=j+len(trim(obsid(i)))+1
	      if(j.gt.69)then
	        call write_message
	        amessage=' '
	        j=4
	      end if
	    end if
	  end do
	  if(j.ne.4) call write_message
	end if

	iflag=0
	do i=1,nobsid
	  if((nmod(i).ne.0).and.(nout(i).eq.0)) then
	    iflag=1
	    exit
	  end if
	end do
	if(iflag.ne.0)then
	  write(amessage,580) trim(obsfle),trim(modfle)
580	  format(' The following identifiers cited in observaton sample ',   &
	  'file ',a,' are cited in model sample file ',a,'. However no ',    &
	  'observation times are within model simulation time frame.')
	  call write_message(leadspace='yes')
	  amessage=' '
	  j=4
	  do i=1,nobsid
	    if((nmod(i).ne.0).and.(nout(i).eq.0))then
	      write(amessage(j:),'(a)') trim(obsid(i))
	      j=j+11
	      if(j.gt.69)then
	        call write_message
	        amessage=' '
	        j=4
	      end if
	    end if
	  end do
	  if(j.ne.4) call write_message
	end if

	close(unit=modunit)
	close(unit=obsunit)
      if(headdiffs)close(hdiffunit)  !MJT
	close(unit=outunit)

	if (iwriteins.eq.1) then
		close(unit=insunit)
		close(unit=pcfunit)
	end if



	go to 9900

9000	write(6,9010) trim(obsfle)
9010	format(/,' Internal error. SMP2SMP Failed: Observation file ',a,' could not be opened',/)
		stop
9020	write(6,9030) trim(modfle)
9030  format(/,' Internal error. SMP2SMP Failed: Model file ',a,' could not be opened',/)
		stop
9040  write(6,9050) trim(hdiffile)
9050  format(/,' Internal error. SMP2SMP Failed: List file ',a,' could not be opened',/)
		stop
9060  write(6,9070) trim(hdiffile)
9070  format(/,' Internal error. SMP2SMP Failed: List file ',a,' could not be correctly read',/)
		stop


9890	call write_message(leadspace='yes')
9900	call close_files
	deallocate(ndays,nsecs,value,stat=ierr)
	write(6,*)

end subroutine smp2smpD



subroutine readOneLine(infile,iinfile,cline)
!   read and return one line from a file, skipping lines that are commented out

    implicit none

    character (len=*), intent(in)  :: infile
    integer,           intent(in)  :: iinfile
    character (len=*), intent(out) :: cline

100  read(iinfile,'(a)',end=900) cline
        if((cline(1:1).eq.'c').or.(cline(1:1).eq.'C').or.&
            (cline(1:1).eq.'*').or.(cline(1:1).eq.'#')) go to 100    ! skip comment line

    go to 999 ! skip past error statements to end of subroutine
    
    !================================================================================
    ! error statements
900 write(6,901) trim(infile)
901 format(/,' *** Unexpected end to file ',a,' ***',/)
      stop
999 end subroutine readOneLine


!================================================================================
     
