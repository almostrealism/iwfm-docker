
  
  
  MODULE DEFN
    integer, parameter                  :: NUM_WORD_DIM=100
    integer, dimension(NUM_WORD_DIM)    :: left_word,right_word
    character (len=300)                 :: cline
    
  END MODULE DEFN
  
  
  
  MODULE INTER

! -- Contains interface blocks for all subprograms.


! utility subprograms ------->

   interface char2num

    subroutine a2i(ifail,string,num)
      integer, intent(out)          :: ifail
      character (len=*), intent(in) :: string
      integer, intent(out)          :: num
    end subroutine a2i
  
    subroutine a2d(ifail,string,num)
      integer, intent(out)          :: ifail
      character (len=*), intent(in) :: string
      double precision, intent(out) :: num
    end subroutine a2d

end interface
    
    
interface



    subroutine casetrans(string,hi_or_lo)
      character (len=*), intent(inout)        :: string
      character (len=*), intent(in)           :: hi_or_lo
    end subroutine casetrans

    subroutine sub_error(subname)
      character (len=*)               ::subname
    end subroutine sub_error

    integer function nextunit()
    end function nextunit

    subroutine close_files
    end subroutine close_files

    subroutine open_input_file(ifail,aprompt,infile,inunit,file_format,form_prompt, &
          fformat)
          integer, intent(out)                     :: ifail
          character (len=*), intent(in)            :: aprompt
          character (len=*), intent(out)           :: infile
          integer, intent(out)                     :: inunit
          character (len=*), intent(in), optional  :: file_format
          character (len=*), intent(in), optional  :: form_prompt
          character (len=*), intent(out), optional :: fformat
    end subroutine open_input_file

    subroutine open_output_file(ifail,aprompt,outfile,outunit)
      integer, intent(out)          :: ifail
      character (len=*)             :: aprompt,outfile
      integer, intent(out)          :: outunit
    end subroutine open_output_file

    subroutine wrtsig(ifail,val,word,nw,precis,tval,nopnt)
      integer               :: ifail,nw,precis,nopnt
      double precision      :: val,tval
      character (len=*)     :: word
    end subroutine wrtsig

    subroutine readfig(specfile,coordfile,sampfile,pumpfile,pilotfile)
      character (len=*), intent(out)                :: specfile
      character (len=*), intent(out), optional      :: coordfile,sampfile,&
                               pumpfile,pilotfile
    end subroutine readfig

    subroutine read_settings(ifail,iidate,iheader)
      integer, intent(out)    :: ifail,iidate,iheader
    end subroutine read_settings

    subroutine char_add(astring,achar)
          character (len=*), intent(inout)        :: astring
          character (len=*), intent(in)           :: achar
    end subroutine char_add

    subroutine int2alph(inum,alph,nsig)
      integer, intent(in)            :: inum
      character (len=*), intent(out)    :: alph
      integer, optional, intent(in)        :: nsig
    end subroutine int2alph

end interface

! interpolation subprograms ------->

interface

 

    subroutine point_interp(ncol,nrow,thresh,fac1,fac2,fac3,fac4, &
    icellno,jcellno,bhead,rarray,imethod)
      integer, intent(in)                   :: ncol,nrow
      real, intent(in)                      :: thresh
      real, intent(in)                      :: fac1,fac2,fac3,fac4
      integer, intent(in)                   :: icellno,jcellno
      real, intent(out)                     :: bhead
      real, dimension(0:ncol+1,0:nrow+1), intent(in)  :: rarray
      character (len=*), intent(in), optional         :: imethod
    end subroutine point_interp

end interface

! reading-a-file subprograms ------->

interface

    subroutine SKIP(IFILE)
      INTEGER, intent(in)             ::IFILE    
    end subroutine SKIP

    subroutine linesplit(ifail,num)
      integer, intent(out)            :: ifail
      integer, intent(in)          :: num
    end subroutine linesplit

    subroutine multisplit(IFAIL,NUM,LW,RW,CLINE) 
      integer, intent(out)            ::IFAIL
      integer, intent(in)                            ::NUM
      integer, dimension(0:NUM),intent(out)            ::LW,RW
      character (len=*), intent(in)   :: cline
    end subroutine multisplit

    subroutine tabrem(CLINE)
      character (len=*), intent(in)   :: cline
    end subroutine tabrem

    subroutine intread(IFAIL,CLINE,iTEMP)
      integer, intent(out)            ::IFAIL
      character (len=*), intent(in)   ::cline
      integer, intent(out)            ::iTEMP
    end subroutine intread

    integer function char2int(ifail,num)
      integer, intent(in)             :: num
      integer, intent(out)            :: ifail
    end function char2int

    real function char2real(ifail,num)
      integer, intent(in)             :: num
      integer, intent(out)            :: ifail
    end function char2real

    double precision function char2double(ifail,num)
      integer, intent(in)             :: num
      integer, intent(out)            :: ifail
    end function char2double

        subroutine getfile(ifail,cline,filename,ibeg,iend)
          integer, intent(out)            :: ifail
          integer, intent(in)             :: ibeg
          integer, intent(inout)          :: iend
          character (len=*), intent(in)   :: cline
          character (len=*), intent(out)  :: filename
       end subroutine getfile

       subroutine addquote(afile,aqfile)
          character (len=*), intent(in)   :: afile
          character (len=*), intent(out)  :: aqfile
       end subroutine addquote

end interface

!integer array subprograms ------->

interface

    subroutine read_integer_array(ifail,aprompt,array,pm_header,rows, &
    columns,defaultfile)
      integer, intent(out)                    :: ifail
      character (len=*), intent(inout)        :: aprompt
      integer, intent(out),dimension(:,:)     :: array
      character (len=*), intent(in), optional :: pm_header
      integer, intent(in), optional           :: rows,columns
          character (len=*), optional             :: defaultfile
    end subroutine read_integer_array

    subroutine write_integer_array(ifail,aprompt,array,pm_header,rows, &
    columns)
      integer, intent(out)              :: ifail
      character (len=*), intent(inout)      :: aprompt
      integer, intent(in), dimension(:,:)      :: array
      character (len=*), intent(in), optional :: pm_header
      integer, intent(in), optional          :: rows,columns
    end subroutine write_integer_array

end interface

! real array subprograms ------->

interface

    subroutine read_real_array(ifail,aprompt,array,pm_header,rows,columns)
      integer, intent(out)                    :: ifail
      character (len=*), intent(inout)        :: aprompt
      real, intent(out),dimension(:,:)        :: array
      character (len=*), intent(in), optional :: pm_header
      integer, intent(in), optional           :: rows,columns
    end subroutine read_real_array

    subroutine write_real_array(ifail,aprompt,array,pm_header,rows,columns,&
    binary_header,atype,ntrans,kstp,kper,pertim,totim,text,ncol,nrow,ilay,istage, &
        realfile,aaformat)
      integer, intent(out)                    :: ifail
      character (len=*), intent(inout)        :: aprompt
      real, intent(in),dimension(:,:)         :: array
      character (len=*), intent(in), optional :: pm_header
      integer, intent(in), optional           :: rows,columns
          character (len=*), optional             :: binary_header
          character (len=*), optional             :: atype
          integer, optional                       :: ntrans,kstp,kper
          real, optional                          :: pertim,totim
          character (len=16),optional             :: text
          integer, optional                       :: ncol,nrow,ilay
          integer, intent(in), optional           :: istage
          character (len=*), optional             :: realfile
          character (len=*), optional             :: aaformat
        end subroutine write_real_array

   

    subroutine write_real_table_file(ifail,outunit,outfile,intarray,realarray)
          use defn
          integer, intent(out)                    :: ifail
          integer, intent(in)                     :: outunit
          character (len=*), intent(in)           :: outfile
          integer, dimension(:,:), intent(in)     :: intarray
          real, dimension(:,:), intent(in)        :: realarray
    end subroutine write_real_table_file

end interface

! message subprograms ------->

interface

    subroutine write_initial_message(leadspace,endspace)
      character (len=*), intent(in), optional :: leadspace,endspace
    end subroutine write_initial_message

    subroutine write_message(increment,iunit,error,leadspace,endspace)
      integer, intent(in), optional           ::increment,iunit
      character (len=*), intent(in), optional ::error,leadspace,endspace
    end subroutine write_message

end interface

! grid subprograms ------->


! bore data manipulation subprograms ------->

interface

    subroutine read_bore_coord_file(ifail,aprompt)
      integer, intent(out)            :: ifail
      character (len=*), intent(in)   :: aprompt
    end subroutine read_bore_coord_file

    subroutine read_bore_list_file(ifail,aprompt,coord_check)
      integer, intent(out)                    :: ifail
      character (len=*), intent(in)           :: aprompt
      character (len=*), intent(in), optional :: coord_check
    end subroutine read_bore_list_file

    subroutine free_bore_mem
    end subroutine free_bore_mem

    subroutine read_rest_of_sample_line(ifail,cols,ndays,nsecs,value, &
    iline,sampfile)
          integer, intent(out)            :: ifail
          integer, intent(in)             :: cols
          integer, intent(out)            :: ndays,nsecs
          double precision, intent(out)   :: value
          integer, intent(in)             :: iline
          character (len=*), intent(in)   :: sampfile
    end subroutine read_rest_of_sample_line

    subroutine read_rest_of_pump_line(ifail,ibore,ndays,nsecs,pumped, &
    iline,pmpfile)
          integer, intent(out)                    :: ifail
          integer, intent(inout)                  :: ibore
          integer, intent(out), dimension (:)     :: ndays,nsecs
          double precision, intent(inout), dimension(:)  :: pumped
          integer, intent(in)                     :: iline
          character (len=*), intent(in)           :: pmpfile
    end subroutine read_rest_of_pump_line

    subroutine time_interp(ifail,nbore,ndays,nsecs,value,intday, &
    intsec,rnear,rconst,valinterp,extrap,direction)
          integer, intent(out)                    :: ifail
          integer, intent(in)                     :: nbore
          integer, intent(in), dimension(nbore)   :: ndays,nsecs
          double precision, intent(in), dimension(nbore)   :: value
          integer, intent(in)                     :: intday,intsec
      real, intent(in)              :: rnear,rconst
          double precision, intent(out)           :: valinterp
      character (len=*), intent(in),optional  :: extrap
      character (len=*), intent(in),optional  :: direction
    end subroutine time_interp

    subroutine get_num_ids(ifail,iunit,afile,numid,maxsamp,ignore_x)
      integer, intent(out)                    :: ifail
          integer, intent(in)                     :: iunit
          character (len=*), intent(in)           :: afile
          integer, intent(out)                    :: numid,maxsamp
      character (len=*), intent(in), optional :: ignore_x
    end subroutine get_num_ids

    subroutine get_ids_and_interval(ifail,iunit,afile,nid,aid,ndays1, &
                                nsecs1,ndays2,nsecs2, ignore_x)
          integer, intent(out)                    :: ifail
          integer, intent(in)                     :: iunit
          character (len=*), intent(in)           :: afile
          integer, intent(in)                     :: nid
          character (len=*), intent(out)          :: aid(nid)
          integer, intent(out)                    :: ndays1(nid),nsecs1(nid), &
                                                     ndays2(nid),nsecs2(nid)
      character (len=*), intent(in), optional :: ignore_x
    end subroutine get_ids_and_interval

end interface

! geostat subprograms ------->

interface

    subroutine read_pilot_points_file(ifail,aprompt,accept_blank)
      integer, intent(out)                    :: ifail
      character (len=*), intent(in)           :: aprompt
          character (len=*), intent(in), optional :: accept_blank
    end subroutine read_pilot_points_file

    subroutine free_point_mem
    end subroutine free_point_mem

        subroutine read_structure_file_dim(ifail,structunit,numstruct,numvario,structfile)
          integer, intent(out)            :: ifail
          integer, intent(in)             :: structunit
          integer, intent(out)            :: numstruct,numvario
          character (len=*), intent(in)   :: structfile
        end subroutine read_structure_file_dim

      


        subroutine read_parameter_replacement_file(ifail,aprompt,ncol,nrow)
      integer, intent(out)            :: ifail
      character (len=*), intent(in)   :: aprompt
          integer, intent(in)             :: ncol,nrow
        end subroutine read_parameter_replacement_file


        subroutine free_replace_mem()
        end subroutine free_replace_mem

end interface


! date manipulation subprograms ------->

interface

    subroutine char2date(ifail,adate,dd,mm,yy)
          integer, intent(out)        :: ifail
          character (len=*), intent(in) :: adate
          integer, intent(out)         :: dd,mm,yy
    end subroutine char2date

    subroutine datestring(dd,mm,yy,hhh,mmm,sss,time,at,adate,atime)
      integer, intent(in)             :: dd,mm,yy,hhh,mmm,sss
      real, intent(in)                :: time
      character (len=1), intent(in)   :: at
      character (len=*), intent(out)  :: adate, atime
    end subroutine datestring



    integer function numdays(dr,mr,yr,d,m,y)
          integer, intent(in)     :: dr,mr,yr,d,m,y
    end function numdays

    integer function numsecs(h1,m1,s1,h2,m2,s2)
      integer, intent(in)     :: h1,m1,s1,h2,m2,s2
    end function numsecs

    subroutine char2time(ifail,adate,hh,mm,ss)
          integer, intent(out)        :: ifail
          character (len=*), intent(in) :: adate
          integer, intent(out)         :: hh,mm,ss
    end subroutine char2time

    subroutine time2char(ifail,hh,mm,ss,atime)
      integer, intent(out)            :: ifail
      integer, intent(in)             :: hh,mm,ss
      character (len=*), intent(out)  :: atime
    end subroutine time2char

    subroutine elapsdate(eltime,dayfactor,day1,mon1,year1,hour1,min1,sec1,&
      day2,mon2,year2,hour2,min2,sec2)
      real, intent(in)        :: eltime,dayfactor
      integer, intent(in)        :: day1,mon1,year1,hour1,min1,sec1
      integer, intent(out)        :: day2,mon2,year2,hour2,min2,sec2
    end subroutine elapsdate

    subroutine newdate(ndays,day1,mon1,year1,day2,mon2,year2)
      integer, intent(in)        :: ndays,day1,mon1,year1
      integer, intent(out)        :: day2,mon2,year2
    end subroutine newdate

    subroutine sectime(nsecs,sec,min,hour)
      integer, intent(in)   :: nsecs
      integer, intent(out)  :: sec,min,hour
    end subroutine sectime

    subroutine smp2smp (obsfle,obsunit,modfle,modunit,outfle,outunit,elim, &
                    iwriteins,insfle,insunit,pcffle,pcfunit)
        character (len=80), intent(in)   :: obsfle,modfle,outfle,insfle,pcffle
        integer, intent(in)              :: obsunit,modunit,outunit
        integer, intent(in)              :: iwriteins,insunit,pcfunit
        real, intent(in)                 :: elim
    end subroutine smp2smp

end interface


! mapinfo interface programs ------->



END MODULE INTER

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
  
  
  double precision function char2double(ifail,num,clin)

! -- Function char2double extracts a double precision number from a word
!    demarcated by subroutine linesplit.

! -- Arguments are as follows:-
!       ifail:    returned as zero unless an error condition arises
!       num:      the number of the word previously extracted by linesplit
!       returns   value of double precision number read from word

! -- Revision history:-
!       June-November, 1995: version 1.

    use DEFN

    integer, intent(in)             :: num
    integer, intent(out)            :: ifail
    integer                         :: ierr
    character (len=10)              :: afmt
    character (len=*)              :: clin
    
  
    ifail=0
!    afmt='(E20.5)'
    read(clin(1:len(clin)),*, iostat=ierr) char2double
    if(ierr.ne.0) go to 110
    return

110     ifail=1
    return
  end
  
  
   subroutine sub_error(subname)
      character (len=*)               ::subname
  end subroutine sub_error
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

  
  real function CalcDistance(x1,y1, x2,y2)
  implicit none
  real          :: x1,x2,y1,y2
    
  CalcDistance = sqrt ( (x1-x2)**2 + (y1-y2)**2)
  return
  end function
 
MODULE readtools
CONTAINS
!-----------------------------------------------------------------------------!
subroutine readKrigeOutput(filepath, filelen, skip, toparray, botarray, &
                           numarray, xarray, yarray)
  implicit none

  character(150)               :: filepath, jnk
  integer, intent(in)          :: filelen, skip
  integer                      :: i, ierr, num
  integer, optional            :: numarray(filelen)
  real*8                       :: elev, x, y, top, bot
  real*8, intent(inout)        :: toparray(filelen), botarray(filelen)
  real*8, optional             :: xarray(filelen), yarray(filelen)

  ! Open, skip lines
  open(10, file=trim(filepath))
  do i=1, skip
    read(10,*)
  end do
  
  ! Read in data (columns: element, xm, xy, xft, yft, GSE, top, bot)
  ierr = 0
  do i=1, filelen
    ! Error handling
    if (ierr /= 0) then
      write(*,'(a)') 'ERROR! - Kriging file missing lines.'
      stop
    end if
    ! Read
    read(10, *) num, x, y, jnk, jnk, elev, top, bot
    ! Store --- CURRENTLY ONLY PULLS OUT X,Y in METERS
    toparray(i) = elev - top
    botarray(i) = elev - bot
    if(present(numarray)) numarray(i) = num
    if(present(xarray)) xarray(i) = x
    if(present(yarray)) yarray(i) = y
  end do
close(10)

end subroutine readKrigeOutput
!-----------------------------------------------------------------------------!
END MODULE readtools