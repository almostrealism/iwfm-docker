!     ******************************************************************
      subroutine init_modflow_usgs(namfname,ierr,cmaxunit)

      ! Initializes the dis/bas packages of MODFLOW

      USE GLOBAL,        ONLY : NLAY,NROW,NCOL,NJA,NODLAY,LAYCBD,          &
                                IUNIT,IOUT,JA,NPER,INCLN,IVSD,IUNSTR
      USE GWFBASMODULE,  ONLY : IBDOPT

      implicit none

      ! variables returned to main program
      !integer                   :: ciunstr,cnlay,cncol,cnrow,cnja,  &
      !                             cnper,cmaxunit,civsd,ifail
      !integer                   :: cnodlay(0:999),ciunit(100)
      integer,intent(inout)     :: ierr, cmaxunit

      ! variables used in routine
      integer                   :: n
      character*200             :: namfname
      integer,parameter         :: mf_iin=141,  &
                                   niunit=100,  &
                                   maxunit=199
      character*80              :: headng(2)

      ! versioning (from mfusg.f)
!-------ASSIGN VERSION NUMBER AND DATE
      CHARACTER*40 VERSION
      CHARACTER*14 MFVNAM
      PARAMETER (VERSION='USG-TRANSPORT VERSION 1.0.0')
      PARAMETER (MFVNAM='USG-TRANSPORT ') !USG = Un-Structured Grids

      ! copy of CUNIT (from mfusg.f)
      character*4               :: CUNIT(niunit)
      
      DATA CUNIT/'BCF6', 'WEL ', 'DRN ', 'RIV ', 'EVT ', 'EVS ', 'GHB ', &  !  7  et time series is now EVS as ETS is for segmented ET
                 'RCH ', 'RTS ', 'TIB ', 'DPF ', 'OC  ', 'SMS ', 'PCB ', &  ! 14
                 'BCT ', 'FHB ', 'RES ', 'STR ', 'IBS ', 'CHD ', 'HFB6', &  ! 21
                 'LAK ', 'LPF ', 'DIS ', 'DISU', 'PVAL', 'SGB ', 'HOB ', &  ! 28
                 'CLN ', 'DPT ', 'ZONE', 'MULT', 'DROB', 'RVOB', 'GBOB', &  ! 35
                 'GNC ', 'DDF ', 'CHOB', 'ETS ', 'DRT ', 'QRT ', 'GMG ', &  ! 42
                 'hyd ', 'SFR ', 'UPW ', 'GAGE', 'LVDA', '    ', 'lmt6', &  ! 49 (LS: added UPW)
                 'MNW2', 'MNWI', 'MNW1', 'KDEP', 'SUB ', 'UZF ', 'gwm ', &  ! 56 (ctm: added MNW2 and MNWI and moved MNW1)
                 'SWT ', 'PATH', 'PTH ', 'GLO ', '    ', '    ', '    ', &  ! 63 (ctm: added glo)
                 'TVM ', 36*'    '/      


      ! initialize
      ierr=0

      ! prep the modflow name file for reading
      open(mf_iin,file=namfname,status='OLD',iostat=ierr)
      if(ierr.ne.0) then
        write(*,*) 'MODFLOW name file does not exist. Terminating.'
        return
      end if

      ! read modflow nam and basic and dis data
      call glo2bas8ar(mf_iin,CUNIT,VERSION,24,31,32,maxunit,12,headng,26,MFVNAM,29,27,30,36)
      
      ! Set unstructured return variables
      !mfunst = IUNSTR
 
      ! close modflow name file
      close(mf_iin)

      ! save maxunit for when closing files opened during MODFLOW initialization
      cmaxunit=maxunit

      end subroutine init_modflow_usgs



!     ******************************************************************
      subroutine get_modflow_arrlens(snlay, snrow, sncol, snodes)
        use GLOBAL, ONLY : IUNSTR,NLAY,NROW,NCOL,DELR,DELC,TOP,BOT,NODES
        implicit none
        
        integer, intent(out)      :: snlay, snrow, sncol, snodes
        
        snlay = NLAY
        snrow = NROW
        sncol = NCOL
        snodes = NODES

      end subroutine get_modflow_arrlens

!     ******************************************************************
      subroutine clear_modflow_memory()

      USE GLOBAL,        ONLY : NLAY,NROW,NCOL,IUNSTR,NODES,IBOUND,LAYCBD,   &
                                TOP,BOT,NODLAY,IUNIT,INGNCn,DELR,DELC,       &
                                IA,JA,NJA,NPER,ISSFLG,TSMULT,NSTP,PERLEN,    &
                                IOUT,IVSD,IVC,IATMP,JAS
      USE CLN1MODULE,    ONLY : NCLNNDS,ACLNGWC
      USE GNCnMODULE,    ONLY : NGNCn
      USE GWFBCFMODULE,  ONLY : HK,CHANI,HANI,LAYTYP,IBCFCB,VKA,LAYCON,CV,   &
                                HDRY
      USE GWFBASMODULE,  ONLY : IHEDUN,IATS
      implicit none

      integer                  :: n, cmaxunit, lop

      ! clear MODFLOW memory
!9------CLOSE FILES AND DEALLOCATE MEMORY.  GWF2BAS7U1DA MUST BE CALLED
!9------LAST BECAUSE IT DEALLOCATES IUNIT.
      IF(IUNIT(1).GT.0) CALL GWF2BCFU1DA(IUNIT(23))
!SP      IF(IUNIT(23).GT.0.AND.IDEALLOC_LPF.EQ.0) CALL GWF2LPFU1DA
      IF(IUNIT(2).GT.0) CALL GWF2WEL7U1DA
      IF(IUNIT(27).GT.0) CALL GLO2SGBU1DA
      IF(IUNIT(3).GT.0) CALL GWF2DRN7U1DA
      IF(IUNIT(4).GT.0) CALL GWF2RIV7U1DA
!      IF(IUNIT(5).GT.0) CALL GWF2EVT8U1DA(IUNIT(15))
      IF(IUNIT(7).GT.0) CALL GWF2GHB7U1DA
      IF(IUNIT(8).GT.0) CALL GWF2RCH8U1DA(IUNIT(15))
      IF(IUNIT(16).GT.0) CALL GWF2FHB7U1DA
!SP      IF(IUNIT(17).GT.0) CALL GWF2RES7U1DA(IGRID)
      IF(IUNIT(18).GT.0) CALL GWF2STR7U1DA
!SP      IF(IUNIT(19).GT.0) CALL GWF2IBS7U1DA(IGRID)
      IF(IUNIT(20).GT.0) CALL GWF2CHD7U1DA
!      IF(IUNIT(21).GT.0) CALL GWF2HFB7U1DA
!      IF(IUNIT(22).GT.0 .OR. IUNIT(44).GT.0)CALL GWF2LAK7U1DA(IUNIT(22))
!SP      IF(IUNIT(39).GT.0) CALL GWF2ETS7U1DA(IGRID)
!SP      IF(IUNIT(40).GT.0) CALL GWF2DRT7U1DA(IGRID)
!SP      IF(IUNIT(42).GT.0) CALL GMG7U1DA(IGRID)
!      IF(IUNIT(44).GT.0) CALL GWF2SFR7U1DA
      IF(IUNIT(46).GT.0) CALL GWF2GAG7U1DA
!SP      IF(IUNIT(50).GT.0) CALL GWF2MNW7U1DA(IGRID)
      IF(IUNIT(54).GT.0) CALL GWF2SUB7U1DA
!SP      IF(IUNIT(55).GT.0) CALL GWF2UZF1DA(IGRID)
!csp      IF(IUNIT(57).GT.0) CALL GWF2SWT7U1DA(IGRID)
      IF(IUNIT(13).GT.0) CALL GWF2SMS7U1DA
!SP      CALL OBS2BAS7U1DA(IUNIT(28),IGRID)
!SP      IF(IUNIT(33).GT.0) CALL OBS2DRN7U1DA(IGRID)
!SP      IF(IUNIT(34).GT.0) CALL OBS2RIV7U1DA(IGRID)
!SP      IF(IUNIT(35).GT.0) CALL OBS2GHB7U1DA(IGRID)
!SP      IF(IUNIT(38).GT.0) CALL OBS2CHD7U1DA(IGRID)
       IF(INGNCn.GT.0) CALL GNCn2DISU1DA
!--------DM: Deallocate TVM package arrays
      IF(IUNIT(64).GT.0) CALL TVMU2DA
!--------END DM
      if(iunit(59).gt.0) call gwt2p3d1da()
      write(IOUT,*)
      write(IOUT,*) 'Finished clearing memory.'
      CALL GWF2BAS7U1DA
      CALL XMD7DA
      CALL XMDLIBDA

      ! close all files still opened
      do n=1,cmaxunit
        INQUIRE(UNIT=n,OPENED=LOP)
        IF(LOP) THEN
          close(n)
        END IF
      end do

end subroutine clear_modflow_memory
