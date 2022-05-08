!-----------------------------------------------------------------------------!
  subroutine writeGWFP(OUT)
    use errorhandle
    use MFSupport
    
!     ******************************************************************
!     MODFLOW READ2WRITE CONVERSION by Leland Scantlebury
!     WRITE DATA FOR LPF/UPW (based on unit passed to OUT)
!     ******************************************************************
!
!        SPECIFICATIONS:
!     ------------------------------------------------------------------
      USE GLOBAL, ONLY:NCOL,NROW,NLAY,ITRSS,LAYHDT,LAYHDS,LAYCBD, &
                  NCNFBD,IBOUND,BUFF,NBOTM,DELR,DELC,IOUT,NOVFC,itrnsp, &
                  NODES,IFREFM,IUNSTR,ICONCV,NOCVCO,NJA,NJAS,IWADI, &
                  IDEALLOC_LPF,HNEW,Sn,So,NODLAY,BOT,TOP,ARAD,iunsat
      USE GWFBCFMODULE,ONLY:IBCFCB,IWDFLG,IWETIT,IHDWET,WETFCT,HDRY,CV, &
                           LAYCON,LAYAVG,SC1,SC2,WETDRY,CHANI,IHANISO, &
                           IKCFLAG,LAYWET,ISFAC,ITHFLG,LAYAVGV, &
                           LAYTYP,LAYVKA,LAYSTRT,alpha,beta,sr,brook, &
                           LAYFLG,VKA,VKCB,HANI,HK,IBPN,BP,IPHDRY
      
      ! Input
      integer,intent(in)     :: out

      ! MODFLOW variables
      DOUBLE PRECISION HD,THCK,TTOP,BBOT,TOTTHICK
      CHARACTER*14 LAYPRN(5),AVGNAM(5),TYPNAM(3),VKANAM(2),WETNAM(2), &
                   HANNAM
      DATA AVGNAM/'      HARMONIC','   LOGARITHMIC','   LOG+ARITHM ', &
         '   ARITHMETIC ','FINITE ELEMENT'/
      DATA TYPNAM/'     CONFINED ','  CONVERTIBLE ','     UPSTREAM '/
      DATA VKANAM/'    VERTICAL K','    ANISOTROPY'/
      DATA WETNAM/'  NON-WETTABLE','      WETTABLE'/
      DATA HANNAM/'      VARIABLE'/
      CHARACTER*200 LINE
      CHARACTER*24 ANAME(9),STOTXT
      CHARACTER*4 PTYP
      
      ! Write Variables
      character(30)        :: fmt(10)
      character(300)       :: lpfoptions
      
      if (gwftyp=='LPF') then
        write(OUT,'(a)') '# Layer Property Flow (LPF) Package Written by Texture2Par'
      else if (gwftyp=='UPW') then
        write(OUT,'(a)') '# Upstream Weighting Package (UPW) Written by Texture2Par'
      else
        write(OUT,'(a)') '# Mystery Flow Package Written by Texture2Par'
      end if

!3A-----WRITE ITEM 1
      ! Assemble Options
      lpfoptions=''
      if(ISFAC==1)  lpfoptions = trim(lpfoptions) // ' STORAGECOEFFICIENT'
      if(ICONCV==1) lpfoptions = trim(lpfoptions) // ' CONSTANTCV'
      if(ITHFLG==1) lpfoptions = trim(lpfoptions) // ' THICKSTRT'
      if(NOCVCO==1) lpfoptions = trim(lpfoptions) // ' NOCVCORRECTION'
      if(NOVFC==1)  lpfoptions = trim(lpfoptions) // ' NOVFC'
      if(NOPCHK==1) lpfoptions = trim(lpfoptions) // ' NOPARCHECK'
      if(IBPN==1)   lpfoptions = trim(lpfoptions) // ' BUBBLEPT'
      
      ! Write data set 1 & options
      IF((IUNSTR == 0).and.(gwftyp=='LPF')) then
        write(OUT,'(I4,1es13.5,I5,A)') IBCFCB, HDRY, NPLPF, lpfoptions
      ELSE IF ((IUNSTR == 0).and.(gwftyp=='UPW')) then  
        write(OUT,'(I4,1es13.5,2I5,A)') IBCFCB, HDRY, NPLPF,IPHDRY, lpfoptions
      else
        write(OUT,'(I4,1es13.5,2I5,A)') IBCFCB, HDRY, NPLPF, IKCFLAG, lpfoptions
      end if
      
!4------WRITE LAYTYP, LAYAVG, CHANI, LAYVKA, LAYWET, LAYSTRT.
      
      write(fmt(1),'("(",i5,"i3)")') nlay
      write(fmt(2),'("(",i5,"es12.4e2)")') nlay
      WRITE(OUT,fmt(1)) (LAYTYP(K),K=1,NLAY)
      WRITE(OUT,fmt(1)) (LAYAVG(K),K=1,NLAY)
      WRITE(OUT,fmt(2)) (CHANI(K),K=1,NLAY)
      WRITE(OUT,fmt(1)) (LAYVKA(K),K=1,NLAY)
      if (gwftype=='UPW') then
        WRITE(OUT,fmt(1)) (0,K=1,NLAY)
      else
        WRITE(OUT,fmt(1)) (LAYWET(K),K=1,NLAY)
      end if
      
!4G-----WRITE WETTING INFORMATION.
       IF(NWETD.EQ.1) THEN
         READ(IN,*) WETFCT,IWETIT,IHDWET
      END IF     

!6------WRITE PARAMETER DEFINITIONS
      IF(NPLPF.GT.0) THEN
        ! Parameters are not supported by Texture2Par
        write(*,'(4x,a,4a,a)') 'Warning - ', gwftype, ' file contains parameters (Uncompatable)'
        write(*,'(4x,a,4a,a)') 'Parameters NOT written to new ', gwftype, ' file'
      end if
      
!7------READ PARAMETERS AND CONVERT FOR UNSTRUCTURED AND STRUCTURED GRIDS
      IF(IUNSTR.EQ.0) THEN
        CALL t2p_SGWF2LPFU1S(OUT,NPHK,NPHANI,NPVK,NPVANI,NPSS,NPSY,NPVKCB, &
                         STOTXT,NOPCHK)
      ELSE
        ! Not implemented yet
        call USGunimplemented()
        !CALL SGWF2LPFU1G(IN,NPHK,NPHANI,NPVK,NPVANI,NPSS,NPSY,NPVKCB, &
        !                 STOTXT,NOPCHK)
      ENDIF      
      
  
  end subroutine writeGWFP
!-----------------------------------------------------------------------------!
  
!-----------------------------------------------------------------------------!    
  SUBROUTINE t2p_SGWF2LPFU1S(OUT,NPHK,NPHANI,NPVK,NPVANI,NPSS,NPSY, &
                          NPVKCB,STOTXT,NOPCHK)
     use MakePar
!     ******************************************************************
!     ALLOCATE AND READ DATA FOR LAYER PROPERTY FLOW PACKAGE FOR STRUCTURED GRID
!     ******************************************************************
!
!        SPECIFICATIONS:
!     ------------------------------------------------------------------
     
      USE GLOBAL, ONLY:NCOL,NROW,NLAY,ITRSS,LAYHDT,LAYHDS,LAYCBD,ISYM,  &
                  NCNFBD,IBOUND,BUFF,NBOTM,DELR,DELC,IOUT,NODES,nodlay, &
                  IFREFM,IUNSTR,IA,JA,JAS,NJA,NJAS,ARAD,IPRCONN,iunsat,IUNIT
      USE GWFBCFMODULE,ONLY:IBCFCB,IWDFLG,IWETIT,IHDWET,WETFCT,HDRY,CV, &
                            LAYCON,LAYAVG,HK,SC1,SC2,WETDRY,            &
                            IKCFLAG,laywet,ISFAC,ITHFLG,                &
                            LAYTYP,CHANI,LAYVKA,LAYSTRT,                &
                            LAYFLG,VKA,VKCB,HANI,IHANISO,               &
                            alpha,beta,sr,brook,BP,IBPN
      
      ! Input
      integer,intent(in)   :: out

      ! MODFLOW variables
      REAL, DIMENSION(:,:),ALLOCATABLE  ::TEMP
      REAL, DIMENSION (:), ALLOCATABLE :: TEMPPL

      CHARACTER*24 ANAME(15),STOTXT
      CHARACTER*4 PTYP

      DATA ANAME(1) /'  HYDRAULIC CONDUCTIVITY'/
      DATA ANAME(2) /'        ANISOTROPY RATIO'/
      DATA ANAME(3) /'     VERTICAL HYD. COND.'/
      DATA ANAME(4) /' HORIZ. TO VERTICAL ANI.'/
      DATA ANAME(5) /'QUASI3D VERT. HYD. COND.'/
      DATA ANAME(6) /'        PRIMARY STORAGE'/
      DATA ANAME(7) /'          SPECIFIC YIELD'/
      DATA ANAME(8) /'        WETDRY PARAMETER'/
      DATA ANAME(9) /'     STORAGE COEFFICIENT'/
      DATA ANAME(10) /'        WETDRY PARAMETER'/
      DATA ANAME(11) /'                   alpha'/
      DATA ANAME(12) /'                    beta'/
      DATA ANAME(13) /'                      sr'/
      DATA ANAME(14) /'                   brook'/
      DATA ANAME(15) /'     BUBBLING POINT HEAD'/
      REAL PI
      
      ! T2P variables
      integer              :: knstart, knend, i, k, wh(nrow,nlay*2)
      character(20)        :: fmt
      real, parameter      :: zero=0.0d0
      
      ! For FMTIN in array headers
      fmt = '(10e12.4)           '
      
      ! Create writehelper
      do k=1,nlay
        do i=1,nrow
          wh(i,k)      = (k-1)*nrow*ncol+(i-1)*ncol+1
          wh(i,nlay+k) = (k-1)*nrow*ncol+(i)*ncol
        end do
      end do
      
!3------DEFINE DATA FOR EACH LAYER -- VIA READING OR NAMED PARAMETERS.
      DO K=1,NLAY
        KK=K
        !knstart = (k-1) * nrow * ncol + 1
        !knend   =  k    * nrow * ncol

!3A-----WRITE HORIZONTAL HYDRAULIC CONDUCTIVITY (HK)
        WRITE(OUT,22) OUT,1.0,fmt,-1,ANAME(1),' Layer',K
        DO i=1, nrow
!          write(OUT,'(10es12.4)') HK(wh(i,k):wh(i,nlay+k))
          write(OUT,'(10es12.4)') KhB(k,(i-1)*ncol+1:i*ncol)
        end do
        
 !3B-----WRITE HORIZONTAL ANISOTROPY IF CHANI IS NON-ZERO
        IF(CHANI(K).LE.ZERO) THEN
          WRITE(OUT,22) OUT,1.0,fmt,-1,ANAME(2),' LAYER ',K
          DO i=1, nrow
            write(OUT,'(10es12.4)') HANI(wh(i,k):wh(i,nlay+k))
          end do
        END IF
 
!3C-----WRITE VERTICAL HYDRAULIC CONDUCTIVITY
        WRITE(OUT,22) OUT,1.0,fmt,-1,ANAME(3),' LAYER ',K
        DO i=1, nrow
!          write(OUT,'(10es12.4)') VKA(wh(i,k):wh(i,nlay+k))
          write(OUT,'(10es12.4)') KvB(k,(i-1)*ncol+1:i*ncol)
        end do

!3D-----DEFINE SPECIFIC STORAGE OR STORAGE COEFFICIENT IN ARRAY SC1 IF TRANSIENT.
        IF(ITRSS.NE.0) THEN
          WRITE(OUT,22) OUT,1.0,fmt,-1,ANAME(6),' LAYER ',K
          DO i=1, nrow
!            write(OUT,'(10es12.4)') SC1(wh(i,k):wh(i,nlay+k))
            write(OUT,'(10es12.4)') SsB(k,(i-1)*ncol+1:i*ncol)
          end do
        end if
        
!3E-----WRITE SPECIFIC YIELD IN ARRAY SC2 IF TRANSIENT AND LAYER IS
!3E-----IS CONVERTIBLE.
      IF(LAYTYP(K).NE.0) THEN
         IF(ITRSS.NE.0) THEN
          WRITE(OUT,22) OUT,1.0,fmt,-1,ANAME(7),' LAYER ',K
          DO i=1, nrow
!            write(OUT,'(10es12.4)') SC2(wh(i,k):wh(i,nlay+k))
            write(OUT,'(10es12.4)') SyB(k,(i-1)*ncol+1:i*ncol)
          end do
         end if
      end if
      
!3F-----WRITE CONFINING BED VERTICAL HYDRAULIC CONDUCTIVITY (VKCB)
      IF(LAYCBD(K).NE.0) THEN
        WRITE(OUT,22) OUT,1.0,fmt,-1,ANAME(5),' LAYER ',K
        DO i=1, nrow
!          write(OUT,'(10es12.4)') VKCB(wh(i,k):wh(i,nlay+k))
          write(OUT,'(10es12.4)') KvB_aqtard(k,(i-1)*ncol+1:i*ncol)
        end do
      end if
        
!3G-----READ WETDRY CODES IF WETTING CAPABILITY HAS BEEN INVOKED
!3G-----(LAYWET NOT 0).      
      IF(LAYWET(K).NE.0) THEN
        WRITE(OUT,22) OUT,1.0,fmt,-1,ANAME(8),' LAYER ',K
        DO i=1, nrow
          write(OUT,'(10es12.4)') WETDRY(wh(i,k):wh(i,nlay+k))
        end do
      end if
      
!3H-----WRITE alpha, beta, sr, brook      
      if(LAYCON(k) == 5) then
        WRITE(OUT,22) OUT,1.0,fmt,-1,ANAME(11),' LAYER ',K
        DO i=1, nrow
          write(OUT,'(10es12.4)') alpha(wh(i,k):wh(i,nlay+k))
        end do
        WRITE(OUT,22) OUT,1.0,fmt,-1,ANAME(12),' LAYER ',K
        DO i=1, nrow
          write(OUT,'(10es12.4)') beta(wh(i,k):wh(i,nlay+k))
        end do
        WRITE(OUT,22) OUT,1.0,fmt,-1,ANAME(13),' LAYER ',K
        DO i=1, nrow
          write(OUT,'(10es12.4)') sr(wh(i,k):wh(i,nlay+k))
        end do
        WRITE(OUT,22) OUT,1.0,fmt,-1,ANAME(14),' LAYER ',K
        DO i=1, nrow
          write(OUT,'(10es12.4)') brook(wh(i,k):wh(i,nlay+k))
        end do
        IF(IBPN.GT.0)THEN
          WRITE(OUT,22) OUT,1.0,fmt,-1,ANAME(15),' LAYER ',K
          DO i=1, nrow
            write(OUT,'(10es12.4)') bP(wh(i,k):wh(i,nlay+k))
          end do
        end if
      end if
      
      END DO

! Format for all layer parameter description lines
22    FORMAT(i10,f10.5,a20,i10,a30,a7,i5)   ! LOCAT CNSTNT FMTIN IPRN label layertext layer#
      
  end SUBROUTINE t2p_SGWF2LPFU1S
!-----------------------------------------------------------------------------!