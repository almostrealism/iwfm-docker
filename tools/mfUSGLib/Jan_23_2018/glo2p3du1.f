      MODULE GWTP3DMODULE
       INTEGER, SAVE,  ALLOCATABLE,   DIMENSION(:)     :: IVELOCITY,
     +                                                    ICXNS,
     +                                                    HYDRO_ZONES
       REAL, SAVE,     ALLOCATABLE,   DIMENSION(:)     :: POROSITY,
     +                                                    RETARDATION,
     +                                                    DISP_HORIZ,
     +                                                    DISP_TRANS,
     +                                                    DISP_VERT
      END MODULE GWTP3DMODULE
c -------------------------------------------------------------------------------------
      SUBROUTINE GWT2P3D1AR(IN,IFAIL)
C     ******************************************************************
C     ALLOCATE ARRAYS AND READ DATA FOR MOD-PATH3DU PACKAGE
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL, ONLY:IOUT,NCOL,NROW,NLAY,ITRSS,IFREFM,IUNSTR,
     * NODES,NEQS,NJA,NJAS,ITRNSP,Sn,NIUNIT,INCLN
      USE GWTP3DMODULE
C
      CHARACTER*200 LINE
      integer                   :: ifail
C
C2------IDENTIFY PACKAGE
      WRITE(IOUT,1) IN
    1 FORMAT(1X,/1X,'PATH -- for MP3DU PACKAGE, VER. 1',
     1', 5/11/2015',/,9X,'INPUT READ FROM UNIT',I3)
C
C4-----ALLOCATE SPACE FOR ARRAYS.
      ALLOCATE(IVELOCITY(NEQS))
      ALLOCATE(POROSITY(NODES),RETARDATION(NODES),
     +         DISP_HORIZ(NODES),DISP_TRANS(NODES),DISP_VERT(NODES),
     +         HYDRO_ZONES(NODES))

      RETARDATION=1.
      DISP_HORIZ=0.
      DISP_TRANS=0.
      DISP_VERT=0.
      HYDRO_ZONES=0
C
C5----READ PARAMETERS AND CONVERT FOR UNSTRUCTURED AND STRUCTURED GRIDS
      ifail=0
      ALLOCATE(ICXNS(NJA))
      IF(IUNSTR.EQ.0) THEN
        CALL SGWT2P3D1S(IN,IFAIL)
      ELSE
        CALL SGWT2P3D1G(IN,IFAIL)
      ENDIF
C
C--------------------------------------------------------------------------------
C
      RETURN
      END
C

C--------------------------------------------------------------------------------
      SUBROUTINE SGWT2P3D1S(IN,IFAIL)
C     ******************************************************************
C-----READ PARAMETERS AND CONVERT FOR STRUCTURED GRID
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:IOUT,NCOL,NROW,NLAY,ITRSS,LAYHDT,LAYHDS,
     1                      IFREFM,IBOUND,NODES,NJA,NJAS,IA,JA,JAS,ARAD
      USE GWTP3DMODULE,ONLY: POROSITY,IVELOCITY,RETARDATION,
     +                       DISP_HORIZ,DISP_TRANS,DISP_VERT,HYDRO_ZONES
      USE GWFBCFMODULE, ONLY: IHANISO
C
      INTEGER, DIMENSION(:,:),    ALLOCATABLE ::ITEMP
      REAL,    DIMENSION(:,:),    ALLOCATABLE ::TEMP
      integer                   :: ifail
      character*100             :: aline
      CHARACTER*24 ANAME(7)
      DATA ANAME(1) /'               IVELOCITY'/
      DATA ANAME(2) /'                POROSITY'/
      DATA ANAME(3) /'             RETARDATION'/
      DATA ANAME(4) /'      LONGITUDINAL DISP.'/
      DATA ANAME(5) /' TRANSVERSE HORIZ. DISP.'/
      DATA ANAME(6) /'  TRANSVERSE VERT. DISP.'/
      DATA ANAME(7) /'            HYDRO. ZONES'/
C     ------------------------------------------------------------------
C-------ALLOCATE TEMP ARRAY FOR STORING 3-D INFORMATION
      ALLOCATE(TEMP(NCOL,NROW))
C-----------------------------------------------------------------------
C1------READ/SET SPECIES INDEPENDENT ARRAYS FOR ALL LAYERS.
C

c     ensure not end-of-file
      read(in,'(a)',iostat=ifail) aline
      if(ifail.ne.0) then
        return
      else
        backspace(in)
      end if
      CALL URDCOM(IN,IOUT,ALINE)

C---------------------------------------------------------
C1A-----READ/SET IVELOCITY ARRAY
      ALLOCATE(ITEMP(NCOL,NROW))
      DO K=1,NLAY
        KK=K
c       ensure not end-of-file
        read(in,'(a)',iostat=ifail) aline
c        if(ifail.ne.0) then
c          return
c        else
        if(ifail.eq.0) then
          backspace(in)
          CALL U2DINT(ITEMP(1,1),ANAME(1),NROW,NCOL,KK,IN,IOUT)
          DO I=1,NROW
            DO J=1,NCOL
              N=J+(I-1)*NCOL+(K-1)*NROW*NCOL
              IVELOCITY(N) = ITEMP(J,I)
            ENDDO
          ENDDO
        end if
      ENDDO
c      DEALLOCATE (ITEMP)
C
C---------------------------------------------------------
C1A-----READ POROSITY INTO ARRAY POROSITY.
      DO K=1,NLAY
        KK=K
c       ensure not end-of-file
        read(in,'(a)',iostat=ifail) aline
        if(ifail.ne.0) then
          return
        else
          backspace(in)
        end if
        CALL U2DREL(TEMP(1,1),ANAME(2),NROW,NCOL,KK,IN,IOUT)
        DO I=1,NROW
          DO J=1,NCOL
            N=J+(I-1)*NCOL+(K-1)*NROW*NCOL
            POROSITY(N) = TEMP(J,I)
          ENDDO
        ENDDO
      ENDDO
C---------------------------------------------------------
C1A-----READ RETARDATION INTO ARRAY RETARDATION.
      DO K=1,NLAY
        KK=K
c       ensure not end-of-file
        read(in,'(a)',iostat=ifail) aline
c        if(ifail.ne.0) then
c          return
c        else
        if(ifail.eq.0) then
          backspace(in)
          CALL U2DREL(TEMP(1,1),ANAME(3),NROW,NCOL,KK,IN,IOUT)
          DO I=1,NROW
            DO J=1,NCOL
              N=J+(I-1)*NCOL+(K-1)*NROW*NCOL
              RETARDATION(N) = TEMP(J,I)
            ENDDO
          ENDDO
        end if
      ENDDO
C---------------------------------------------------------
C1A-----READ DISPERSION TERMS INTO ARRAYS.
c     long.
      DO K=1,NLAY
        KK=K
c       ensure not end-of-file
        read(in,'(a)',iostat=ifail) aline
c        if(ifail.ne.0) then
c          return
c        else
        if(ifail.eq.0) then
          backspace(in)
          CALL U2DREL(TEMP(1,1),ANAME(4),NROW,NCOL,KK,IN,IOUT)
          DO I=1,NROW
            DO J=1,NCOL
              N=J+(I-1)*NCOL+(K-1)*NROW*NCOL
              DISP_HORIZ(N) = TEMP(J,I)
            ENDDO
          ENDDO
        end if
      ENDDO
c     trans.
      DO K=1,NLAY
        KK=K
c       ensure not end-of-file
        read(in,'(a)',iostat=ifail) aline
c        if(ifail.ne.0) then
c          return
c        else
        if(ifail.eq.0) then
          backspace(in)
          CALL U2DREL(TEMP(1,1),ANAME(5),NROW,NCOL,KK,IN,IOUT)
          DO I=1,NROW
            DO J=1,NCOL
              N=J+(I-1)*NCOL+(K-1)*NROW*NCOL
              DISP_TRANS(N) = TEMP(J,I)
            ENDDO
          ENDDO
        end if
      ENDDO
c     vert.
      DO K=1,NLAY
        KK=K
c       ensure not end-of-file
        read(in,'(a)',iostat=ifail) aline
c        if(ifail.ne.0) then
c          return
c        else
        if(ifail.eq.0) then
          backspace(in)
          CALL U2DREL(TEMP(1,1),ANAME(6),NROW,NCOL,KK,IN,IOUT)
          DO I=1,NROW
            DO J=1,NCOL
              N=J+(I-1)*NCOL+(K-1)*NROW*NCOL
              DISP_VERT(N) = TEMP(J,I)
            ENDDO
          ENDDO
        end if
      ENDDO

C---------------------------------------------------------
C1A-----READ/SET HYDRO ZONES if present
c      DO K=1,NLAY
c        KK=K
cc       ensure not end-of-file
c        read(in,'(a)',iostat=ifail) aline
c        if(ifail.ne.0) then
cc         make each layer a hydrocode
c          N1=1+(1-1)*NCOL+(K-1)*NROW*NCOL
c          N2=NCOL+(NROW-1)*NCOL+(K-1)*NROW*NCOL
c          HYDRO_ZONES(N1:N2)=K
cc          if(k.eq.nlay) return
c        else
c          backspace(in)
c          CALL U2DINT(ITEMP(1,1),ANAME(7),NROW,NCOL,KK,IN,IOUT)
c          DO I=1,NROW
c            DO J=1,NCOL
c              N=J+(I-1)*NCOL+(K-1)*NROW*NCOL
c              HYDRO_ZONES(N) = ITEMP(J,I)
c            ENDDO
c          ENDDO
c        end if
c      ENDDO

      DEALLOCATE (ITEMP)
      DEALLOCATE(TEMP)

      IFAIL=0

      RETURN
      END

C----------------------------------------------------------------------------
      SUBROUTINE SGWT2P3D1G(IN,ifail)
C     ******************************************************************
C-----READ PARAMETERS AND CONVERT FOR UNSTRUCTURED (GENERALIZED) GRID
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,     ONLY:IOUT,NCOL,NROW,NLAY,ITRSS,IA,JA,JAS,NJA,NJAG,
     1                     ARAD,IFREFM,IBOUND,NODES,NODLAY,IDSYMRD,NJAS,
     2                     IATMP,NJATMP
      USE GWTP3DMODULE,ONLY: POROSITY,IVELOCITY,RETARDATION,
     +                       DISP_HORIZ,DISP_TRANS,DISP_VERT,ICXNS,
     +                       HYDRO_ZONES
      USE GWFBCFMODULE, ONLY: IHANISO
C
c      REAL,  DIMENSION(:),    ALLOCATABLE ::TEMPC
      REAL,  DIMENSION(:),    ALLOCATABLE ::TEMP
      CHARACTER*24 ANAME(8)
      integer                   :: ifail
      character*100             :: aline
      DATA ANAME(1) /'               IVELOCITY'/
      DATA ANAME(2) /'                POROSITY'/
      DATA ANAME(3) /'             RETARDATION'/
      DATA ANAME(4) /'      LONGITUDINAL DISP.'/
      DATA ANAME(5) /' TRANSVERSE HORIZ. DISP.'/
      DATA ANAME(6) /'  TRANSVERSE VERT. DISP.'/
      DATA ANAME(7) /' ICXNS: -1 BT,0 SD,1 TP.'/
      DATA ANAME(8) /'            HYDRO. ZONES'/
C     ------------------------------------------------------------------


c     ensure not end-of-file
      read(in,'(a)',iostat=ifail) aline
      if(ifail.ne.0) then
        return
      else
        backspace(in)
      end if
      CALL URDCOM(IN,IOUT,ALINE)

c      ALLOCATE (TEMPC(NODES))
C1A-----READ/SET IVELOCITY ARRAY
      DO K=1,NLAY
        NNDLAY = NODLAY(K)
        NSTRT = NODLAY(K-1)+1
        NDSLAY = NNDLAY - NODLAY(K-1)
c       ensure not end-of-file
        read(in,'(a)',iostat=ifail) aline
c        if(ifail.ne.0) then
c          return
c        else
        if(ifail.eq.0) then
          backspace(in)
          CALL U1DINT(IVELOCITY(NSTRT),ANAME(1),NDSLAY,K,IN,IOUT)
        end if
      ENDDO
C
C---------------------------------------------------------
C1A-----READ POROSITY INTO ARRAY PRSITY.
      DO K = 1,NLAY
        NNDLAY = NODLAY(K)
        NSTRT = NODLAY(K-1)+1
        NDSLAY = NNDLAY - NODLAY(K-1)
c       ensure not end-of-file
        read(in,'(a)',iostat=ifail) aline
c        if(ifail.ne.0) then
c          return
c        else
        if(ifail.eq.0) then
          backspace(in)
          CALL U1DREL(POROSITY(NSTRT),ANAME(2),NDSLAY,K,IN,IOUT)
        end if
      ENDDO
C
C---------------------------------------------------------
C1A-----READ RETARDATION INTO ARRAY RETARDATION.
      DO K = 1,NLAY
        NNDLAY = NODLAY(K)
        NSTRT = NODLAY(K-1)+1
        NDSLAY = NNDLAY - NODLAY(K-1)
c       ensure not end-of-file
        read(in,'(a)',iostat=ifail) aline
c        if(ifail.ne.0) then
c          return
c        else
        if(ifail.eq.0) then
          backspace(in)
          CALL U1DREL(RETARDATION(NSTRT),ANAME(3),NDSLAY,K,IN,IOUT)
        end if
      ENDDO
C
C---------------------------------------------------------
C1A-----READ DISPERSION TERMS
c     LONG.
      DO K = 1,NLAY
        NNDLAY = NODLAY(K)
        NSTRT = NODLAY(K-1)+1
        NDSLAY = NNDLAY - NODLAY(K-1)
c       ensure not end-of-file
        read(in,'(a)',iostat=ifail) aline
c        if(ifail.ne.0) then
c          return
c        else
        if(ifail.eq.0) then
          backspace(in)
          CALL U1DREL(DISP_HORIZ(NSTRT),ANAME(4),NDSLAY,K,IN,IOUT)
        end if
      ENDDO
c     TRANS.
      DO K = 1,NLAY
        NNDLAY = NODLAY(K)
        NSTRT = NODLAY(K-1)+1
        NDSLAY = NNDLAY - NODLAY(K-1)
c       ensure not end-of-file
        read(in,'(a)',iostat=ifail) aline
c        if(ifail.ne.0) then
c          return
c        else
        if(ifail.eq.0) then
          backspace(in)
          CALL U1DREL(DISP_TRANS(NSTRT),ANAME(5),NDSLAY,K,IN,IOUT)
        end if
      ENDDO
c     VERT.
      DO K = 1,NLAY
        NNDLAY = NODLAY(K)
        NSTRT = NODLAY(K-1)+1
        NDSLAY = NNDLAY - NODLAY(K-1)
c       ensure not end-of-file
        read(in,'(a)',iostat=ifail) aline
c        if(ifail.ne.0) then
c          return
c        else
        if(ifail.eq.0) then
          backspace(in)
          CALL U1DREL(DISP_VERT(NSTRT),ANAME(6),NDSLAY,K,IN,IOUT)
        end if
      ENDDO
C
C---------------------------------------------------------
C1A-----READ CONNECTION ARRAY
c        NNDLAY = NJA
c        NSTRT = 1
c        NDSLAY = NJA
cc       ensure not end-of-file
c        read(in,'(a)',iostat=ifail) aline
c        if(ifail.ne.0) then
c          ifail=0
cc          return
c        else
c          backspace(in)
c          CALL U1DINT(ICXNS(NSTRT),ANAME(7),NDSLAY,-1,IN,IOUT)
c        end if
C
C---------------------------------------------------------
C

C1A-----READ/SET HYDRO ZONES
c      DO K=1,NLAY
c        NNDLAY = NODLAY(K)
c        NSTRT = NODLAY(K-1)+1
c        NDSLAY = NNDLAY - NODLAY(K-1)
cc       ensure not end-of-file
c        read(in,'(a)',iostat=ifail) aline
c        if(ifail.ne.0) then
c          HYDRO_ZONES(NSTRT:NNDLAY)=K
cc          return
c        else
c          backspace(in)
c          CALL U1DINT(HYDRO_ZONES(NSTRT),ANAME(8),NDSLAY,K,IN,IOUT)
c        end if
c      ENDDO

      IFAIL=0
      RETURN
      END

C----------------------------------------------------------------------------
      SUBROUTINE GWT2P3D1DA()
C     ******************************************************************
C-----DEALLOCATE PARTICLE TRACKING ARRAYS
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GWTP3DMODULE

      deallocate(ivelocity)
      deallocate(porosity)
      deallocate(retardation)
      deallocate(disp_horiz)
      deallocate(disp_trans)
      deallocate(disp_vert)
      deallocate(icxns)
      deallocate(hydro_zones)

      return
      end

