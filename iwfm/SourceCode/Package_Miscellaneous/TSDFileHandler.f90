!***********************************************************************
!  Integrated Water Flow Model (IWFM)
!  Copyright (C) 2005-2021  
!  State of California, Department of Water Resources 
!
!  This program is free software; you can redistribute it and/or
!  modify it under the terms of the GNU General Public License
!  as published by the Free Software Foundation; either version 2
!  of the License, or (at your option) any later version.
!
!  This program is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!  GNU General Public License for more details.
!  (http://www.gnu.org/copyleft/gpl.html)
!
!  You should have received a copy of the GNU General Public License
!  along with this program; if not, write to the Free Software
!  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
!
!  For tecnical support, e-mail: IWFMtechsupport@water.ca.gov 
!***********************************************************************
MODULE TSDFileHandler
  USE MessageLogger        , ONLY: SetLastMessage                 , &
                                   MessageArray                   , &
                                   f_iFatal                         
  USE IOInterface          , ONLY: GenericFileType                , &
                                   f_iTXT                         , &
                                   f_iBIN                         , &
                                   f_iDSS                         , &
                                   f_iUNKNOWN                        
  USE GeneralUtilities     , ONLY: StripTextUntilCharacter        , &
                                   LowerCase                      , &
                                   IntToText                      , &
                                   FirstLocation                  , &
                                   EstablishAbsolutePathFileName  , &
                                   AllocArray                     , &
                                   CleanSpecialCharacters
  USE TimeSeriesUtilities  , ONLY: TimeStepType
  IMPLICIT NONE


! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** VARIABLE DEFINITIONS
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- PUBLIC ENTITIES
  ! -------------------------------------------------------------
  PRIVATE
  PUBLIC :: IntTSDataInFileType    ,  &
            RealTSDataInFileType   ,  &
            Real2DTSDataInFileType ,  &
            PrepareTSDOutputFile   ,  &
            ReadTSData


  ! -------------------------------------------------------------
  ! --- GENERIC TS DATA INPUT FILE
  ! -------------------------------------------------------------
  TYPE TSDataInFileType
    INTEGER               :: iSize     = 0
    LOGICAL               :: lUpdated  = .FALSE.
    TYPE(GenericFileType) :: File
  CONTAINS
    PROCEDURE,PASS :: GetNDataColumns 
    PROCEDURE,PASS :: GetFileName
    PROCEDURE,PASS :: CheckColNum
  END TYPE TSDataInFileType
  
    
  ! -------------------------------------------------------------
  ! --- DATA TYPE FOR INTEGER TS DATA INPUT FILE
  ! -------------------------------------------------------------
  TYPE,EXTENDS(TSDataInFileType) :: IntTSDataInFileType
    INTEGER,ALLOCATABLE :: iValues(:)
  CONTAINS
    PROCEDURE,PASS :: Init  => IntTSDataInFile_New
    PROCEDURE,PASS :: Close => IntTSDataInFile_Kill
  END TYPE IntTSDataInFileType
  

  ! -------------------------------------------------------------
  ! --- DATA TYPE FOR REAL TS DATA INPUT FILE
  ! -------------------------------------------------------------
  TYPE,EXTENDS(TSDataInFileType) :: RealTSDataInFileType
    REAL(8),ALLOCATABLE :: rValues(:)
  CONTAINS
    PROCEDURE,PASS :: RealTSDataInFile_AsAsciiFile_New
    PROCEDURE,PASS :: RealTSDataInFile_AsDSSFile_New
    PROCEDURE,PASS :: RealTSDataInFile_New
    PROCEDURE,PASS :: Close                                   => RealTSDataInFile_Kill
    PROCEDURE,PASS :: RealTSDataInFile_ReadData_ForTimeRange
    GENERIC        :: Init                                    => RealTSDataInFile_New                    , &
                                                                 RealTSDataInFile_AsAsciiFile_New        , &
                                                                 RealTSDataInFile_AsDSSFile_New
    GENERIC        :: ReadData                                => RealTSDataInFile_ReadData_ForTimeRange
  END TYPE RealTSDataInFileType
  
  
  ! -------------------------------------------------------------
  ! --- DATA TYPE FOR 2-D REAL TS DATA INPUT FILE
  ! -------------------------------------------------------------
  TYPE,EXTENDS(TSDataInFileType) :: Real2DTSDataInFileType
    INTEGER             :: nRow         = 0
    INTEGER             :: nCol         = 0
    REAL(8),ALLOCATABLE :: rValues(:,:)
  CONTAINS
    PROCEDURE,PASS :: Real2DTSDataInFile_New
    PROCEDURE,PASS :: Real2DTSDataInFile_AsAsciiFile_New
    PROCEDURE,PASS :: Real2DTSDataInFile_AsDSSFile_New
    PROCEDURE,PASS :: Close                                     => Real2DTSDataInFile_Kill
    PROCEDURE,PASS :: Real2DTSDataInFile_ReadData_ForTimeRange
    GENERIC        :: Init                                      => Real2DTSDataInFile_New                   , &
                                                                   Real2DTSDataInFile_AsDSSFile_New         , &
                                                                   Real2DTSDataInFile_AsAsciiFile_New
    GENERIC        :: ReadData                                  => Real2DTSDataInFile_ReadData_ForTimeRange
  END TYPE Real2DTSDataInFileType
  
  
  ! -------------------------------------------------------------
  ! --- OVERLOADED METHODS
  ! -------------------------------------------------------------
  
  !Data readers
  INTERFACE ReadTSData
    MODULE PROCEDURE IntTSDataInFile_ReadData
    MODULE PROCEDURE RealTSDataInFile_ReadData
    MODULE PROCEDURE Real2DTSDataInFile_ReadData
  END INTERFACE ReadTSData

  
  ! -------------------------------------------------------------
  ! --- MISC. DATA
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                   :: ModNameLen = 16
  CHARACTER(LEN=ModNameLen),PARAMETER :: ModName    = 'TSDFileHandler::'





CONTAINS




! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** CONSTRUCTORS
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- NEW INTEGER TS DATA INPUT FILE
  ! -------------------------------------------------------------
  SUBROUTINE IntTSDataInFile_New(TSFile,cFileName,cWorkingDirectory,cFileDescription,TrackTime,BlocksToSkip,iStat)
    CLASS(IntTSDataInFileType)  :: TSFile
    CHARACTER(LEN=*),INTENT(IN) :: cFileName,cWorkingDirectory,cFileDescription
    LOGICAL,INTENT(IN)          :: TrackTime
    INTEGER,INTENT(IN)          :: BlocksToSkip
    INTEGER,INTENT(OUT)         :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+19) :: ThisProcedure = ModName // 'IntTSDataInFile_New'
    CHARACTER                    :: DSSFL*1000
    INTEGER                      :: NCOL,NSP,NFQ,ErrorCode
    LOGICAL                      :: lDummyArray(1) = .FALSE.
    CHARACTER(:),ALLOCATABLE     :: cAbsPathFileName
    
    !Initialize
    iStat = 0
    
    ASSOCIATE (pFile => TSFile%File)
    
      !Initialize file
      CALL pFile%New(FileName=ADJUSTL(cFileName),InputFile=.TRUE.,IsTSFile=.TRUE.,Descriptor=cFileDescription,iStat=iStat)  ;  IF (iStat .EQ. -1) RETURN
      CALL pFile%ReadData(NCOL,iStat)                                                                                       ;  IF (iStat .EQ. -1) RETURN 
      CALL pFile%ReadData(NSP,iStat)                                                                                        ;  IF (iStat .EQ. -1) RETURN 
      CALL pFile%ReadData(NFQ,iStat)                                                                                        ;  IF (iStat .EQ. -1) RETURN 
      CALL pFile%ReadData(DSSFL,iStat)                                                                                      ;  IF (iStat .EQ. -1) RETURN 
      CALL CleanSpecialCharacters(DSSFL)
      DSSFL = ADJUSTL(StripTextUntilCharacter(DSSFL,'/'))
      CALL EstablishAbsolutePathFileName(TRIM(DSSFL),cWorkingDirectory,cAbsPathFileName)
      CALL PrepareTSDInputFile(pFile,TrackTime,TRIM(cFileDescription),NCOL,cAbsPathFileName,NSP,NFQ,BlocksToSkip,lDummyArray,iStat)
      IF (iStat .EQ. -1) RETURN
    
      !Allocate memory for iValue
      ALLOCATE (TSFile%iValues(NCOL)  ,  STAT=ErrorCode)
      IF (ErrorCode .NE. 0) THEN
          CALL SetLastMessage('Error in allocating memory for input data from ' // TRIM(cFileDescription) // '!',f_iFatal,ThisProcedure)
          iStat = -1
          RETURN
      END IF

      !Dimension of iValue
      TSFile%iSize = NCOL
      
    END ASSOCIATE
    
  END SUBROUTINE IntTSDataInFile_New
  
  
  ! -------------------------------------------------------------
  ! --- NEW REAL TS DATA INPUT FILE
  ! -------------------------------------------------------------
  SUBROUTINE RealTSDataInFile_New(TSFile,cFileName,cWorkingDirectory,cFileDescription,TrackTime,BlocksToSkip,lFactorDefined,Factor,RateTypeData,cOtherData,iStat)
    CLASS(RealTSDataInFileType)           :: TSFile
    CHARACTER(LEN=*),INTENT(IN)           :: cFileName,cWorkingDirectory,cFileDescription
    LOGICAL,INTENT(IN)                    :: TrackTime
    INTEGER,INTENT(IN)                    :: BlocksToSkip
    LOGICAL,INTENT(IN)                    :: lFactorDefined
    REAL(8),OPTIONAL,INTENT(OUT)          :: Factor(:)
    LOGICAL,OPTIONAL,INTENT(IN)           :: RateTypeData(:)
    CHARACTER(LEN=*),OPTIONAL,ALLOCATABLE :: cOtherData(:,:)
    INTEGER,INTENT(OUT)                   :: iStat

    !Local variables
    CHARACTER(LEN=ModNameLen+20) :: ThisProcedure = ModName // 'RealTSDataInFile_New'
    CHARACTER                    :: DSSFL*1000
    INTEGER                      :: NCOL,NSP,NFQ,ErrorCode,indx
    LOGICAL                      :: lDummyArray(1) = .FALSE.
    CHARACTER(:),ALLOCATABLE     :: cAbsPathFileName
    
    !Initialize 
    iStat = 0
    IF (lFactorDefined) Factor = 1.0
    
    ASSOCIATE (pFile => TSFile%File)
    
      !Initialize file
      CALL pFile%New(FileName=ADJUSTL(cFileName),InputFile=.TRUE.,IsTSFile=.TRUE.,Descriptor=cFileDescription,iStat=iStat)  ;  IF (iStat .EQ. -1) RETURN
      CALL pFile%ReadData(NCOL,iStat)  ;  IF (iStat .EQ. -1) RETURN 
      IF (lFactorDefined) THEN
        DO indx=1,SIZE(Factor)
          CALL pFile%ReadData(Factor(indx),iStat)  
          IF (iStat .EQ. -1) RETURN 
        END DO
      END IF
      CALL pFile%ReadData(NSP,iStat)    ;  IF (iStat .EQ. -1) RETURN 
      CALL pFile%ReadData(NFQ,iStat)    ;  IF (iStat .EQ. -1) RETURN 
      CALL pFile%ReadData(DSSFL,iStat)  ;  IF (iStat .EQ. -1) RETURN 
      
      !Process additional blocks of data
      IF (BlocksToSkip .GT. 1) THEN
          CALL ProcessAdditionalDataBlocks(pFile,BlocksToSkip,cOtherData,iStat)  
          IF (iStat .EQ. -1) RETURN
      END IF
      
      !Process DSS file if specified
      CALL CleanSpecialCharacters(DSSFL)
      DSSFL = ADJUSTL(StripTextUntilCharacter(DSSFL,'/'))
      CALL EstablishAbsolutePathFileName(TRIM(DSSFL),cWorkingDirectory,cAbsPathFileName)
      IF (PRESENT(RateTypeData)) THEN
          CALL PrepareTSDInputFile(pFile,TrackTime,TRIM(cFileDescription),NCOL,cAbsPathFileName,NSP,NFQ,BlocksToSkip,RateTypeData,iStat)
      ELSE
          CALL PrepareTSDInputFile(pFile,TrackTime,TRIM(cFileDescription),NCOL,cAbsPathFileName,NSP,NFQ,BlocksToSkip,lDummyArray,iStat)
      END IF
      IF (iStat .EQ. -1) RETURN
      
      !Allocate memory for iValue
      ALLOCATE (TSFile%rValues(NCOL)  ,  STAT=ErrorCode)
      IF (ErrorCode .NE. 0) THEN
          CALL SetLastMessage('Error in allocating memory for input data from ' // TRIM(cFileDescription) // '!',f_iFatal,ThisProcedure)
          iStat = -1
          RETURN
      END IF
      TSFile%rValues = 0.0
      
      !Dimension of rValue
      TSFile%iSize = NCOL
      
    END ASSOCIATE
  
  END SUBROUTINE RealTSDataInFile_New
  
  
  ! -------------------------------------------------------------
  ! --- NEW REAL TS DATA INPUT FILE INSTANTIATED DIRECTLY (NOTHING IS READ FROM FILE)
  ! --- Note: Assumes time-stamped data
  ! -------------------------------------------------------------
  SUBROUTINE RealTSDataInFile_AsAsciiFile_New(TSFile,cFileName,cFileDescription,BlocksToSkip,nCol,RateTypeData,iStat)
    CLASS(RealTSDataInFileType) :: TSFile
    CHARACTER(LEN=*),INTENT(IN) :: cFileName,cFileDescription
    INTEGER,INTENT(IN)          :: BlocksToSkip,nCol
    LOGICAL,OPTIONAL,INTENT(IN) :: RateTypeData(:)
    INTEGER,INTENT(OUT)         :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+32),PARAMETER :: ThisProcedure = ModName // 'RealTSDataInFile_AsAsciiFile_New'
    INTEGER                                :: ErrorCode
    
    !Initialize
    iStat = 0
    
    !Open file
    CALL TSFile%File%New(FileName=ADJUSTL(cFileName),InputFile=.TRUE.,IsTSFile=.TRUE.,Descriptor=cFileDescription,iStat=iStat)
    IF (iStat .EQ. -1) RETURN

    !Allocate memory for iValue
    ALLOCATE (TSFile%rValues(nCol)  ,  STAT=ErrorCode)
    IF (ErrorCode .NE. 0) THEN
        CALL SetLastMessage('Error in allocating memory for input data from ' // TRIM(cFileDescription) // '!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF

    !Dimension of rValue
    TSFile%iSize = nCol
    
    !Blocks to skip
    CALL TSFile%File%SetBlocksToSkip(BlocksToSkip)  
    
    !Rate type data?
    IF (PRESENT(RateTypeData)) CALL TSFile%File%SetRateTypeDataVariable(RateTypeData)  
   
  END SUBROUTINE RealTSDataInFile_AsAsciiFile_New
    
  
  ! -------------------------------------------------------------
  ! --- NEW REAL TS DATA INPUT FILE DIRECTLY DEFINED AS A DSS FILE
  ! -------------------------------------------------------------
  SUBROUTINE RealTSDataInFile_AsDSSFile_New(TSFile,cFileName,cFileDescription,TrackTime,nCol,cPathNames,RateTypeData,iStat)
    CLASS(RealTSDataInFileType) :: TSFile
    CHARACTER(LEN=*),INTENT(IN) :: cFileName,cFileDescription,cPathNames(:)
    LOGICAL,INTENT(IN)          :: TrackTime
    INTEGER,INTENT(IN)          :: nCol
    LOGICAL,OPTIONAL,INTENT(IN) :: RateTypeData(:)
    INTEGER,INTENT(OUT)         :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+30) :: ThisProcedure = ModName // 'RealTSDataInFile_AsDSSFile_New'
    INTEGER                      :: ErrorCode
    
    !Initialize
    iStat = 0

    !Check if it is a time-tracking simulation
    IF (.NOT. TrackTime) THEN
        MessageArray(1) = 'DSS input file is being used for ' // cFileDescription
        MessageArray(2) = 'when simulation time is not tracked!'
        CALL SetLastMessage(MessageArray(1:2),f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
      
    ASSOCIATE (pFile => TSFile%File)
    
      !Open DSS file
      CALL pFile%New(FileName=ADJUSTL(cFileName),InputFile=.TRUE.,IsTSFile=.TRUE.,Descriptor=cFileDescription,iStat=iStat)
      IF (iStat .EQ. -1) RETURN

      !Set parameters for the DSS input file
      CALL pFile%SetParametersForDSSFile(PathNames=cPathNames,NColumnsOfData=nCol,iStat=iStat)  
      IF (iStat .EQ. -1) RETURN 
      
      !Set the RateTypeData flag
      IF (PRESENT(RateTypeData)) THEN
          CALL pFile%SetRateTypeDataVariable(RateTypeData)  
      END IF
      
    END ASSOCIATE
    
    !Allocate memory for rValue
    ALLOCATE (TSFile%rValues(nCol)  ,  STAT=ErrorCode)
    IF (ErrorCode .NE. 0) THEN
        CALL SetLastMessage('Error in allocating memory for input data from ' // TRIM(cFileDescription) // '!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF

    !Dimension of rValue
    TSFile%iSize = nCol

  END SUBROUTINE RealTSDataInFile_AsDSSFile_New

  
  ! -------------------------------------------------------------
  ! --- NEW 2D REAL TS DATA INPUT FILE INITIALLY DEFINED AS A TEXT FILE (MAY BE POINTING TO A DSS FILE FOR TS DATA)
  ! -------------------------------------------------------------
  SUBROUTINE Real2DTSDataInFile_New(TSFile,cFileName,cWorkingDirectory,cFileDescription,TrackTime,BlocksToSkip,lFactorDefined,lReadDims,nRow,nCol,Factor,RateTypeData,cOtherData,iStat)
    CLASS(Real2DTSDataInFileType)         :: TSFile
    CHARACTER(LEN=*),INTENT(IN)           :: cFileName,cWorkingDirectory,cFileDescription
    LOGICAL,INTENT(IN)                    :: TrackTime,lFactorDefined,lReadDims
    INTEGER,INTENT(IN)                    :: BlocksToSkip,nRow,nCol
    REAL(8),INTENT(OUT)                   :: Factor(:)
    LOGICAL,OPTIONAL,INTENT(IN)           :: RateTypeData(:)
    CHARACTER(LEN=*),OPTIONAL,ALLOCATABLE :: cOtherData(:,:)
    INTEGER,INTENT(OUT)                   :: iStat

    !Local variables
    CHARACTER(LEN=ModNameLen+22) :: ThisProcedure = ModName // 'Real2DTSDataInFile_New'
    CHARACTER                    :: DSSFL*1000
    INTEGER                      :: nRowLocal,nColLocal,NSP,NFQ,ErrorCode,indx
    LOGICAL                      :: lDummyArray(1) = .FALSE.
    CHARACTER(:),ALLOCATABLE     :: cAbsPathFileName

    !Initialize 
    iStat  = 0
    Factor = 1.0
    
    ASSOCIATE (pFile => TSFile%File)
    
      !Initialize file
      CALL pFile%New(FileName=ADJUSTL(cFileName),InputFile=.TRUE.,IsTSFile=.TRUE.,Descriptor=cFileDescription,iStat=iStat)
      IF (iStat .EQ. -1) RETURN
      
      !Number of rows and columns
      IF (lReadDims) THEN
        CALL pFile%ReadData(nRowLocal,iStat)  ;  IF (iStat .EQ. -1) RETURN 
        CALL pFile%ReadData(nColLocal,iStat)  ;  IF (iStat .EQ. -1) RETURN 
      ELSE
        nRowLocal = nRow
        nColLocal = nCol
      END IF
      
      !Conversion factor
      IF (lFactorDefined) THEN
        DO indx=1,SIZE(Factor)
          CALL pFile%ReadData(Factor(indx),iStat)  
          IF (iStat .EQ. -1) RETURN 
        END DO
      END IF
      
      !Other data
      CALL pFile%ReadData(NSP,iStat)    ;  IF (iStat .EQ. -1) RETURN 
      CALL pFile%ReadData(NFQ,iStat)    ;  IF (iStat .EQ. -1) RETURN 
      CALL pFile%ReadData(DSSFL,iStat)  ;  IF (iStat .EQ. -1) RETURN 
      
      !Process additional blocks of data
      IF (BlocksToSkip .GT. 1) THEN
          CALL ProcessAdditionalDataBlocks(pFile,BlocksToSkip,cOtherData,iStat)  
          IF (iStat .EQ. -1) RETURN
      END IF
      
      !Process DSS file if specified
      CALL CleanSpecialCharacters(DSSFL)
      DSSFL = ADJUSTL(StripTextUntilCharacter(DSSFL,'/'))
      CALL EstablishAbsolutePathFileName(TRIM(DSSFL),cWorkingDirectory,cAbsPathFileName)
      IF (PRESENT(RateTypeData)) THEN
          CALL PrepareTSDInputFile(pFile,TrackTime,TRIM(cFileDescription),nRowLocal*nColLocal,cAbsPathFileName,NSP,NFQ,BlocksToSkip,RateTypeData,iStat)
      ELSE
          CALL PrepareTSDInputFile(pFile,TrackTime,TRIM(cFileDescription),nRowLocal*nColLocal,cAbsPathFileName,NSP,NFQ,BlocksToSkip,lDummyArray,iStat)
      END IF
      IF (iStat .EQ. -1) RETURN
      
      !Allocate memory for iValue
      ALLOCATE (TSFile%rValues(nRowLocal,nColLocal)  ,  STAT=ErrorCode)
      IF (ErrorCode .NE. 0) THEN
          CALL SetLastMessage('Error in allocating memory for input data from ' // TRIM(cFileDescription) // '!',f_iFatal,ThisProcedure)
          iStat = -1
          RETURN
      END IF

      !Dimension of rValue
      TSFile%nRow = nRowLocal
      TSFile%nCol = nColLocal
      
    END ASSOCIATE
  
  END SUBROUTINE Real2DTSDataInFile_New

  
  ! -------------------------------------------------------------
  ! --- NEW 2D REAL TS DATA INPUT FILE INSTANTIATED DIRECTLY (NOTHING IS READ FROM FILE)
  ! --- Note: Assumes time-stamped data
  ! -------------------------------------------------------------
  SUBROUTINE Real2DTSDataInFile_AsAsciiFile_New(TSFile,cFileName,cFileDescription,BlocksToSkip,nRow,nCol,RateTypeData,iStat)
    CLASS(Real2DTSDataInFileType) :: TSFile
    CHARACTER(LEN=*),INTENT(IN)   :: cFileName,cFileDescription
    INTEGER,INTENT(IN)            :: BlocksToSkip,nRow,nCol
    LOGICAL,OPTIONAL,INTENT(IN)   :: RateTypeData(:)
    INTEGER,INTENT(OUT)           :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+34),PARAMETER :: ThisProcedure = ModName // 'Real2DTSDataInFile_AsAsciiFile_New'
    INTEGER                                :: ErrorCode
    
    !Initialize
    iStat = 0
    
    !Open file
    CALL TSFile%File%New(FileName=ADJUSTL(cFileName),InputFile=.TRUE.,IsTSFile=.TRUE.,Descriptor=cFileDescription,iStat=iStat)
    IF (iStat .EQ. -1) RETURN

    !Allocate memory for iValue
    ALLOCATE (TSFile%rValues(nRow,nCol)  ,  STAT=ErrorCode)
    IF (ErrorCode .NE. 0) THEN
        CALL SetLastMessage('Error in allocating memory for input data from ' // TRIM(cFileDescription) // '!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF

    !Dimension of rValue
    TSFile%nRow = nRow
    TSFile%nCol = nCol
    
    !Blocks to skip
    CALL TSFile%File%SetBlocksToSkip(BlocksToSkip)  
    
    !Rate type data?
    IF (PRESENT(RateTypeData)) THEN
        CALL TSFile%File%SetRateTypeDataVariable(RateTypeData)  
    END IF

    
  END SUBROUTINE Real2DTSDataInFile_AsAsciiFile_New
    
  
  ! -------------------------------------------------------------
  ! --- NEW 2D REAL TS DATA INPUT FILE DIRECTLY DEFINED AS A DSS FILE
  ! -------------------------------------------------------------
  SUBROUTINE Real2DTSDataInFile_AsDSSFile_New(TSFile,cFileName,cFileDescription,TrackTime,nRow,nCol,cPathNames,RateTypeData,iStat)
    CLASS(Real2DTSDataInFileType) :: TSFile
    CHARACTER(LEN=*),INTENT(IN)   :: cFileName,cFileDescription,cPathNames(:)
    LOGICAL,INTENT(IN)            :: TrackTime
    INTEGER,INTENT(IN)            :: nRow,nCol
    LOGICAL,OPTIONAL,INTENT(IN)   :: RateTypeData(:)
    INTEGER,INTENT(OUT)           :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+32) :: ThisProcedure = ModName // 'Real2DTSDataInFile_AsDSSFile_New'
    INTEGER                      :: ErrorCode
    
    !Initialize
    iStat = 0

    !Check if it is a time-tracking simulation
    IF (.NOT. TrackTime) THEN
        MessageArray(1) = 'DSS input file is being used for ' // cFileDescription
        MessageArray(2) = 'when simulation time is not tracked!'
        CALL SetLastMessage(MessageArray(1:2),f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
      
    ASSOCIATE (pFile => TSFile%File)
    
      !Open DSS file
      CALL pFile%New(FileName=ADJUSTL(cFileName),InputFile=.TRUE.,IsTSFile=.TRUE.,Descriptor=cFileDescription,iStat=iStat)
      IF (iStat .EQ. -1) RETURN

      !Set parameters for the DSS input file
      CALL pFile%SetParametersForDSSFile(PathNames=cPathNames,NColumnsOfData=nRow*nCol,iStat=iStat)  ;  IF (iStat .EQ. -1) RETURN 
      
      !Set the RateTypeData flag
      IF (PRESENT(RateTypeData)) THEN
          CALL pFile%SetRateTypeDataVariable(RateTypeData)  
      END IF
      
    END ASSOCIATE
    
    !Allocate memory for iValue
    ALLOCATE (TSFile%rValues(nRow,nCol)  ,  STAT=ErrorCode)
    IF (ErrorCode .NE. 0) THEN
        CALL SetLastMessage('Error in allocating memory for input data from ' // TRIM(cFileDescription) // '!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF

    !Dimension of rValue
    TSFile%nRow = nRow
    TSFile%nCol = nCol

  END SUBROUTINE Real2DTSDataInFile_AsDSSFile_New
  
  
  
  
! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** DESTRUCTORS
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- KILL GENERIC TS DATA INPUT FILE
  ! -------------------------------------------------------------
  SUBROUTINE TSDataInFile_Kill(TSFile)
    CLASS(TSDataInFileType) :: TSFile
    
    !LOcal variables
    TYPE(TSDataInFileType) :: Dummy
    
    CALL TSFile%File%Kill()
    TSFile%iSize    = Dummy%iSize
    TSFile%lUpdated = Dummy%lUpdated
    TSFile%File     = Dummy%File
    
  END SUBROUTINE TSDataInFile_Kill
  
  
  ! -------------------------------------------------------------
  ! --- KILL INTEGER TS DATA INPUT FILE
  ! -------------------------------------------------------------
  SUBROUTINE IntTSDataInFile_Kill(TSFile)
    CLASS(IntTSDataInFileType) :: TSFile
    
    !Local variables
    INTEGER :: ErrorCode
    
    !Release file
    CALL TSDataInFile_Kill(TSFile)
    
    !Kill rest of data
    DEALLOCATE (TSFile%iValues , STAT=ErrorCode)       
    
  END SUBROUTINE IntTSDataInFile_Kill
  
  
  ! -------------------------------------------------------------
  ! --- KILL REAL TS DATA INPUT FILE
  ! -------------------------------------------------------------
  SUBROUTINE RealTSDataInFile_Kill(TSFile)
    CLASS(RealTSDataInFileType) :: TSFile
    
    !Local variables
    INTEGER :: ErrorCode
    
    !Release file
    CALL TSDataInFile_Kill(TSFile)
    
    !Kill rest of data
    DEALLOCATE (TSFile%rValues , STAT=ErrorCode)
    
  END SUBROUTINE RealTSDataInFile_Kill

  
  ! -------------------------------------------------------------
  ! --- KILL 2D REAL TS DATA INPUT FILE
  ! -------------------------------------------------------------
  SUBROUTINE Real2DTSDataInFile_Kill(TSFile)
    CLASS(Real2DTSDataInFileType) :: TSFile
    
    !Local variables
    INTEGER                      :: ErrorCode
    TYPE(Real2DTSDataInFileType) :: Dummy
    
    !Release file
    CALL TSDataInFile_Kill(TSFile)
    
    !Kill rest of data
    DEALLOCATE (TSFile%rValues , STAT=ErrorCode)
    TSFile%nRow = Dummy%nRow
    TSFile%nCol = Dummy%nCol
    
  END SUBROUTINE Real2DTSDataInFile_Kill

  
  
  
! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** GETTERS
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- GET FILENAME
  ! -------------------------------------------------------------
  SUBROUTINE GetFileName(TSDataInFile,cName)
    CLASS(TSDataInFileType),INTENT(IN)   :: TSDataInFile
    CHARACTER(:),ALLOCATABLE,INTENT(OUT) :: cName
    
    CALL TSDataInFile%File%GetName(cName)
    
  END SUBROUTINE GetFileName
  
  
  ! -------------------------------------------------------------
  ! --- GET NUMBER OF DATA COLUMNS
  ! -------------------------------------------------------------
  PURE FUNCTION GetNDataColumns(TSDataInFile) RESULT(NCol)
    CLASS(TSDataInFileType),INTENT(IN) :: TSDataInFile
    INTEGER                            :: NCol
    
    NCol = TSDataInFile%iSize
    
  END FUNCTION GetNDataColumns
  
  
  
  
! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** DATA READERS
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- READ INTEGER DATA
  ! -------------------------------------------------------------
  SUBROUTINE IntTSDataInFile_ReadData(TimeStep,cDescription,TSFile,FileReadError,iStat)
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    CHARACTER(LEN=*),INTENT(IN)   :: cDescription
    TYPE(IntTSDataInFileType)     :: TSFile
    INTEGER,INTENT(OUT)           :: FileReadError,iStat
    
    !Local variables
    REAL(8) :: rValues(TSFile%iSize)
    
    !Set the update flag to False
    TSFile%lUpdated = .FALSE.

    !Read data
    CALL Read1DTSData(TSFile%File,TimeStep,cDescription,rValues,FileReadError,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Store read data if ErrorCode is zero
    IF (FileReadError .EQ. 0) THEN
      TSFile%iValues  = INT(rValues)
      TSFile%lUpdated = .TRUE.
    END IF

  END SUBROUTINE IntTSDataInFile_ReadData
  
  
  ! -------------------------------------------------------------
  ! --- READ REAL DATA
  ! -------------------------------------------------------------
  SUBROUTINE RealTSDataInFile_ReadData(TimeStep,cDescription,TSFile,FileReadError,iStat)
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    CHARACTER(LEN=*),INTENT(IN)   :: cDescription
    TYPE(RealTSDataInFileType)    :: TSFile
    INTEGER,INTENT(OUT)           :: FileReadError,iStat
    
    !Set the update flag to False
    TSFile%lUpdated = .FALSE.

    !Read data
    CALL Read1DTSData(TSFile%File,TimeStep,cDescription,TSFile%rValues,FileReadError,iStat)
    IF (iStat .EQ. -1) RETURN

    IF (FileReadError .EQ. 0) TSFile%lUpdated = .TRUE.

  END SUBROUTINE RealTSDataInFile_ReadData
  
  
  ! -------------------------------------------------------------
  ! --- READ DATA FOR A LOCATION FOR TIME RANGE FROM 1-D REAL TS INPUT FILE
  ! -------------------------------------------------------------
  SUBROUTINE RealTSDataInFile_ReadData_ForTimeRange(ThisFile,iCol,cBeginDateAndTime,cEndDateAndTime,nActualOutput,rData,rDataDates,FileReadCode,iStat)
    CLASS(RealTSDataInFileType) :: ThisFile
    INTEGER,INTENT(IN)          :: iCol                               !Defines the column number of data if data is being read from ASCII file
    CHARACTER(LEN=*),INTENT(IN) :: cBeginDateAndTime,cEndDateAndTime
    REAL(8),INTENT(OUT)         :: rData(:),rDataDates(:)
    INTEGER,INTENT(OUT)         :: nActualOutput,FileReadCode,iStat
    
    CALL ThisFile%File%ReadData(1,iCol,1,ThisFile%iSize,iCol,cBeginDateAndTime,cEndDateAndTime,nActualOutput,rData,rDataDates,FileReadCode,iStat)
    
  END SUBROUTINE RealTSDataInFile_ReadData_ForTimeRange
  
  
  ! -------------------------------------------------------------
  ! --- READ 2-D REAL DATA
  ! -------------------------------------------------------------
  SUBROUTINE Real2DTSDataInFile_ReadData(TimeStep,cDescription,TSFile,FileReadError,iStat)
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    CHARACTER(LEN=*),INTENT(IN)   :: cDescription
    TYPE(Real2DTSDataInFileType)  :: TSFile
    INTEGER,INTENT(OUT)           :: FileReadError,iStat
    
    !Set the update flag to False
    TSFile%lUpdated = .FALSE.

    !Read data
    CALL Read2DTSData(TSFile%File,TimeStep,cDescription,TSFile%rValues,FileReadError,iStat)
    IF (iStat .EQ. -1) RETURN

    IF (FileReadError .EQ. 0) TSFile%lUpdated = .TRUE.

  END SUBROUTINE Real2DTSDataInFile_ReadData
  
  
  ! -------------------------------------------------------------
  ! --- READ DATA FOR A LOCATION FOR TIME RANGE FROM 2-D REAL TS INPUT FILE
  ! -------------------------------------------------------------
  SUBROUTINE Real2DTSDataInFile_ReadData_ForTimeRange(ThisFile,iRow,iCol,iPathNameIndex,cBeginDateAndTime,cEndDateAndTime,nActualOutput,rData,rDataDates,FileReadCode,iStat)
    CLASS(Real2DTSDataInFileType) :: ThisFile
    INTEGER,INTENT(IN)            :: iRow,iCol           !Defines the location within the matrix data if data is being read from ASCII file
    INTEGER,INTENT(IN)            :: iPathNameIndex      !Defined ptahname index if data is being read from DSS file
    CHARACTER(LEN=*),INTENT(IN)   :: cBeginDateAndTime,cEndDateAndTime
    REAL(8),INTENT(OUT)           :: rData(:),rDataDates(:)
    INTEGER,INTENT(OUT)           :: nActualOutput,FileReadCode,iStat
    
    FileReadCode = 0
    CALL ThisFile%File%ReadData(iRow,iCol,ThisFile%nRow,ThisFile%nCol,iPathNameIndex,cBeginDateAndTime,cEndDateAndTime,nActualOutput,rData,rDataDates,FileReadCode,iStat)
    
  END SUBROUTINE Real2DTSDataInFile_ReadData_ForTimeRange
  
  
  ! -------------------------------------------------------------
  ! --- READ 1-D ARRAY TIME SERIES DATA
  ! -------------------------------------------------------------
  SUBROUTINE Read1DTSData(ThisFile,TimeStep,DataDescriptor,TSData,FileReadCode,iStat)
    TYPE(GenericFileType)         :: ThisFile
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    CHARACTER(LEN=*),INTENT(IN)   :: DataDescriptor
    REAL(8)                       :: TSData(:)
    INTEGER,INTENT(OUT)           :: FileReadCode,iStat
    
    !Initialize
    iStat = 0

    IF (ThisFile%iGetFileType() .EQ. f_iUNKNOWN) THEN
        FileReadCode = -2  !File not found
        
    ELSE
        !Read data 
        CALL ThisFile%ReadData(TimeStep%CurrentDateAndTime,TSData,FileReadCode,iStat,TimeStep%TrackTime)
        IF (iStat .EQ. -1) RETURN
        
        !Proceed based on the returned error code
        SELECT CASE (FileReadCode)
          !It wasn't time to read; do nothing
          CASE (-1)
        
          !The data is not properly time stamped
          CASE (1)
            CALL GenerateTimeStampError(DataDescriptor)
            iStat = -1
        
          !Error in reading data from DSS file
          CASE (2)
            CALL GenerateDataRetrievalError(DataDescriptor)
            iStat = -1
            
          !Data was read without any problem
          CASE (0)
        
        END SELECT
    END IF

  END SUBROUTINE Read1DTSData


  ! -------------------------------------------------------------
  ! --- READ 2-D ARRAY TIME SERIES DATA
  ! -------------------------------------------------------------
  SUBROUTINE Read2DTSData(ThisFile,TimeStep,DataDescriptor,TSData,FileReadCode,iStat)
    TYPE(GenericFileType)         :: ThisFile
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    CHARACTER(LEN=*),INTENT(IN)   :: DataDescriptor
    REAL(8)                       :: TSData(:,:)
    INTEGER,INTENT(OUT)           :: FileReadCode,iStat
    
    !Initialize
    iStat = 0

    IF (ThisFile%iGetFileType() .EQ. f_iUNKNOWN) THEN
        FileReadCode = -2   !File not found
        
    ELSE 
        !Read data 
        CALL ThisFile%ReadData(TimeStep%CurrentDateAndTime,TSData,FileReadCode,iStat,TimeStep%TrackTime)
        IF (iStat .EQ. -1) RETURN
        
        !Proceed based on the returned error code
        SELECT CASE (FileReadCode)
          !It wasn't time to read; do nothing
          CASE (-1)
        
          !The data is not properly time stamped
          CASE (1)
            CALL GenerateTimeStampError(DataDescriptor)
            iStat = -1
            
          !Error in reading data from DSS file
          CASE (2)
            CALL GenerateDataRetrievalError(DataDescriptor)
            iStat = -1
        
          !Data was read without any problem
          CASE (0)
        
        END SELECT
    END IF

  END SUBROUTINE Read2DTSData





! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** MISC. METHODS
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- CHECK IF NUMBER OF COLUMNS IN A TS DATA INPUT FILE IS SUFFICENT
  ! -------------------------------------------------------------
  SUBROUTINE CheckColNum(TSDataInFile,cFileDescriptor,iColPointers,lCheckMinColNum,iStat)
    CLASS(TSDataInFileType),INTENT(IN) :: TSDataInFile
    CHARACTER(LEN=*),INTENT(IN)        :: cFileDescriptor
    INTEGER,INTENT(IN)                 :: iColPointers(:)
    LOGICAL,INTENT(IN)                 :: lCheckMinColNum
    INTEGER,INTENT(OUT)                :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+11) :: ThisProcedure = ModName // 'CheckColNum'
    INTEGER                      :: iMaxColPointed,iMinColPointed
    
    !Initialize
    iStat          = 0
    iMaxColPointed = MAXVAL(iColPointers)
    
    IF (iMaxColPointed .GT. TSDataInFile%iSize) THEN
      MessageArray(1) = 'There are not enough data columns in '//TRIM(ADJUSTL(LowerCase(cFileDescriptor)))//'!'
      MessageArray(2) = 'Number of columns in file        = '//TRIM(IntToText(TSDataInFile%iSize))
      MessageArray(3) = 'Highest column number referenced = '//TRIM(IntToText(iMaxColPointed))
      CALL SetLastMessage(MessageArray(1:3),f_iFatal,ThisProcedure)
      iStat = -1
      RETURN
    END IF

    IF (lCheckMinColNum) THEN
      iMinColPointed = MINVAL(iColPointers)
      IF (iMinColPointed .LE. 0) THEN
        CALL SetLastMessage('Data column number '//TRIM(IntToText(iMinColPointed))//' does not exist in '//TRIM(ADJUSTL(LowerCase(cFileDescriptor)))//'!',f_iFatal,ThisProcedure) 
        iStat = -1
        RETURN
      END IF
    END IF
       
  END SUBROUTINE CheckColNum
  

  ! -------------------------------------------------------------
  ! --- PROCESS ADDITIONAL DATA BLOCKS IN TS INPUT FILES
  ! -------------------------------------------------------------
  SUBROUTINE ProcessAdditionalDataBlocks(File,BlocksToSkip,cOtherData,iStat)
    TYPE(GenericFileType)        :: File
    INTEGER,INTENT(IN)           :: BlocksToSkip
    CHARACTER(LEN=*),ALLOCATABLE :: cOtherData(:,:)
    INTEGER,INTENT(OUT)          :: iStat
    
    !Local variables
    INTEGER                         :: iCount(BlocksToSkip-1),indx,indxLine,ErrorCode
    CHARACTER(LEN=1000),ALLOCATABLE :: cLines(:)
    
    !Find the maximum number of lines in blocks of data
    iCount = 0
    DO indx=1,BlocksToSkip-1
      CALL File%ReadData(cLines,iStat)  
      IF (iStat .EQ. -1) RETURN
      iCount(indx) = SIZE(cLines)
    END DO
      
    !Allocate memory
    ALLOCATE (cOtherData(MAXVAL(iCount),BlocksToSkip-1))
    cOtherData = ''
      
    !Re-position pointer in data file
    CALL File%RewindFile()                                              !Rewind file
    CALL File%ReadData(cLines,iStat)  ;  IF (iStat .EQ. -1) RETURN      !Skip first block
      
    !Read blocks of data lines
    DO indx=1,BlocksToSkip-1
      DO indxLine=1,iCount(indx)
        CALL File%ReadData(cOtherData(indxLine,indx),iStat)  
        IF (iStat .EQ. -1) RETURN
      END DO
    END DO
    
    !Free memory
    DEALLOCATE (cLines , STAT=ErrorCode)
      
  END SUBROUTINE ProcessAdditionalDataBlocks
  
  
  ! -------------------------------------------------------------
  ! --- SUBROUTINE THAT PREPARES THE TIME SERIES OUTPUT FILES FOR OUTPUT
  ! -------------------------------------------------------------
  SUBROUTINE PrepareTSDOutputFile(ThisFile                 , &
                                  NColumnsOfData           , &
                                  NRowsOfData              , &
                                  OverwriteNColumnsOfData  , &
                                  FormatSpec               , &
                                  Title                    , &
                                  Header                   , &
                                  HeaderFormat             , &
                                  PrintColumnNo            , &
                                  DataUnit                 , &
                                  DataType                 , &
                                  CPart                    , &
                                  FPart                    , &
                                  UnitT                    , &
                                  Subregions               , &
                                  StrmNodes                , &
                                  Layers                   , &
                                  Elements                 , &
                                  GWNodes                  , &
                                  IDs                      , &
                                  MiscArray                , &
                                  iStat                    )
    TYPE(GenericFileType)                :: ThisFile
    INTEGER                              :: NColumnsOfData
    INTEGER,INTENT(IN)                   :: NRowsOfData
    LOGICAL,INTENT(IN)                   :: OverwriteNColumnsOfData,PrintColumnNo
    CHARACTER(LEN=*),INTENT(IN)          :: FormatSpec  ,Title(:)    ,Header(:,:)  ,HeaderFormat(:) , &
                                            DataUnit(:) ,DataType(:) ,CPart(:)     ,FPart(:)        , &
                                            UnitT        
    INTEGER,OPTIONAL,INTENT(IN)          :: Subregions(:),StrmNodes(:),Layers(:),Elements(:),GWNodes(:),IDs(:)
    CHARACTER(LEN=*),OPTIONAL,INTENT(IN) :: MiscArray(:)
    INTEGER,INTENT(OUT)                  :: iStat

    !Local variables
    CHARACTER(LEN=ModNameLen+20)  :: ThisProcedure = ModName // 'PrepareTSDOutputFile'
    INTEGER                       :: indx
    CHARACTER(LEN=32),ALLOCATABLE :: BPart(:)
    CHARACTER(LEN=32)             :: Text
    
    !Initialize
    iStat = 0

    !Prepare time series data output files based on file type
    SELECT CASE (ThisFile%iGetFileType())
      !Undefined file
      CASE (f_iUNKNOWN)
        IF (OverwriteNColumnsOfData) NColumnsOfData=0 

      !ASCII file
      CASE (f_iTXT)
        !Set cache size
        CALL ThisFile%SetCacheSize(NColumnsOfData,NRowsOfData,iStat)  ;  IF (iStat .EQ. -1) RETURN
        !Set print format specification
        CALL ThisFile%SetPrintFormatSpec(FormatSpec,iStat)  ;  IF (iStat .EQ. -1) RETURN
        !Write title lines
        CALL ThisFile%WriteData(Title)
        !Write column headers
        DO indx=1,SIZE(Header(:,:),DIM=1)
            CALL ThisFile%WriteData(Header(indx,:),FormatSpec=HeaderFormat(indx))  
        END DO

      !DSS file
      CASE (f_iDSS)
        !Generate pathnames
        CALL AllocArray(BPart,NColumnsOfData*NRowsOfData,ThisProcedure,iStat)  ;  IF (iStat .EQ. -1) RETURN
        DO indx=1,NColumnsOfData*NRowsOfData
          Text=''
          IF (PrintColumnNo) Text=ADJUSTL(TRIM(IntToText(indx)))
          IF (PRESENT(IDs)) Text=TRIM(Text)//':ID'//TRIM(IntToText(IDs(indx)))
          IF (PRESENT(Subregions)) Text=TRIM(Text)//':SR'//TRIM(IntToText(Subregions(indx)))
          IF (PRESENT(StrmNodes)) Text=TRIM(Text)//':R'//TRIM(IntToText(StrmNodes(indx)))
          IF (PRESENT(Layers)) Text=TRIM(Text)//':L'//TRIM(IntToText(Layers(indx)))
          IF (PRESENT(Elements)) Text=TRIM(Text)//':E'//TRIM(IntToText(Elements(indx)))
          IF (PRESENT(GWNodes)) Text=TRIM(Text)//':GW'//TRIM(IntToText(GWNodes(indx)))
          IF (PRESENT(MiscArray)) Text=TRIM(Text)//':'//TRIM(ADJUSTL(MiscArray(indx)))
          IF (Text(1:1).EQ.':') Text=Text(2:)
          BPart(indx)=Text
        END DO
        CALL ThisFile%SetParametersForDSSFile('IWFM'               , &
                                              BPart                , &
                                              CPart                , &
                                              UnitT                , &
                                              FPart                , &
                                              DataUnit             , &
                                              DataType             , &
                                              '01/01/1800_24:00'   , &    !SimulationStartDateAndTime = '01/01/1800_24:00' ; This is dummy data
                                              1                    , &    !NTIME = 1 ; This is dummy data
                                              NColumnsOfData       , &
                                              NRowsOfData          , &
                                              iStat=iStat          )
        IF (iStat .EQ. -1) RETURN

      !Fortran binary file
      CASE (f_iBIN)
        CALL SetLastMessage('Fortran binary files cannot be used for time series output!',f_iFatal,ThisProcedure)
        iStat = -1

    END SELECT

  END SUBROUTINE PrepareTSDOutputFile


  ! -------------------------------------------------------------
  ! --- SUBROUTINE TO REDEFINE A TIME SERIES ASCII INPUT FILE AS DSS FILE
  ! -------------------------------------------------------------
  SUBROUTINE PrepareTSDInputFile(ThisFile,TrackTime,FileDefinition,NPathNames,DSSFileName,NSP,NFQ,BlocksToSkip,RateTypeData,iStat)
    TYPE(GenericFileType)       :: ThisFile
    LOGICAL,INTENT(IN)          :: TrackTime
    CHARACTER(LEN=*),INTENT(IN) :: FileDefinition
    INTEGER,INTENT(IN)          :: NPathNames,NSP,NFQ,BlocksToSkip
    CHARACTER(LEN=*)            :: DSSFileName
    LOGICAL,OPTIONAL,INTENT(IN) :: RateTypeData(:)
    INTEGER,INTENT(OUT)         :: iStat

    !Local variables
    CHARACTER(LEN=ModNameLen+19)   :: ThisProcedure = ModName // 'PrepareTSDInputFile'
    INTEGER                        :: indx,StartLocation
    CHARACTER(LEN=300)             :: DummyChar*300
    CHARACTER(LEN=300),ALLOCATABLE :: PathNames(:)
    
    !Initialize
    iStat = 0

    !Proceed based on the supplied DSS file name
    DSSFileName=ADJUSTL(StripTextUntilCharacter(DSSFileName,'/'))
    IF (DSSFileName .EQ. '') THEN
      !Set the information related to the ASCII file
      CALL ThisFile%SetNSPVariable(NSP)
      CALL ThisFile%SetNFQVariable(NFQ)
      CALL ThisFile%SetBlocksToSkip(BlocksToSkip)
      IF (PRESENT(RateTypeData)) THEN
          CALL ThisFile%SetRateTypeDataVariable(RateTypeData)
      END IF
    ELSE
      !Check if it is a time-tracking simulation
      IF (.NOT. TrackTime) THEN
          MessageArray(1)='DSS input file is being used for '//FileDefinition
          MessageArray(2)='when simulation time is not tracked!'
          CALL SetLastMessage(MessageArray(1:2),f_iFatal,ThisProcedure)
          iStat = -1
          RETURN
      END IF
      !Allocate aray for pathnames
      ALLOCATE (PathNames(NPathNames))
      !Read pathnames and redefine the time series file as DSS file
      DO indx=1,NPathNames
        CALL ThisFile%ReadData(DummyChar,iStat)  ;  IF (iStat .EQ. -1) RETURN
        CALL CleanSpecialCharacters(DummyChar)
        StartLocation=FirstLocation('/',TRIM(DummyChar))
        IF (StartLocation.EQ.0) THEN
            CALL SetLastMessage('Error in pathnames defined in '//TRIM(LowerCase(FileDefinition))//'!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        PathNames(indx)=ADJUSTL(DummyChar(StartLocation:LEN(DummyChar)))
      END DO
      !Close the ASCII file
      CALL ThisFile%Kill()
      !Open the DSS file
      CALL ThisFile%New(DSSFileName,InputFile=.TRUE.,IsTSFile=.TRUE.,Descriptor=FileDefinition,iStat=iStat)  ;  IF (iStat .EQ. -1) RETURN
      !Set parameters for the DSS input file
      CALL ThisFile%SetParametersForDSSFile(PathNames=PathNames(:),NColumnsOfData=NPathNames,iStat=iStat)  ;  IF (iStat .EQ. -1) RETURN
      !Set the RateTypeData flag
      IF (PRESENT(RateTypeData)) THEN
          CALL ThisFile%SetRateTypeDataVariable(RateTypeData)
      END IF
      !Clear memory
      DEALLOCATE (PathNames)
    END IF

  END SUBROUTINE PrepareTSDInputFile


  ! -------------------------------------------------------------
  ! --- SUBROUTINE TO GENERATE ERROR WHEN A TIME SERIES DATA FILE IS NOT 
  ! ---   TIME-STAMPED PROPERLY FOR A TIME-TRACING SIMULATION
  ! -------------------------------------------------------------
  SUBROUTINE GenerateTimeStampError(ThisDataFile)
    CHARACTER(LEN=*),INTENT(IN) :: ThisDataFile
    
    !Local variables
    CHARACTER(LEN=ModNameLen+22) :: ThisProcedure = ModName // 'GenerateTimeStampError'
    
    MessageArray(1) = TRIM(ADJUSTL(ThisDataFile)) // ' file is not properly time-stamped '
    MessageArray(2) = 'for a time tracking simulation.'
    CALL SetLastMessage(MessageArray(1:2),f_iFatal,ThisProcedure)

  END SUBROUTINE GenerateTimeStampError


  ! -------------------------------------------------------------
  ! --- SUBROUTINE TO GENERATE ERROR WHEN DATA FROM A DSS FILE CANNOT BE RETRIEVED
  ! -------------------------------------------------------------
  SUBROUTINE GenerateDataRetrievalError(ThisDataFile)
    CHARACTER(LEN=*),INTENT(IN) :: ThisDataFile

    !Local variables
    CHARACTER(LEN=ModNameLen+26) :: ThisProcedure = ModName // 'GenerateDataRetrievalError'
    
    MessageArray(1) = TRIM(ADJUSTL(ThisDataFile)) // ' could not be retrieved from '
    MessageArray(2) = 'the DSS file.'
    CALL SetLastMessage(MessageArray(1:2),f_iFatal,ThisProcedure)

  END SUBROUTINE GenerateDataRetrievalError
  
END MODULE TSDFileHandler