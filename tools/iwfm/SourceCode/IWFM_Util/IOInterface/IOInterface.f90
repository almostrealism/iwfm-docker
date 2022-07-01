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
MODULE IOInterface
  USE GeneralUtilities          , ONLY: UpperCase             , &
                                        LowerCase             , &
                                        FirstLocation       
  USE TimeSeriesUtilities       , ONLY: TimeStepType
  USE MessageLogger             , ONLY: SetLastMessage        , &
                                        LogMessage            , &
                                        f_iWarn               , &
                                        f_iFatal              
  USE Class_BaseFileType        , ONLY: BaseFileType          
  USE Class_AsciiFileType       , ONLY: AsciiInFileType       , &
                                        AsciiTSDInFileType    , &
                                        AsciiOutFileType      , &
                                        AsciiTSDOutFileType   
  USE Class_FortBinaryFileType  , ONLY: FortBinFileType       
  USE Class_DssFileType         , ONLY: DSSInFileType         , &
                                        DSSOutFileType        
  USE Class_HDF5FileType        , ONLY: HDF5FileType          , &
                                        f_iGroup              , &
                                        f_iDataSet            , &
                                        f_iAttribute          , &
                                        f_iMaxDatasetNameLen  
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
  PUBLIC :: GenericFileType       , &
            GetFileDate           , &
            iGetFileType_FromName , &
            DoesFileExist         , &
            f_iUNKNOWN            , &
            f_iTXT                , &
            f_iBIN                , &
            f_iDSS                , &
            f_iHDF                , &
            f_iGroup              , &
            f_iDataSet            , &
            f_iAttribute          , &
            f_iMaxDatasetNameLen  

  
  ! -------------------------------------------------------------
  ! --- FILE TYPE ENUMURATOR
  ! -------------------------------------------------------------
  INTEGER,PARAMETER :: f_iUNKNOWN         = 0      , &
                       f_iTXT             = 1      , &
                       f_iBIN             = 3      , &
                       f_iDSS             = 4      , &
                       f_iHDF             = 5      
  INTEGER,PARAMETER :: f_iAsciiInFile     = 1      , &
                       f_iAsciiOutFile    = 11     , &
                       f_iAsciiTSDInFile  = 2      , &
                       f_iAsciiTSDOutFile = 21     , &
                       f_iFortBinFile     = 3      , &
                       f_iDSSInFile       = 4      , &
                       f_iDSSOutFile      = 41     , &
                       f_iHDFFile         = 5


  ! -------------------------------------------------------------
  ! --- GENERIC FILE DATA TYPE
  ! -------------------------------------------------------------
  TYPE GenericFileType
    PRIVATE
    INTEGER                         :: FileType = f_iUNKNOWN          !File type ID; initially set to zero, i.e. file does not exist to identify the file type
    CLASS(BaseFileType),ALLOCATABLE :: Me
  CONTAINS
    PROCEDURE,PASS :: New
    PROCEDURE,PASS :: Kill
    PROCEDURE,PASS :: DoesHDFObjectExist
    PROCEDURE,PASS :: GetTimeStepRelatedData
    PROCEDURE,PASS :: GetName
    PROCEDURE,PASS :: GetFileType
    PROCEDURE,PASS :: GetPositionInFile
    PROCEDURE,PASS :: GetDSSPathNames          
    PROCEDURE,PASS :: iGetFileType
    PROCEDURE,PASS :: SetNSPVariable
    PROCEDURE,PASS :: SetNFQVariable           
    PROCEDURE,PASS :: SetBlocksToSkip          
    PROCEDURE,PASS :: SetRateTypeDataVariable  
    PROCEDURE,PASS :: SetCacheSize            
    PROCEDURE,PASS :: SetPrintFormatSpec      
    PROCEDURE,PASS :: SetParametersForAsciiFile
    PROCEDURE,PASS :: SetParametersForDSSFile  
    PROCEDURE,PASS :: ReadScalarData
    PROCEDURE,PASS :: ReadIntegerArray
    PROCEDURE,PASS :: ReadRealArray
    PROCEDURE,PASS :: ReadLogicalArray
    PROCEDURE,PASS :: ReadCharacterArray
    PROCEDURE,PASS :: ReadMatrixData     
    PROCEDURE,PASS :: ReadTSScalarData
    PROCEDURE,PASS :: ReadTSArrayData
    PROCEDURE,PASS :: ReadTSMatrixData
    PROCEDURE,PASS :: ReadTSData_ForTimeRange
    PROCEDURE,PASS :: ReadEntireTSD_FromDssFile
    PROCEDURE,PASS :: ReadAttribute_FromHDFFile
    PROCEDURE,PASS :: ReadData_OneColumn_Or_OneLocation_FromHDFFile
    PROCEDURE,PASS :: ReadData_OneColumn_OneLocation_FromHDFFile
    PROCEDURE,PASS :: ReadData_AllColumns_OneLocation_SeveralTimeSteps_FromHDFFile
    PROCEDURE,PASS :: ReadData_OneColumn_OneLocation_SeveralTimeSteps_FromHDFFile
    PROCEDURE,PASS :: ReadData_SomeColumns_OneLocation_SeveralTimeSteps_FromHDFFile
    PROCEDURE,PASS :: Read1DArrayDataSet_FromHDFFile
    PROCEDURE,PASS :: Read2DArrayDataSet_FromHDFFile
    PROCEDURE,PASS :: WriteScalarData
    PROCEDURE,PASS :: WriteArrayData
    PROCEDURE,PASS :: WriteMatrixData
    PROCEDURE,PASS :: WriteTSArrayData
    PROCEDURE,PASS :: WriteTSMatrixData
    PROCEDURE,PASS :: WriteEntireTSD_ToDSSFile
    PROCEDURE,PASS :: RewindFile   
    PROCEDURE,PASS :: RewindFile_To_BeginningOfTSData
    PROCEDURE,PASS :: BackspaceFile
    PROCEDURE,PASS :: CreateHDFGroup
    PROCEDURE,PASS :: CreateHDFDataSet
    PROCEDURE,PASS :: WriteHDFAttribute
    GENERIC        :: ReadData           => ReadScalarData                                                 , &
                                            ReadIntegerArray                                               , &
                                            ReadRealArray                                                  , &
                                            ReadLogicalArray                                               , &
                                            ReadCharacterArray                                             , &
                                            ReadMatrixData                                                 , &
                                            ReadTSScalarData                                               , &
                                            ReadTSArrayData                                                , &
                                            ReadTSMatrixData                                               , &
                                            ReadTSData_ForTimeRange                                        , &
                                            ReadEntireTSD_FromDssFile                                      , &
                                            ReadAttribute_FromHDFFile                                      , &
                                            Read1DArrayDataSet_FromHDFFile                                 , &
                                            Read2DArrayDataSet_FromHDFFile                                 , &
                                            ReadData_OneColumn_Or_OneLocation_FromHDFFile                  , &
                                            ReadData_OneColumn_OneLocation_FromHDFFile                     , &
                                            ReadData_AllColumns_OneLocation_SeveralTimeSteps_FromHDFFile   , &
                                            ReadData_OneColumn_OneLocation_SeveralTimeSteps_FromHDFFile    , &
                                            ReadData_SomeColumns_OneLocation_SeveralTimeSteps_FromHDFFile
    GENERIC        :: WriteData          => WriteScalarData                                                , &
                                            WriteArrayData                                                 , &
                                            WriteMatrixData                                                , &
                                            WriteTSArrayData                                               , &
                                            WriteTSMatrixData                                              , &
                                            WriteEntireTSD_ToDSSFile                                       , &
                                            WriteHDFAttribute
    GENERIC        :: CreateHDFObject    => CreateHDFGroup                                                 , &
                                            CreateHDFDataSet          
  END TYPE GenericFileType


  ! -------------------------------------------------------------
  ! --- MISC. ENTITIES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                   :: ModNameLen = 13
  CHARACTER(LEN=ModNameLen),PARAMETER :: ModName    = 'IOInterface::'



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
  ! --- INSTANTIATE A GENERIC FILE
  ! -------------------------------------------------------------    
  RECURSIVE SUBROUTINE New(ThisFile,FileName,InputFile,IsTSFile,Descriptor,FileType,AccessType,FileOpenCode,iStat)
    CLASS(GenericFileType)               :: ThisFile
    CHARACTER(LEN=*),INTENT(IN)          :: FileName
    LOGICAL,INTENT(IN)                   :: InputFile
    LOGICAL,OPTIONAL,INTENT(IN)          :: IsTSFile
    CHARACTER(LEN=*),OPTIONAL,INTENT(IN) :: Descriptor
    CHARACTER(LEN=3),OPTIONAL,INTENT(IN) :: FileType
    CHARACTER(LEN=*),OPTIONAL,INTENT(IN) :: AccessType  !Only used for ASCII files, can be 'SEQUENTIAL' or 'APPEND'; default is 'SEQUENTIAL'
    INTEGER,OPTIONAL,INTENT(OUT)         :: FileOpenCode
    INTEGER,INTENT(OUT)                  :: iStat

    !Local variables
    CHARACTER(LEN=ModNameLen+3),PARAMETER :: ThisProcedure = ModName // 'New'
    CHARACTER                             :: LocalDescriptor*50,LocalAccessType*10
    LOGICAL                               :: LocalIsTSFile
    CHARACTER(LEN=LEN(FileName))          :: TempFileName

    !Initialize
    iStat           = 0
    LocalIsTSFile   = .FALSE.      ; IF (PRESENT(IsTSFile))   LocalIsTSFile   = IsTSFile
    LocalDescriptor = ''           ; IF (PRESENT(Descriptor)) LocalDescriptor = Descriptor
    LocalAccessType = 'SEQUENTIAL' ; IF (PRESENT(AccessType)) LocalAccessType = AccessType
    
    SELECT CASE (ThisFile%FileType)
      CASE (f_iUNKNOWN)  !Identify file type
          IF (PRESENT(FileType)) THEN
              !Check if user-specified file type is recognized
              IF (.NOT. ANY(['TXT','BIN','DSS','HDF'] .EQ. UpperCase(FileType))) THEN
                  CALL SetLastMessage('File type '//UpperCase(FileType)//' is not a recognized type for '//TRIM(LowerCase(LocalDescriptor))//'!',f_iFatal,ThisProcedure) 
                  iStat = -1
              ELSE
                  TempFileName      = FileName(1:FirstLocation('.',FileName,Back=.TRUE.))//ADJUSTL(FileType)
                  ThisFile%FileType = IdentifyFileType(TempFileName,InputFile,LocalIsTSFile,LocalDescriptor)
                  IF (ThisFile%FileType .EQ. f_iUNKNOWN) THEN
                      CALL SetLastMessage('File type '//UpperCase(FileType)//' is not a recognized type for '//TRIM(LowerCase(LocalDescriptor))//'!',f_iFatal,ThisProcedure) 
                      iStat = -1
                  END IF
              END IF
              IF (iStat .EQ. -1) RETURN
          ELSE
              ThisFile%FileType = IdentifyFileType(FileName,InputFile,LocalIsTSFile,LocalDescriptor)
              IF (ThisFile%FileType .EQ. f_iUNKNOWN) THEN
                  CALL SetLastMessage('File type '//UpperCase(FileType)//' is not a recognized type for '//TRIM(LowerCase(LocalDescriptor))//'!',f_iFatal,ThisProcedure) 
                  iStat = -1
                  RETURN
              END IF
          END IF
          IF (PRESENT(FileOpenCode)) THEN
              CALL ThisFile%New(FileName,InputFile,Descriptor=LocalDescriptor,AccessType=LocalAccessType,FileOpenCode=FileOpenCode,iStat=iStat)
          ELSE
              CALL ThisFile%New(FileName,InputFile,Descriptor=LocalDescriptor,AccessType=LocalAccessType,iStat=iStat)
          END IF
          IF (iStat .EQ. -1) RETURN

      CASE (f_iAsciiInFile)  !ASCII input file type
          ALLOCATE (AsciiInFileType :: ThisFile%Me)
          IF (PRESENT(FileOpenCode)) THEN
              CALL ThisFile%Me%New(FileName,lInputFile=.TRUE.,FileOpenCode=FileOpenCode,iStat=iStat)
          ELSE
              CALL ThisFile%Me%New(FileName,lInputFile=.TRUE.,iStat=iStat)
          END IF
          IF (iStat .EQ. -1) RETURN
          ThisFile%FileType = f_iAsciiInFile

      CASE (f_iASciiOutFile)  !ASCII output file type
          ALLOCATE (AsciiOutFileType :: ThisFile%Me)
          IF (PRESENT(FileOpenCode)) THEN
              CALL ThisFile%Me%New(FileName,lInputFile=.FALSE.,AccessType=AccessType,FileOpenCode=FileOpenCode,iStat=iStat)
          ELSE
              CALL ThisFile%Me%New(FileName,lInputFile=.FALSE.,AccessType=AccessType,iStat=iStat)
          END IF
          IF (iStat .EQ. -1) RETURN
          ThisFile%FileType = f_iAsciiOutFile

      CASE (f_iAsciiTSDInFile)  !ASCII time series data input file type
          ALLOCATE (AsciiTSDInFileType :: ThisFile%Me)
          IF (PRESENT(FileOpenCode)) THEN
              CALL ThisFile%Me%New(FileName,lInputFile=.TRUE.,FileOpenCode=FileOpenCode,iStat=iStat)
          ELSE
              CALL ThisFile%Me%New(FileName,lInputFile=.TRUE.,iStat=iStat)
          END IF
          IF (iStat .EQ. -1) RETURN
          ThisFile%FileType = f_iAsciiTSDInFile

      CASE (f_iAsciiTSDOutFile)  !ASCII time series data output file type
          ALLOCATE (AsciiTSDOutFileType :: ThisFile%Me)
          IF (PRESENT(FileOpenCode)) THEN
              CALL ThisFile%Me%New(FileName,lInputFile=.FALSE.,AccessType=AccessType,FileOpenCode=FileOpenCode,iStat=iStat)
          ELSE
              CALL ThisFile%Me%New(FileName,lInputFile=.FALSE.,AccessType=AccessType,iStat=iStat)
          END IF
          IF (iStat .EQ. -1) RETURN
          ThisFile%FileType = f_iAsciiTSDOutFile

      CASE (f_iFortBinFile)  !Fortran binary  file type
          ALLOCATE (FortBinFileType :: ThisFile%Me)
          IF (PRESENT(FileOpenCode)) THEN
              CALL ThisFile%Me%New(FileName,lInputFile=InputFile,FileOpenCode=FileOpenCode,iStat=iStat)
          ELSE
              CALL ThisFile%Me%New(FileName,lInputFile=InputFile,iStat=iStat)
          END IF
          IF (iStat .EQ. -1) RETURN
          ThisFile%FileType = f_iFortBinFile

      CASE (f_iDSSInFile)  !DSS input file type
          ALLOCATE (DSSInFileType :: ThisFile%Me)
          IF (PRESENT(FileOpenCode)) THEN
              CALL ThisFile%Me%New(FileName,lInputFile=.TRUE.,FileOpenCode=FileOpenCode,iStat=iStat)
          ELSE
              CALL ThisFile%Me%New(FileName,lInputFile=.TRUE.,iStat=iStat)
          END IF
          IF (iStat .EQ. -1) RETURN
          ThisFile%FileType = f_iDSSInFile

      CASE (f_iDSSOutFile)  !DSS output file type
          ALLOCATE (DSSOutFileType :: ThisFile%Me)
          IF (PRESENT(FileOpenCode)) THEN
              CALL ThisFile%Me%New(FileName,lInputFile=.FALSE.,FileOpenCode=FileOpenCode,iStat=iStat)
          ELSE
              CALL ThisFile%Me%New(FileName,lInputFile=.FALSE.,iStat=iStat)
          END IF
          IF (iStat .EQ. -1) RETURN
          ThisFile%FileType = f_iDSSOutFile
        
      CASE (f_iHDFFile)   !HDF file type
          ALLOCATE (HDF5FileType :: ThisFile%Me)
          IF (PRESENT(FileOpenCode)) THEN
              CALL ThisFile%Me%New(FileName,lInputFile=InputFile,FileOpenCode=FileOpenCode,iStat=iStat)
          ELSE
              CALL ThisFile%Me%New(FileName,lInputFile=InputFile,iStat=iStat)
          END IF
          IF (iStat .EQ. -1) RETURN
          ThisFile%FileType = f_iHDFFile
          
    END SELECT
    
  END SUBROUTINE New

  
  

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
  ! --- KILL A GENERIC FILE
  ! -------------------------------------------------------------    
  SUBROUTINE Kill(ThisFile,Status)
    CLASS(GenericFileType)               :: ThisFile
    CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: Status

    !Local variables
    CHARACTER             :: LocalStatus*6
    TYPE(GenericFileType) :: Dummy
    
    !Initialize    
    IF (PRESENT(Status)) THEN
        LocalStatus = Status
    ELSE
        LocalStatus = 'KEEP'
    END IF

    SELECT CASE (ThisFile%FileType)
      CASE (f_iUNKNOWN)  !File is not even open; do nothing
          
      CASE DEFAULT
          CALL ThisFile%Me%Kill(LocalStatus)
          DEALLOCATE (ThisFile%Me)
          ThisFile%FileType = Dummy%FileType

    END SELECT
      
  END SUBROUTINE Kill



  
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
  ! --- GET TIMESTEP RELATED DATA
  ! -------------------------------------------------------------
  SUBROUTINE GetTimeStepRelatedData(ThisFile,NTimeSteps,TimeStep)
    CLASS(GenericFileType),INTENT(IN) :: ThisFile
    INTEGER,INTENT(OUT)               :: NTimeSteps
    TYPE(TimeStepType),INTENT(OUT)    :: TimeStep
    
    !Local variables
    CHARACTER(LEN=ModNameLen+22),PARAMETER :: ThisProcedure = ModName // 'GetTimeStepRelatedData'
    
    SELECT TYPE (p => ThisFile%Me)
      TYPE IS (HDF5FileType)
          CALL p%GetTimeStepRelatedData(NTimeSteps,TimeStep)

      CLASS DEFAULT
          CALL LogMessage('GetTimeStepRelatedData method is not supported for file '//ThisFile%Me%Name//'!',f_iWarn,ThisProcedure)
          
    END SELECT
      
  END SUBROUTINE GetTimeStepRelatedData
  
  
  ! -------------------------------------------------------------
  ! --- GET FILE NAME
  ! -------------------------------------------------------------
  SUBROUTINE GetName(ThisFile,cName)
    CLASS(GenericFileType),INTENT(IN) :: ThisFile
    CHARACTER(:),ALLOCATABLE          :: cName
    
    
    SELECT TYPE (p => ThisFile%Me)
        TYPE IS (DSSInFileType)
           CALL p%GetName(cName)
           
        TYPE IS (DSSOutFileType)
           CALL p%GetName(cName)
        
        CLASS DEFAULT
           ALLOCATE(CHARACTER(LEN(ThisFile%Me%Name)) :: cName)
           cName = ThisFile%Me%Name
           
    END SELECT
        
  END SUBROUTINE GetName
  
  
  ! -------------------------------------------------------------
  ! --- GET POSITION IN FILE
  ! -------------------------------------------------------------
  SUBROUTINE GetPositionInFile(ThisFile,iPos)
    CLASS(GenericFileType),INTENT(IN) :: ThisFile
    INTEGER(KIND=8),INTENT(OUT)       :: iPos

    !Local variables
    CHARACTER(LEN=ModNameLen+17),PARAMETER :: ThisProcedure = ModName // 'GetPositionInFile'
    
    IF (.NOT. ALLOCATED(ThisFile%Me)) THEN
        iPos = -1
        RETURN
    END IF

    SELECT TYPE (p => ThisFile%Me)
      TYPE IS (FortBinFileType)
        iPos = p%GetPosition()

      CLASS DEFAULT
        CALL LogMessage('GetPositionInFile method is not supported for file '//ThisFile%Me%Name//'!',f_iWarn,ThisProcedure)

    END SELECT
    
  END SUBROUTINE GetPositionInFile 

 
  ! -------------------------------------------------------------
  ! --- GET THE FILE TYPE ID CHARACTER
  ! -------------------------------------------------------------
  FUNCTION GetFileType(ThisFile) RESULT(FileType)
    CLASS(GenericFileType),INTENT(IN) :: ThisFile
    CHARACTER(LEN=7)                  :: FileType

    SELECT CASE (ThisFile%FileType)
      CASE (f_iUNKNOWN)
          FileType = 'UNKNOWN'

      CASE (f_iAsciiInFile , f_iAsciiOutFile , f_iAsciiTSDInFile , f_iAsciiTSDOutFile)
          FileType = 'TXT'

      CASE (f_iFortBinFile)
          FileType = 'BIN'

      CASE (f_iDSSInFile , f_iDSSOutFile)
          FileType = 'DSS'

      CASE (f_iHDFFile)
          FileType = 'HDF'

    END SELECT
    
  END FUNCTION GetFileType 
  
  
  ! -------------------------------------------------------------
  ! --- GET THE FILE TYPE ID AS INTEGER
  ! -------------------------------------------------------------
  PURE FUNCTION iGetFileType(ThisFile) RESULT(FileType)
    CLASS(GenericFileType),INTENT(IN) :: ThisFile
    INTEGER                           :: FileType

    SELECT CASE (ThisFile%FileType)
      CASE (f_iUNKNOWN)
          FileType = f_iUNKNOWN

      CASE (f_iAsciiInFile , f_iAsciiOutFile , f_iAsciiTSDInFile , f_iAsciiTSDOutFile)
          FileType = f_iTXT

      CASE (f_iFortBinFile)
          FileType = f_iBIN

      CASE (f_iDSSInFile , f_iDSSOutFile)
          FileType = f_iDSS
        
      CASE (f_iHDFFile)
          FileType = f_iHDF
          
    END SELECT
    
  END FUNCTION iGetFileType
  

  ! -------------------------------------------------------------
  ! --- GET THE FILE TYPE ID AS INTEGER FROM FILENAME
  ! -------------------------------------------------------------
  FUNCTION iGetFileType_FromName(cFileName) RESULT(FileType)
    CHARACTER(LEN=*),INTENT(IN) :: cFileName
    INTEGER                     :: FileType
    
    !Local variables
    INTEGER :: FileTypeTemp
    
    FileTypeTemp = IdentifyFileType(cFileName,InputFile=.TRUE.,IsTSFile=.TRUE.,cDescriptor='file type identification')
    
    SELECT CASE (FileTypeTemp)
      CASE (f_iUNKNOWN)
          FileType = f_iUNKNOWN

      CASE (f_iAsciiInFile , f_iAsciiOutFile , f_iAsciiTSDInFile , f_iAsciiTSDOutFile)
          FileType = f_iTXT

      CASE (f_iFortBinFile)
          FileType = f_iBIN

      CASE (f_iDSSInFile , f_iDSSOutFile)
          FileType = f_iDSS
        
      CASE (f_iHDFFile)
          FileType = f_iHDF
          
    END SELECT
    
  END FUNCTION iGetFileType_FromName
  

  ! -------------------------------------------------------------
  ! --- GET PATHNAME LIST FROM INPUT DSS FILE
  ! -------------------------------------------------------------
  SUBROUTINE GetDSSPathnames(ThisFile,PathnameList)                  
    CLASS(GenericFileType)       :: ThisFile
    CHARACTER(LEN=*),ALLOCATABLE :: PathNameList(:)
    
    !Local variables
    CHARACTER(LEN=ModNameLen+15),PARAMETER :: ThisProcedure = ModName // 'GetDSSPathnames'
    
    SELECT TYPE (p => ThisFile%Me)
      TYPE IS (DSSInFileType)  !DSS input file
        CALL p%GetDSSPathnames(PathnameList)
        
      CLASS DEFAULT
        CALL LogMessage('GetDSSPathnames method is not defined for file '//ThisFile%Me%Name//'!',f_iWarn,ThisProcedure)
    
    END SELECT 
  
  END SUBROUTINE GetDSSPathnames
 
  
  
  
  
! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** SETTERS
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************
    
  ! -------------------------------------------------------------
  ! --- SET NSP VARIABLE
  ! -------------------------------------------------------------
  SUBROUTINE SetNSPVariable(ThisFile,NSPValue)
    CLASS(GenericFileType),TARGET :: ThisFile
    INTEGER,INTENT(IN)            :: NSPValue
    
    !Local variables
    CHARACTER(LEN=ModNameLen+14),PARAMETER :: ThisProcedure = ModName // 'SetNSPVariable'

    SELECT TYPE (p => ThisFile%Me)
      TYPE IS (AsciiTSDInFileType)               !ASCII Time series data input file
        CALL p%SetNSP(NSPValue)

      CLASS DEFAULT
        CALL LogMessage('SetNSPVariable method is not defined for file '//ThisFile%Me%Name//'!',f_iWarn,ThisProcedure)

    END SELECT

  END SUBROUTINE SetNSPVariable


  ! -------------------------------------------------------------
  ! --- SET NFQ VARIABLE
  ! -------------------------------------------------------------
  SUBROUTINE SetNFQVariable(ThisFile,NFQValue)
    CLASS(GenericFileType) :: ThisFile
    INTEGER,INTENT(IN)     :: NFQValue

    !Local variables
    CHARACTER(LEN=ModNameLen+14),PARAMETER :: ThisProcedure = ModName // 'SetNFQVariable'

    SELECT TYPE (p => ThisFile%Me)
      TYPE IS (AsciiTSDInFileType)        !ASCII Time series data input file
        CALL p%SetNFQ(NFQValue)

      CLASS DEFAULT
        CALL LogMessage('SetNFQVariable method is not defined for file '//ThisFile%Me%Name//'!',f_iWarn,ThisProcedure) 
        
    END SELECT

  END SUBROUTINE SetNFQVariable


  ! -------------------------------------------------------------
  ! --- SET NumberOfBlocksToSkip
  ! -------------------------------------------------------------
  SUBROUTINE SetBlocksToSkip(ThisFile,NBlocksToSkip)
    CLASS(GenericFileType) :: ThisFile
    INTEGER,INTENT(IN)     :: NBlocksToSkip

    !Local variables
    CHARACTER(LEN=ModNameLen+15),PARAMETER :: ThisProcedure = ModName // 'SetBlocksToSkip'

    SELECT TYPE (p => ThisFile%Me)
      TYPE IS (AsciiTSDInFileType)  !ASCII Time series data input file
        CALL p%SetNumberOfBlocksToSkip(NBlocksToSkip)

      CLASS DEFAULT
        CALL LogMessage('SetBlocksToSkip method is not defined for file '//ThisFile%Me%Name//'!',f_iWarn,ThisProcedure) 

    END SELECT

  END SUBROUTINE SetBlocksToSkip


  ! -------------------------------------------------------------
  ! --- SET RateTypeData
  ! -------------------------------------------------------------
  SUBROUTINE SetRateTypeDataVariable(ThisFile,RateTypeData)
    CLASS(GenericFileType) :: ThisFile
    LOGICAL,INTENT(IN)     :: RateTypeData(:)

    !Local variables
    CHARACTER(LEN=ModNameLen+23),PARAMETER :: ThisProcedure = ModName // 'SetRateTypeDataVariable'
    
    SELECT TYPE (p => ThisFile%Me)
      TYPE IS (AsciiTSDInFileType)  !ASCII Time series data input file
        CALL p%SetRateTypeData(RateTypeData)

      TYPE IS (DSSInFileType)  !DSS input file
        CALL p%SetRateTypeData(RateTypeData)

      CLASS DEFAULT
        CALL LogMessage('SetRateTypeDataVariable method is not defined for the file '//ThisFile%Me%Name//'!',f_iWarn,ThisProcedure) 
        
    END SELECT

  END SUBROUTINE SetRateTypeDataVariable


  ! -------------------------------------------------------------
  ! --- SET PARAMETERS FOR ASCII FILES
  ! -------------------------------------------------------------
  SUBROUTINE SetParametersForAsciiFile(ThisFile,BlocksToSkip,RateTypeData,NSP,NFQ)
    CLASS(GenericFileType)      :: ThisFile
    INTEGER,OPTIONAL,INTENT(IN) :: BlocksToSkip,NSP,NFQ
    LOGICAL,OPTIONAL,INTENT(IN) :: RateTypeData(:)

    !Local variables
    CHARACTER(LEN=ModNameLen+25),PARAMETER :: ThisProcedure = ModName // 'SetParametersForAsciiFile'
    
    SELECT TYPE (p => ThisFile%Me)
      TYPE IS (AsciiTSDInFileType) !ASCII Time series data input file
        IF (PRESENT(NSP)) THEN
          CALL p%SetParameters(BlocksToSkip,RateTypeData,NSP,NFQ)
        ELSE
          CALL p%SetParameters(BlocksToSkip,RateTypeData)
        END IF

      CLASS DEFAULT
        CALL LogMessage('SetParametersForAsciiFile method is not defined for file '//ThisFile%Me%Name//'!',f_iWarn,ThisProcedure) 
    END SELECT

  END SUBROUTINE SetParametersForASCIIFile


  ! -------------------------------------------------------------
  ! --- SET PARAMETERS FOR DSS FILES
  ! -------------------------------------------------------------
  SUBROUTINE SetParametersForDSSFile(ThisFile,APart,BPart,CPart,EPart,FPart,DataUnit,DataType,SimulationStartTime,NTimeSteps,NColumnsOfData,NRowsOfData,PathNames,iStat)
    CLASS(GenericFileType)               :: ThisFile
    CHARACTER(LEN=*),OPTIONAL,INTENT(IN) :: APart,EPart,SimulationStartTime,BPart(:),CPart(:),FPart(:),DataUnit(:),DataType(:),PathNames(:) 
    INTEGER,OPTIONAL,INTENT(IN)          :: NTimeSteps,NRowsOfData
    INTEGER,INTENT(IN)                   :: NColumnsOfData
    INTEGER,INTENT(OUT)                  :: iStat

    !Local variables
    CHARACTER(LEN=ModNameLen+23),PARAMETER :: ThisProcedure = ModName // 'SetParametersForDSSFile'
    
    SELECT TYPE (p => ThisFile%Me)
      TYPE IS (DSSInFileType)  !DSS input file
          IF (PRESENT(NRowsOfData)) THEN
              CALL p%SetParameters_DssInFile(PathNames,NColumnsOfData,NRowsOfData,iStat=iStat)
          ELSE
              CALL p%SetParameters_DssInFile(PathNames,NColumnsOfData,iStat=iStat)
          END IF

      TYPE IS (DSSOutFileType) !DSS output file
          IF (PRESENT(NRowsOfData)) THEN
            IF (PRESENT(APart)) THEN  !The parts of the pathnames are specified
                CALL p%SetParameters(APart,BPart,CPart,EPart,FPart,DataUnit,DataType,SimulationStartTime,NTimeSteps,NColumnsOfdata,NRowsOfData,iStat=iStat)
            ELSE                      !Whole pathnames are specified
                CALL p%SetParameters(PathNames,DataUnit,DataType,SimulationStartTime,NTimeSteps,NColumnsOfdata,NRowsOfData,iStat=iStat)
            END IF
        ELSE
            IF (PRESENT(APart)) THEN  !The parts of the pathnames are specified
                CALL p%SetParameters(APart,BPart,CPart,EPart,FPart,DataUnit,DataType,SimulationStartTime,NTimeSteps,NColumnsOfData,iStat=iStat)
            ELSE                      !Whole pathnames are specified
                CALL p%SetParameters(PathNames,DataUnit,DataType,SimulationStartTime,NTimeSteps,NColumnsOfData,iStat=iStat)
            END IF  
        END IF

      CLASS DEFAULT
        CALL SetLastMessage('SetParametersForDSSFile method is not defined for file '//ThisFile%Me%Name//'!',f_iFatal,ThisProcedure) 
        iStat = -1
    END SELECT

  END SUBROUTINE SetParametersForDSSFile


  ! -------------------------------------------------------------
  ! --- SET TIME SERIES DATA STORAGE ARRAYS
  ! -------------------------------------------------------------
  SUBROUTINE SetCacheSize(ThisFile,NColumnsOfData,NRowsOfData,iStat)
    CLASS(GenericFileType)      :: ThisFile
    INTEGER,INTENT(IN)          :: NColumnsOfData
    INTEGER,OPTIONAL,INTENT(IN) :: NRowsOfData
    INTEGER,INTENT(OUT)         :: iStat

    !Local variables
    CHARACTER(LEN=ModNameLen+12),PARAMETER :: ThisProcedure = ModName // 'SetCacheSize'
    
    SELECT TYPE (p => ThisFile%Me)
      TYPE IS (AsciiTSDOutFileType)  !ASCII Time series data output file
        IF (PRESENT(NRowsOfData)) THEN
          CALL p%SetCacheSize(NColumnsOfData,NRowsOfData)
        ELSE
          CALL p%SetCacheSize(NColumnsOfData)
        END IF
        iStat = 0
        
      CLASS DEFAULT
        CALL SetLastMessage('SetCacheSize method is not defined for file '//ThisFile%Me%Name//'!',f_iFatal,ThisProcedure) 
        iStat = -1

    END SELECT

  END SUBROUTINE SetCacheSize


  ! -------------------------------------------------------------
  ! --- SET FormatStatement FOR ASCII TSD OUTPUT FILES
  ! -------------------------------------------------------------
  SUBROUTINE SetPrintFormatSpec(ThisFile,FormatSpec,iStat)
    CLASS(GenericFileType)      :: ThisFile
    CHARACTER(LEN=*),INTENT(IN) :: FormatSpec
    INTEGER,INTENT(OUT)         :: iStat

    !Local variables
    CHARACTER(LEN=ModNameLen+18),PARAMETER :: ThisProcedure = ModName // 'SetPrintFormatSpec'
    
    SELECT TYPE (p => ThisFile%Me)
      TYPE IS (AsciiTSDOutFileType)  !ASCII Time series data output file
        CALL p%SetFormatStatement(FormatSpec,iStat)

      CLASS DEFAULT
        CALL SetLastMessage('SetPrintFormatSpec method is not defined for file '//ThisFile%Me%Name//'!',f_iFatal,ThisProcedure) 
        iStat = -1
        
    END SELECT

  END SUBROUTINE SetPrintFormatSpec
 
  
  
  
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
  ! --- READ A SCALAR DATA
  ! -------------------------------------------------------------
  SUBROUTINE ReadScalarData(ThisFile,Data,iStat,iPos)
    CLASS(GenericFileType)   :: ThisFile
    CLASS(*),INTENT(OUT)     :: Data
    INTEGER,INTENT(OUT)      :: iStat
    INTEGER(KIND=8),OPTIONAL :: iPos

    !Local variables
    CHARACTER(LEN=ModNameLen+14),PARAMETER :: ThisProcedure = ModName // 'ReadScalarData'
    
    !File is not defined
    IF (.NOT. ALLOCATED(ThisFile%Me)) THEN
        CALL SetLastMessage('An unopened file is being accessed!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    SELECT TYPE (p => ThisFile%Me)
        TYPE IS (AsciiInFileType)
            CALL p%ReadData(Data,iStat=iStat)
            
        
        TYPE IS (AsciiTSDInFileType)
            CALL p%ReadData(Data,iStat=iStat)
            
                
        TYPE IS (FortBinFileType)
            IF (PRESENT(iPos)) THEN
                CALL p%ReadData(Data,iPos,iStat=iStat)
            ELSE
                CALL p%ReadData(Data,iStat=iStat)
            END IF                    
            

        CLASS DEFAULT
            CALL SetLastMessage('Scalar data cannot be read from file '//ThisFile%Me%Name//'!',f_iFatal,ThisProcedure)  
            iStat = -1
            
    END SELECT
        
  END SUBROUTINE ReadScalarData

  
  ! -------------------------------------------------------------
  ! --- READ INTEGER ARRAY DATA
  ! -------------------------------------------------------------
  SUBROUTINE ReadIntegerArray(ThisFile,i,iStat,iPos)
    CLASS(GenericFileType)   :: ThisFile
    INTEGER,INTENT(OUT)      :: i(:)
    INTEGER,INTENT(OUT)      :: iStat
    INTEGER(KIND=8),OPTIONAL :: iPos

    !Local variables
    CHARACTER(LEN=ModNameLen+16),PARAMETER :: ThisProcedure = ModName // 'ReadIntegerArray'
    
    !File is not defined
    IF (.NOT. ALLOCATED(ThisFile%Me)) THEN
        CALL SetLastMessage('An unopened file is being accessed!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    SELECT TYPE (p => ThisFile%Me)
        TYPE IS (AsciiInFileType)
            CALL p%ReadData(i,iStat=iStat)
            
        
        TYPE IS (AsciiTSDInFileType)
            CALL p%ReadData(i,iStat=iStat)
            
                
        TYPE IS (FortBinFileType)
            IF (PRESENT(iPos)) THEN
                CALL p%ReadData(i,iPos,iStat=iStat)
            ELSE
                CALL p%Readdata(i,iStat=iStat)
            END IF                    
            

        CLASS DEFAULT
            CALL SetLastMessage('Array data cannot be read from file '//ThisFile%Me%Name//'!',f_iFatal,ThisProcedure)  
            iStat = -1
            
    END SELECT
        
  END SUBROUTINE ReadIntegerArray
 
  
  ! -------------------------------------------------------------
  ! --- READ REAL ARRAY DATA
  ! -------------------------------------------------------------
  SUBROUTINE ReadRealArray(ThisFile,r,iStat,iPos)
    CLASS(GenericFileType)   :: ThisFile
    REAL(8),INTENT(OUT)      :: r(:)
    INTEGER,INTENT(OUT)      :: iStat
    INTEGER(KIND=8),OPTIONAL :: iPos

    !Local variables
    CHARACTER(LEN=ModNameLen+13),PARAMETER :: ThisProcedure = ModName // 'ReadRealArray'
    
    !File is not defined
    IF (.NOT. ALLOCATED(ThisFile%Me)) THEN
        CALL SetLastMessage('An unopened file is being accessed!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    SELECT TYPE (p => ThisFile%Me)
        TYPE IS (AsciiInFileType)
            CALL p%ReadData(r,iStat=iStat)
            
        
        TYPE IS (AsciiTSDInFileType)
            CALL p%ReadData(r,iStat=iStat)
            
                
        TYPE IS (FortBinFileType)
            IF (PRESENT(iPos)) THEN
                CALL p%ReadData(r,iPos,iStat=iStat)
            ELSE
                CALL p%Readdata(r,iStat=iStat)
            END IF                    
            

        CLASS DEFAULT
            CALL SetLastMessage('Array data cannot be read from file '//ThisFile%Me%Name//'!',f_iFatal,ThisProcedure)    
            iStat = -1
            
    END SELECT
        
  END SUBROUTINE ReadRealArray
 
  
  ! -------------------------------------------------------------
  ! --- READ LOGICAL ARRAY DATA
  ! -------------------------------------------------------------
  SUBROUTINE ReadLogicalArray(ThisFile,l,iStat,iPos)
    CLASS(GenericFileType)   :: ThisFile
    LOGICAL,INTENT(OUT)      :: l(:)
    INTEGER,INTENT(OUT)      :: iStat
    INTEGER(KIND=8),OPTIONAL :: iPos

    !Local variables
    CHARACTER(LEN=ModNameLen+16),PARAMETER :: ThisProcedure = ModName // 'ReadLogicalArray'
    
    !File is not defined
    IF (.NOT. ALLOCATED(ThisFile%Me)) THEN
        CALL SetLastMessage('An unopened file is being accessed!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    SELECT TYPE (p => ThisFile%Me)
        TYPE IS (AsciiInFileType)
            CALL p%ReadData(l,iStat=iStat)
            
        
        TYPE IS (AsciiTSDInFileType)
            CALL p%ReadData(l,iStat=iStat)
            
                
        TYPE IS (FortBinFileType)
            IF (PRESENT(iPos)) THEN
                CALL p%ReadData(l,iPos,iStat=iStat)
            ELSE
                CALL p%Readdata(l,iStat=iStat)
            END IF                    
            

        CLASS DEFAULT
            CALL SetLastMessage('Array data cannot be read from file '//ThisFile%Me%Name//'!',f_iFatal,ThisProcedure)        
            iStat = -1
            
    END SELECT
        
  END SUBROUTINE ReadLogicalArray
 
  
  ! -------------------------------------------------------------
  ! --- READ CHARACTER ARRAY
  ! -------------------------------------------------------------
  SUBROUTINE ReadCharacterArray(ThisFile,DataLines,iStat)
    CLASS(GenericFileType)       :: ThisFile
    CHARACTER(LEN=*),ALLOCATABLE :: DataLines(:)
    INTEGER,INTENT(OUT)          :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+18),PARAMETER :: ThisProcedure = ModName // 'ReadCharacterArray'
    
    !File is not defined
    IF (.NOT. ALLOCATED(ThisFile%Me)) THEN
        CALL SetLastMessage('An unopened file is being accessed!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    SELECT TYPE (p => ThisFile%Me)
        TYPE IS (AsciiInFileType)
            CALL p%ReadCharacterArray(DataLines,iStat)
            
        TYPE IS (AsciiTSDInFileType)
            CALL p%ReadCharacterArray(DataLines,iStat)
            
        TYPE IS (FortBinFileType)
            CALL p%ReadData(DataLines,iStat=iStat)
            
        CLASS DEFAULT
            CALL SetLastMessage('Characater array data cannot be read from file '//ThisFile%Me%Name//'!',f_iFatal,ThisProcedure)
            iStat = -1
    END SELECT
    
  END SUBROUTINE ReadCharacterArray

  
  ! -------------------------------------------------------------
  ! --- READ MATRIX DATA
  ! -------------------------------------------------------------
  SUBROUTINE ReadMatrixData(ThisFile,Data,iStat,iPos)
    CLASS(GenericFileType)   :: ThisFile
    CLASS(*),INTENT(OUT)     :: Data(:,:)
    INTEGER,INTENT(OUT)      :: iStat
    INTEGER(KIND=8),OPTIONAL :: iPos

    !Local variables
    CHARACTER(LEN=ModNameLen+14),PARAMETER :: ThisProcedure = ModName // 'ReadMatrixData'
    
    !File is not defined
    IF (.NOT. ALLOCATED(ThisFile%Me)) THEN
        CALL SetLastMessage('An unopened file is being accessed!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    SELECT TYPE (p => ThisFile%Me)
        TYPE IS (AsciiInFileType)
            CALL p%ReadData(Data,iStat=iStat)
            
        
        TYPE IS (AsciiTSDInFileType)
            CALL p%ReadData(Data,iStat=iStat)
            
                
        TYPE IS (FortBinFileType)
            IF (PRESENT(iPos)) THEN
                CALL p%ReadData(Data,iPos,iStat=iStat)
            ELSE
                CALL p%Readdata(Data,iStat=iStat)
            END IF                    
            

        CLASS DEFAULT
            CALL SetLastMessage('Array data cannot be read from file '//ThisFile%Me%Name//'!',f_iFatal,ThisProcedure) 
            iStat = -1
            
    END SELECT
        
  END SUBROUTINE ReadMatrixData
 
  
  ! -------------------------------------------------------------
  ! --- READ TIME SERIES SCALAR DATA FROM TSD FILE
  ! -------------------------------------------------------------
  SUBROUTINE ReadTSScalarData(ThisFile,Time,Data,FileReadCode,iStat,TraceTime)
    CLASS(GenericFileType)      :: ThisFile
    CHARACTER(LEN=*),INTENT(IN) :: Time
    CLASS(*),INTENT(OUT)        :: Data
    INTEGER,INTENT(OUT)         :: FileReadCode,iStat
    LOGICAL,OPTIONAL,INTENT(IN) :: TraceTime

    !Local variables
    CHARACTER(LEN=ModNameLen+16),PARAMETER :: ThisProcedure = ModName // 'ReadTSScalarData'
    
    !File is not defined
    IF (.NOT. ALLOCATED(ThisFile%Me)) THEN
        CALL SetLastMessage('An unopened file is being accessed!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF

    SELECT TYPE (p => ThisFile%Me)
      TYPE IS (AsciiTSDInFileType) !Ascii time series input data file
          IF (PRESENT(TraceTime)) THEN
              CALL p%ReadData(Time,Data,FileReadCode,iStat,TraceTime)
          ELSE
              CALL p%ReadData(Time,Data,FileReadCode,iStat)
          END IF

          
      TYPE IS (DSSInFileType)  !DSS input file
          CALL p%ReadData(Time,Data,FileReadCode,iStat=iStat)

          
      CLASS DEFAULT
          CALL SetLastMessage('Time series scalar data cannot be read from file '//ThisFile%Me%Name//'!',f_iFatal,ThisProcedure)
          iStat = -1

    END SELECT

  END SUBROUTINE ReadTSScalarData


   ! -------------------------------------------------------------
  ! --- READ TIME SERIES ARRAY DATA FROM TSD FILE
  ! -------------------------------------------------------------
  SUBROUTINE ReadTSArrayData(ThisFile,Time,Data,FileReadCode,iStat,TraceTime)
    CLASS(GenericFileType)      :: ThisFile
    CHARACTER(LEN=*),INTENT(IN) :: Time
    CLASS(*),INTENT(OUT)        :: Data(:)
    INTEGER,INTENT(OUT)         :: FileReadCode,iStat
    LOGICAL,OPTIONAL,INTENT(IN) :: TraceTime

    !Local variables
    CHARACTER(LEN=ModNameLen+15),PARAMETER :: ThisProcedure = ModName // 'ReadTSArrayData'
    
    !File is not defined
    IF (.NOT. ALLOCATED(ThisFile%Me)) THEN
        CALL SetLastMessage('An unopened file is being accessed!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF

    SELECT TYPE (p => ThisFile%Me)
        TYPE IS (AsciiTSDInFileType) !Ascii time series input data file
            IF (PRESENT(TraceTime)) THEN
                CALL p%ReadData(Time,Data,FileReadCode,iStat,TraceTime)
            ELSE
                CALL p%ReadData(Time,Data,FileReadCode,iStat)
            END IF

            
        TYPE IS (DSSInFileType)  !DSS input file
            CALL p%ReadData(Time,Data,FileReadCode,iStat=iStat)

            
        CLASS DEFAULT
            CALL SetLastMessage('Time series array data cannot be read from file '//ThisFile%Me%Name//'!',f_iFatal,ThisProcedure)      
            iStat = -1
            
    END SELECT

  END SUBROUTINE ReadTSArrayData


  ! -------------------------------------------------------------
  ! --- READ TIME SERIES MATRIX DATA TSD FILE
  ! -------------------------------------------------------------
  SUBROUTINE ReadTSMatrixData(ThisFile,Time,Data,FileReadCode,iStat,TraceTime)
    CLASS(GenericFileType)      :: ThisFile
    CHARACTER(LEN=*),INTENT(IN) :: Time
    CLASS(*),INTENT(OUT)        :: Data(:,:)
    INTEGER,INTENT(OUT)         :: FileReadCode,iStat
    LOGICAL,OPTIONAL,INTENT(IN) :: TraceTime

    !Local variables
    CHARACTER(LEN=ModNameLen+16),PARAMETER :: ThisProcedure = ModName // 'ReadTSMatrixData'
    
    !File is not defined
    IF (.NOT. ALLOCATED(ThisFile%Me)) THEN
        CALL SetLastMessage('An unopened file is being accessed!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF

    SELECT TYPE (p => ThisFile%Me)
        TYPE IS (AsciiTSDInFileType) !Ascii time series input data file
            IF (PRESENT(TraceTime)) THEN
                CALL p%ReadData(Time,Data,FileReadCode,iStat,TraceTime)
            ELSE
                CALL p%ReadData(Time,Data,FileReadCode,iStat)
            END IF

            
        TYPE IS (DSSInFileType)  !DSS input file
            CALL p%ReadData(Time,Data,FileReadCode,iStat)

            
        TYPE IS (HDF5FileType)  
            CALL p%ReadData(Time,Data,FileReadCode,iStat=iStat)

            
        CLASS DEFAULT
            CALL SetLastMessage('Time series matrix data cannot be read from file '//ThisFile%Me%Name//'!',f_iFatal,ThisProcedure)      
            iStat = -1
            
    END SELECT

  END SUBROUTINE ReadTSMatrixData


  ! -------------------------------------------------------------
  ! --- READ TIME-SERIES DATA FOR TIME RANGE 
  ! -------------------------------------------------------------
  SUBROUTINE ReadTSData_ForTimeRange(ThisFile,iRow,iCol,iRowMax,iColMax,iPathNameIndex,cBeginDateAndTime,cEndDateAndTime,nActualOutput,Data,rDataDates,FileReadCode,iStat)
    CLASS(GenericFileType)      :: ThisFile
    INTEGER,INTENT(IN)          :: iRow,iCol           !Defines the location within the matrix data
    INTEGER,INTENT(IN)          :: iRowMax,iColMax     !Defines the size of the matrix to be read; if iRowMax=1 input data is array data
    INTEGER,INTENT(IN)          :: iPathNameIndex
    CHARACTER(LEN=*),INTENT(IN) :: cBeginDateAndTime,cEndDateAndTime
    CLASS(*),INTENT(OUT)        :: Data(:)
    REAL(8),INTENT(OUT)         :: rDataDates(:)
    INTEGER,INTENT(OUT)         :: nActualOutput,FileReadCode,iStat

    !Local variables
    CHARACTER(LEN=ModNameLen+23),PARAMETER :: ThisProcedure = ModName // 'ReadTSData_ForTimeRange'
    
    !File is not defined
    IF (.NOT. ALLOCATED(ThisFile%Me)) THEN
        CALL SetLastMessage('An unopened file is being accessed!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF

    SELECT TYPE (p => ThisFile%Me)
        TYPE IS (AsciiTSDInFileType) !Ascii time series input data file
            CALL p%ReadData(iRow,iCol,iRowMax,iColMax,cBeginDateAndTime,cEndDateAndTime,nActualOutput,Data,rDataDates,FileReadCode,iStat=iStat)

        TYPE IS (DSSInFileType)
            CALL p%ReadData(iPathNameIndex,cBeginDateAndTime,cEndDateAndTime,nActualOutput,Data,rDataDates,FileReadCode,iStat=iStat)
            
        CLASS DEFAULT
            CALL SetLastMessage('Time series data for a time range cannot be read from file '//ThisFile%Me%Name//'!',f_iFatal,ThisProcedure)      
            iStat = -1
            
    END SELECT

  END SUBROUTINE ReadTSData_ForTimeRange


  ! -------------------------------------------------------------
  ! --- READ ENTIRE TIME SERIES ARRAY FROM DSS FILE
  ! -------------------------------------------------------------
  SUBROUTINE ReadEntireTSD_FromDssFile(ThisFile,TSValues,PathName,NVals,BDate,BTime,Stat,iStat)                      
    CLASS(GenericFileType)       :: ThisFile
    INTEGER,INTENT(OUT)          :: Stat,iStat
    INTEGER,INTENT(IN)           :: NVals
    CHARACTER(LEN=80),INTENT(IN) :: PathName
    CHARACTER(LEN=*),INTENT(IN)  :: BDate
    CHARACTER(LEN=4),INTENT(IN)  :: BTime
    REAL(8),INTENT(OUT)          :: TSValues(NVals)

    !Local variables
    CHARACTER(LEN=ModNameLen+25),PARAMETER :: ThisProcedure = ModName // 'ReadEntireTSD_FromDssFile'
    
    !File is not defined
    IF (.NOT. ALLOCATED(ThisFile%Me)) THEN
        CALL SetLastMessage('An unopened file is being accessed!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    SELECT TYPE (p => ThisFile%Me)
        TYPE IS (DSSInFileType)  !DSS input file
            CALL p%ReadData(Pathname,NVals,BDate,BTime,TSValues,Stat)
            iStat = 0
            
        CLASS DEFAULT
            CALL SetLastMessage('ReadEntireTSD_FromDssFile method is not defined for file '//ThisFile%Me%Name//'!',f_iFatal,ThisProcedure)
            iStat = -1
            
    END SELECT

  END SUBROUTINE ReadEntireTSD_FromDssFile
  
  
  ! -------------------------------------------------------------
  ! --- READ ATTRIBUTE FROM HDF5 FILE
  ! -------------------------------------------------------------
  SUBROUTINE ReadAttribute_FromHDFFile(ThisFile,cGrpOrDset,cAttrName,ScalarAttrData,ArrayAttrData,iStat)
    CLASS(GenericFileType),INTENT(IN) :: ThisFile
    CHARACTER(LEN=*),INTENT(IN)       :: cGrpOrDset,cAttrName
    CLASS(*),OPTIONAL,INTENT(OUT)     :: ScalarAttrData,ArrayAttrData(:)  !Only one must be supplied
    INTEGER,INTENT(OUT)               :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+25),PARAMETER :: ThisProcedure = ModName // 'ReadAttribute_FromHDFFile'
    
    !File is not defined
    IF (.NOT. ALLOCATED(ThisFile%Me)) THEN
        CALL SetLastMessage('An unopened file is being accessed!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    SELECT TYPE (p => ThisFile%Me)
        TYPE IS (HDF5FileType)  
            CALL p%ReadData(cGrpOrDset,cAttrName,ScalarAttrData,ArrayAttrData,iStat=iStat)

            
        CLASS DEFAULT
            CALL SetLastMessage('ReadAttribute_FromHDFFile method is not defined for file '//ThisFile%Me%Name//'!',f_iFatal,ThisProcedure)
            iStat = -1
            
    END SELECT

    
  END SUBROUTINE ReadAttribute_FromHDFFile
  
    
  ! -------------------------------------------------------------
  ! --- READ AN ENTIRE DATA COLUMN FOR ALL LOCATIONS FROM HDF5 FILE
  ! -------------------------------------------------------------
  SUBROUTINE ReadData_OneColumn_Or_OneLocation_FromHDFFile(ThisFile,lReadCol,iColOrLocNo,Data,Stat,iStat)
    CLASS(GenericFileType) :: ThisFile
    LOGICAL,INTENT(IN)     :: lReadCol
    INTEGER,INTENT(IN)     :: iColOrLocNo
    CLASS(*),INTENT(OUT)   :: Data(:,:)   
    INTEGER,INTENT(OUT)    :: Stat,iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+45),PARAMETER :: ThisProcedure = ModName // 'ReadData_OneColumn_Or_OneLocation_FromHDFFile'

    !File is not defined
    IF (.NOT. ALLOCATED(ThisFile%Me)) THEN
        CALL SetLastMessage('An unopened file is being accessed!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    SELECT TYPE (p => ThisFile%Me)
        TYPE IS (HDF5FileType)  
            CALL p%ReadData(lReadCol,iColOrLocNo,Data,Stat)
            iStat = 0
            
        CLASS DEFAULT
            CALL SetLastMessage('ReadData_OneColumn_Or_OneLocation method is not defined for file '//ThisFile%Me%Name//'!',f_iFatal,ThisProcedure)
            iStat = -1
            
    END SELECT
    
  END SUBROUTINE ReadData_OneColumn_Or_OneLocation_FromHDFFile
  
  
  ! -------------------------------------------------------------
  ! --- READ DATA FOR ONE COLUMN ONE LOCATION
  ! -------------------------------------------------------------
  SUBROUTINE ReadData_OneColumn_OneLocation_FromHDFFile(ThisFile,iColumnNo,iLocationNo,Data,Stat,iStat)
    CLASS(GenericFileType) :: ThisFile
    INTEGER,INTENT(IN)     :: iColumnNo,iLocationNo
    CLASS(*),INTENT(OUT)   :: Data(:)   !Data for (time) for the given column and location
    INTEGER,INTENT(OUT)    :: Stat,iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+42),PARAMETER :: ThisProcedure = ModName // 'ReadData_OneColumn_OneLocation_FromHDFFile'

    !File is not defined
    IF (.NOT. ALLOCATED(ThisFile%Me)) THEN
        CALL SetLastMessage('An unopened file is being accessed!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    SELECT TYPE (p => ThisFile%Me)
        TYPE IS (HDF5FileType)  
            CALL p%ReadData(iColumnNo,iLocationNo,Data,Stat)
            iStat = 0

            
        CLASS DEFAULT
            CALL SetLastMessage('ReadData_OneColumn_OneLocation method is not defined for file '//ThisFile%Me%Name//'!',f_iFatal,ThisProcedure)
            iStat = -1
            
    END SELECT
    
  END SUBROUTINE ReadData_OneColumn_OneLocation_FromHDFFile
  
  
  ! -------------------------------------------------------------
  ! --- READ DATA FOR SEVERAL CONSECUTIVE TIMESTEPS FOR ONE LOCATION
  ! -------------------------------------------------------------
  SUBROUTINE ReadData_AllColumns_OneLocation_SeveralTimeSteps_FromHDFFile(ThisFile,Time,iLocationNo,Data,Stat,iStat)
    CLASS(GenericFileType)      :: ThisFile
    CHARACTER(LEN=*),INTENT(IN) :: Time
    INTEGER,INTENT(IN)          :: iLocationNo
    CLASS(*),INTENT(OUT)        :: Data(:,:)   !Data for (column,timestep) combination for the for the given location
    INTEGER,INTENT(OUT)         :: Stat,iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+60),PARAMETER :: ThisProcedure = ModName // 'ReadData_AllColumns_OneLocation_SeveralTimeSteps_FromHDFFile'

    !File is not defined
    IF (.NOT. ALLOCATED(ThisFile%Me)) THEN
        CALL SetLastMessage('An unopened file is being accessed!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    SELECT TYPE (p => ThisFile%Me)
        TYPE IS (HDF5FileType)  
            CALL p%ReadData(Time,iLocationNo,Data,Stat)
            iStat = 0
            
        CLASS DEFAULT
            CALL SetLastMessage('ReadData_AllColumns_OneLocation_SeveralTimeSteps method is not defined for file '//ThisFile%Me%Name//'!',f_iFatal,ThisProcedure)
            iStat = -1
            
    END SELECT
    
  END SUBROUTINE ReadData_AllColumns_OneLocation_SeveralTimeSteps_FromHDFFile


  ! -------------------------------------------------------------
  ! --- READ DATA FOR SEVERAL CONSECUTIVE TIMESTEPS FOR ONE LOCATION FOR SOME DATA COLUMNS
  ! -------------------------------------------------------------
  SUBROUTINE ReadData_SomeColumns_OneLocation_SeveralTimeSteps_FromHDFFile(ThisFile,Time,iLocationNo,iReadCols,Data,Stat,iStat)
    CLASS(GenericFileType)      :: ThisFile
    CHARACTER(LEN=*),INTENT(IN) :: Time
    INTEGER,INTENT(IN)          :: iLocationNo,iReadCols(:)
    CLASS(*),INTENT(OUT)        :: Data(:,:)   !Data for (column,timestep) combination for the for the given location
    INTEGER,INTENT(OUT)         :: Stat,iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+61),PARAMETER :: ThisProcedure = ModName // 'ReadData_SomeColumns_OneLocation_SeveralTimeSteps_FromHDFFile'

    !File is not defined
    IF (.NOT. ALLOCATED(ThisFile%Me)) THEN
        CALL SetLastMessage('An unopened file is being accessed!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    SELECT TYPE (p => ThisFile%Me)
        TYPE IS (HDF5FileType)  
            CALL p%ReadData(Time,iLocationNo,iReadCols,Data,Stat)
            iStat = 0
            
        CLASS DEFAULT
            CALL SetLastMessage('ReadData_SomeColumns_OneLocation_SeveralTimeSteps method is not defined for file '//ThisFile%Me%Name//'!',f_iFatal,ThisProcedure)
            iStat = -1
            
    END SELECT
    
  END SUBROUTINE ReadData_SomeColumns_OneLocation_SeveralTimeSteps_FromHDFFile

  
  ! -------------------------------------------------------------
  ! --- READ DATA FOR SEVERAL CONSECUTIVE TIMESTEPS FOR ONE LOCATION FOR ONE DATA COLUMN
  ! -------------------------------------------------------------
  SUBROUTINE ReadData_OneColumn_OneLocation_SeveralTimeSteps_FromHDFFile(ThisFile,Time,iLocationNo,iCol,Data,Stat,iStat)
    CLASS(GenericFileType)      :: ThisFile
    CHARACTER(LEN=*),INTENT(IN) :: Time
    INTEGER,INTENT(IN)          :: iLocationNo,iCol
    CLASS(*),INTENT(OUT)        :: Data(:)   !Data for (timestep) for the for the given location and column
    INTEGER,INTENT(OUT)         :: Stat,iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+59),PARAMETER :: ThisProcedure = ModName // 'ReadData_OneColumn_OneLocation_SeveralTimeSteps_FromHDFFile'

    !File is not defined
    IF (.NOT. ALLOCATED(ThisFile%Me)) THEN
        CALL SetLastMessage('An unopened file is being accessed!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    SELECT TYPE (p => ThisFile%Me)
        TYPE IS (HDF5FileType)  
            CALL p%ReadData(Time,iLocationNo,iCol,Data,Stat)
            iStat = 0
            
            
        CLASS DEFAULT
            CALL SetLastMessage('ReadData_OneColumn_OneLocation_SeveralTimeSteps method is not defined for file '//ThisFile%Me%Name//'!',f_iFatal,ThisProcedure)
            iStat = -1
            
    END SELECT
    
  END SUBROUTINE ReadData_OneColumn_OneLocation_SeveralTimeSteps_FromHDFFile

  
  ! -------------------------------------------------------------
  ! --- READ 1-D ARRAY DATASET FROM HDF FILE
  ! -------------------------------------------------------------
  SUBROUTINE Read1DArrayDataSet_FromHDFFile(ThisFile,cPath,Data,iStat)
    CLASS(GenericFileType)      :: ThisFile
    CHARACTER(LEN=*),INTENT(IN) :: cPath
    CLASS(*),INTENT(OUT)        :: Data(:)   
    INTEGER,INTENT(OUT)         :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+30),PARAMETER :: ThisProcedure = ModName // 'Read1DArrayDataSet_FromHDFFile'

    !File is not defined
    IF (.NOT. ALLOCATED(ThisFile%Me)) THEN
        CALL SetLastMessage('An unopened file is being accessed!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    SELECT TYPE (p => ThisFile%Me)
        TYPE IS (HDF5FileType)  
            CALL p%ReadData(cPath,Data,iStat=iStat)

            
        CLASS DEFAULT
            CALL SetLastMessage('Read1DArrayDataSet_FromHDFFile method is not defined for file '//ThisFile%Me%Name//'!',f_iFatal,ThisProcedure)
            iStat = -1
            
    END SELECT
    
  END SUBROUTINE Read1DArrayDataSet_FromHDFFile
  
  
  ! -------------------------------------------------------------
  ! --- READ 2-D ARRAY DATASET FROM HDF FILE
  ! -------------------------------------------------------------
  SUBROUTINE Read2DArrayDataSet_FromHDFFile(ThisFile,cPath,Data,iStat)
    CLASS(GenericFileType)      :: ThisFile
    CHARACTER(LEN=*),INTENT(IN) :: cPath
    CLASS(*),INTENT(OUT)        :: Data(:,:)   
    INTEGER,INTENT(OUT)         :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+30),PARAMETER :: ThisProcedure = ModName // 'Read2DArrayDataSet_FromHDFFile'

    !File is not defined
    IF (.NOT. ALLOCATED(ThisFile%Me)) THEN
        CALL SetLastMessage('An unopened file is being accessed!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    SELECT TYPE (p => ThisFile%Me)
        TYPE IS (HDF5FileType)  
            CALL p%ReadData(cPath,Data,iStat=iStat)

            
        CLASS DEFAULT
            CALL SetLastMessage('Read2DArrayDataSet_FromHDFFile method is not defined for file '//ThisFile%Me%Name//'!',f_iFatal,ThisProcedure)
            iStat = -1
            
    END SELECT
    
  END SUBROUTINE Read2DArrayDataSet_FromHDFFile
 
  
  
  
! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** DATA WRITERS
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************
    
  ! -------------------------------------------------------------
  ! --- WRITE SCALAR DATA 
  ! -------------------------------------------------------------
  SUBROUTINE WriteScalarData(ThisFile,Data)
    CLASS(GenericFileType) :: ThisFile
    CLASS(*),INTENT(IN)    :: Data

    !Local variables
    CHARACTER(LEN=ModNameLen+15),PARAMETER :: ThisProcedure = ModName // 'WriteScalarData'
    
    !File is not defined
    IF (.NOT. ALLOCATED(ThisFile%Me)) THEN
        CALL LogMessage('An unopened file is being accessed!',f_iWarn,ThisProcedure)
        RETURN
    END IF
    
    SELECT TYPE (p => ThisFile%Me)
        TYPE IS (AsciiOutFileType)
            CALL p%WriteData(Data)
            
            
        TYPE IS (AsciiTSDOutFileType)
            CALL p%WriteData(Data)

            
        TYPE IS (FortBinFileType) 
            CALL p%WriteData(Data)
      
            
        CLASS DEFAULT
            CALL LogMessage('Scalar data cannot be written to file '//ThisFile%Me%Name,f_iWarn,ThisProcedure)

    END SELECT
      
  END SUBROUTINE WriteScalarData


  ! -------------------------------------------------------------
  ! --- WRITE ARRAY DATA
  ! -------------------------------------------------------------
  SUBROUTINE WriteArrayData(ThisFile,Data,FormatSpec,cHDFPath)
    CLASS(GenericFileType)               :: ThisFile
    CLASS(*),INTENT(IN)                  :: Data(:)
    CHARACTER(LEN=*),OPTIONAL,INTENT(IN) :: FormatSpec,cHDFPath

    !Local variables
    CHARACTER(LEN=ModNameLen+14),PARAMETER :: ThisProcedure = ModName // 'WriteArrayData'
    
    !File is not defined
    IF (.NOT. ALLOCATED(ThisFile%Me)) THEN
        CALL LogMessage('An unopened file is being accessed!',f_iWarn,ThisProcedure)
        RETURN
    END IF
    
    SELECT TYPE (p => ThisFile%Me)
        TYPE IS (AsciiOutFileType)
            IF (PRESENT(FormatSpec)) THEN
                CALL p%WriteData(Data,FormatSpec)
            ELSE
                CALL p%WriteData(Data)
            END IF
        
            
        TYPE IS (AsciiTSDOutFileType) 
            IF (PRESENT(FormatSpec)) THEN
                CALL p%WriteData(Data,FormatSpec)
            ELSE
                CALL p%WriteData(Data)
            END IF

            
        TYPE IS (FortBinFileType)
            CALL p%WriteData(Data)
            
            
        TYPE IS (HDF5FileType)
            CALL p%WriteData(cHDFPath,Data)
            
        
        CLASS DEFAULT
            CALL LogMessage('Array data cannot be written to file '//ThisFile%Me%Name,f_iWarn,ThisProcedure)
            
    END SELECT
      
  END SUBROUTINE WriteArrayData


  ! -------------------------------------------------------------
  ! --- WRITE MATRIX DATA 
  ! -------------------------------------------------------------
  SUBROUTINE WriteMatrixData(ThisFile,Data,indxDataset,cHDFPath)
    CLASS(GenericFileType)               :: ThisFile
    CLASS(*),INTENT(IN)                  :: Data(:,:)
    INTEGER,OPTIONAL,INTENT(IN)          :: indxDataset
    CHARACTER(LEN=*),OPTIONAL,INTENT(IN) :: cHDFPath

    !Local variables
    CHARACTER(LEN=ModNameLen+15),PARAMETER :: ThisProcedure = ModName // 'WriteMatrixData'
    
    !File is not defined
    IF (.NOT. ALLOCATED(ThisFile%Me)) THEN
        CALL LogMessage('An unopened file is being accessed!',f_iWarn,ThisProcedure)
        RETURN
    END IF
    
    SELECT TYPE (p => ThisFile%Me)
        TYPE IS (FortBinFileType)
            CALL p%WriteData(Data)
            
        TYPE IS (HDF5FileType)
            IF (PRESENT(indxDataset)) THEN
                CALL p%WriteData(indxDataset,Data)
            ELSEIF (PRESENT(cHDFPath)) THEN
                CALL p%WriteData(cHDFPath,Data)
            ELSE
                CALL p%WriteData(Data)
            END IF
        
        CLASS DEFAULT
            CALL LogMessage('Matrix data cannot be written to file '//ThisFile%Me%Name,f_iWarn,ThisProcedure)
            
    END SELECT
      
  END SUBROUTINE WriteMatrixData


  ! -------------------------------------------------------------
  ! --- WRITE TIME SERIES ARRAY DATA 
  ! -------------------------------------------------------------
  SUBROUTINE WriteTSArrayData(ThisFile,SimulationTime,Data,FinalPrint)
    CLASS(GenericFileType)      :: ThisFile
    CHARACTER(LEN=*),INTENT(IN) :: SimulationTime
    CLASS(*),INTENT(IN)         :: Data(:)
    LOGICAL,INTENT(IN)          :: FinalPrint

    !Local variables
    CHARACTER(LEN=ModNameLen+16),PARAMETER :: ThisProcedure = ModName // 'WriteTSArrayData'
    
    !File is not defined
    IF (.NOT. ALLOCATED(ThisFile%Me)) THEN
        CALL LogMessage('An unopened file is being accessed!',f_iWarn,ThisProcedure)
        RETURN
    END IF
    
    SELECT TYPE (p => ThisFile%Me)
        TYPE IS (AsciiTSDOutFileType) !Ascii time series data file
            CALL p%WriteData(SimulationTime,Data,FinalPrint)

            
        TYPE IS (DSSOutFileType)  !DSS output file
            CALL p%WriteData(SimulationTime,Data,FinalPrint)

            
        CLASS DEFAULT
            CALL LogMessage('Time-series array data cannot be written to file '//ThisFile%Me%Name,f_iWarn,ThisProcedure)
            
    END SELECT
      
  END SUBROUTINE WriteTSArrayData

  
  ! -------------------------------------------------------------
  ! --- WRITE MATRIX TIME SERIES DATA 
  ! -------------------------------------------------------------
  SUBROUTINE WriteTSMatrixData(ThisFile,SimulationTime,Data,FinalPrint)
    CLASS(GenericFileType)      :: ThisFile
    CHARACTER(LEN=*),INTENT(IN) :: SimulationTime
    CLASS(*),INTENT(IN)         :: Data(:,:)
    LOGICAL,INTENT(IN)          :: FinalPrint

    !Local variables
    CHARACTER(LEN=ModNameLen+17),PARAMETER :: ThisProcedure = ModName // 'WriteTSMatrixData'
    
    !File is not defined
    IF (.NOT. ALLOCATED(ThisFile%Me)) THEN
        CALL LogMessage('An unopened file is being accessed!',f_iWarn,ThisProcedure)
        RETURN
    END IF
    
    SELECT TYPE (p => ThisFile%Me)
        TYPE IS (AsciiTSDOutFileType) !Ascii time series data file
            CALL p%WriteData(SimulationTime,Data,FinalPrint)

            
        TYPE IS (DSSOutFileType)  !DSS output file
            CALL p%WriteData(SimulationTime,Data,FinalPrint)

            
        TYPE IS (HDF5FileType)  !HDF5 file
            CALL p%WriteData(Data)

            
        CLASS DEFAULT
            CALL LogMessage('Time-series matrix data cannot be written to file '//ThisFile%Me%Name,f_iWarn,ThisProcedure)
            
    END SELECT
      
  END SUBROUTINE WriteTSMatrixData


  ! -------------------------------------------------------------
  ! --- WRITE ENTIRE ARRAY TIME SERIES DATA 
  ! -------------------------------------------------------------
  SUBROUTINE WriteEntireTSD_ToDSSFile(ThisFile,r,PName,NValues,BeginDate,BeginTime)            
    CLASS(GenericFileType) :: ThisFile
    INTEGER,INTENT(IN)     :: NValues
    REAL(8),INTENT(IN)     :: r(NValues)
    CHARACTER,INTENT(IN)   :: PName*80,BeginDate*10,BeginTime*4

    !Local variables
    CHARACTER(LEN=ModNameLen+24),PARAMETER :: ThisProcedure = ModName // 'WriteEntireTSD_ToDSSFile'
    
    !File is not defined
    IF (.NOT. ALLOCATED(ThisFile%Me)) THEN
        CALL LogMessage('An unopened file is being accessed!',f_iWarn,ThisProcedure)
        RETURN
    END IF
    
    SELECT TYPE (p => ThisFile%Me)
        TYPE IS (DSSOutFileType)  !DSS input file
            CALL p%WriteData(r,PName,NValues,BeginDate,BeginTime)

            
        CLASS DEFAULT
            CALL LogMessage('WriteEntireTSD_FromDssFile method is not defined for file '//ThisFile%Me%Name//'!',f_iWarn,ThisProcedure)
            
    END SELECT
      
  END SUBROUTINE WriteEntireTSD_ToDSSFile

  
  
  
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
  ! --- IDENTIFY FILE TYPE FROM ITS EXTENSION
  ! -------------------------------------------------------------
  FUNCTION IdentifyFileType(FileName,InputFile,IsTSFile,cDescriptor) RESULT(FileType)
    CHARACTER(LEN=*),INTENT(IN) :: FileName,cDescriptor
    LOGICAL,INTENT(IN)          :: InputFile
    LOGICAL                     :: IsTSFile
    INTEGER                     :: FileType

    !Local variables
    CHARACTER(LEN=ModNameLen+16),PARAMETER :: ThisProcedure            = ModName // 'IdentifyFileType'
    CHARACTER(LEN=3),PARAMETER             :: AsciiFileExtensions(7)   = ['TXT','DAT','IN ','IN1','IN2','OUT','BUD']
    CHARACTER(LEN=3),PARAMETER             :: FortBinFileExtensions(1) = ['BIN']
    CHARACTER(LEN=3),PARAMETER             :: DSSFileExtensions(1)     = ['DSS']
    CHARACTER(LEN=4),PARAMETER             :: HDF5FileExtensions(4)    = ['HDF ','H5  ','HE5 ','HDF5']
    CHARACTER(LEN=LEN(FileName))           :: TrimmedFileName
    CHARACTER(LEN=3)                       :: FileNameExtension
    INTEGER                                :: BeginLocation
    
    !Trim the file name and find its length
    TrimmedFileName = TRIM(ADJUSTL(FileName))

    !Find the file name extension
    FileNameExtension = ''
    BeginLocation     = SCAN(TRIM(TrimmedFileName),'.',BACK=.TRUE.)
    IF (BeginLocation .GT. 0) FileNameExtension = UpperCase(TrimmedFileName(BeginLocation+1:))

    !Find the file type based on its extension
    FileType = f_iUNKNOWN
    
    !ASCII file
    IF (ANY(AsciiFileExtensions .EQ. FileNameExtension)) THEN        
        IF (IsTSFile) THEN
            SELECT CASE (InputFile)
                CASE (.TRUE.)
                    FileType = f_iAsciiTSDInFile
                CASE (.FALSE.)
                    FileType = f_iAsciiTSDOutFile
            END SELECT
        ELSE
            SELECT CASE (InputFile)
                CASE (.TRUE.)
                    FileType = f_iAsciiInFile
                CASE (.FALSE.)
                    FileType = f_iAsciiOutFile
            END SELECT
        END IF
    
    !Fortran binary file  
    ELSE IF (ANY(FortBinFileExtensions .EQ. FileNameExtension)) THEN  
        FileType = f_iFortBinFile
        
    !DSS File
    ELSE IF (ANY(DSSFileExtensions .EQ. FileNameExtension)) THEN      
        SELECT CASE (InputFile)
            CASE (.TRUE.)
                FileType = f_iDSSInFile
            CASE (.FALSE.)
                FileType = f_iDSSOutFile
        END SELECT
            
    !HDF5 file
    ELSE IF (ANY(HDF5FileExtensions .EQ. FileNameExtension)) THEN  !Is it HDF5 
        FileType = f_iHDFFile
    END IF

  END FUNCTION IdentifyFileType
 
  
  ! -------------------------------------------------------------
  ! --- REWIND FILE
  ! -------------------------------------------------------------
  SUBROUTINE RewindFile(ThisFile)
    CLASS(GenericFileType) :: ThisFile
    
    !Local variables
    CHARACTER(LEN=ModNameLen+10),PARAMETER :: ThisProcedure = ModName // 'RewindFile'
    
    IF (.NOT. ALLOCATED(ThisFile%Me)) THEN
        CALL LogMessage('An unopened file is being rewound!',f_iWarn,ThisProcedure) 
        RETURN
    END IF

    SELECT TYPE (p => ThisFile%Me)
      TYPE IS (AsciiInFileType)  !ASCII input file type
          CALL p%Rewind()
    
      TYPE IS (AsciiOutFileType) !ASCII output file type
          CALL p%Rewind()

      TYPE IS (AsciiTSDInFileType)  !ASCII time series data input file 
          CALL p%Rewind()

      TYPE IS (FortBinFileType)  !Fortran binary file type
          CALL p%Rewind()
        
      TYPE IS (HDF5FileType)
          CALL p%Rewind()

      CLASS DEFAULT   !Cannot rewind other types of files
          CALL LogMessage('Rewind method for file '//ThisFile%Me%Name//' is not defined!',f_iWarn,ThisProcedure)
          
    END SELECT
      
  END SUBROUTINE RewindFile


  ! -------------------------------------------------------------
  ! --- REWIND ASCII TIME SERIES INPUT FILE TO THE BEGINNING OF TIME SERIES DATA
  ! -------------------------------------------------------------
  SUBROUTINE RewindFile_To_BeginningOfTSData(ThisFile,iStat)
    CLASS(GenericFileType) :: ThisFile
    INTEGER,INTENT(OUT)    :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+31),PARAMETER :: ThisProcedure = ModName // 'RewindFile_To_BeginningOfTSData'
    
    IF (.NOT. ALLOCATED(ThisFile%Me)) THEN
        CALL SetLastMessage('An unopened file is being rewound!',f_iFatal,ThisProcedure) 
        iStat = -1
        RETURN
    END IF

    SELECT TYPE (p => ThisFile%Me)
      TYPE IS (AsciiTSDInFileType)  
          CALL p%Rewind_To_BeginningOfTSData(iStat)
          
      TYPE IS (DSSInFileType)  
          iStat = 0
          CALL p%Rewind_To_BeginningOfTSData()

    END SELECT
      
  END SUBROUTINE RewindFile_To_BeginningOfTSData


  ! -------------------------------------------------------------
  ! --- BACKSPACE FILE
  ! -------------------------------------------------------------
  SUBROUTINE BackspaceFile(ThisFile,NBackspace)
    CLASS(GenericFileType)      :: ThisFile
    INTEGER,OPTIONAL,INTENT(IN) :: NBackspace

    !Local variables
    CHARACTER(LEN=ModNameLen+13),PARAMETER :: ThisProcedure = ModName // 'BackspaceFile'
    INTEGER                                :: LocalNBackspace
    
    !Initialize
    IF (PRESENT(NBackspace)) THEN
        LocalNBackspace = NBackspace
    ELSE
        LocalNBackspace = 1  
    END IF
    
    !File is not defined
    IF (.NOT. ALLOCATED(ThisFile%Me)) THEN
        CALL LogMessage('An unopened file is being backspaced!',f_iWarn,ThisProcedure)
        RETURN
    END IF

    SELECT TYPE (p => ThisFile%Me)
      TYPE IS (AsciiInFileType)  !ASCII input file type
        CALL p%Backspace(LocalNBackspace)

      TYPE IS (AsciiTSDInFileType)  !ASCII time series data input file 
        CALL p%Backspace(LocalNBackspace)

      CLASS DEFAULT 
        CALL LogMessage('Backspace method for file '//ThisFile%Me%Name//' is not defined!',f_iWarn,ThisProcedure)

      END SELECT
      
  END SUBROUTINE BackspaceFile


! ! -------------------------------------------------------------
! ! --- SUBROUTINE TO FIND OUT THE DATE OF A FILE
! ! -------------------------------------------------------------
  FUNCTION GetFileDate(FileName) RESULT(FileDateAndTime)
!   USE IFPORT
   CHARACTER(LEN=*),INTENT(IN) :: FileName
   CHARACTER(LEN=22)           :: FileDateAndTime

!   !Local variables
!   !^^^^^^^^^^^^^^^
!   CHARACTER(LEN=22)::FileDateAndTime_temp
!   INTEGER(4)::Length,Handle
!   TYPE(FILE$INFO)::MyFile
!   INTEGER(2)::Year,Month,Day,Hour,Minute,Second
!   CHARACTER(LEN=2)::CharMonth,CharDay,CharHour,CharMinute,CharSecond
!   LOGICAL::FileFound

    !Note: This subroutine is made redundent for portability reasons (Can Dogrul 09/23/2010)
    FileDateAndTime = ''

!   Handle    = FILE$FIRST
!   FileFound = .FALSE.
!   Length    = GETFILEINFOQQ(FileName,MyFile,Handle)
!   IF (Handle .NE. FILE$ERROR) THEN
!     CALL UNPACKTIMEQQ(MyFile%LastWrite,Year,Month,Day,Hour,Minute,Second)
!     FileFound = .TRUE.
!   END IF

!   IF (.NOT. FileFound) THEN
!     FileDateAndTime = ''
!     RETURN
!   END IF

!   !Convert date and time that is in numbers to characters
!   CharMonth =TRIM(IntToText(INT(Month,4)))
!   CharDay   =TRIM(IntToText(INT(Day,4)))
!   CharHour  =TRIM(IntToText(INT(Hour,4)))
!   CharMinute=TRIM(IntToText(INT(Minute,4)))
!   CharSecond=TRIM(IntToText(INT(Second,4)))

!   !Pad the date and time components with zero if necessary
!   IF (LEN(CharMonth)-LEN_TRIM(CharMonth)  .GT.0) CharMonth =REPEAT('0',LEN(CharMonth)-LEN_TRIM(CharMonth))//TRIM(CharMonth)
!   IF (LEN(CharDay)-LEN_TRIM(CharDay)      .GT.0) CharDay   =REPEAT('0',LEN(CharDay)-LEN_TRIM(CharDay))//TRIM(CharDay)
!   IF (LEN(CharHour)-LEN_TRIM(CharHour)    .GT.0) CharHour  =REPEAT('0',LEN(CharHour)-LEN_TRIM(CharHour))//TRIM(CharHour)
!   IF (LEN(CharMinute)-LEN_TRIM(CharMinute).GT.0) CharMinute=REPEAT('0',LEN(CharMinute)-LEN_TRIM(CharMinute))//TRIM(CharMinute)
!   IF (LEN(CharSecond)-LEN_TRIM(CharSecond).GT.0) CharSecond=REPEAT('0',LEN(CharSecond)-LEN_TRIM(CharSecond))//TRIM(CharSecond)

!   WRITE (FileDateAndTime_Temp,'(4A,I4,2X,5A)') CharMonth,'/',CharDay,'/',Year,CharHour,':',CharMinute,':',CharSecond
!   FileDateAndTime=ADJUSTL(FileDateAndTime_temp)

  END FUNCTION GetFileDate
  
  
  ! -------------------------------------------------------------
  ! --- CREATE A GROUP IN AN HDF5 FILE
  ! -------------------------------------------------------------
  SUBROUTINE CreateHDFGroup(ThisFile,cPathname)
    CLASS(GenericFileType)       :: ThisFile
    CHARACTER(LEN=*),INTENT(IN)  :: cPathname

    !Local variables
    CHARACTER(LEN=ModNameLen+14),PARAMETER :: ThisProcedure = ModName // 'CreateHDFGroup'
    
    !File is not defined
    IF (.NOT. ALLOCATED(ThisFile%Me)) THEN
        CALL LogMessage('An unopened file is being accessed!',f_iWarn,ThisProcedure)
        RETURN
    END IF

    SELECT TYPE (p => ThisFile%Me)
        TYPE IS (HDF5FileType)  !HDF5 file type
            CALL p%CreateGroup(cPathname)
                    
        CLASS DEFAULT 
            CALL LogMessage('CreateHDFGroup method for file '//ThisFile%Me%Name//' is not defined!',f_iWarn,ThisProcedure)

    END SELECT
      
  END SUBROUTINE CreateHDFGroup
 

  ! -------------------------------------------------------------
  ! --- CREATE A DATASET IN AN HDF5 FILE
  ! -------------------------------------------------------------
  SUBROUTINE CreateHDFDataSet(ThisFile,cPathNames,NColumns,NTime,TimeStep,DataType,iStat)
    CLASS(GenericFileType),INTENT(IN) :: ThisFile
    CHARACTER(LEN=*),INTENT(IN)       :: cPathNames(:)
    INTEGER,INTENT(IN)                :: NColumns(:),NTime
    TYPE(TimeStepType),INTENT(IN)     :: TimeStep
    CLASS(*),INTENT(IN)               :: DataType
    INTEGER,INTENT(OUT)               :: iStat

    !Local variables
    CHARACTER(LEN=ModNameLen+16),PARAMETER :: ThisProcedure = ModName // 'CreateHDFDataSet'
    
    !File is not defined
    IF (.NOT. ALLOCATED(ThisFile%Me)) THEN
        CALL SetLastMessage('An unopened file is being accessed!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF

    SELECT TYPE (p => ThisFile%Me)
        TYPE IS (HDF5FileType)  !HDF5 file type
            CALL p%CreateDataSet(cPathNames,NColumns,NTime,TimeStep,DataType,iStat=iStat)
                    
        CLASS DEFAULT 
            CALL SetLastMessage('CreateHDFDataSet method for file '//ThisFile%Me%Name//' is not defined!',f_iFatal,ThisProcedure)
            iStat = -1
            
    END SELECT
      
  END SUBROUTINE CreateHDFDataSet


  ! -------------------------------------------------------------
  ! --- CREATE AN ATTRIBUTE IN AN HDF5 FILE
  ! -------------------------------------------------------------
  SUBROUTINE WriteHDFAttribute(ThisFile,iObjectType,cGrpOrDset,cAttrName,ScalarAttrData,ArrayAttrData)
    CLASS(GenericFileType),INTENT(IN) :: ThisFile
    INTEGER,INTENT(IN)                :: iObjectType
    CHARACTER(LEN=*),INTENT(IN)       :: cGrpOrDset,cAttrName
    CLASS(*),OPTIONAL,INTENT(IN)      :: ScalarAttrData,ArrayAttrData(:)   !Only one must be specified

    !Local variables
    CHARACTER(LEN=ModNameLen+17),PARAMETER :: ThisProcedure = ModName // 'WriteHDFAttribute'
    
    !File is not defined
    IF (.NOT. ALLOCATED(ThisFile%Me)) THEN
        CALL LogMessage('An unopened file is being accessed!',f_iWarn,ThisProcedure)
        RETURN
    END IF

    SELECT TYPE (p => ThisFile%Me)
        TYPE IS (HDF5FileType)  !HDF5 file type
            CALL p%WriteData(iObjectType,cGrpOrDset,cAttrName,ScalarAttrData,ArrayAttrData)
                    
        CLASS DEFAULT 
            CALL LogMessage('WriteHDFAttribute method for file '//ThisFile%Me%Name//' is not defined!',f_iWarn,ThisProcedure)
            
    END SELECT
      
  END SUBROUTINE WriteHDFAttribute
  
  
  ! -------------------------------------------------------------
  ! --- DOES THE FILE EXIST
  ! -------------------------------------------------------------
  FUNCTION DoesFileExist(cFileName) RESULT(lExist)
    CHARACTER(LEN=*),INTENT(IN) :: cFileName
    LOGICAL                     :: lExist
    
    !Local variables
    INTEGER               :: ErrorCode,iStat
    TYPE(GenericFileType) :: AFile
    
    !Try to open the file to see if an error occurs
    CALL AFile%New(cFileName,InputFile=.TRUE.,FileOpenCode=ErrorCode,iStat=iStat)
    IF (ErrorCode .EQ. 0) THEN
        lExist = .TRUE.
    ELSE
        lExist = .FALSE.
    END IF
    
    !Close the file
    CALL AFile%Kill()
    
  END FUNCTION DoesFileExist
  
  
  ! -------------------------------------------------------------
  ! --- CHECK BY NAME IF AN OBJECT EXISTS IN AN HDF5 FILE
  ! -------------------------------------------------------------
  FUNCTION DoesHDFObjectExist(ThisFile,cName) RESULT(lExist)
    CLASS(GenericFileType),INTENT(IN) :: ThisFile
    CHARACTER(LEN=*),INTENT(IN)       :: cName
    LOGICAL                           :: lExist

    !Local variables
    CHARACTER(LEN=ModNameLen+18),PARAMETER :: ThisProcedure = ModName // 'DoesHDFObjectExist'
    
    !File is not defined
    IF (.NOT. ALLOCATED(ThisFile%Me)) THEN
        lExist = .FALSE.
        RETURN
    END IF

    SELECT TYPE (p => ThisFile%Me)
        TYPE IS (HDF5FileType)  !HDF5 file type
            lExist = p%DoesObjectExist(cName)
            
        CLASS DEFAULT 
            lExist = .FALSE.
            
    END SELECT
      
  END FUNCTION DoesHDFObjectExist
  

END MODULE