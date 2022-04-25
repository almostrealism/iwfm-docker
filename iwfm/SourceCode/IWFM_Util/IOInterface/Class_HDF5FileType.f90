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
MODULE Class_HDF5FileType
  USE ISO_C_BINDING
  USE MessageLogger       , ONLY: SetLastMessage          , &
                                  LogMessage              , &
                                  f_iWarn                 , &
                                  f_iFatal                  
  USE TimeSeriesUtilities , ONLY: TimeStepType            , &
                                  NPeriods                
  USE GeneralUtilities    , ONLY: UpperCase               , &
                                  IntToText
  USE Class_BaseFileType  , ONLY: BaseFileType
  USE HDF5           
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
  PUBLIC :: HDF5FileType         , &
            f_iGroup             , &
            f_iDataSet           , &
            f_iAttribute         , &
            f_iMaxDatasetNameLen 


  ! -------------------------------------------------------------
  ! --- LIST OF OPEN HDF5 FILES
  ! -------------------------------------------------------------
  INTEGER,SAVE                         :: nOpenHDFFiles   = 0
  INTEGER(HID_T),ALLOCATABLE,SAVE      :: OpenFileIDs(:)
  CHARACTER(LEN=1000),ALLOCATABLE,SAVE :: OpenFileNames(:)
  
  
  ! -------------------------------------------------------------
  ! --- DATASET TYPE
  ! -------------------------------------------------------------
  TYPE DatasetType
      INTEGER(HID_T) :: iDataSetID               = -1     !Dataset ID 
      INTEGER(HID_T) :: iDataSpaceID             = -1     !Dataspace ID for all data (all columns and timesteps)
      INTEGER(HID_T) :: iDataSpaceID_OneTimeStep = -1     !Dataspace ID for a single timestep (all columns)
      INTEGER(HID_T) :: iDataSpaceID_OneColumn   = -1     !Dataspace ID for a single column (all time steps)
      INTEGER        :: iDataPointer_File        = 0      !Data pointer on file
      INTEGER        :: nColumns                 = 0      !Number of data columns 
  END TYPE DatasetType
  
      
  ! -------------------------------------------------------------
  ! --- HDF5 FILE DATA TYPE
  ! -------------------------------------------------------------
  INTEGER,PARAMETER :: MaxPathNameLen = 1000
  TYPE,EXTENDS(BaseFileType) :: HDF5FileType
      INTEGER(HID_T)                :: FileID                = -1   !File ID
      INTEGER(HID_T)                :: iIntegerTypeID        = -1   !Integer data type (may vary from one file to next)
      INTEGER(HID_T)                :: iReal8TypeID          = -1   !Real8 data type (may vary from one file to next)
      INTEGER(HID_T)                :: iCharacterTypeID      = -1   !Character data type (may vary from one file to next)
      INTEGER(HID_T)                :: iScalarDataSpaceID    = -1   !Data space ID for scalar dta (may vary from one file to next?)
      TYPE(TimeStepType)            :: TimeStep                     !Simulation timestep related information
      INTEGER                       :: nTime                 = 0    !Number of time steps
      INTEGER                       :: nDatasets             = 0    !Number of datasets
      TYPE(DatasetType),ALLOCATABLE :: Datasets(:)                  !Information about the dataset for which I/O is performed 
  CONTAINS
      PROCEDURE,PASS :: New             => New_HDF5File
      PROCEDURE,PASS :: Kill            => Kill_HDF5File
      PROCEDURE,PASS :: GetTimeStepRelatedData
      PROCEDURE,PASS :: CreateGroup
      PROCEDURE,PASS :: CreateDataSet
      PROCEDURE,PASS :: DoesObjectExist
      PROCEDURE,PASS :: ReadAttribute
      PROCEDURE,PASS :: Read1DArrayDataSet
      PROCEDURE,PASS :: Read2DArrayDataSet
      PROCEDURE,PASS :: ReadData_OneTimeStep_AllColumns_AllDatasets_GivenTime
      PROCEDURE,PASS :: ReadData_OneColumn_OneDataset
      PROCEDURE,PASS :: ReadData_OneColumnAllDatasets_Or_OneDatasetAllColumns
      PROCEDURE,PASS :: ReadData_AllColumns_OneDataset_SeveralTimeSteps
      PROCEDURE,PASS :: ReadData_OneColumn_OneDataset_SeveralTimeSteps
      PROCEDURE,PASS :: ReadData_SomeColumns_OneDataset_SeveralTimeSteps
      PROCEDURE,PASS :: WriteAttribute
      PROCEDURE,PASS :: WriteMatrixData
      PROCEDURE,PASS :: WriteMatrixData_ToSpecifiedDataset
      PROCEDURE,PASS :: Write1DArrayData
      PROCEDURE,PASS :: Write2DArrayData
      PROCEDURE,PASS :: Rewind
      GENERIC        :: ReadData        => ReadAttribute                                         , &   
                                           Read1DArrayDataSet                                    , &
                                           Read2DArrayDataSet                                    , &
                                           ReadData_OneTimeStep_AllColumns_AllDatasets_GivenTime , &
                                           ReadData_OneColumnAllDatasets_Or_OneDatasetAllColumns , &
                                           ReadData_OneColumn_OneDataset                         , &
                                           ReadData_AllColumns_OneDataset_SeveralTimeSteps       , &
                                           ReadData_OneColumn_OneDataset_SeveralTimeSteps        , &
                                           ReadData_SomeColumns_OneDataset_SeveralTimeSteps
      GENERIC        :: WriteData       => WriteMatrixData                                       , &
                                           WriteMatrixData_ToSpecifiedDataset                    , &
                                           Write1DArrayData                                      , &
                                           Write2DArrayData                                      , &
                                           WriteAttribute
  END TYPE HDF5FileType
  
  
  ! -------------------------------------------------------------
  ! --- PARAMETERS
  ! -------------------------------------------------------------
  INTEGER,PARAMETER         :: f_iGroup              = 1           , &
                               f_iDataSet            = 2           , &
                               f_iAttribute          = 3           , &
                               f_iMaxDatasetNameLen  = 500         
  INTEGER(SIZE_T),PARAMETER :: f_iMaxCacheSize       = 1024*1024      !1 MBytes
  
  
  ! -------------------------------------------------------------
  ! --- OVERLOADED METHODS
  ! -------------------------------------------------------------
  INTERFACE GetOpenFileIndex
    MODULE PROCEDURE GetOpenFileIndex_FromName
    MODULE PROCEDURE GetOpenFileINdex_FromFileID
  END INTERFACE GetOpenFileIndex
  
  
  ! -------------------------------------------------------------
  ! --- MISC. ENTITIES
  ! -------------------------------------------------------------
  CHARACTER(LEN=1),PARAMETER          :: f_cHeaderDir = '/'
  CHARACTER(LEN=11),PARAMETER         :: f_cHeaderDir1 = '/Attributes'
  INTEGER,PARAMETER                   :: ModNameLen = 20
  CHARACTER(LEN=ModNameLen),PARAMETER :: ModName    = 'Class_HDF5FileType::'

  


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
  ! --- NEW HDF FILE
  ! -------------------------------------------------------------
  SUBROUTINE New_HDF5File(ThisFile,FileName,lInputFile,AccessType,FileOpenCode,iStat) 
    CLASS(HDF5FileType)                  :: ThisFile
    CHARACTER(LEN=*),INTENT(IN)          :: FileName 
    LOGICAL,INTENT(IN)                   :: lInputFile
    CHARACTER(LEN=*),OPTIONAL,INTENT(IN) :: AccessType  !Never used for HDF5 files
    INTEGER,OPTIONAL,INTENT(OUT)         :: FileOpenCode
    INTEGER,INTENT(OUT)                  :: iStat

    !Local variables
    CHARACTER(LEN=ModNameLen+12),PARAMETER          :: ThisProcedure = ModName // 'New_HDF5File'
    INTEGER                                         :: ErrorCode,indx,iBytes,iChunkDims(2)
    INTEGER(HID_T)                                  :: iDataAccessPropListID
    INTEGER(SIZE_T)                                 :: iChunkCacheSize
    CHARACTER(LEN=1000),ALLOCATABLE                 :: TempFileNames(:)
    CHARACTER(LEN=f_iMaxDatasetNameLen),ALLOCATABLE :: cDatasetNames(:)
    INTEGER(HID_T),ALLOCATABLE                      :: TempFileIDs(:)
    REAL(8),ALLOCATABLE                             :: DummyArray(:,:,:)
    INTEGER(HSIZE_T)                                :: iDims(2),iMaxDims(2),i1DDim(1)
    CHARACTER(LEN=11)                               :: cAttributesDir
    LOGICAL                                         :: lAttributesDirExists
    
    !Initialize
    iStat = 0
    IF (PRESENT(FileOpenCode)) FileOpenCode = 0
    
    !Initialize the Fortran interface
    CALL H5OPEN_F(ErrorCode)
    
    !If the file is already open, produce an error
    IF (GetOpenFileIndex(FileName) .GT. 0) THEN
        IF (PRESENT(FileOpenCode)) THEN
            FileOpenCode = -1
            RETURN
        ELSE
            CALL SetLastMessage('File '//TRIM(ADJUSTL(FileName))//' has already been opened!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
    END IF
    
    !If datatypes are not initialized, do so
    CALL InitDataTypeIDs(ThisFile%iIntegerTypeID,ThisFile%iReal8TypeID,ThisFile%iCharacterTypeID,ThisFile%iScalarDataSpaceID)
    
    !Instantiate the BaseFileType
    CALL ThisFile%NewBaseFile(FileName)
    
    !Create dataset access property list
    CALL H5PCREATE_F(H5P_DATASET_ACCESS_F,iDataAccessPropListID,ErrorCode)
    IF (ErrorCode .NE. 0) THEN
        CALL SetLastMessage('Error in creating dataset access property list!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
        
    !Open file
    IF (lInputFile) THEN
        CALL H5FOPEN_F(FileName,H5F_ACC_RDONLY_F,ThisFile%FileID,ErrorCode)
        IF (ErrorCode .NE. 0) THEN
            IF (PRESENT(FileOpenCode)) THEN
                FileOpenCode = ErrorCode
                RETURN
            ELSE
                CALL SetLastMessage('Error in opening HDF5 file '//TRIM(FileName)//'!',f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
        END IF
        
        !Check where the general attributes are stored in the file
        IF (ThisFile%DoesObjectExist(f_cHeaderDir1)) THEN
            cAttributesDir       = f_cHeaderDir1
            lAttributesDirExists = .TRUE.
        ELSE
            cAttributesDir       = f_cHeaderDir
            lAttributesDirExists = .FALSE.
        END IF
        
        !Read time step information
        CALL ThisFile%ReadAttribute(cAttributesDir,'TimeStep%TrackTime',ScalarAttrData=ThisFile%TimeStep%TrackTime,iStat=iStat)                  ;  IF (iStat .EQ. -1) RETURN
        CALL ThisFile%ReadAttribute(cAttributesDir,'TimeStep%BeginTime',ScalarAttrData=ThisFile%TimeStep%CurrentTime,iStat=iStat)                ;  IF (iStat .EQ. -1) RETURN
        CALL ThisFile%ReadAttribute(cAttributesDir,'TimeStep%BeginDateAndTime',ScalarAttrData=ThisFile%TimeStep%CurrentDateAndTime,iStat=iStat)  ;  IF (iStat .EQ. -1) RETURN
        CALL ThisFile%ReadAttribute(cAttributesDir,'TimeStep%DeltaT',ScalarAttrData=ThisFile%TimeStep%DeltaT,iStat=iStat)                        ;  IF (iStat .EQ. -1) RETURN
        CALL ThisFile%ReadAttribute(cAttributesDir,'TimeStep%DeltaT_InMinutes',ScalarAttrData=ThisFile%TimeStep%DeltaT_InMinutes,iStat=iStat)    ;  IF (iStat .EQ. -1) RETURN
        CALL ThisFile%ReadAttribute(cAttributesDir,'TimeStep%Unit',ScalarAttrData=ThisFile%TimeStep%Unit,iStat=iStat)                            ;  IF (iStat .EQ. -1) RETURN
        
        !Read dataset information
        CALL ThisFile%ReadAttribute(cAttributesDir,'nLocations',ScalarAttrData=ThisFile%nDatasets,iStat=iStat)  ;  IF (iStat .EQ. -1) RETURN
        ALLOCATE (ThisFile%Datasets(ThisFile%nDatasets) , cDatasetNames(ThisFile%nDatasets))
        IF (lAttributesDirExists) THEN
            CALL ThisFile%Read1DArrayDataSet(cAttributesDir//'/cLocationNames',cDatasetNames,iStat=iStat)  
            IF (iStat .EQ. -1) RETURN
        ELSE
            DO indx=1,ThisFile%nDataSets
                CALL ThisFile%ReadAttribute(cAttributesDir,'cLocationName_'//TRIM(IntToText(indx)),ScalarAttrData=cDatasetNames(indx),iStat=iStat)  
                IF (iStat .EQ. -1) RETURN
            END DO
        END IF
        DO indx=1,ThisFile%nDatasets
            ASSOCIATE (pDataset => ThisFile%Datasets(indx))
                !Read chunk size and byte size of each data piece in the dataset
                CALL ThisFile%ReadAttribute(cDatasetNames(indx),'ChunkDims',ArrayAttrData=iChunkDims,iStat=iStat)  ;  IF (iStat .EQ. -1) RETURN
                CALL ThisFile%ReadAttribute(cDatasetNames(indx),'DataByteSize',ScalarAttrData=iBytes,iStat=iStat)  ;  IF (iStat .EQ. -1) RETURN

                !Chunk cache size equals the size of the chunk or maximum cache size, whichever is smaller 
                iChunkCacheSize = MIN(PRODUCT(iChunkDims)*iBytes , f_iMaxCacheSize)
                CALL H5PSET_CHUNK_CACHE_F(iDataAccessPropListID,101,iChunkCacheSize,1.0,ErrorCode)
                IF (ErrorCode .NE. 0) THEN
                    CALL SetLastMessage('Error in setting chunk cache size for dataset '//TRIM(cDatasetNames(indx))//'!',f_iFatal,ThisProcedure) 
                    iStat = -1
                    RETURN
                END IF
            
                !Open dataset
                CALL H5DOPEN_F(ThisFile%FileID,cDatasetNames(indx),pDataset%iDataSetID,ErrorCode,iDataAccessPropListID)
            
                !Full array dataspace
                CALL H5DGET_SPACE_F(pDataset%iDataSetID,pDataset%iDataSpaceID,ErrorCode)
            
                !Dimensions of the data array on file
                CALL H5SGET_SIMPLE_EXTENT_DIMS_F(pDataset%iDataSpaceID,iDims,iMaxDims,ErrorCode)
                pDataset%nColumns = iDims(1)
                
                !Partial data spaces (one column only and one timestep only)
                i1DDim(1) = pDataset%nColumns
                CALL H5SCREATE_SIMPLE_F(1,i1DDim,pDataset%iDataSpaceID_OneTimeStep,ErrorCode)
                i1DDim(1) = iDims(2)
                CALL H5SCREATE_SIMPLE_F(1,i1dDim,pDataset%iDataSpaceID_OneColumn,ErrorCode)
            
            END ASSOCIATE
        END DO
        
        !Obtain number of simulation timesteps from one of the datasets (they should all have the same number of timesteps)
        CALL H5SGET_SIMPLE_EXTENT_DIMS_F(ThisFile%Datasets(1)%iDataSpaceID,iDims,iMaxDims,ErrorCode)
        ThisFile%nTime = iDims(2)
        
    ELSE
        CALL H5FCREATE_F(FileName,H5F_ACC_TRUNC_F,ThisFile%FileID,ErrorCode)  
        IF (ErrorCode .NE. 0) THEN
            IF (PRESENT(FileOpenCode)) THEN
                FileOpenCode = ErrorCode
                RETURN
            ELSE
                CALL SetLastMessage('Error in opening HDF5 file '//TRIM(FileName)//'!',f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
        END IF
        
    END IF
    
    !Add file to the open files list
    ALLOCATE (TempFileNames(nOpenHDFFiles+1) , TempFileIDs(nOpenHDFFiles+1))
    TempFileNames(1:nOpenHDFFiles) = OpenFileNames
    TempFileIDs(1:nOpenHDFFiles)   = OpenFileIDs
    nOpenHDFFiles                  = nOpenHDFFiles + 1
    TempFileNames(nOpenHDFFiles)   = UpperCase(ADJUSTL(FileName))
    TempFileIDs(nOpenHDFFiles)     = ThisFile%FileID
    CALL MOVE_ALLOC(TempFileNames , OpenFileNames)
    CALL MOVE_ALLOC(TempFileIDs , OpenFileIDs)
    
    !Close property lists
    CALL H5PCLOSE_F(iDataAccessPropListID,ErrorCode)
    
    !Clear memeory
    DEALLOCATE (DummyArray , cDataSetNames , STAT=ErrorCode)
    
  END SUBROUTINE New_HDF5File
  
    
  
  
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
  ! --- KILL HDF FILE
  ! -------------------------------------------------------------
  SUBROUTINE Kill_HDF5File(ThisFile,Status) 
    CLASS(HDF5FileType)                  :: ThisFile
    CHARACTER(LEN=*),OPTIONAL,INTENT(IN) :: Status   !Not used
        
    !Local variable
    INTEGER                         :: ErrorCode,iIndex,indx
    TYPE(HDF5FileType)              :: Dummy
    CHARACTER(LEN=1000),ALLOCATABLE :: TempFileNames(:)
    INTEGER(HID_T),ALLOCATABLE      :: TempFileIDs(:)
    
    !If the file was never really opened, return
    IF (ThisFile%FileID .EQ. -1) RETURN
    
    !Close datasets and dataspaces
    CALL H5SCLOSE_F(ThisFile%iScalarDataSpaceID,ErrorCode)
    DO indx=1,ThisFile%nDatasets
        CALL H5SCLOSE_F(ThisFile%Datasets(indx)%iDataSpaceID,ErrorCode)
        CALL H5SCLOSE_F(ThisFile%Datasets(indx)%iDataSpaceID_OneColumn,ErrorCode)
        CALL H5SCLOSE_F(ThisFile%Datasets(indx)%iDataSpaceID_OneTimeStep,ErrorCode)
        CALL H5DCLOSE_F(ThisFile%Datasets(indx)%iDataSetID,ErrorCode)
    END DO
    
    !Deallocate reserved memory
    DEALLOCATE (ThisFile%Datasets , STAT=ErrorCode)
    
    !Close file
    CALL H5FCLOSE_F(ThisFile%FileID,ErrorCode)

    !Remove the file info from the list of open files
    iIndex = GetOpenFileIndex(ThisFile%FileID)
    ALLOCATE (TempFileNames(nOpenHDFFiles-1) , TempFileIDs(nOpenHDFFiles-1))
    TempFileNames(1:iIndex-1)    = OpenFileNames(1:iIndex-1)  ; TempFileNames(iIndex:)    = OpenFileNames(iIndex+1:)  ;  CALL MOVE_ALLOC(TempFileNames, OpenFileNames)
    TempFileIDs(1:iIndex-1)      = OpenFileIDs(1:iIndex-1)    ; TempFileIDs(iIndex:)      = OpenFileIDs(iIndex+1:)    ;  CALL MOVE_ALLOC(TempFileIDs, OpenFileIDs)
    nOpenHDFFiles = nOpenHDFFiles - 1
    
    !Kill BaseFileType
    CALL ThisFile%KillBaseFile()
    
    !Restore default values
    ThisFile%FileID             = Dummy%FileID
    ThisFile%TimeStep           = Dummy%TimeStep
    ThisFile%nTime              = Dummy%nTime
    ThisFile%nDatasets          = Dummy%nDatasets
    ThisFile%iIntegerTypeID     = Dummy%iIntegerTypeID
    ThisFile%iReal8TypeID       = Dummy%iReal8TypeID
    ThisFile%iCharacterTypeID   = Dummy%iCharacterTypeID
    ThisFile%iScalarDataSpaceID = Dummy%iScalarDataSpaceID
    
    !Close fortran interface if there are no open HDF5 files
    IF (nOpenHDFFiles .EQ. 0) CALL H5CLOSE_F(ErrorCode)
    
  END SUBROUTINE Kill_HDF5File
    

 

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
  ! --- GET SIMULATION TIMESTEP RELATED DATA
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetTimeStepRelatedData(ThisFile,NTimeSteps,TimeStep)
    CLASS(HDF5FileType),INTENT(IN) :: ThisFile
    INTEGER,INTENT(OUT)            :: NTimeSteps
    TYPE(TimeStepType),INTENT(OUT) :: TimeStep
    
    NTimeSteps = ThisFile%nTime
    TimeStep   = ThisFile%TimeStep
    
  END SUBROUTINE GetTimeStepRelatedData
  
  
  
  
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
  ! --- READ AN ATTRIBUTE
  ! -------------------------------------------------------------
  SUBROUTINE ReadAttribute(ThisFile,cGrpOrDset,cAttrName,ScalarAttrData,ArrayAttrData,iStat)
    CLASS(HDF5FileType),INTENT(IN) :: ThisFile
    CHARACTER(LEN=*),INTENT(IN)    :: cGrpOrDset,cAttrName
    CLASS(*),OPTIONAL,INTENT(OUT)  :: ScalarAttrData,ArrayAttrData(:)  !Only one must be supplied
    INTEGER,INTENT(OUT)            :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+13),PARAMETER :: ThisProcedure = ModName // 'ReadAttribute'
    INTEGER                                :: ErrorCode
    INTEGER(HID_T)                         :: obj_id,attr_id,type_id
    INTEGER(SIZE_T)                        :: HLen
    TYPE(C_PTR)                            :: pData
    
    !Initialize
    iStat = 0
    
    !Open group or dataset
    CALL H5OOPEN_F(ThisFile%FileID,cGrpOrDset,obj_id,ErrorCode)
    IF (ErrorCode .NE. 0) THEN
        CALL SetLastMessage('Group/dataset '//cGrpOrDset//' cannot be found in file '//ThisFile%Name//'!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    !Open attribute
    CALL H5AOPEN_F(obj_id,cAttrName,attr_id,ErrorCode)    
    IF (ErrorCode .NE. 0) THEN
        CALL SetLastMessage('Attribute '//cAttrName//' cannot be found in file '//ThisFile%Name//'!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    !Read attribute
    !Scalar data is being read
    IF (PRESENT(ScalarAttrData)) THEN
        !Get a C pointer to data
        pData = C_LOC(ScalarAttrData)
        
        SELECT TYPE (ScalarAttrData)
            TYPE IS (INTEGER)
                CALL H5AREAD_F(attr_id,ThisFile%iIntegerTypeID,pData,ErrorCode)
                
            TYPE IS (REAL(8))
                CALL H5AREAD_F(attr_id,ThisFile%iReal8TypeID,pData,ErrorCode)
                
            TYPE IS (CHARACTER(LEN=*))
                pData = C_LOC(ScalarAttrData(1:1))
                HLen  = LEN(ScalarAttrData)
                CALL H5TCOPY_F(H5T_FORTRAN_S1,type_id,ErrorCode)
                CALL H5TSET_SIZE_F(type_id,HLen,ErrorCode)
                CALL H5AREAD_F(attr_id,type_id,pData,ErrorCode)
                
            TYPE IS (LOGICAL)
                CALL H5AREAD_F(attr_id,ThisFile%iIntegerTypeID,pData,ErrorCode)
                
            CLASS DEFAULT    
                CALL SetLastMessage('Attribute '//cAttrName//' with the specified data type cannot be read from file '//ThisFile%Name//'!',f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
                
        END SELECT
        
        
    !Array data is being read
    ELSEIF (PRESENT(ArrayAttrData)) THEN
        !Get a C pointer to data
        pData = C_LOC(ArrayAttrData(1))

        SELECT TYPE (ArrayAttrData)
            TYPE IS (INTEGER)
                CALL H5AREAD_F(attr_id,ThisFile%iIntegerTypeID,pData,ErrorCode)
                
            TYPE IS (REAL(8))
                CALL H5AREAD_F(attr_id,ThisFile%iReal8TypeID,pData,ErrorCode)
                
            TYPE IS (CHARACTER(LEN=*))
                pData = C_LOC(ArrayAttrData(1)(1:1))
                HLen  = LEN(ArrayAttrData(1))
                CALL H5TCOPY_F(H5T_FORTRAN_S1,type_id,ErrorCode)
                CALL H5TSET_SIZE_F(type_id,HLen,ErrorCode)
                CALL H5AREAD_F(attr_id,type_id,pData,ErrorCode)
                
            TYPE IS (LOGICAL)
                CALL H5AREAD_F(attr_id,ThisFile%iIntegerTypeID,pData,ErrorCode)
                
            CLASS DEFAULT    
                CALL SetLastMessage('Attribute '//cAttrName//' with the specified data type cannot be read from file '//ThisFile%Name//'!',f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
                
            END SELECT
        
    !No data to be read is defined
    ELSE
        CALL SetLastMessage('Either a scalar or an array attribute must be specified!',f_iFatal,ThisProcedure)   
        iStat = -1
        RETURN
    END IF
    IF (ErrorCode .NE. 0) THEN
        CALL SetLastMessage('Error in reading attribute '//cAttrName//' from file '//ThisFile%Name//'!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    !Release resources
    CALL H5ACLOSE_F(attr_id,ErrorCode)
    CALL H5OCLOSE_F(obj_id,ErrorCode)
    
  END SUBROUTINE ReadAttribute
  
  
  ! -------------------------------------------------------------
  ! --- READ 1-D ARRAY DATA FROM A FULLY QUALIFIED DATASET
  ! -------------------------------------------------------------
  SUBROUTINE Read1DArrayDataSet(ThisFile,cPath,Data,iStat)
    CLASS(HDF5FileType)         :: ThisFile
    CHARACTER(LEN=*),INTENT(IN) :: cPath
    CLASS(*),INTENT(OUT)        :: Data(:)
    INTEGER,INTENT(OUT)         :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+18),PARAMETER :: ThisProcedure = 'Read1DArrayDataSet'
    INTEGER                                :: ErrorCode,IntData(SIZE(Data))
    INTEGER(HID_T)                         :: iDataSetID
    INTEGER(HSIZE_T)                       :: HDims(1)
    INTEGER(SIZE_T)                        :: HLen
    TYPE(C_PTR)                            :: pData  
    
    !Initialize
    iStat = 0
    
    !Open dataset
    CALL H5DOPEN_F(ThisFile%FileID,cPath,iDataSetID,ErrorCode)
    IF (ErrorCode .NE. 0) THEN
        CALL SetLastMessage("Can't find "//cPath//" dataset in file "//TRIM(ThisFile%Name), f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    !Data dimension
    HDims = SHAPE(Data)

    !Read data based on type
    SELECT TYPE(Data)
        TYPE IS (INTEGER)
            CALL H5DREAD_F(iDataSetID,ThisFile%iIntegerTypeID,Data,HDims,ErrorCode)
            
        TYPE IS (REAL(8))
            CALL H5DREAD_F(iDataSetID,ThisFile%iReal8TypeID,Data,HDims,ErrorCode)
            
        TYPE IS (LOGICAL)
            CALL H5DREAD_F(iDataSetID,ThisFile%iIntegerTypeID,IntData,HDims,ErrorCode)
            Data = IntData
            
        TYPE IS (CHARACTER(LEN=*))
            pData = C_LOC(Data(1)(1:1))
            HLen  = LEN(Data(1))
            CALL H5TSET_SIZE_F(ThisFile%iCharacterTypeID,HLen,ErrorCode)
            CALL H5DREAD_F(iDataSetID,ThisFile%iCharacterTypeID,pData,ErrorCode)
            
        END SELECT
        
    !Release resources
    CALL H5DCLOSE_F(iDataSetID,ErrorCode)
    
  END SUBROUTINE Read1DArrayDataSet
  
  
  ! -------------------------------------------------------------
  ! --- READ 2-D ARRAY DATA FROM A FULLY QUALIFIED DATASET
  ! -------------------------------------------------------------
  SUBROUTINE Read2DArrayDataSet(ThisFile,cPath,Data,iStat)
    CLASS(HDF5FileType)         :: ThisFile
    CHARACTER(LEN=*),INTENT(IN) :: cPath
    CLASS(*),INTENT(OUT)        :: Data(:,:)
    INTEGER,INTENT(OUT)         :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+18),PARAMETER :: ThisProcedure = 'Read2DArrayDataSet'
    INTEGER                                :: ErrorCode,IntData(SIZE(Data,DIM=1),SIZE(Data,DIM=2))
    INTEGER(HID_T)                         :: iDataSetID
    INTEGER(HSIZE_T)                       :: HDims(2)
    INTEGER(SIZE_T)                        :: HLen
    TYPE(C_PTR)                            :: pData   
    
    !Initialize
    iStat = 0
    
    !Open dataset
    CALL H5DOPEN_F(ThisFile%FileID,cPath,iDataSetID,ErrorCode)
    IF (ErrorCode .NE. 0) THEN
        CALL SetLastMessage("Can't find "//cPath//" dataset in file "//TRIM(ThisFile%Name), f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    !Data dimension
    HDims = SHAPE(Data)

    !Read data based on type
    SELECT TYPE(Data)
        TYPE IS (INTEGER)
            CALL H5DREAD_F(iDataSetID,ThisFile%iIntegerTypeID,Data,HDims,ErrorCode)
            
        TYPE IS (REAL(8))
            CALL H5DREAD_F(iDataSetID,ThisFile%iReal8TypeID,Data,HDims,ErrorCode)
            
        TYPE IS (LOGICAL)
            CALL H5DREAD_F(iDataSetID,ThisFile%iIntegerTypeID,IntData,HDims,ErrorCode)
            Data = IntData
            
        TYPE IS (CHARACTER(LEN=*))
            pData = C_LOC(Data(1,1)(1:1))
            HLen  = LEN(Data(1,1))
            CALL H5TSET_SIZE_F(ThisFile%iCharacterTypeID,HLen,ErrorCode)
            CALL H5DREAD_F(iDataSetID,ThisFile%iCharacterTypeID,pData,ErrorCode)
            
        END SELECT
        
    !Release resources
    CALL H5DCLOSE_F(iDataSetID,ErrorCode)
    
  END SUBROUTINE Read2DArrayDataSet
  
  
  ! -------------------------------------------------------------
  ! --- READ DATA FROM ONE DATASET FOR SOME COLUMNS FOR CONSECUTIVE TIMESTEPS STARTING FROM A TIME
  ! -------------------------------------------------------------
  SUBROUTINE ReadData_SomeColumns_OneDataset_SeveralTimeSteps(ThisFile,Time,iDataSetNo,iReadCols,Data,Stat)
    CLASS(HDF5FileType)         :: ThisFile
    CHARACTER(LEN=*),INTENT(IN) :: Time
    INTEGER,INTENT(IN)          :: iDataSetNo,iReadCols(:)
    CLASS(*),INTENT(OUT)        :: Data(:,:)  !Returns data for (column,timestep) combination
    INTEGER,INTENT(OUT)         :: Stat
    
    !Local variables
    INTEGER :: iColNo,indx,iData(SIZE(Data,DIM=2))
    REAL(8) :: rData(SIZE(Data,DIM=2))
    
    !Read each column one by one
    SELECT TYPE (Data)
        TYPE IS (REAL(8))
            DO indx=1,SIZE(iReadCols)
               iColNo = iReadCols(indx)
               CALL ReadData_OneColumn_OneDataset_SeveralTimeSteps(ThisFile,Time,iDataSetNo,iColNo,rData,Stat)
               Data(indx,:) = rData
            END DO
            
        TYPE IS (INTEGER)
            DO indx=1,SIZE(iReadCols)
               iColNo = iReadCols(indx)
               CALL ReadData_OneColumn_OneDataset_SeveralTimeSteps(ThisFile,Time,iDataSetNo,iColNo,iData,Stat)
               Data(indx,:) = iData
            END DO
    END SELECT
    
  END SUBROUTINE ReadData_SomeColumns_OneDataset_SeveralTimeSteps

  
  ! -------------------------------------------------------------
  ! --- READ DATA FROM ONE DATASET FOR CONSECUTIVE TIMESTEPS STARTING FROM A TIME
  ! -------------------------------------------------------------
  SUBROUTINE ReadData_AllColumns_OneDataset_SeveralTimeSteps(ThisFile,Time,iDataSetNo,Data,Stat)
    CLASS(HDF5FileType)         :: ThisFile
    CHARACTER(LEN=*),INTENT(IN) :: Time
    INTEGER,INTENT(IN)          :: iDataSetNo
    CLASS(*),INTENT(OUT)        :: Data(:,:)  !Returns data for (column,timestep) combination
    INTEGER,INTENT(OUT)         :: Stat
    
    !Local variables
    INTEGER(HSIZE_T),PARAMETER :: iStride(2)  = [1,1] 
    INTEGER(HSIZE_T),PARAMETER :: iCount(2)   = [1,1] 
    INTEGER(HSIZE_T)           :: iOffset(2),iBlock(2)
    INTEGER(HID_T)             :: iMemSpaceID
    TYPE(C_PTR)                :: pData
    REAL(8)                    :: rTime
    INTEGER                    :: iTimeOffset,ErrorCode1,ErrorCode2,ErrorCode3

    !Calculate time offset
    IF (ThisFile%TimeStep%TrackTime) THEN
        iTimeOffset = NPeriods(ThisFile%TimeStep%DeltaT_InMinutes,ThisFile%TimeStep%CurrentDateAndTime,Time)
    ELSE
        READ (Time,*) rTime
        iTimeOffset = NPeriods(ThisFile%TimeStep%DeltaT,ThisFile%TimeStep%CurrentTime,rTime)
    END IF
    ThisFile%Datasets(iDatasetNo)%iDataPointer_File = iTimeOffset
    
    !Select partial data on file
    iOffset = [0 , iTimeOffset]
    iBlock  = [ThisFile%Datasets(iDatasetNo)%nColumns , MIN(SIZE(Data,DIM=2),ThisFile%nTime-iTimeOffset)]
    CALL H5SSELECT_HYPERSLAB_F(ThisFile%Datasets(iDatasetNo)%iDataSpaceID,H5S_SELECT_SET_F,iOffset,iCount,ErrorCode1,iStride,iBlock)
    
    !C pointer to Data
    pData = C_LOC(Data(1,1))
    
    !Create memory data space
    CALL H5SCREATE_SIMPLE_F(2,iBlock,iMemSPaceID,ErrorCode2)
    
    !Read data
    SELECT TYPE (Data)
        TYPE IS (REAL(8))
            CALL H5DREAD_F(ThisFile%Datasets(iDataSetNo)%iDataSetID,ThisFile%iReal8TypeID,pData,ErrorCode3,iMemSpaceID,ThisFile%Datasets(iDatasetNo)%iDataSpaceID)
            
        TYPE IS (INTEGER)
            CALL H5DREAD_F(ThisFile%Datasets(iDataSetNo)%iDataSetID,ThisFile%iIntegerTypeID,pData,ErrorCode3,iMemSpaceID,ThisFile%Datasets(iDatasetNo)%iDataSpaceID)
    END SELECT
    
    !Adjust data pointer on file
    ThisFile%Datasets(iDatasetNo)%iDataPointer_File = MIN(ThisFile%Datasets(iDatasetNo)%iDataPointer_File + SIZE(Data,DIM=2) , ThisFile%nTime)
    
    !Update Stat
    Stat = ErrorCode1 + ErrorCode2 + ErrorCode3
    
    !Discard iMemSpaceID
    CALL H5SCLOSE_F(iMemSpaceID,ErrorCode1)
    
  END SUBROUTINE ReadData_AllColumns_OneDataset_SeveralTimeSteps
  
  
  ! -------------------------------------------------------------
  ! --- READ ONE COLUMN DATA FROM ONE DATASET FOR CONSECUTIVE TIMESTEPS STARTING FROM A TIME
  ! -------------------------------------------------------------
  SUBROUTINE ReadData_OneColumn_OneDataset_SeveralTimeSteps(ThisFile,Time,iDataSetNo,iCol,Data,Stat)
    CLASS(HDF5FileType)         :: ThisFile
    CHARACTER(LEN=*),INTENT(IN) :: Time
    INTEGER,INTENT(IN)          :: iDataSetNo,iCol
    CLASS(*),INTENT(OUT)        :: Data(:)  !Returns data for (timestep) 
    INTEGER,INTENT(OUT)         :: Stat
    
    !Local variables
    INTEGER(HSIZE_T),PARAMETER :: iStride(2)  = [1,1] 
    INTEGER(HSIZE_T),PARAMETER :: iCount(2)   = [1,1] 
    INTEGER(HSIZE_T)           :: iOffset(2),iBlock(2)
    INTEGER(HID_T)             :: iMemSpaceID
    TYPE(C_PTR)                :: pData
    REAL(8)                    :: rTime
    INTEGER                    :: iTimeOffset,ErrorCode1,ErrorCode2,ErrorCode3

    !Calculate time offset
    IF (ThisFile%TimeStep%TrackTime) THEN
        iTimeOffset = NPeriods(ThisFile%TimeStep%DeltaT_InMinutes,ThisFile%TimeStep%CurrentDateAndTime,Time)
    ELSE
        READ (Time,*) rTime
        iTimeOffset = NPeriods(ThisFile%TimeStep%DeltaT,ThisFile%TimeStep%CurrentTime,rTime)
    END IF
    ThisFile%Datasets(iDatasetNo)%iDataPointer_File = iTimeOffset
    
    !Select partial data on file
    iOffset = [iCol-1 , iTimeOffset]
    iBlock  = [1 , MIN(SIZE(Data),ThisFile%nTime-iTimeOffset)]
    CALL H5SSELECT_HYPERSLAB_F(ThisFile%Datasets(iDatasetNo)%iDataSpaceID,H5S_SELECT_SET_F,iOffset,iCount,ErrorCode1,iStride,iBlock)
    
    !C pointer to Data
    pData = C_LOC(Data(1))
    
    !Create memory data space
    CALL H5SCREATE_SIMPLE_F(2,iBlock,iMemSPaceID,ErrorCode2)
    
    !Read data
    SELECT TYPE (Data)
        TYPE IS (REAL(8))
            CALL H5DREAD_F(ThisFile%Datasets(iDataSetNo)%iDataSetID,ThisFile%iReal8TypeID,pData,ErrorCode3,iMemSpaceID,ThisFile%Datasets(iDatasetNo)%iDataSpaceID)
            
        TYPE IS (INTEGER)
            CALL H5DREAD_F(ThisFile%Datasets(iDataSetNo)%iDataSetID,ThisFile%iIntegerTypeID,pData,ErrorCode3,iMemSpaceID,ThisFile%Datasets(iDatasetNo)%iDataSpaceID)
    END SELECT
    
    !Adjust data pointer on file
    ThisFile%Datasets(iDatasetNo)%iDataPointer_File = MIN(ThisFile%Datasets(iDatasetNo)%iDataPointer_File + SIZE(Data) , ThisFile%nTime)
    
    !Update Stat
    Stat = ErrorCode1 + ErrorCode2 + ErrorCode3
    
    !Discard iMemSpaceID
    CALL H5SCLOSE_F(iMemSpaceID,ErrorCode1)
    
  END SUBROUTINE ReadData_OneColumn_OneDataset_SeveralTimeSteps
  
  
  ! -------------------------------------------------------------
  ! --- READ DATA FROM ALL DATASETS FOR ONE TIME STEP FOR A GIVEN TIME
  ! -------------------------------------------------------------
  SUBROUTINE ReadData_OneTimeStep_AllColumns_AllDatasets_GivenTime(ThisFile,Time,Data,FileReadCode,iStat)
    CLASS(HDF5FileType)         :: ThisFile
    CHARACTER(LEN=*),INTENT(IN) :: Time
    CLASS(*),INTENT(OUT)        :: Data(:,:)    !Data for each (column,dataset) combination
    INTEGER,INTENT(OUT)         :: FileReadCode,iStat
    
    !Local variables
    INTEGER                    :: indx,iData(SIZE(Data,DIM=1)),iTimeOffset
    REAL(8)                    :: rData(SIZE(Data,DIM=1)),rTime
    INTEGER(HSIZE_T),PARAMETER :: iStride(2)  = [1,1] 
    INTEGER(HSIZE_T),PARAMETER :: iCount(2)   = [1,1] 
    INTEGER(HSIZE_T)           :: iOffset(2),iBlock(2)
    TYPE(C_PTR)                :: pData
    
    !Initialize
    iStat        = 0
    FileReadCode = 0
    
    !Calculate time offset
    IF (ThisFile%TimeStep%TrackTime) THEN
        iTimeOffset = NPeriods(ThisFile%TimeStep%DeltaT_InMinutes,ThisFile%TimeStep%CurrentDateAndTime,Time)
    ELSE
        READ (Time,*) rTime
        iTimeOffset = NPeriods(ThisFile%TimeStep%DeltaT,ThisFile%TimeStep%CurrentTime,rTime)
    END IF
    
    !Defined ofseet and block, select partial array on file 
    iOffset = [0 , iTimeOffset]
    iBlock  = [ThisFile%Datasets(indx)%nColumns , 1]
    CALL H5SSELECT_HYPERSLAB_F(ThisFile%Datasets(indx)%iDataSpaceID,H5S_SELECT_SET_F,iOffset,iCount,FileReadCode,iStride,iBlock)
    
    !Read data   
    SELECT TYPE(Data)
        TYPE IS (REAL(8))
            !Iterate over datasets
            DO indx=1,ThisFile%nDatasets
                pData = C_LOC(rData(1))
                CALL H5DREAD_F(ThisFile%Datasets(indx)%iDataSetID,ThisFile%iReal8TypeID,pData,FileReadCode,ThisFile%Datasets(indx)%iDataSpaceID_OneTimeStep,ThisFile%Datasets(indx)%iDataSpaceID)
                Data(:,indx) = rData
            END DO
    
        TYPE IS (INTEGER)
            !Iterate over datasets
            DO indx=1,ThisFile%nDatasets
                pData = C_LOC(iData(1))
                CALL H5DREAD_F(ThisFile%Datasets(indx)%iDataSetID,ThisFile%iIntegerTypeID,pData,FileReadCode,ThisFile%Datasets(indx)%iDataSpaceID_OneTimeStep,ThisFile%Datasets(indx)%iDataSpaceID)
                Data(:,indx) = iData
            END DO
    END SELECT 
        
    !Adjust data pointer
    ThisFile%Datasets%iDataPointer_File = iTimeOffset + 1

    
  END SUBROUTINE ReadData_OneTimeStep_AllColumns_AllDatasets_GivenTime
  
  
  ! -------------------------------------------------------------
  ! --- GATEWAY METHOD TO READ EITHER ONE COLUMN FROM ALL DATASETS OR ALL DATA FROM ONE DATASET
  ! -------------------------------------------------------------
  SUBROUTINE ReadData_OneColumnAllDatasets_Or_OneDatasetAllColumns(ThisFile,lReadCol,iColOrDatasetNo,Data,Stat)
    CLASS(HDF5FileType)  :: ThisFile
    LOGICAL,INTENT(IN)   :: lReadCol
    INTEGER,INTENT(IN)   :: iColOrDatasetNo
    CLASS(*),INTENT(OUT) :: Data(:,:)   
    INTEGER,INTENT(OUT)  :: Stat
    
    IF (lReadCol) THEN
        CALL ReadData_OneColumn_AllDatasets(ThisFile,iColOrDatasetNo,Data,Stat)
    ELSE
        CALL ReadData_AllColumns_OneDataset(ThisFile,iColOrDatasetNo,Data,Stat)
    END IF
    
  END SUBROUTINE ReadData_OneColumnAllDatasets_Or_OneDatasetAllColumns
  
  
  ! -------------------------------------------------------------
  ! --- READ DATA FROM ALL DATASETS FOR ONE COLUMN
  ! -------------------------------------------------------------
  SUBROUTINE ReadData_OneColumn_AllDatasets(ThisFile,iColumnNo,Data,Stat)
    CLASS(HDF5FileType)  :: ThisFile
    INTEGER,INTENT(IN)   :: iColumnNo
    CLASS(*),INTENT(OUT) :: Data(:,:)   !Data for (time,dataset) combination
    INTEGER,INTENT(OUT)  :: Stat
    
    !Local variables
    INTEGER                    :: indx,iData(SIZE(Data,DIM=1))
    REAL(8)                    :: rData(SIZE(Data,DIM=1))
    INTEGER(HSIZE_T),PARAMETER :: iStride(2)  = [1,1] 
    INTEGER(HSIZE_T),PARAMETER :: iCount(2)   = [1,1] 
    INTEGER(HSIZE_T)           :: iOffset(2),iBlock(2)
    TYPE(C_PTR) :: pData
    
    !File data offset
    iOffset = [iColumnNo-1 , 0]
    iBlock  = [1 , SIZE(Data,DIM=1)]
    
    !Select partial array on file 
    CALL H5SSELECT_HYPERSLAB_F(ThisFile%Datasets(indx)%iDataSpaceID,H5S_SELECT_SET_F,iOffset,iCount,Stat,iStride,iBlock)
    
    !Read data   
    SELECT TYPE(Data)
        TYPE IS (REAL(8))
            !Iterate over datasets
            DO indx=1,ThisFile%nDatasets
                pData = C_LOC(rData(1))
                CALL H5DREAD_F(ThisFile%Datasets(indx)%iDataSetID,ThisFile%iReal8TypeID,pData,Stat,ThisFile%Datasets(indx)%iDataSpaceID_OneColumn,ThisFile%Datasets(indx)%iDataSpaceID)
                Data(:,indx) = rData
            END DO
    
        TYPE IS (INTEGER)
            !Iterate over datasets
            DO indx=1,ThisFile%nDatasets
                pData = C_LOC(iData(1))
                CALL H5DREAD_F(ThisFile%Datasets(indx)%iDataSetID,ThisFile%iIntegerTypeID,pData,Stat,ThisFile%Datasets(indx)%iDataSpaceID_OneColumn,ThisFile%Datasets(indx)%iDataSpaceID)
                Data(:,indx) = iData
            END DO
    END SELECT 
        
    !Rewind datasets
    ThisFile%Datasets%iDataPointer_File = 0    
    
  END SUBROUTINE ReadData_OneColumn_AllDatasets
  
  
  ! -------------------------------------------------------------
  ! --- READ ALL DATA FOR ONE DATASET
  ! -------------------------------------------------------------
  SUBROUTINE ReadData_AllColumns_OneDataset(ThisFile,iDatasetNo,Data,Stat)
    CLASS(HDF5FileType)  :: ThisFile
    INTEGER,INTENT(IN)   :: iDatasetNo
    CLASS(*),INTENT(OUT) :: Data(:,:)   !Data for (column,time) combination
    INTEGER,INTENT(OUT)  :: Stat
    
    !Local variables
    TYPE(C_PTR) :: pData
    
    !C pointer to data
    pData = C_LOC(Data(1,1))
    
    !Read data   
    SELECT TYPE(Data)
        TYPE IS (REAL(8))
            CALL H5DREAD_F(ThisFile%Datasets(iDatasetNo)%iDataSetID,ThisFile%iReal8TypeID,pData,Stat)

        TYPE IS (INTEGER)
            CALL H5DREAD_F(ThisFile%Datasets(iDatasetNo)%iDataSetID,ThisFile%iIntegerTypeID,pData,Stat)

    END SELECT
        
    ThisFile%Datasets(iDatasetNo)%iDataPointer_File = 0
    
  END SUBROUTINE ReadData_AllColumns_OneDataset


  ! -------------------------------------------------------------
  ! --- READ DATA FOR ONE COLUMN FROM ONE DATASET
  ! -------------------------------------------------------------
  SUBROUTINE ReadData_OneColumn_OneDataset(ThisFile,iColumnNo,iDatasetNo,Data,Stat)
    CLASS(HDF5FileType)  :: ThisFile
    INTEGER,INTENT(IN)   :: iColumnNo,iDatasetNo
    CLASS(*),INTENT(OUT) :: Data(:)   !Data for (time) for the given column and dataset
    INTEGER,INTENT(OUT)  :: Stat
    
    !Local variables
    INTEGER                    :: indx
    INTEGER(HSIZE_T),PARAMETER :: iStride(2)  = [1,1] 
    INTEGER(HSIZE_T),PARAMETER :: iCount(2)   = [1,1] 
    INTEGER(HSIZE_T)           :: iOffset(2),iBlock(2)
    TYPE(C_PTR)                :: pData
    
    !File data offset
    iOffset = [iColumnNo-1 , 0]
    iBlock  = [1 , SIZE(Data)]
    
    !C pointer to Data
    pData = C_LOC(Data(1))
    
    !Select partial array on file 
    CALL H5SSELECT_HYPERSLAB_F(ThisFile%Datasets(iDatasetNo)%iDataSpaceID,H5S_SELECT_SET_F,iOffset,iCount,Stat,iStride,iBlock)
    
    !Read data   
    SELECT TYPE(Data)
        TYPE IS (REAL(8))
            CALL H5DREAD_F(ThisFile%Datasets(indx)%iDataSetID,ThisFile%iReal8TypeID,pData,Stat,ThisFile%Datasets(indx)%iDataSpaceID_OneColumn,ThisFile%Datasets(indx)%iDataSpaceID)
    
        TYPE IS (INTEGER)
            CALL H5DREAD_F(ThisFile%Datasets(indx)%iDataSetID,ThisFile%iIntegerTypeID,pData,Stat,ThisFile%Datasets(indx)%iDataSpaceID_OneColumn,ThisFile%Datasets(indx)%iDataSpaceID)
    END SELECT 
        
    ThisFile%Datasets(iDatasetNo)%iDataPointer_File = 0
    
  END SUBROUTINE ReadData_OneColumn_OneDataset
 
 
  
  
  
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
  ! --- WRITE MATRIX TO A FULLY QUALIFIED DATASET THROUGHOUT A SIMULATION PERIOD 
  ! -------------------------------------------------------------
  SUBROUTINE WriteMatrixData_ToSpecifiedDataset(ThisFile,indxDataset,Data)
    CLASS(HDF5FileType) :: ThisFile
    INTEGER,INTENT(IN)  :: indxDataset
    CLASS(*),INTENT(IN) :: Data(:,:)       !Data comes in (column,row) format
    
    !Local variables
    CHARACTER(LEN=ModNameLen+34),PARAMETER :: ThisProcedure = ModName // 'WriteMatrixData_ToSpecifiedDataset'
    INTEGER                                :: ErrorCode,nColumns,nRows
    INTEGER(HSIZE_T),PARAMETER             :: iStride(2)  = [1,1] 
    INTEGER(HSIZE_T),PARAMETER             :: iCount(2)   = [1,1] 
    INTEGER(HSIZE_T)                       :: iOffset(2),iBlock(2)
    
    !Number of data columns and rows
    nColumns = ThisFile%Datasets(indxDataset)%nColumns
    nRows    = SIZE(Data,DIM=2)
    
    !Select partial array on file 
    iOffset = [0 , ThisFile%Datasets(indxDataset)%iDataPointer_File]
    iBlock  = [nColumns , nRows]
    CALL H5SSELECT_HYPERSLAB_F(ThisFile%Datasets(indxDataset)%iDataSpaceID,H5S_SELECT_SET_F,iOffset,iCount,ErrorCode,iStride,iBlock)
    
    !Write data
    SELECT TYPE(Data)
        TYPE IS (REAL(8))
            CALL H5DWRITE_F(ThisFile%Datasets(indxDataset)%iDataSetID,ThisFile%iReal8TypeID,Data,iBlock,ErrorCode,ThisFile%Datasets(indxDataset)%iDataSpaceID_OneTimeStep,ThisFile%Datasets(indxDataset)%iDataSpaceID)
    
        TYPE IS (INTEGER)
            CALL H5DWRITE_F(ThisFile%Datasets(indxDataset)%iDataSetID,ThisFile%iIntegerTypeID,Data,iBlock,ErrorCode,ThisFile%Datasets(indxDataset)%iDataSpaceID_OneTimeStep,ThisFile%Datasets(indxDataset)%iDataSpaceID)
        
        CLASS DEFAULT
            CALL LogMessage('Selected data type cannot be written to file '//ThisFile%Name//'!',f_iWarn,ThisProcedure)
            RETURN
    END SELECT

    !Increment data pointer in file
    ThisFile%Datasets(indxDataset)%iDataPointer_File = ThisFile%Datasets(indxDataset)%iDataPointer_File + nRows
    
  END SUBROUTINE WriteMatrixData_ToSpecifiedDataset
  
  
  ! -------------------------------------------------------------
  ! --- WRITE MATRIX TO DATASET(S) THROUGHOUT A SIMULATION PERIOD 
  ! -------------------------------------------------------------
  SUBROUTINE WriteMatrixData(ThisFile,Data)
    CLASS(HDF5FileType) :: ThisFile
    CLASS(*),INTENT(IN) :: Data(:,:)       !Data comes in (column,dataset) format
    
    !Local variables
    CHARACTER(LEN=ModNameLen+15),PARAMETER :: ThisProcedure = ModName // 'WriteMatrixData'
    INTEGER                                :: ErrorCode,indxDataset,nColumns
    INTEGER(HSIZE_T),PARAMETER             :: iStride(2)  = [1,1] 
    INTEGER(HSIZE_T),PARAMETER             :: iCount(2)   = [1,1] 
    INTEGER(HSIZE_T)                       :: iOffset(2),iBlock(2)
    
    !Process data to be written
    DO indxDataset=1,ThisFile%nDatasets        
        !Number of data columns
        nColumns = ThisFile%Datasets(indxDataset)%nColumns
        
        !Select partial array on file 
        iOffset = [0 , ThisFile%Datasets(indxDataset)%iDataPointer_File]
        iBlock  = [nColumns , 1]
        CALL H5SSELECT_HYPERSLAB_F(ThisFile%Datasets(indxDataset)%iDataSpaceID,H5S_SELECT_SET_F,iOffset,iCount,ErrorCode,iStride,iBlock)
        
        !Write data
        SELECT TYPE(Data)
            TYPE IS (REAL(8))
                CALL H5DWRITE_F(ThisFile%Datasets(indxDataset)%iDataSetID,ThisFile%iReal8TypeID,Data(1:nColumns,indxDataset),iBlock,ErrorCode,ThisFile%Datasets(indxDataset)%iDataSpaceID_OneTimeStep,ThisFile%Datasets(indxDataset)%iDataSpaceID)
      
            TYPE IS (INTEGER)
                CALL H5DWRITE_F(ThisFile%Datasets(indxDataset)%iDataSetID,ThisFile%iIntegerTypeID,Data(1:nColumns,indxDataset),iBlock,ErrorCode,ThisFile%Datasets(indxDataset)%iDataSpaceID_OneTimeStep,ThisFile%Datasets(indxDataset)%iDataSpaceID)
            
            CLASS DEFAULT
                CALL LogMessage('Selected data type cannot be written to file '//ThisFile%Name//'!',f_iWarn,ThisProcedure)
                RETURN
        END SELECT

        !Increment data pointer in file
        ThisFile%Datasets(indxDataset)%iDataPointer_File = ThisFile%Datasets(indxDataset)%iDataPointer_File + 1
        
    END DO
    
  END SUBROUTINE WriteMatrixData
  
  
  ! -------------------------------------------------------------
  ! --- WRITE 1-D ARRAY DATA TO A FULLY-QUALIFIED DATASET
  ! -------------------------------------------------------------
  SUBROUTINE Write1DArrayData(ThisFile,cPath,Data)
    CLASS(HDF5FileType)         :: ThisFile
    CHARACTER(LEN=*),INTENT(IN) :: cPath
    CLASS(*),INTENT(IN)         :: Data(:) 
    
    !Local variables
    CHARACTER(LEN=ModNameLen+16),PARAMETER :: ThisProcedure = ModName // 'Write1DArrayData'
    INTEGER(HID_T)                         :: iDataSpaceID,iDataSetID,iExistingDataSpaceID
    INTEGER(HSIZE_T)                       :: HDims(1),HExistingDims(1),HMaxDims(1)
    INTEGER(SIZE_T)                        :: HLen
    INTEGER                                :: ErrorCode,IntData(SIZE(Data))
    TYPE(C_PTR)                            :: pData  
    LOGICAL                                :: lArrayExists
    
    !Array dimension
    HDims = SHAPE(Data)
    
    !Does array exists?
    lArrayExists = ThisFile%DoesObjectExist(cPath)
    
    !Check if dataset already exists; if it does open it and check its previous dimensions
    IF (lArrayExists) THEN
        CALL H5DOPEN_F(ThisFile%FileID,cPath,iDataSetID,ErrorCode)
        CALL H5DGET_SPACE_F(iDatasetID,iExistingDataSpaceID,ErrorCode)
        CALL H5SGET_SIMPLE_EXTENT_DIMS_F(iExistingDataSpaceID,HExistingDims,HMaxDims,ErrorCode)
        CALL H5SCLOSE_F(iExistingDataSpaceID,ErrorCode)
        IF (HDims(1) .NE. HExistingDims(1)) THEN
            CALL LogMessage('An existing array is being written with different dimensions!',f_iWarn,ThisProcedure)
            RETURN
        END IF
        
    !Otherwise, create the new dataspace
    ELSE
        CALL H5SCREATE_SIMPLE_F(1,HDims,iDataSpaceID,ErrorCode)
    END IF
    
    !Write data
    SELECT TYPE(Data)
        TYPE IS (REAL(8))
            !Create dataset if it does not exist
            IF (.NOT. lArrayExists) THEN
                CALL H5DCREATE_F(ThisFile%FileID,cPath,ThisFile%iReal8TypeID,iDataSpaceID,iDataSetID,ErrorCode)
            END IF
            CALL H5DWRITE_F(iDataSetID,ThisFile%iReal8TypeID,Data,HDims,ErrorCode)
            
        TYPE IS (INTEGER)
            !Create dataset if it does not exist
            IF (.NOT. lArrayExists) THEN
                CALL H5DCREATE_F(ThisFile%FileID,cPath,ThisFile%iIntegerTypeID,iDataSpaceID,iDataSetID,ErrorCode)
            END IF
            CALL H5DWRITE_F(iDataSetID,ThisFile%iIntegerTypeID,Data,HDims,ErrorCode)
            
        TYPE IS (LOGICAL)
            !Create dataset if it does not exist
            IF (.NOT. lArrayExists) THEN
                CALL H5DCREATE_F(ThisFile%FileID,cPath,ThisFile%iIntegerTypeID,iDataSpaceID,iDataSetID,ErrorCode)
            END IF
            IntData = Data
            CALL H5DWRITE_F(iDataSetID,ThisFile%iIntegerTypeID,IntData,HDims,ErrorCode)
            
        TYPE IS (CHARACTER(LEN=*))
            pData = C_LOC(Data(1)(1:1))
            HLen  = LEN(Data(1))
            CALL H5TSET_SIZE_F(ThisFile%iCharacterTypeID,HLen,ErrorCode)
            !Create dataset if it does not exist
            IF (.NOT. lArrayExists) THEN
                CALL H5DCREATE_F(ThisFile%FileID,cPath,ThisFile%iCharacterTypeID,iDataSpaceID,iDataSetID,ErrorCode)
            END IF
            CALL H5DWRITE_F(iDataSetID,ThisFile%iCharacterTypeID,pData,ErrorCode)
            
    END SELECT
        
    !Release resources    
    CALL H5DCLOSE_F(iDataSetID,ErrorCode)
    CALL H5SCLOSE_F(iDataSpaceID,ErrorCode)
    
  END SUBROUTINE Write1DArrayData
  

  ! -------------------------------------------------------------
  ! --- WRITE 2-D ARRAY DATA TO A FULLY-QUALIFIED DATASET
  ! -------------------------------------------------------------
  SUBROUTINE Write2DArrayData(ThisFile,cPath,Data)
    CLASS(HDF5FileType)         :: ThisFile
    CHARACTER(LEN=*),INTENT(IN) :: cPath
    CLASS(*),INTENT(IN)         :: Data(:,:) 
    
    !Local variables
    CHARACTER(LEN=ModNameLen+16),PARAMETER :: ThisProcedure = ModName // 'Write2DArrayData'
    INTEGER(HID_T)                         :: iDataSpaceID,iDataSetID,iExistingDataSpaceID
    INTEGER(HSIZE_T)                       :: HDims(2),HExistingDims(2),HMaxDims(2)
    INTEGER(SIZE_T)                        :: HLen
    INTEGER                                :: ErrorCode,IntData(SIZE(Data,DIM=1),SIZE(Data,DIM=2))
    TYPE(C_PTR)                            :: pData  
    LOGICAL                                :: lArrayExists
    
    !Array dimension
    HDims = SHAPE(Data)
    
    !Does array exists?
    lArrayExists = ThisFile%DoesObjectExist(cPath)
    
    !Check if dataset already exists; if it does open it and check its previous dimensions
    IF (lArrayExists) THEN
        CALL H5DOPEN_F(ThisFile%FileID,cPath,iDataSetID,ErrorCode)
        CALL H5DGET_SPACE_F(iDatasetID,iExistingDataSpaceID,ErrorCode)
        CALL H5SGET_SIMPLE_EXTENT_DIMS_F(iExistingDataSpaceID,HExistingDims,HMaxDims,ErrorCode)
        CALL H5SCLOSE_F(iExistingDataSpaceID,ErrorCode)
        IF (HDims(1).NE.HExistingDims(1) .OR. HDims(2).NE.HExistingDims(2)) THEN
            CALL LogMessage('An existing array is being written with different dimensions!',f_iWarn,ThisProcedure)
            RETURN
        END IF
        
    !Otherwise, create the new dataspace
    ELSE
        CALL H5SCREATE_SIMPLE_F(2,HDims,iDataSpaceID,ErrorCode)
    END IF
    
    !Write data
    SELECT TYPE(Data)
        TYPE IS (REAL(8))
            !Create dataset if it does not exist
            IF (.NOT. lArrayExists) THEN
                CALL H5DCREATE_F(ThisFile%FileID,cPath,ThisFile%iReal8TypeID,iDataSpaceID,iDataSetID,ErrorCode)
            END IF
            CALL H5DWRITE_F(iDataSetID,ThisFile%iReal8TypeID,Data,HDims,ErrorCode)
            
        TYPE IS (INTEGER)
            !Create dataset if it does not exist
            IF (.NOT. lArrayExists) THEN
                CALL H5DCREATE_F(ThisFile%FileID,cPath,ThisFile%iIntegerTypeID,iDataSpaceID,iDataSetID,ErrorCode)
            END IF
            CALL H5DWRITE_F(iDataSetID,ThisFile%iIntegerTypeID,Data,HDims,ErrorCode)
            
        TYPE IS (LOGICAL)
            !Create dataset if it does not exist
            IF (.NOT. lArrayExists) THEN
                CALL H5DCREATE_F(ThisFile%FileID,cPath,ThisFile%iIntegerTypeID,iDataSpaceID,iDataSetID,ErrorCode)
            END IF
            IntData = Data
            CALL H5DWRITE_F(iDataSetID,ThisFile%iIntegerTypeID,IntData,HDims,ErrorCode)
            
       TYPE IS (CHARACTER(LEN=*))
            pData = C_LOC(Data(1,1)(1:1))
            HLen  = LEN(Data(1,1))
            CALL H5TSET_SIZE_F(ThisFile%iCharacterTypeID,HLen,ErrorCode)
            !Create dataset if it does not exist
            IF (.NOT. lArrayExists) THEN
                CALL H5DCREATE_F(ThisFile%FileID,cPath,ThisFile%iCharacterTypeID,iDataSpaceID,iDataSetID,ErrorCode)
            END IF
            CALL H5DWRITE_F(iDataSetID,ThisFile%iCharacterTypeID,pData,ErrorCode)
            
    END SELECT
        
    !Release resources    
    CALL H5DCLOSE_F(iDataSetID,ErrorCode)
    CALL H5SCLOSE_F(iDataSpaceID,ErrorCode)
    
  END SUBROUTINE Write2DArrayData
  

  

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
  ! --- CREATE A GROUP
  ! -------------------------------------------------------------
  SUBROUTINE CreateGroup(ThisFile,cPathName) 
    CLASS(HDF5FileType),INTENT(IN) :: ThisFile
    CHARACTER(LEN=*),INTENT(IN)    :: cPathName
       
    !Local variables
    CHARACTER(LEN=ModNameLen+11),PARAMETER :: ThisProcedure = ModName // 'CreateGroup'
    INTEGER(HID_T)                         :: grp_id,grpcrpl_id
    INTEGER                                :: ErrorCode
    
    !Define attribute dense storage threshold for group
    CALL H5PCREATE_F(H5P_GROUP_CREATE_F,grpcrpl_id,ErrorCode)
    CALL H5PSET_ATTR_PHASE_CHANGE_F(grpcrpl_id,0,0,ErrorCode)
    
    CALL H5GCREATE_F(ThisFile%FileID,cPathName,grp_id,ErrorCode,gcpl_id=grpcrpl_id)
    IF (ErrorCode .NE. 0) THEN
        CALL LogMessage('Error in creating group '//cPathName//' in file '//ThisFile%Name//'!',f_iWarn,ThisProcedure)
        RETURN
    END IF
    
    !Release resources
    CALL H5GCLOSE_F(grp_id,ErrorCode)
               
  END SUBROUTINE CreateGroup
  
  
  ! -------------------------------------------------------------
  ! --- CREATE A DATASET
  ! -------------------------------------------------------------
  SUBROUTINE CreateDataSet(ThisFile,cPathNames,NColumns,NTime,TimeStep,DataType,iStat)
    CLASS(HDF5FileType)           :: ThisFile
    CHARACTER(LEN=*),INTENT(IN)   :: cPathNames(:)      !Pathname for each dataset to be created
    INTEGER,INTENT(IN)            :: NColumns(:),NTime  !Number of columns for each dataset can be different but number of timesteps must be the same 
    TYPE(TimeStepType),INTENT(IN) :: TimeStep           !This represents data begin date and time
    CLASS(*),INTENT(IN)           :: DataType           !Integer or Real(8)
    INTEGER,INTENT(OUT)           :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+13),PARAMETER          :: ThisProcedure = ModName // 'CreateDataSet'
    INTEGER                                         :: ErrorCode,iSize,indx,nDatasets,iBytes,HDims_NativeInt(2)
    INTEGER(HID_T)                                  :: iChunkPropID,iAccessPropID
    INTEGER(HSIZE_T)                                :: HDims(2),iDims(1)
    INTEGER(SIZE_T)                                 :: iCacheSize,iMaxCacheSize
    TYPE(DatasetType),ALLOCATABLE                   :: TempDatasets(:)
    CHARACTER(LEN=f_iMaxDatasetNameLen),ALLOCATABLE :: cTempDatasetNames(:)
    
    !Initialize
    iStat = 0
        
    !First make sure that number of timesteps are the same for all datasets
    IF (ALLOCATED(ThisFile%Datasets)) THEN
        IF (ThisFile%nTime .NE. NTime) THEN
            CALL SetLastMessage('Number of timesteps for all datasets must be the same in file '//ThisFile%Name//'!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
    END IF
    
    !Initialize
    iSize     = SIZE(cPathNames)
    nDatasets = ThisFile%nDatasets
    
    !Create /Attributes group id it doesn't exist
    IF (.NOT. ThisFile%DoesObjectExist(f_cHeaderDir1)) CALL ThisFile%CreateGroup(f_cHeaderDir1) 
           
    !Allocate temporary work array
    ALLOCATE (TempDatasets(nDatasets+iSize),cTempDatasetNames(nDatasets+iSize))
    TempDatasets(1:nDatasets) = ThisFile%Datasets
    IF (nDatasets .GT. 0) THEN
        CALL ThisFile%Read1DArrayDataSet(f_cHeaderDir1//'/cLocationNames',cTempDatasetNames(1:nDatasets),iStat=iStat)  
        IF (iStat .EQ. -1) RETURN
    END IF
    cTempDatasetNames(nDatasets+1:) = cPathNames
    
    !Store number of time steps
    ThisFile%nTime = NTime
    
    !Byte size of the data type
    iBytes = STORAGE_SIZE(DataType) / 8
    
    !Create property ID for chunking
    CALL H5PCREATE_F(H5P_DATASET_CREATE_F,iChunkPropID,ErrorCode)
    
    !Create property ID for chunk cache size
    CALL H5PCREATE_F(H5P_DATASET_ACCESS_F,iAccessPropID,ErrorCode)
    
    !Create datasets one by one
    DO indx=1,iSize
        ASSOCIATE (pDataset => TempDatasets(nDatasets+indx))
            !Establish the full array dataspace
            HDims = [NColumns(indx) , NTime]
            CALL H5SCREATE_SIMPLE_F(2,HDims,pDataset%iDataSpaceID,ErrorCode)
            IF (ErrorCode .NE. 0) THEN
                CALL SetLastMessage('Error in creating file data space for '//TRIM(cPathNames(indx))//'!',f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
            
            !Establish the partial data dataspaces (one column only and one timestep only)
            iDims(1) = NColumns(indx)
            CALL H5SCREATE_SIMPLE_F(1,iDims,pDataset%iDataSpaceID_OneTimeStep,ErrorCode)
            IF (ErrorCode .NE. 0) THEN
                CALL SetLastMessage('Error in creating one-timestep data space for '//TRIM(cPathNames(indx))//'!',f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
            
            iDims(1) = NTime
            CALL H5SCREATE_SIMPLE_F(1,iDims,pDataset%iDataSpaceID_OneColumn,ErrorCode)
            IF (ErrorCode .NE. 0) THEN
                CALL SetLastMessage('Error in creating one-column data space for '//TRIM(cPathNames(indx))//'!',f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
            
            !Number of columns
            pDataset%nColumns = NColumns(indx)
            
            !Set chunk size so that there are NColumns and one year worth of timesesteps or as many as that fits into max cache size, whicever is smaller
            HDims(1) = MAX(1 , pDataset%nColumns)
            HDims(2) = MIN(MAX(1 , 525600/TimeStep%DeltaT_InMinutes)  , NTime)   !525600 = one year in minutes
            IF (PRODUCT(HDims)*iBytes .GT. iMaxCacheSize) THEN
                HDims(2) = MAX(1 , iMaxCacheSize/(HDims(1)*iBytes))
            END IF
            CALL H5PSET_CHUNK_F(iChunkPropID,2,HDims,ErrorCode)
            
            !Set chunk cache size equal to chunk size or maximum cache size, whichever is smaller
            iCacheSize = MIN(PRODUCT(HDims)*iBytes , iMaxCacheSize)
            CALL H5PSET_CHUNK_CACHE_F(iAccessPropID,101,iCacheSize,1.0,ErrorCode)
            
            !Create dataset
            SELECT TYPE (DataType)
                TYPE IS (REAL(8))
                    CALL H5DCREATE_F(ThisFile%FileID,cPathNames(indx),ThisFile%iReal8TypeID,pDataset%iDataSpaceID,pDataset%iDataSetID,ErrorCode,dcpl_id=iChunkPropID,dapl_id=iAccessPropID)
                        
                TYPE IS (INTEGER)
                    CALL H5DCREATE_F(ThisFile%FileID,cPathNames(indx),ThisFile%iIntegerTypeID,pDataset%iDataSpaceID,pDataset%iDataSetID,ErrorCode,dcpl_id=iChunkPropID,dapl_id=iAccessPropID)
                        
                CLASS DEFAULT
                    CALL SetLastMessage('Specified data type cannot be written to file '//ThisFile%Name//'!',f_iFatal,ThisProcedure)
                    iStat = -1
                    RETURN
                    
            END SELECT
            IF (ErrorCode .NE. 0) THEN
                CALL SetLastMessage('Error in creating data set '//cPathNames(indx)//' in file '//ThisFile%Name//'!',f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
            
            !Write the chunk dimensions and byte size of each data as attribute for the dataset
            HDims_NativeInt = HDims
            CALL ThisFile%WriteAttribute(f_iDataset,cPathNames(indx),'ChunkDims',ArrayAttrData=HDims_NativeInt)   
            CALL ThisFile%WriteAttribute(f_iDataset,cPathNames(indx),'DataByteSize',ScalarAttrData=iBytes)        
    
        END ASSOCIATE
    END DO
    
    !Transfer temporary work array to persistent array
    CALL MOVE_ALLOC(TempDatasets,ThisFile%Datasets)
    
    !Update number of datasets
    ThisFile%nDatasets = nDatasets + iSize
    
    !Write time related information
    IF (nDatasets .EQ. 0) THEN
        CALL ThisFile%WriteAttribute(f_iGroup,f_cHeaderDir1,'NTimeSteps',ScalarAttrData=NTime)
        CALL ThisFile%WriteAttribute(f_iGroup,f_cHeaderDir1,'TimeStep%TrackTime',ScalarAttrData=TimeStep%TrackTime)
        CALL ThisFile%WriteAttribute(f_iGroup,f_cHeaderDir1,'TimeStep%DeltaT',ScalarAttrData=TimeStep%DeltaT)
        CALL ThisFile%WriteAttribute(f_iGroup,f_cHeaderDir1,'TimeStep%DeltaT_InMinutes',ScalarAttrData=TimeStep%DeltaT_InMinutes)
        CALL ThisFile%WriteAttribute(f_iGroup,f_cHeaderDir1,'TimeStep%Unit',ScalarAttrData=TimeStep%Unit)
        CALL ThisFile%WriteAttribute(f_iGroup,f_cHeaderDir1,'TimeStep%BeginDateAndTime',ScalarAttrData=TimeStep%CurrentDateAndTime)
        CALL ThisFile%WriteAttribute(f_iGroup,f_cHeaderDir1,'TimeStep%BeginTime',ScalarAttrData=TimeStep%CurrentTime)
    END IF
    
    !Write number of datasets and dataset names to root group
    CALL ThisFile%WriteAttribute(f_iGroup,f_cHeaderDir1,'nLocations',ScalarAttrData=ThisFile%nDatasets)
    CALL ThisFile%Write1DArrayData(f_cHeaderDir1//'/cLocationNames',cTempDatasetNames)                 
    
    !Delete dataset creation property list
    CALL H5PCLOSE_F(iChunkPropID,ErrorCode)
    CALL H5PCLOSE_F(iAccessPropID,ErrorCode)
    
  END SUBROUTINE CreateDataSet

  
  ! -------------------------------------------------------------
  ! --- WRITE AN ATTRIBUTE
  ! -------------------------------------------------------------
  SUBROUTINE WriteAttribute(ThisFile,iObjectType,cGrpOrDset,cAttrName,ScalarAttrData,ArrayAttrData)
    CLASS(HDF5FileType),INTENT(IN) :: ThisFile
    INTEGER,INTENT(IN)             :: iObjectType
    CHARACTER(LEN=*),INTENT(IN)    :: cGrpOrDset,cAttrName
    CLASS(*),OPTIONAL,INTENT(IN)   :: ScalarAttrData,ArrayAttrData(:)   !Only one must be specified
    
    !Local variables
    CHARACTER(LEN=ModNameLen+14),PARAMETER :: ThisProcedure = ModName // 'WriteAttribute'
    INTEGER                                :: ErrorCode
    INTEGER(HID_T)                         :: space_id,obj_id,attr_id
    INTEGER(HSIZE_T)                       :: HDims(1)
    INTEGER(SIZE_T)                        :: HLen
    TYPE(C_PTR)                            :: pData
    LOGICAL                                :: lAttrExists
    
    !Get ID for group or dataset
    SELECT CASE (iObjectType)
        CASE (f_iGroup)
            CALL H5GOPEN_F(ThisFile%FileID,cGrpOrDset,obj_id,ErrorCode)
        CASE (f_iDataSet)
            CALL H5DOPEN_F(ThisFile%FileID,cGrpOrDset,obj_id,ErrorCode)
    END SELECT
        
    !Does the attribute exist already?
    CALL H5AEXISTS_F(obj_id,cAttrName,lAttrExists,ErrorCode)
        
    !Establish dataspace and data type, create and write attribute
    !Write scalar data
    IF (PRESENT(ScalarAttrData)) THEN
        
        !Get a C pointer to scalar data
        pData = C_LOC(ScalarAttrData)
        
        !Create or open attribute
        SELECT TYPE (ScalarAttrData)
            TYPE IS (REAL(8))
                IF (lAttrExists) THEN
                    CALL H5AOPEN_F(obj_id,cAttrName,attr_id,ErrorCode)
                ELSE
                    CALL H5ACREATE_F(obj_id,cAttrName,ThisFile%iReal8TypeID,ThisFile%iScalarDataSpaceID,attr_id,ErrorCode)
                END IF
                CALL H5AWRITE_F(attr_id,ThisFile%iReal8TypeID,pData,ErrorCode)
                
            TYPE IS (INTEGER)
                IF (lAttrExists) THEN
                    CALL H5AOPEN_F(obj_id,cAttrName,attr_id,ErrorCode)
                ELSE
                    CALL H5ACREATE_F(obj_id,cAttrName,ThisFile%iIntegerTypeID,ThisFile%iScalarDataSpaceID,attr_id,ErrorCode)
                END IF
                CALL H5AWRITE_F(attr_id,ThisFile%iIntegerTypeID,pData,ErrorCode)
                
            TYPE IS (CHARACTER(LEN=*))
                pData = C_LOC(ScalarAttrData(1:1))
                HLen  = LEN(ScalarAttrData)
                CALL H5TSET_SIZE_F(ThisFile%iCharacterTypeID,HLen,ErrorCode)
                IF (lAttrExists) THEN
                    CALL H5AOPEN_F(obj_id,cAttrName,attr_id,ErrorCode)
                ELSE
                    CALL H5ACREATE_F(obj_id,cAttrName,ThisFile%iCharacterTypeID,ThisFile%iScalarDataSpaceID,attr_id,ErrorCode)
                END IF
                CALL H5AWRITE_F(attr_id,ThisFile%iCharacterTypeID,pData,ErrorCode)
                
            TYPE IS (LOGICAL)
                IF (lAttrExists) THEN
                    CALL H5AOPEN_F(obj_id,cAttrName,attr_id,ErrorCode)
                ELSE
                    CALL H5ACREATE_F(obj_id,cAttrName,ThisFile%iIntegerTypeID,ThisFile%iScalarDataSpaceID,attr_id,ErrorCode)
                END IF
                CALL H5AWRITE_F(attr_id,ThisFile%iIntegerTypeID,pData,ErrorCode)
                
            CLASS DEFAULT
                CALL LogMessage('Data type is not supported for attribute definition for file '//ThisFile%Name//'!',f_iWarn,ThisProcedure)
                RETURN
        END SELECT
        IF (ErrorCode .NE. 0) THEN
            CALL LogMessage('Error in writing attribute '//cAttrName//' to file '//ThisFile%Name//'!',f_iWarn,ThisProcedure)
            RETURN
        END IF
            
            
    !Write array data
    ELSEIF (PRESENT(ArrayAttrData)) THEN
        !Data space
        HDims(1) = SIZE(ArrayAttrData)
        CALL H5SCREATE_SIMPLE_F(1,HDims,space_id,ErrorCode)
        
        !Get a C pointer to attribute to be written
        pData = C_LOC(ArrayAttrData(1))
        
        !Create attribute
        SELECT TYPE (ArrayAttrData)
            TYPE IS (REAL(8))
                IF (lAttrExists) THEN
                    CALL H5AOPEN_F(obj_id,cAttrName,attr_id,ErrorCode)
                ELSE
                    CALL H5ACREATE_F(obj_id,cAttrName,ThisFile%iReal8TypeID,space_id,attr_id,ErrorCode)
                END IF
                CALL H5AWRITE_F(attr_id,ThisFile%iReal8TypeID,pData,ErrorCode)
                
            TYPE IS (INTEGER)
                IF (lAttrExists) THEN
                    CALL H5AOPEN_F(obj_id,cAttrName,attr_id,ErrorCode)
                ELSE
                    CALL H5ACREATE_F(obj_id,cAttrName,ThisFile%iIntegerTypeID,space_id,attr_id,ErrorCode)
                END IF
                CALL H5AWRITE_F(attr_id,ThisFile%iIntegerTypeID,pData,ErrorCode)
                
            TYPE IS (CHARACTER(LEN=*))
                pData = C_LOC(ArrayAttrData(1)(1:1))
                HLen  = LEN(ArrayAttrData(1))
                CALL H5TSET_SIZE_F(ThisFile%iCharacterTypeID,HLen,ErrorCode)
                IF (lAttrExists) THEN
                    CALL H5AOPEN_F(obj_id,cAttrName,attr_id,ErrorCode)
                ELSE
                    CALL H5ACREATE_F(obj_id,cAttrName,ThisFile%iCharacterTypeID,space_id,attr_id,ErrorCode)
                END IF
                CALL H5AWRITE_F(attr_id,ThisFile%iCharacterTypeID,pData,ErrorCode)
                
            TYPE IS (LOGICAL)
                IF (lAttrExists) THEN
                    CALL H5AOPEN_F(obj_id,cAttrName,attr_id,ErrorCode)
                ELSE
                    CALL H5ACREATE_F(obj_id,cAttrName,ThisFile%iIntegerTypeID,space_id,attr_id,ErrorCode)
                END IF
                CALL H5AWRITE_F(attr_id,ThisFile%iIntegerTypeID,pData,ErrorCode)
                
            CLASS DEFAULT
                CALL LogMessage('Data type is not supported for attribute definition for file '//ThisFile%Name//'!',f_iWarn,ThisProcedure)
                RETURN
        END SELECT
        IF (ErrorCode .NE. 0) THEN
            CALL LogMessage('Error in writing attribute '//cAttrName//' to file '//ThisFile%Name//'!',f_iWarn,ThisProcedure)
            RETURN
        END IF
        
        !Release dataspace id
        CALL H5SCLOSE_F(space_id,ErrorCode)
        
    !No data to be written is defined
    ELSE
        CALL LogMessage('Either a scalar or an array attribute must be specified!',f_iWarn,ThisProcedure)
        RETURN
    END IF
       
    !Release resources
    CALL H5ACLOSE_F(attr_id,ErrorCode)
    CALL H5OCLOSE_F(obj_id,ErrorCode)
    
  END SUBROUTINE WriteAttribute
  
  
  ! -------------------------------------------------------------
  ! --- GIVEN FILENAME, GET INDEX IN OPEN FILE LIST
  ! -------------------------------------------------------------
  FUNCTION GetOpenFileIndex_FromName(FileName) RESULT(iIndex)
    CHARACTER(LEN=*),INTENT(IN) :: FileName
    INTEGER                     :: iIndex
    
    !Local variables
    INTEGER                      :: indx
    CHARACTER(LEN=LEN(FileName)) :: WorkFileName
    
    iIndex       = 0
    WorkFileName = UpperCase(ADJUSTL(FileName))
    
    DO indx=1,nOpenHDFFiles
        IF (TRIM(WorkFileName) .EQ. TRIM(OpenFileNames(indx))) THEN
          iIndex = indx
          RETURN
        END IF
    END DO
    
  END FUNCTION GetOpenFileIndex_FromName
  
  
  ! -------------------------------------------------------------
  ! --- GIVEN THE FILE ID, GET INDEX FROM OPEN FILE LIST
  ! -------------------------------------------------------------
  FUNCTION GetOpenFileIndex_FromFileID(FileID) RESULT(iIndex)
    INTEGER(HID_T),INTENT(IN) :: FileID
    INTEGER                   :: iIndex
    
    !Local variables
    INTEGER :: indx
    
    iIndex = 0
    
    DO indx=1,nOpenHDFFiles
        IF (OpenFileIDs(indx) .EQ. FileID) THEN
          iIndex = indx
          RETURN
        END IF
    END DO
    
  END FUNCTION GetOpenFileIndex_FromFileID
  
  
  ! -------------------------------------------------------------
  ! --- INITIALIZE DATA TYPE IDs
  ! -------------------------------------------------------------
  SUBROUTINE InitDataTypeIDs(iIntegerTypeID,iReal8TypeID,iCharacterTypeID,iScalarDataSpaceID)
    INTEGER(HID_T),INTENT(OUT) :: iIntegerTypeID,iReal8TypeID,iCharacterTypeID,iScalarDataSpaceID
  
    !Local variables
    INTEGER :: ErrorCode
  
    iIntegerTypeID = H5KIND_TO_TYPE(KIND(0),H5_INTEGER_KIND)
    iReal8TypeID   = H5KIND_TO_TYPE(KIND(0d0),H5_REAL_KIND)
    CALL H5TCOPY_F(H5T_FORTRAN_S1,iCharacterTypeID,ErrorCode)
    CALL H5SCREATE_F(H5S_SCALAR_F,iScalarDataSpaceID,ErrorCode)
      
  END SUBROUTINE InitDataTypeIDs
  
  
  ! -------------------------------------------------------------
  ! --- REWIND FILE (for HDF5, simply set data pointer to 0)
  ! -------------------------------------------------------------
  SUBROUTINE Rewind(ThisFile)
    CLASS(HDF5FileType) :: ThisFile
    
    ThisFile%Datasets%iDataPointer_File = 0
    
  END SUBROUTINE Rewind


  ! -------------------------------------------------------------
  ! --- CHECK BY AND NAME IF AN OBJECT EXISTS
  ! -------------------------------------------------------------
  FUNCTION DoesObjectExist(ThisFile,cName) RESULT(lExist)
    CLASS(HDF5FileType),INTENT(IN) :: ThisFile
    CHARACTER(LEN=*),INTENT(IN)    :: cName
    LOGICAL                        :: lExist
    
    !Local variables
    INTEGER          :: ErrorCode
    TYPE(H5O_INFO_T) :: objinfo
    
    !Initialize
    lExist = .FALSE.
    
    !Turn off automatic error printing
    CALL H5ESET_AUTO_F(0,ErrorCode)
    
    !Check if the object exists
    CALL H5OGET_INFO_BY_NAME_F(ThisFile%FileID,cName,objinfo,ErrorCode)
    IF (ErrorCode .EQ. 0) lExist = .TRUE.
    
    !Turn on automatic error printing
    CALL H5ESET_AUTO_F(1,ErrorCode)
    
  END FUNCTION DoesObjectExist
  
END MODULE