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
MODULE Class_BudgetInputFile
  USE MessageLogger           , ONLY: SetLastMessage        , &
                                      f_iFatal
  USE TimeSeriesUtilities     , ONLY: TimeStepType 
  USE GeneralUtilities        , ONLY: IntToText             , &
                                      UpperCase             , &
                                      ReplaceString         , &
                                      AllocArray
  USE IOInterface             , ONLY: GenericFileType       , &
                                      iGetFileType_FromName , &
                                      f_iHDF                , &
                                      f_iGroup
  USE Budget_Parameters
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
  PUBLIC :: BudgetInputFileType               , &
            LocationDataType                  , &
            BudgetHeaderType                        
  
  
  ! -------------------------------------------------------------
  ! --- BUDGET INPUT FILE TYPE
  ! -------------------------------------------------------------
  TYPE,EXTENDS(GenericFileType) :: BudgetInputFileType
  CONTAINS
      PROCEDURE,PASS :: Create
      PROCEDURE,PASS :: Open
      PROCEDURE,PASS :: Close
      PROCEDURE,PASS :: ReadHeader
      GENERIC        :: NewFile    => Create , &
                                      Open
  END TYPE BudgetInputFileType
  
  
  ! -------------------------------------------------------------
  ! --- ASCII OUTPUT RELATED DATA
  ! -------------------------------------------------------------
  TYPE ASCIIOutputType
    INTEGER                                   :: TitleLen                = 0       
    INTEGER                                   :: NTitles                 = 0       
    CHARACTER(LEN=f_iMaxTitleLen),ALLOCATABLE :: cTitles(:)              
    LOGICAL,ALLOCATABLE                       :: lTitlePersist(:)             !Flag to check if a title is to be generated in all occasions (i.e. ASCII or GUI output)
    CHARACTER(LEN=f_iFormatSpecLen)           :: cFormatSpec             = ''  
    INTEGER                                   :: NColumnHeaderLines      = 0
  END TYPE ASCIIOutputType
            
  
  ! -------------------------------------------------------------
  ! --- DSS OUTPUT DATA TYPE
  ! -------------------------------------------------------------
  TYPE DSSOutputType
    CHARACTER(LEN=f_iPathNameLen),ALLOCATABLE :: cPathNames(:)
    INTEGER,ALLOCATABLE                       :: iDataTypes(:)
  END TYPE DSSOutputType
  
  
  ! -------------------------------------------------------------
  ! --- LOCATION DATA
  ! -------------------------------------------------------------
  TYPE LocationDataType
    INTEGER                                       :: NDataColumns                = 0  !Number of data columns excluding Time column
    INTEGER                                       :: iStorUnitsInFile            = 0  !Number of storage units (bytes, words, etc) each set of output for the location occupies in the binary file
    CHARACTER(LEN=f_iColumnHeaderLen),ALLOCATABLE :: cFullColumnHeaders(:)            !For undefined output for each (column+1); +1 is for Time column
    INTEGER,ALLOCATABLE                           :: iDataColumnTypes(:)              !For each (column)
    INTEGER,ALLOCATABLE                           :: iColWidth(:)                     !For each (column+1); +1 is for Time column
    CHARACTER(LEN=f_iColumnHeaderLen),ALLOCATABLE :: cColumnHeaders(:,:)              !For ASCII output for each (column+1,line); +1 is for Time column
    CHARACTER(LEN=f_iFormatSpecLen),ALLOCATABLE   :: cColumnHeadersFormatSpec(:)      !For ASCII output for each (line)       
  END TYPE LocationDataType
  

  ! -------------------------------------------------------------
  ! --- BUDGET OUTPUT DATA TYPE
  ! -------------------------------------------------------------
  TYPE BudgetHeaderType
    CHARACTER(LEN=f_iBudgetDescriptorLen)            :: cBudgetDescriptor = ''
    INTEGER                                          :: NTimeSteps        = 0
    TYPE(TimeStepType)                               :: TimeStep
    INTEGER                                          :: NAreas            = 0
    REAL(8),ALLOCATABLE                              :: Areas(:)
    TYPE(ASCIIOutputType)                            :: ASCIIOutput
    INTEGER                                          :: NLocations        = 0
    CHARACTER(LEN=f_iMaxLocationNameLen),ALLOCATABLE :: cLocationNames(:)
    TYPE(LocationDataType),ALLOCATABLE               :: Locations(:)
    TYPE(DSSOutputType)                              :: DSSOutput
    INTEGER(KIND=8)                                  :: iPosition         = -1      !Position in file after Header
  CONTAINS
    PROCEDURE,PASS :: Kill
  END TYPE BudgetHeaderType
  
  
  ! -------------------------------------------------------------
  ! --- MISC. ENTITIES
  ! -------------------------------------------------------------
  CHARACTER(LEN=1),PARAMETER          :: cHeaderDir  = '/'            !This is where budget file attributes were previusly written in HDF5 file
  CHARACTER(LEN=11),PARAMETER         :: cHeaderDir1 = '/Attributes'  !This is where budget file attributes are now written in HDF5 file
  INTEGER,PARAMETER                   :: ModNameLen  = 23
  CHARACTER(LEN=ModNameLen),PARAMETER :: ModName     = 'Class_BudgetInputFile::'

  
  
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
  ! --- CREATE BUDGET INPUT FILE
  ! -------------------------------------------------------------
  SUBROUTINE Create(BudgetFile,cFileName,OutputData,iStat)
    CLASS(BudgetInputFileType)        :: BudgetFile
    CHARACTER(LEN=*),INTENT(IN)       :: cFileName
    TYPE(BudgetHeaderType),INTENT(IN) :: OutputData
    INTEGER,INTENT(OUT)               :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+6),PARAMETER :: ThisProcedure = ModName // 'Create'
    INTEGER                               :: nDataColumns(OutputData%NLocations)
    
    !Initialize
    iStat = 0
    
    !Make sure that file is an HDF5 file
    IF (iGetFileType_FromName(cFileName) .NE. f_iHDF) THEN
        CALL SetLastMessage('File '//TRIM(ADJUSTL(cFileName))//' must be an HDF5 file for budget output!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    !Open file
    CALL BudgetFile%New(FileName=ADJUSTL(cFileName),InputFile=.FALSE.,Descriptor=OutputData%cBudgetDescriptor,iStat=iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Write header
    CALL WriteHeader(BudgetFile,OutputData)
    
    !Create releated datasets
    IF (SIZE(OutputData%Locations) .EQ. 1) THEN
        nDataColumns = OutputData%Locations(1)%NDataColumns
    ELSE
        nDataColumns = OutputData%Locations%NDataColumns
    END IF
    CALL BudgetFile%CreateHDFDataSet(OutputData%cLocationNames,nDataColumns,OutputData%NTimeSteps,OutputData%TimeStep,DataType=0d0,iStat=iStat)
      
  END SUBROUTINE Create
  
  
  ! -------------------------------------------------------------
  ! --- OPEN AN EXISTING BUDGET INPUT FILE
  ! -------------------------------------------------------------
  SUBROUTINE Open(BudgetFile,cFileName,iStat) 
    CLASS(BudgetInputFileType)  :: BudgetFile
    CHARACTER(LEN=*),INTENT(IN) :: cFileName
    INTEGER,INTENT(OUT)         :: iStat

    !Open file
    CALL BudgetFile%New(FileName=cFileName,InputFile=.TRUE.,iStat=iStat)
    
  END SUBROUTINE Open
  
  
  
  
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
  ! --- KILL BUDGET INPUT FILE
  ! -------------------------------------------------------------
  SUBROUTINE Close(BudgetFile)
    CLASS(BudgetInputFileType) :: BudgetFile
    
    CALL BudgetFile%Kill()
    
  END SUBROUTINE Close
  
  
  ! -------------------------------------------------------------
  ! --- KILL OUTPUT DATA
  ! -------------------------------------------------------------
  SUBROUTINE Kill(Header)
    CLASS(BudgetHeaderType) :: Header
    
    !Local variables
    INTEGER                :: ErrorCode
    TYPE(BudgetHeaderType) :: Dummy
    
    !Clear memory
    DEALLOCATE (Header%Areas                      , &
                Header%ASCIIOutput%lTitlePersist  , &
                Header%cLocationNames             , &
                Header%Locations                  , &
                Header%DSSOutput%cPathNames       , &
                Header%DSSOutput%iDataTypes       , &
                STAT=ErrorCode                    )
    
    !Restore attributes to their default values
    Header%cBudgetDescriptor = Dummy%cBudgetDescriptor 
    Header%NTimeSteps        = Dummy%NTimeSteps        
    Header%TimeStep          = Dummy%TimeStep
    Header%NAreas            = Dummy%NAreas            
    Header%ASCIIOutput       = Dummy%ASCIIOutput
    Header%NLocations        = Dummy%NLocations        
    Header%DSSOutput         = Dummy%DSSOutput
    Header%iPosition         = Dummy%iPosition
    
  END SUBROUTINE Kill

  
   

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
  ! --- WRITE HEADER DATA
  ! -------------------------------------------------------------
  SUBROUTINE WriteHeader(File,Header)
    CLASS(GenericFileType)            :: File
    TYPE(BudgetHeaderType),INTENT(IN) :: Header
    
    !Local variables
    INTEGER            :: indx,indx1
    CHARACTER(LEN=100) :: cLocation
    
    !Create Header directory
    CALL File%CreateHDFGroup(cHeaderDir1)
    
    !Budget descriptor
    CALL File%WriteData(f_iGroup,cHeaderDir1,'Descriptor',ScalarAttrData=Header%cBudgetDescriptor)
    
    !Simulation time related data (do not write these; they are written as part of HDF5 file class)
    !CALL File%WriteData(iGroup,cHeaderDir1,'NTimeSteps',ScalarAttrData=Header%NTimeSteps)
    !CALL File%WriteData(iGroup,cHeaderDir1,'TimeStep%TrackTime',ScalarAttrData=Header%TimeStep%TrackTime)
    !CALL File%WriteData(iGroup,cHeaderDir1,'TimeStep%DeltaT',ScalarAttrData=Header%TimeStep%DeltaT)
    !CALL File%WriteData(iGroup,cHeaderDir1,'TimeStep%DeltaT_InMinutes',ScalarAttrData=Header%TimeStep%DeltaT_InMinutes)
    !CALL File%WriteData(iGroup,cHeaderDir1,'TimeStep%Unit',ScalarAttrData=Header%TimeStep%Unit)
    !CALL File%WriteData(iGroup,cHeaderDir1,'TimeStep%CurrentDateAndTime',ScalarAttrData=Header%TimeStep%CurrentDateAndTime)
    !CALL File%WriteData(iGroup,cHeaderDir1,'TimeStep%CurrentTime',ScalarAttrData=Header%TimeStep%CurrentTime)
    
    !Areas
    CALL File%WriteData(f_iGroup,cHeaderDir1,'NAreas',ScalarAttrData=Header%NAreas)
    IF (Header%NAreas .GT. 0) THEN
        CALL File%WriteData(Header%Areas,cHDFPath=cHeaderDir1//'/Areas')                   
    END IF
    
    !Data for ASCII output
    CALL File%WriteData(f_iGroup,cHeaderDir1,'ASCIIOutput%TitleLen',ScalarAttrData=Header%ASCIIOutput%TitleLen)                      
    CALL File%WriteData(f_iGroup,cHeaderDir1,'ASCIIOutput%NTitles',ScalarAttrData=Header%ASCIIOutput%NTitles)                           
    CALL File%WriteData(f_iGroup,cHeaderDir1,'ASCIIOutput%cTitles',ArrayAttrData=Header%ASCIIOutput%cTitles)                         
    CALL File%WriteData(f_iGroup,cHeaderDir1,'ASCIIOutput%lTitlePersist',ArrayAttrData=Header%ASCIIOutput%lTitlePersist)             
    CALL File%WriteData(f_iGroup,cHeaderDir1,'ASCIIOutput%cFormatSpec',ScalarAttrData=Header%ASCIIOutput%cFormatSpec)                
    CALL File%WriteData(f_iGroup,cHeaderDir1,'ASCIIOutput%NColumnHeaderLines',ScalarAttrData=Header%ASCIIOutput%NColumnHeaderLines)  
    
    !Location names (do not write these; they are written as part of an HDF5 file class)
    !CALL File%WriteData(iGroup,cHeaderDir1,'NLocations',ScalarAttrData=Header%NLocations)
    !CALL File%WriteData(iGroup,cHeaderDir1,'cLocationNames',ArrayAttrData=Header%cLocationNames)
    
    !Location data
    CALL File%WriteData(f_iGroup,cHeaderDir1,'NLocationData',ScalarAttrData=SIZE(Header%Locations))
    DO indx=1,SIZE(Header%Locations) 
        cLocation = 'LocationData'//TRIM(IntToText(indx))//'%'
        CALL File%WriteData(f_iGroup,cHeaderDir1,TRIM(cLocation)//'NDataColumns',ScalarAttrData=Header%Locations(indx)%NDataColumns)             
        CALL File%WriteData(f_iGroup,cHeaderDir1,TRIM(cLocation)//'cFullColumnHeaders',ArrayAttrData=Header%Locations(indx)%cFullColumnHeaders)  
        CALL File%WriteData(f_iGroup,cHeaderDir1,TRIM(cLocation)//'iDataColumnTypes',ArrayAttrData=Header%Locations(indx)%iDataColumnTypes)      
        CALL File%WriteData(f_iGroup,cHeaderDir1,TRIM(cLocation)//'iColWidth',ArrayAttrData=Header%Locations(indx)%iColWidth)                    
        DO indx1=1,Header%ASCIIOutput%NColumnHeaderLines
            CALL File%WriteData(f_iGroup,cHeaderDir1,TRIM(cLocation)//'L'//TRIM(IntToText(indx1))//'_cColumnHeaders',ArrayAttrData=Header%Locations(indx)%cColumnHeaders(:,indx1))  
        END DO              
        CALL File%WriteData(f_iGroup,cHeaderDir1,TRIM(cLocation)//'cColumnHeadersFormatSpec',ArrayAttrData=Header%Locations(indx)%cColumnHeadersFormatSpec)  
    END DO
    
    !DSS output data (write pathnames one by one instead of as an array to avoid memory errors from HDF5 API)
    CALL File%WriteData(f_iGroup,cHeaderDir1,'DSSOutput%NPathNames',ScalarAttrData=SIZE(Header%DSSOutput%cPathNames))  
    CALL File%WriteData(Header%DSSOutput%cPathNames,cHDFPath=cHeaderDir1//'/DSSOutput%cPathNames')                   
    CALL File%WriteData(f_iGroup,cHeaderDir1,'DSSOutput%NDataTypes',ScalarAttrData=SIZE(Header%DSSOutput%iDataTypes))  
    CALL File%WriteData(f_iGroup,cHeaderDir1,'DSSOutput%iDataTypes',ArrayAttrData=Header%DSSOutput%iDataTypes)         
    
  END SUBROUTINE WriteHeader
  
  
  
  
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
  ! --- READ HEADER DATA FROM HDF5 OR BIN FILE
  ! -------------------------------------------------------------
  SUBROUTINE ReadHeader(InputFile,Header,iStat)
    CLASS(BudgetInputFileType)         :: InputFile
    TYPE(BudgetHeaderType),INTENT(OUT) :: Header
    INTEGER,INTENT(OUT)                :: iStat
    
    IF (InputFile%iGetFileType() .EQ. f_iHDF) THEN
        CALL ReadHeader_FromHDFFile(InputFile,Header,iStat)
    ELSE
        CALL ReadHeader_FromBinFile(InputFile,Header,iStat)
    END IF
    
  END SUBROUTINE ReadHeader
  
  
  ! -------------------------------------------------------------
  ! --- READ HEADER DATA FROM HDF5 FILE
  ! -------------------------------------------------------------
  SUBROUTINE ReadHeader_FromHDFFile(InputFile,Header,iStat)
    CLASS(GenericFileType)             :: InputFile
    TYPE(BudgetHeaderType),INTENT(OUT) :: Header
    INTEGER,INTENT(OUT)                :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+22) :: ThisProcedure = ModName // 'ReadHeader_FromHDFFile'
    INTEGER                      :: indx,NLocs,indx1,iSize,NDataColumns,NColumnHeaderLines,NTitles
    CHARACTER(LEN=100)           :: cLocation*100,cAttributesDir*11
    LOGICAL                      :: lAttributesDirExist
    CHARACTER(:),ALLOCATABLE     :: cFileName
    
    !Initialize
    iStat = 0
    
    ASSOCIATE (pFile => InputFile)
    
      !Check where the attributes are written
      lAttributesDirExist = pFile%DoesHDFObjectExist('/Attributes')
      IF (lAttributesDirExist) THEN
          cAttributesDir = cHeaderDir1
      ELSE
          cAttributesDir = cHeaderDir
      END IF
      
      !Make sure that this is inded a Budget file
      IF (.NOT. InputFile%DoesHDFObjectExist(cAttributesDir//'/DSSOutput%cPathNames')) THEN
          CALL InputFile%GetName(cFileName)
          CALL SetLastMessage('File '//TRIM(cFileName)//' is not a Budget file type!',f_iFatal,ThisProcedure)
          iStat = -1
          RETURN
      END IF
    
      !Budget descriptor
      CALL pFile%ReadData(cAttributesDir,'Descriptor',ScalarAttrData=Header%cBudgetDescriptor,iStat=iStat)  
      IF (iStat .EQ. -1) RETURN

      !Simulation time related data
      CALL pFile%GetTimeStepRelatedData(Header%NTimeSteps,Header%TimeStep)  
    
      !Areas
      CALL pFile%ReadData(cAttributesDir,'NAreas',ScalarAttrData=Header%NAreas,iStat=iStat)  ;  IF (iStat .EQ. -1) RETURN
      IF (Header%NAreas .GT. 0) THEN
          CALL AllocArray(Header%Areas,Header%NAreas,ThisProcedure,iStat)                     ;  IF (iStat .EQ. -1) RETURN
          !Backward compatibility: Check if the Areas is written as a dadaset or attribute
          IF (pFile%DoesHDFObjectExist(cAttributesDir//'/Areas')) THEN
             !Areas exist as a dataset
             CALL pFile%ReadData(cAttributesDir//'/Areas',Header%Areas,iStat=iStat)  
          ELSE
             !Areas exist as an attribute 
             CALL pFile%ReadData(cAttributesDir,'Areas',ArrayAttrData=Header%Areas,iStat=iStat)  ;  IF (iStat .EQ. -1) RETURN
          END IF
      END IF
    
      !Data for ASCII output
      CALL pFile%ReadData(cAttributesDir,'ASCIIOutput%TitleLen',ScalarAttrData=Header%ASCIIOutput%TitleLen,iStat=iStat)  ;  IF (iStat .EQ. -1) RETURN
      CALL pFile%ReadData(cAttributesDir,'ASCIIOutput%NTitles',ScalarAttrData=Header%ASCIIOutput%NTitles,iStat=iStat)  ;  IF (iStat .EQ. -1) RETURN   ;  NTitles = Header%ASCIIOutput%NTitles
      ALLOCATE (Header%ASCIIOutput%cTitles(NTitles)        , &
                Header%ASCIIOutput%lTitlePersist(NTitles)  )
      CALL pFile%ReadData(cAttributesDir,'ASCIIOutput%cTitles',ArrayAttrData=Header%ASCIIOutput%cTitles,iStat=iStat)              ;  IF (iStat .EQ. -1) RETURN   
      CALL pFile%ReadData(cAttributesDir,'ASCIIOutput%lTitlePersist',ArrayAttrData=Header%ASCIIOutput%lTitlePersist,iStat=iStat)  ;  IF (iStat .EQ. -1) RETURN
      CALL pFile%ReadData(cAttributesDir,'ASCIIOutput%cFormatSpec',ScalarAttrData=Header%ASCIIOutput%cFormatSpec,iStat=iStat)     ;  IF (iStat .EQ. -1) RETURN
      CALL pFile%ReadData(cAttributesDir,'ASCIIOutput%NColumnHeaderLines',ScalarAttrData=NColumnHeaderLines,iStat=iStat)  ;  IF (iStat .EQ. -1) RETURN  ;  Header%ASCIIOutput%NColumnHeaderLines = NColumnHeaderLines
      
      !Location names
      CALL pFile%ReadData(cAttributesDir,'nLocations',ScalarAttrData=Header%NLocations,iStat=iStat)  ;  IF (iStat .EQ. -1) RETURN  ;  ALLOCATE (Header%cLocationNames(Header%NLocations))
      IF (lAttributesDirExist) THEN
          CALL pFile%ReadData(cAttributesDir//'/cLocationNames',Header%cLocationNames,iStat=iStat)  
          IF (iStat .EQ. -1) RETURN
      ELSE
          DO indx=1,Header%NLocations
              CALL pFile%ReadData(cAttributesDir,'cLocationName_'//TRIM(IntToText(indx)),ScalarAttrData=Header%cLocationNames(indx),iStat=iStat)  
              IF (iStat .EQ. -1) RETURN
          END DO
      END IF
      
      !Location data
      CALL pFile%ReadData(cAttributesDir,'NLocationData',ScalarAttrData=NLocs,iStat=iStat)  ;  IF (iStat .EQ. -1) RETURN  ;  ALLOCATE (Header%Locations(NLocs))
      DO indx=1,NLocs
        ASSOCIATE (pLocation => Header%Locations(indx))
        
          cLocation = 'LocationData'//TRIM(IntTotext(indx))//'%'
          CALL pFile%ReadData(cAttributesDir,TRIM(cLocation)//'NDataColumns',ScalarAttrData=NDataColumns,iStat=iStat)  ;  IF (iStat .EQ. -1) RETURN  ;  pLocation%NDataColumns = NDataColumns
          ALLOCATE (pLocation%cFullColumnHeaders(NDataColumns+1)     , &
                    pLocation%iDataColumnTypes(NDataColumns)         , &
                    pLocation%iColWidth(NDataColumns+1)              )
          CALL pFile%ReadData(cAttributesDir,TRIM(cLocation)//'cFullColumnHeaders',ArrayAttrData=pLocation%cFullColumnHeaders,iStat=iStat)  ;  IF (iStat .EQ. -1) RETURN
          CALL pFile%ReadData(cAttributesDir,TRIM(cLocation)//'iDataColumnTypes',ArrayAttrData=pLocation%iDataColumnTypes,iStat=iStat)      ;  IF (iStat .EQ. -1) RETURN
          CALL pFile%ReadData(cAttributesDir,TRIM(cLocation)//'iColWidth',ArrayAttrData=pLocation%iColWidth,iStat=iStat)                    ;  IF (iStat .EQ. -1) RETURN
          ALLOCATE (pLocation%cColumnHeaders(NDataColumns+1,NColumnHeaderLines) , &
                    pLocation%cColumnHeadersFormatSpec(NColumnHeaderLines)      )
          DO indx1=1,NColumnHeaderLines
              CALL pFile%ReadData(cAttributesDir,TRIM(cLocation)//'L'//TRIM(IntToText(indx1))//'_cColumnHeaders',ArrayAttrData=pLocation%cColumnHeaders(:,indx1),iStat=iStat)  
              IF (iStat .EQ. -1) RETURN
          END DO 
          CALL pFile%ReadData(cAttributesDir,TRIM(cLocation)//'cColumnHeadersFormatSpec',ArrayAttrData=pLocation%cColumnHeadersFormatSpec,iStat=iStat)  
          IF (iStat .EQ. -1) RETURN              
          
        END ASSOCIATE         
      END DO

      !DSS output data
      CALL pFile%ReadData(cAttributesDir,'DSSOutput%NPathNames',ScalarAttrData=iSize,iStat=iStat)  ;  IF (iStat .EQ. -1) RETURN  ;  ALLOCATE (Header%DSSOutput%cPathNames(iSize))
      IF (lAttributesDirExist) THEN
          CALL pFile%ReadData(cAttributesDir//'/DSSOutput%cPathNames',Header%DSSOutput%cPathNames,iStat=iStat)  
          IF (iStat .EQ. -1) RETURN
      ELSE
          DO indx=1,iSize
              CALL pFile%ReadData(cAttributesDir,'DSSOutput%cPathNames'//TRIM(IntToText(indx)),ScalarAttrData=Header%DSSOutput%cPathNames(indx),iStat=iStat)  
              IF (iStat .EQ. -1) RETURN
          END DO
      END IF
      CALL pFile%ReadData(cAttributesDir,'DSSOutput%NDataTypes',ScalarAttrData=iSize,iStat=iStat)  ;  IF (iStat .EQ. -1) RETURN  ;  ALLOCATE (Header%DSSOutput%iDataTypes(iSize))
      CALL pFile%ReadData(cAttributesDir,'DSSOutput%iDataTypes',ArrayAttrData=Header%DSSOutput%iDataTypes,iStat=iStat)  
      
    END ASSOCIATE  
    
  END SUBROUTINE ReadHeader_FromHDFFile
  
  
  ! -------------------------------------------------------------
  ! --- READ HEADER DATA FROM BIN FILE
  ! -------------------------------------------------------------
  SUBROUTINE ReadHeader_FromBinFile(InputFile,Header,iStat)
    CLASS(GenericFileType)             :: InputFile
    TYPE(BudgetHeaderType),INTENT(OUT) :: Header
    INTEGER,INTENT(OUT)                :: iStat
    
    !Local variables
    INTEGER :: indx,NLocs,indx1,iSize,NDataColumns,NColumnHeaderLines,indx2,NTitles
    
    !Initialize
    iStat = 0
    
    ASSOCIATE (pFile => InputFile)
    
      !First rewind file
      CALL pFile%RewindFile()  
    
      !Budget descriptor
      CALL pFile%ReadData(Header%cBudgetDescriptor,iStat)  
      IF (iStat .EQ. -1) RETURN

      !Simulation time related data
      CALL pFile%ReadData(Header%NTimeSteps,iStat)                   ;  IF (iStat .EQ. -1) RETURN
      CALL pFile%ReadData(Header%TimeStep%TrackTime,iStat)           ;  IF (iStat .EQ. -1) RETURN
      CALL pFile%ReadData(Header%TimeStep%DeltaT,iStat)              ;  IF (iStat .EQ. -1) RETURN
      CALL pFile%ReadData(Header%TimeStep%DeltaT_InMinutes,iStat)    ;  IF (iStat .EQ. -1) RETURN
      CALL pFile%ReadData(Header%TimeStep%Unit,iStat)                ;  IF (iStat .EQ. -1) RETURN  
      CALL pFile%ReadData(Header%TimeStep%CurrentDateAndTime,iStat)  ;  IF (iStat .EQ. -1) RETURN
      CALL pFile%ReadData(Header%TimeStep%CurrentTime,iStat)         ;  IF (iStat .EQ. -1) RETURN
    
      !Areas
      CALL pFile%ReadData(Header%NAreas,iStat)  ;  IF (iStat .EQ. -1) RETURN  ;  ALLOCATE(Header%Areas(Header%NAreas))  
      CALL pFile%ReadData(Header%Areas,iStat)   ;  IF (iStat .EQ. -1) RETURN
    
      !Data for ASCII output
      CALL pFile%ReadData(Header%ASCIIOutput%TitleLen,iStat)  ;  IF (iStat .EQ. -1) RETURN
      CALL pFile%ReadData(Header%ASCIIOutput%NTitles,iStat)  ;  IF (iStat .EQ. -1) RETURN   ;  NTitles = Header%ASCIIOutput%NTitles
          ALLOCATE (Header%ASCIIOutput%cTitles(NTitles)        , &
                    Header%ASCIIOutput%lTitlePersist(NTitles)  )
      DO indx=1,NTitles   
        CALL pFile%ReadData(Header%ASCIIOutput%cTitles(indx),iStat)  
        IF (iStat .EQ. -1) RETURN
      END DO
      CALL pFile%ReadData(Header%ASCIIOutput%lTitlePersist,iStat)  ;  IF (iStat .EQ. -1) RETURN
      CALL pFile%ReadData(Header%ASCIIOutput%cFormatSpec,iStat)  ;  IF (iStat .EQ. -1) RETURN
      CALL pFile%ReadData(NColumnHeaderLines,iStat)  ;  IF (iStat .EQ. -1) RETURN  ;  Header%ASCIIOutput%NColumnHeaderLines = NColumnHeaderLines
      
      !Location names
      CALL pFile%ReadData(Header%NLocations,iStat)  ;  IF (iStat .EQ. -1) RETURN  ;  ALLOCATE (Header%cLocationNames(Header%NLocations))
      DO indx=1,Header%NLocations
        CALL pFile%ReadData(Header%cLocationNames(indx),iStat)  
        IF (iStat .EQ. -1) RETURN
      END DO
    
      !Location data
      CALL pFile%ReadData(NLocs,iStat)  ;  IF (iStat .EQ. -1) RETURN  ;  ALLOCATE (Header%Locations(NLocs))
      DO indx=1,NLocs
        ASSOCIATE (pLocation => Header%Locations(indx))
        
          CALL pFile%ReadData(NDataColumns,iStat)  ;  IF (iStat .EQ. -1) RETURN  ;  pLocation%NDataColumns = NDataColumns
          CALL pFile%ReadData(pLocation%iStorUnitsInFile,iStat)  ;  IF (iStat .EQ. -1) RETURN
          ALLOCATE (pLocation%cFullColumnHeaders(NDataColumns+1)     , &
                    pLocation%iDataColumnTypes(NDataColumns)         , &
                    pLocation%iColWidth(NDataColumns+1)              )
          DO indx1=1,NDataColumns+1
            CALL pFile%ReadData(pLocation%cFullColumnHeaders(indx1),iStat)  
            IF (iStat .EQ. -1) RETURN
          END DO
          CALL pFile%ReadData(pLocation%iDataColumnTypes,iStat)  ;  IF (iStat .EQ. -1) RETURN
          CALL pFile%ReadData(pLocation%iColWidth,iStat)  ;  IF (iStat .EQ. -1) RETURN
          ALLOCATE (pLocation%cColumnHeaders(NDataColumns+1,NColumnHeaderLines) , &
                    pLocation%cColumnHeadersFormatSpec(NColumnHeaderLines)      )
          DO indx1=1,NColumnHeaderLines
            DO indx2=1,NDataColumns+1
              CALL pFile%ReadData(pLocation%cColumnHeaders(indx2,indx1),iStat)  
              IF (iStat .EQ. -1) RETURN 
            END DO
          END DO 
          DO indx1=1,NColumnHeaderLines
            CALL pFile%ReadData(pLocation%cColumnHeadersFormatSpec(indx1),iStat)  
            IF (iStat .EQ. -1) RETURN
          END DO  
          
        END ASSOCIATE         
      END DO

      !DSS output data
      CALL pFile%ReadData(iSize,iStat)  ;  IF (iStat .EQ. -1) RETURN  ;  ALLOCATE (Header%DSSOutput%cPathNames(iSize))
      DO indx=1,iSize
        CALL pFile%ReadData(Header%DSSOutput%cPathNames(indx),iStat)  
        IF (iStat .EQ. -1) RETURN
      END DO
      CALL pFile%ReadData(iSize,iStat)  ;  IF (iStat .EQ. -1) RETURN  ;  ALLOCATE (Header%DSSOutput%iDataTypes(iSize))
      CALL pFile%ReadData(Header%DSSOutput%iDataTypes,iStat)  ;  IF (iStat .EQ. -1) RETURN
      
      !Position in file
      CALL pFile%GetPositionInFile(Header%iPosition)
      
    END ASSOCIATE  

  END SUBROUTINE ReadHeader_FromBinFile
  
END MODULE