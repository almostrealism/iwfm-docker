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
MODULE Class_Budget
  USE MessageLogger           , ONLY: SetLastMessage           , &
                                      MessageArray             , &
                                      f_iFatal                   
  USE GeneralUtilities        , ONLY: LocateInList             , &
                                      UpperCase                , &
                                      FirstLocation            , &
                                      ArrangeText              , &
                                      FindSubstringInString    , &
                                      GetArrayData             , &
                                      ShellSort
  USE TimeSeriesUtilities     , ONLY: TimeStepType             , &
                                      f_iTimeStampLength       , &
                                      NPeriods                 , &
                                      IncrementTimeStamp       , &
                                      TimeStampToJulian        , &
                                      CTimeStep_To_RTimeStep   , &
                                      SetCacheLimit            , &
                                      OPERATOR(.TSGT.)         , &
                                      OPERATOR(.TSLT.)
  USE IOInterface             , ONLY: GenericFileType          , &
                                      f_iTXT                   , &
                                      f_iDSS                   , &
                                      f_iHDF                   , &
                                      f_iUNKNOWN
  USE Package_Misc            , ONLY: f_iDataUnitType_Length   , &
                                      f_iDataUnitType_Area     , &
                                      f_iDataUnitType_Volume
  USE Budget_Parameters       , ONLY: f_iBudgetDescriptorLen   , &
                                      f_iMaxLocationNameLen    , &
                                      f_iColumnHeaderLen       , &
                                      f_iDSSDataUnitLen        , &
                                      f_cLocationNameMarker    , &
                                      f_cAreaMarker            , &
                                      f_cLengthUnitMarker      , &
                                      f_cAreaUnitMarker        , &
                                      f_cVolumeUnitMarker      , &
                                      f_cDataTypes             , & 
                                      f_iVR_lwu_PotCUAW        , &
                                      f_iVR_lwu_AgSupplyReq    , &
                                      f_iVR_lwu_AgShort        , &
                                      f_iVR_lwu_AgPump         , &
                                      f_iVR_lwu_AgDiv          , &
                                      f_iVR_lwu_AgOthIn        , &
                                      f_iVR                    , &
                                      f_iVLB                   , &
                                      f_iVLE                   , &
                                      f_iAR                    , &
                                      f_iLT                  
  USE Class_BudgetInputFile   , ONLY: BudgetInputFileType      , &
                                      BudgetHeaderType         , &
                                      LocationDataType
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
  PUBLIC :: BudgetType          , &
            PrintIntervalType   , &
            ModifiedAgSupplyReq 

  
  ! -------------------------------------------------------------
  ! --- PRINT INTERVAL DATA TYPE
  ! -------------------------------------------------------------
  TYPE PrintIntervalType
    CHARACTER(LEN=f_iTimeStampLength) :: PrintBeginDateAndTime = ''
    CHARACTER(LEN=f_iTimeStampLength) :: PrintEndDateAndTime   = ''
    REAL(8)                           :: PrintBeginTime        = 0.0
    REAL(8)                           :: PrintEndTime          = 0.0
  END TYPE PrintIntervalType
  
  
  ! -------------------------------------------------------------
  ! --- BUDGET DATA TYPE
  ! -------------------------------------------------------------
  TYPE BudgetType
    PRIVATE
    TYPE(BudgetInputFileType) :: InputFile
    TYPE(BudgetHeaderType)    :: Header
  CONTAINS
    PROCEDURE,PASS   :: Create
    PROCEDURE,PASS   :: Open
    PROCEDURE,PASS   :: Kill
    PROCEDURE,PASS   :: GetFileName
    PROCEDURE,PASS   :: GetDescriptor                  
    PROCEDURE,PASS   :: GetNTimeSteps 
    PROCEDURE,PASS   :: GetTimeSpecs
    PROCEDURE,PASS   :: GetNLocations                 
    PROCEDURE,PASS   :: GetLocationNames               
    PROCEDURE,PASS   :: GetNPersistentTitles           
    PROCEDURE,PASS   :: GetTitleLen                    
    PROCEDURE,PASS   :: GetPersistentTitles            
    PROCEDURE,PASS   :: GetNDataColumns               
    PROCEDURE,PASS   :: GetFullColumnHeaders           
    PROCEDURE,PASS   :: ReadData_SelectedColumns
    PROCEDURE,PASS   :: ReadData_OneColumn_FromHDFFile
    PROCEDURE,PASS   :: PrintResults  
    PROCEDURE,PASS   :: WriteMatrixData
    PROCEDURE,PASS   :: IsDefined
    PROCEDURE,PASS   :: ModifyASCIITitles              
    PROCEDURE,NOPASS :: ModifyFullColumnHeaders
    GENERIC          :: New                     => Create                             , &
                                                   Open
    GENERIC          :: ReadData                => ReadData_SelectedColumns           , &
                                                   ReadData_OneColumn_FromHDFFile
    GENERIC          :: WriteData               => WriteMatrixData
  END TYPE BudgetType


  ! -------------------------------------------------------------
  ! --- MISC. ENTITIES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                   :: f_iLeft   = -1  , &
                                         f_iCenter = 0   , &
                                         f_iRight  = 1
  INTEGER,PARAMETER                   :: ModNameLen = 14
  CHARACTER(LEN=ModNameLen),PARAMETER :: ModName    = 'Class_Budget::'
  
  
  
  
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
  ! --- CREATE A NEW BUDGET FILE
  ! -------------------------------------------------------------
  SUBROUTINE Create(Budget,cInputFileName,HeaderData,iStat) 
    CLASS(BudgetType)                 :: Budget
    CHARACTER(LEN=*),INTENT(IN)       :: cInputFileName
    TYPE(BudgetHeaderType),INTENT(IN) :: HeaderData
    INTEGER,INTENT(OUT)               :: iStat
    
    !Instantiate input file
    CALL Budget%InputFile%NewFile(cInputFileName,HeaderData,iStat)
    
  END SUBROUTINE Create

  
  ! -------------------------------------------------------------
  ! --- OPEN AN EXISTING BUDGET FILE
  ! -------------------------------------------------------------
  SUBROUTINE Open(Budget,cInputFileName,iStat) 
    CLASS(BudgetType)           :: Budget
    CHARACTER(LEN=*),INTENT(IN) :: cInputFileName
    INTEGER,INTENT(OUT)         :: iStat
    
    !Initialize
    iStat = 0
    
    !Instantiate input file
    CALL Budget%InputFile%NewFile(cInputFileName,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Get the output related data
    CALL Budget%InputFile%ReadHeader(Budget%Header,iStat)
    
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
  ! --- KILL BUDGET
  ! -------------------------------------------------------------
  SUBROUTINE Kill(Budget)
    CLASS(BudgetType) :: Budget
    
    CALL Budget%InputFile%Close()
    CALL Budget%Header%Kill()
  
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
  ! --- GET RAW BUDGET FILE NAME
  ! -------------------------------------------------------------
  SUBROUTINE GetFileName(Budget,cFileName)
    CLASS(BudgetType),INTENT(IN)         :: Budget
    CHARACTER(:),ALLOCATABLE,INTENT(OUT) :: cFileName   
    
    !Local variables
    INTEGER :: ErrorCode
    
    DEALLOCATE (cFileName , STAT=ErrorCode)
    CALL Budget%InputFile%GetName(cFileName)
        
  END SUBROUTINE GetFileName
  
  
  ! -------------------------------------------------------------
  ! --- GET NUMBER OF TIME STEPS
  ! -------------------------------------------------------------
  FUNCTION GetNTimeSteps(Budget) RESULT(N)
    CLASS(BudgetType),INTENT(IN) :: Budget
    INTEGER                      :: N
    
    N = Budget%Header%NTimeSteps

  END FUNCTION GetNTimeSteps
  
  
  ! -------------------------------------------------------------
  ! --- GET BUDGET DATA TIME RELATED SPECS
  ! -------------------------------------------------------------
  FUNCTION GetTimeSpecs(Budget) RESULT(TimeStep)
    CLASS(BudgetType),INTENT(IN) :: Budget
    TYPE(TimeStepType)           :: TimeStep
    
    TimeStep = Budget%Header%TimeStep

  END FUNCTION GetTimeSpecs
  
  
  ! -------------------------------------------------------------
  ! --- GET NUMBER OF PERSISTENT TITLES
  ! -------------------------------------------------------------
  FUNCTION GetNPersistentTitles(Budget) RESULT(NTitles)
    CLASS(BudgetType),INTENT(IN) :: Budget
    INTEGER                      :: NTitles
    
    NTitles = COUNT(Budget%Header%ASCIIOutput%lTitlePersist)
    
  END FUNCTION GetNPersistentTitles
  
  
  ! -------------------------------------------------------------
  ! --- GET TITLE LENGTH
  ! -------------------------------------------------------------
  FUNCTION GetTitleLen(Budget) RESULT(TitleLen)
    CLASS(BudgetType),INTENT(IN) :: Budget
    INTEGER                      :: TitleLen
    
    TitleLen = Budget%Header%ASCIIOutput%TitleLen
    
  END FUNCTION GetTitleLen
  
  
  ! -------------------------------------------------------------
  ! --- GET PERSISTENT TITLES
  ! -------------------------------------------------------------
  FUNCTION GetPersistentTitles(Budget,NTitles) RESULT(cTitles)
    CLASS(BudgetType),INTENT(IN)                      :: Budget
    INTEGER,INTENT(IN)                                :: NTitles
    CHARACTER(LEN=Budget%Header%ASCIIOutput%TitleLen) :: cTitles(NTitles)
    
    !Local variables
    INTEGER :: indx,iCount
    
    !Initialize
    iCount = 0
    
    !Compile titles
    ASSOCIATE (pASCIIOutput => Budget%Header%ASCIIOutput)
        DO indx=1,pASCIIOutput%NTitles
            IF (pASCIIOutput%lTitlePersist(indx)) THEN
                iCount          = iCount + 1   
                cTitles(iCount) = pASCIIOutput%cTitles(indx)
            END IF
        END DO
    END ASSOCIATE
    
  END FUNCTION GetPersistentTitles
  
  
  ! -------------------------------------------------------------
  ! --- GET NUMBER OF LOCATIONS
  ! -------------------------------------------------------------
  FUNCTION GetNLocations(Budget) RESULT(NLocations)
    CLASS(BudgetType),INTENT(IN) :: Budget
    INTEGER                      :: NLocations
    
    NLocations = Budget%Header%NLocations
    
  END FUNCTION GetNLocations
  
  
  ! -------------------------------------------------------------
  ! --- GET DESCRIPTOR
  ! -------------------------------------------------------------
  FUNCTION GetDescriptor(Budget) RESULT(cDescriptor)
    CLASS(BudgetType),INTENT(IN)          :: Budget
    CHARACTER(LEN=f_iBudgetDescriptorLen) :: cDescriptor
    
    cDescriptor = Budget%Header%cBudgetDescriptor
    
  END FUNCTION GetDescriptor
  
  
  ! -------------------------------------------------------------
  ! --- GET LOCATION NAMES
  ! -------------------------------------------------------------
  FUNCTION GetLocationNames(Budget,NLoc) RESULT(cLocationNames)
    CLASS(BudgetType),INTENT(IN)         :: Budget
    INTEGER,INTENT(IN)                   :: NLoc
    CHARACTER(LEN=f_iMaxLocationNameLen) :: cLocationNames(NLoc)
    
    cLocationNames = Budget%Header%cLocationNames
    
  END FUNCTION GetLocationNames
  
  
  ! -------------------------------------------------------------
  ! --- GET NUMBER OF DATA COLUMNS FOR A LOCATION (INLUDING THE TIME COLUMN)
  ! -------------------------------------------------------------
  SUBROUTINE GetNDataColumns(Budget,iLoc,NColumns,iStat)
    CLASS(BudgetType),INTENT(IN) :: Budget
    INTEGER,INTENT(IN)           :: iLoc
    INTEGER ,INTENT(OUT)         :: NColumns,iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+22) :: ThisProcedure = ModName // 'Budget_GetNDataColumns'
    INTEGER                      :: iDummyArray(Budget%Header%NLocations)
    
    CALL GetArrayData(Budget%Header%Locations%NDataColumns,iDummyArray,ThisProcedure,iStat)
    NColumns = iDummyArray(iLoc) + 1  !+1 is for the Time column which NDataColumns excludes
    
  END SUBROUTINE GetNDataColumns
  
  
  ! -------------------------------------------------------------
  ! --- GET BUDGET COLUMN FULL HEADERS
  ! -------------------------------------------------------------
  FUNCTION GetFullColumnHeaders(Budget,iLoc,NDataColumns) RESULT(cHeaders)
    CLASS(BudgetType),INTENT(IN)      :: Budget
    INTEGER,INTENT(IN)                :: iLoc,NDataColumns
    CHARACTER(LEN=f_iColumnHeaderLen) :: cHeaders(NDataColumns)
    
    IF (SIZE(Budget%Header%Locations) .EQ. 1) THEN
        cHeaders = Budget%Header%Locations(1)%cFullColumnHeaders
    ELSE   
        cHeaders = Budget%Header%Locations(iLoc)%cFullColumnHeaders
    END IF
    
  END FUNCTION GetFullColumnHeaders



  
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
  ! --- WRITE BUDGET MATRIX DATA TO BUDGET RAW OUTPUT FILE
  ! -------------------------------------------------------------
  SUBROUTINE WriteMatrixData(Budget,Data)
    CLASS(BudgetType)   :: Budget
    CLASS(*),INTENT(IN) :: Data(:,:)
    
    CALL Budget%InputFile%WriteData(Data)
    
  END SUBROUTINE WriteMatrixData
    
    
  ! -------------------------------------------------------------
  ! --- READ DATA FROM HDF OR BIN FILE AND PRINT OUT RESULTS TO OUTPUT FILE
  ! -------------------------------------------------------------
  SUBROUTINE PrintResults(Budget,cOutputFileName,Cache,FactLength,FactArea,FactVolume,LengthUnit,AreaUnit,VolumeUnit,PrintInterval,cPrintTimeStep,iLocations,iStat)
    CLASS(BudgetType),INTENT(IN)       :: Budget
    INTEGER,INTENT(IN)                 :: Cache
    CHARACTER(LEN=*),INTENT(IN)        :: cOutputFileName,LengthUnit,AreaUnit,VolumeUnit,cPrintTimeStep
    REAL(8),INTENT(IN)                 :: FactLength,FactArea,FactVolume
    TYPE(PrintIntervalType),INTENT(IN) :: PrintInterval
    INTEGER,INTENT(IN)                 :: iLocations(:)
    INTEGER,INTENT(OUT)                :: iStat
    
    IF (Budget%InputFile%iGetFileType() .EQ. f_iHDF) THEN
        CALL PrintResults_ReadFromHDFFile(Budget,cOutputFileName,Cache,FactLength,FactArea,FactVolume,LengthUnit,AreaUnit,VolumeUnit,PrintInterval,cPrintTimeStep,iLocations,iStat)
    ELSE
        CALL PrintResults_ReadFromBinFile(Budget,cOutputFileName,Cache,FactLength,FactArea,FactVolume,LengthUnit,AreaUnit,VolumeUnit,PrintInterval,cPrintTimeStep,iLocations,iStat)
    END IF
    
  END SUBROUTINE PrintResults
  
  
  ! -------------------------------------------------------------
  ! --- READ DATA FROM HDF FILE AND PRINT OUT RESULTS TO OUTPUT FILE
  ! -------------------------------------------------------------
  SUBROUTINE PrintResults_ReadFromHDFFile(Budget,cOutputFileName,Cache,FactLength,FactArea,FactVolume,LengthUnit,AreaUnit,VolumeUnit,PrintInterval,cPrintTimeStep,iLocations,iStat)
    TYPE(BudgetType),TARGET,INTENT(IN) :: Budget
    INTEGER,INTENT(IN)                 :: Cache
    CHARACTER(LEN=*),INTENT(IN)        :: cOutputFileName,LengthUnit,AreaUnit,VolumeUnit,cPrintTimeStep
    REAL(8),INTENT(IN)                 :: FactLength,FactArea,FactVolume
    TYPE(PrintIntervalType),INTENT(IN) :: PrintInterval
    INTEGER,INTENT(IN)                 :: iLocations(:)
    INTEGER,INTENT(OUT)                :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+28),PARAMETER   :: ThisProcedure = ModName // 'PrintResults_ReadFromHDFFile'
    INTEGER                                  :: iLocationsLocal(SIZE(iLocations)),iLoc,indxLocation,NTimeSteps,indxTime, &
                                                iCol,iFileType,indxS,indxL,NIntervals,DeltaT_InMinutes,ErrorCode,        &
                                                iDataTypes(SIZE(Budget%Header%DSSOutput%cPathNames)),iTimeCounter,       &
                                                NDataColumns(Budget%Header%NLocations),iPrintDeltaT_InMinutes,           &
                                                NPrintIntervals,indxCol,iAgShortCol,iAgSupReqCol,iAgPumpCol,iAgDivCol,   &
                                                iAgOtherInflowCol,nReadTimes
    REAL(8)                                  :: CurrentTime,DeltaT,rPrintDeltaT,rAgShortPrevious,rAgSupReq_Modified
    REAL(8),ALLOCATABLE                      :: rTempValues(:,:),rValues(:)
    CHARACTER(LEN=f_iTimeStampLength)        :: CurrentDateAndTime,PrintTimeStamp,cTime
    CHARACTER(LEN=LEN(cPrintTimeStep))       :: cPrintTimeStepLocal
    CHARACTER(LEN=80),TARGET                 :: cPathNames(SIZE(Budget%Header%DSSOutput%cPathNames))
    TYPE(LocationDataType),POINTER           :: pLocationData
    TYPE(GenericFileType)                    :: OutputFile
    LOGICAL                                  :: TrackTime,lFinalPrint,lFirstAfterPrint,lTimeToPrint,lLWUBudget
    TYPE(PrintIntervalType)                  :: PrintIntervalLocal
    CHARACTER(LEN=f_iDSSDataUnitLen),TARGET  :: cDataUnits(MAXVAL(Budget%Header%Locations%NDataColumns))
    CHARACTER(LEN=f_iDSSDataUnitLen),POINTER :: pcDataUnits(:)
    CHARACTER(LEN=80),POINTER                :: pcPathNames(:)
    CHARACTER(:),ALLOCATABLE                 :: cFileName
    
    !Initialize
    iStat              = 0
    TrackTime          = Budget%Header%TimeStep%TrackTime
    DeltaT_InMinutes   = Budget%Header%TimeStep%DeltaT_InMinutes
    DeltaT             = Budget%Header%TimeStep%DeltaT
    PrintIntervalLocal = PrintInterval
    IF (cPrintTimeStep .EQ. '') THEN
        cPrintTimeStepLocal = Budget%Header%TimeStep%Unit
    ELSE
        cPrintTimeStepLocal = cPrintTimeStep
    END IF
    lLWUBudget = .FALSE.
    
    !For time-tracking simulations, check that the print interval is consistent with the data time step
    IF (TrackTime) THEN
        CALL CTimeStep_To_RTimeStep(cPrintTimeStepLocal,rPrintDeltaT,iPrintDeltaT_InMinutes,ErrorCode)
        IF (ErrorCode .NE. 0) THEN
            CALL SetLastMessage(TRIM(cPrintTimeStepLocal)//' is not a recognized time interval for printing of '//TRIM(Budget%Header%cBudgetDescriptor)//'!',f_iFatal,ThisProcedure) 
            iStat = -1
            RETURN
        END IF
        IF (iPrintDeltaT_InMinutes .LT. DeltaT_InMinutes) THEN
            MessageArray(1) = 'Print interval ('//TRIM(cPrintTimeStepLocal)//') for '//TRIM(Budget%Header%cBudgetDescriptor)//' cannot be less than the'
            MessageArray(2) = 'simulation time step ('//TRIM(Budget%Header%TimeStep%Unit)//')!'
            CALL SetLastMessage(MessageArray(1:2),f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
    END IF
    
    !Adjust print begin and end times
    CALL AdjustOutputTimes(Budget%Header,iPrintDeltaT_InMinutes,PrintIntervalLocal,iStat)  ;  IF (iStat .EQ. -1) RETURN
    IF (TrackTime) THEN
        NIntervals      = NPeriods(DeltaT_InMinutes,PrintIntervalLocal%PrintBeginDateAndTime,PrintIntervalLocal%PrintEndDateAndTime) + 1
        PrintTimeStamp  = IncrementTimeStamp(PrintIntervalLocal%PrintBeginDateAndTime,DeltaT_InMinutes,-1)
        NPrintIntervals = NPeriods(iPrintDeltaT_InMinutes,PrintTimeStamp,PrintIntervalLocal%PrintEndDateAndTime)
    ELSE
        NIntervals      = NPeriods(DeltaT,PrintIntervalLocal%PrintBeginTime,PrintIntervalLocal%PrintEndTime) + 1
        NPrintIntervals = NIntervals
    END IF
      
    !Open output file
    CALL OutputFile%New(FileName=cOutputFileName,InputFile=.FALSE.,IsTSFile=.TRUE.,Descriptor=Budget%Header%cBudgetDescriptor,iStat=iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Get the file type
    iFileType = OutputFile%iGetFileType()
    
    !Make sure that output file type is recognized
    IF (iFileType.NE.f_iTXT  .AND. iFileType.NE.f_iDSS) THEN
        MessageArray(1) = 'Currently budget tables can be printed only to text or DSS files!'
        MessageArray(2) = '('//TRIM(Budget%Header%cBudgetDescriptor)//')'
        CALL SetLastMessage(MessageArray(1:2),f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF  
    
    !Set the output file specific data
    CALL SetCacheLimit(Cache)
    
    !ASCII output
    IF (iFileType .EQ. f_iTXT) THEN
        CALL OutputFile%SetCacheSize(MAXVAL(Budget%Header%Locations%NDataColumns),1,iStat)  ;  IF(iStat .EQ. -1) RETURN
        CALL OutputFile%SetPrintFormatSpec(Budget%Header%ASCIIOutput%cFormatSpec,iStat)     ;  IF(iStat .EQ. -1) RETURN
    !DSS output
    ELSE IF (iFileType .EQ. f_iDSS) THEN
        CALL GetArrayData(Budget%Header%DSSOutput%iDataTypes,iDataTypes,ThisProcedure,iStat)  ;  IF (iStat .EQ. -1) RETURN
        CALL GetArrayData(Budget%Header%Locations%NDataColumns,NDataColumns,ThisProcedure,iStat)  ;  IF (iStat .EQ. -1) RETURN
    END IF

    !Sort the location indices
    iLocationsLocal = iLocations
    CALL ShellSort(iLocationsLocal)
    
    !Loop over locations
    NTimeSteps = Budget%Header%NTimeSteps
    indxS      = 0
    DO indxLocation=1,SIZE(iLocationsLocal)
      iLoc = iLocationsLocal(indxLocation)
      
      !Get a pointer to location data
      IF (SIZE(Budget%Header%Locations) .EQ. 1) THEN
          pLocationData => Budget%Header%Locations(1)
      ELSE
          pLocationData => Budget%Header%Locations(iLoc)
      END IF
      
      !Number of columns for the location
      iCol = pLocationData%NDataColumns
      
      !If this is Land & Water Use Budget, there will be special accumulation of some columns
      !Check if this Land & Water Use Budget, and identify special data columns
      IF (LocateInList(f_iVR_lwu_PotCUAW,pLocationData%iDataColumnTypes) .GT. 0) THEN
          iAgShortCol       = LocateInList(f_iVR_lwu_AgShort,pLocationData%iDataColumnTypes)
          iAgSupReqCol      = LocateInList(f_iVR_lwu_AgSupplyReq,pLocationData%iDataColumnTypes)
          iAgPumpCol        = LocateInList(f_iVR_lwu_AgPump,pLocationData%iDataColumnTypes)
          iAgDivCol         = LocateInList(f_iVR_lwu_AgDiv,pLocationData%iDataColumnTypes)
          iAgOtherInflowCol = LocateInList(f_iVR_lwu_AgOthIn,pLocationData%iDataColumnTypes)
          lLWUBudget        = .TRUE.
      END IF
      
      !Allocate work arrays
      DEALLOCATE (rTempValues , rValues ,STAT=ErrorCode)
      nReadTimes = MAX(MIN(Cache/iCol , NIntervals) , 1)
      ALLOCATE (rTempValues(iCol,nReadTimes) , rValues(iCol))
      
      !Initial print time
      IF (TrackTime) THEN
          CurrentDateAndTime = PrintIntervalLocal%PrintBeginDateAndTime
      ELSE
          CurrentTime        = PrintIntervalLocal%PrintBeginTime
      END IF
      
      !If the output is a DSS file, close and open the file, and set the parameters
      IF (iFileType .EQ. f_iDSS) THEN
          !Initialize file
          CALL OutputFile%Kill()
          CALL OutputFile%New(FileName=cOutputFileName,InputFile=.FALSE.,IsTSFile=.TRUE.,Descriptor=Budget%Header%cBudgetDescriptor,iStat=iStat)
          IF (iStat .EQ. -1) RETURN
        
          !Array indices
          indxS = SUM(NDataColumns(1:iLoc-1))+1
          indxL = indxS + NDataColumns(iLoc) - 1
        
          !Pointers
          pcDataUnits => cDataUnits(1:iCol)

          !Define data units
          WHERE (pLocationData%iDataColumnTypes .EQ. f_iLT)
              pcDataUnits = LengthUnit
          ELSE WHERE (pLocationData%iDataColumnTypes .EQ. f_iAR)
              pcDataUnits = AreaUnit
          ELSE WHERE (pLocationData%iDataColumnTypes .EQ. f_iVR  .OR.  &
                      pLocationData%iDataColumnTypes .EQ. f_iVLB .OR.  &
                      pLocationData%iDataColumnTypes .EQ. f_iVLE       )
              pcDataUnits = VolumeUnit
          END WHERE
        
          !Redefine the E part of the pathnames according to print-out interval specified by user
          IF (DeltaT_InMinutes .NE. iPrintDeltaT_InMinutes) THEN
              cPathNames(indxS:indxL) =  ModifyPathnameEPart(Budget%Header%DSSOutput%cPathNames(indxS:indxL),indxL-indxS+1,cPrintTimeStep)
              pcPathnames             => cPathNames(indxS:indxL)
          ELSE
              pcPathnames             => Budget%Header%DSSOutput%cPathNames(indxS:indxL)
          END IF
        
          !Set the parameters for DSS output
          CALL OutputFile%SetParametersForDSSFile(PathNames=pcPathNames                                         , &
                                                  DataUnit=pcDataUnits                                          , &
                                                  DataType=f_cDataTypes(iDataTypes(indxS:indxL))                , &
                                                  SimulationStartTime=CurrentDateAndTime                        , &    
                                                  NTimeSteps=NPrintIntervals                                    , &    
                                                  NColumnsOfData=iCol                                           , &
                                                  NRowsOfData=1                                                 , &
                                                  iStat=iStat                                                   )
          IF(iStat .EQ. -1) RETURN
      END IF
      
      !Print titles if it is ASCII output
      IF (iFileType .EQ. f_iTXT) THEN
          CALL PrintASCIITitles(Budget%Header,iLoc,FactArea,LengthUnit,AreaUnit,VolumeUnit,OutputFile)  
      END IF
      
      !Initialize values for print-out
      rValues          = 0.0
      rAgShortPrevious = 0.0
      lFirstAfterPrint = .TRUE.
      IF (TrackTime) THEN
          PrintTimeStamp = IncrementTimeStamp(CurrentDateAndTime,DeltaT_InMinutes,-1)
          PrintTimeStamp = IncrementTimeStamp(PrintTimeStamp,iPrintDeltaT_InMinutes,1)
      END IF
      
      !Read values from input file, convert them to desired units and print out results
      iTimeCounter = 1
      DO indxTime=1,NIntervals
      
        !Is it time to print?
        IF (TrackTime) THEN
            IF (CurrentDateAndTime .EQ. PrintTimeStamp) THEN
                lTimeToPrint = .TRUE.
            ELSE
                lTimeToPrint = .FALSE.
            END IF
        ELSE
            lTimeToPrint = .TRUE.
        END IF
        
        !Is it the last print-out
        IF (indxTime .EQ. NIntervals) THEN
            lFinalPrint = .TRUE.
        ELSE
            lFinalPrint = .FALSE.
        END IF
        
        !Current time in CHARACTER format
        IF (TrackTime) THEN
            cTime = ADJUSTL(CurrentDateAndTime)
        ELSE
            WRITE (cTime,'(F16.2)') CurrentTime
        END IF
        
        !Read
        IF (iTimeCounter .EQ. 1) THEN
            CALL Budget%InputFile%ReadData(cTime,iLoc,rTempValues,ErrorCode,iStat) 
            IF (ErrorCode .NE. 0) THEN
                CALL Budget%InputFile%GetName(cFileName)
                CALL SetLastMessage('Error in reading data from file '//cFileName//'!',f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
        END IF
        
        !If Land & Water Use Budget, compute modified agricultural supply requirement for special accumulation
        IF (lLWUBudget) rAgSupReq_Modified = ModifiedAgSupplyReq(rAgShortPrevious,rTempValues(iAgSupReqCol,iTimeCounter))
        
        !Convert units
        DO indxCol=1,iCol
          SELECT CASE (pLocationData%iDataColumnTypes(indxCol))
            CASE (f_iVR, f_iVR_lwu_AgPump, f_iVR_lwu_AgDiv, f_iVR_lwu_AgOthIn)
              rValues(indxCol) = rValues(indxCol) + rTempValues(indxCol,iTimeCounter) * FactVolume
            CASE (f_iVR_lwu_PotCUAW)
                IF (NIntervals .EQ. NPrintIntervals) THEN
                    rValues(indxCol) = rTempValues(indxCol,iTimeCounter) * FactVolume
                ELSE
                    !Special accumulation method 
                    IF (rTempValues(iAgSupReqCol,iTimeCounter) .NE. 0.0)  &
                        rValues(indxCol) = rValues(indxCol) + rAgSupReq_Modified * rTempValues(indxCol,iTimeCounter) / rTempValues(iAgSupReqCol,iTimeCounter) * FactVolume
                END IF
            CASE (f_iVR_lwu_AgSupplyReq)
                IF (NIntervals .EQ. NPrintIntervals) THEN
                    rValues(indxCol) = rTempValues(indxCol,iTimeCounter) * FactVolume
                ELSE
                    !Special accumulation method 
                    rValues(indxCol) = rValues(indxCol) + rAgSupReq_Modified * FactVolume
                END IF
            CASE (f_iVR_lwu_AgShort)
                IF (NIntervals .EQ. NPrintIntervals) THEN
                    rValues(indxCol) = rTempValues(indxCol,iTimeCounter) * FactVolume
                ELSE
                    !Special accumulation method 
                    rValues(indxCol) = rValues(indxCol) + (rAgSupReq_Modified - rTempValues(iAgPumpCol,iTimeCounter) - rTempValues(iAgDivCol,iTimeCounter) - rTempValues(iAgOtherInflowCol,iTimeCounter)) * FactVolume
                END IF
            CASE (f_iVLB)
                IF (lFirstAfterPrint) rValues(indxCol) = rTempValues(indxCol,iTimeCounter) * FactVolume
            CASE (f_iVLE)
                IF (lTimeToPrint) rValues(indxCol) = rTempValues(indxCol,iTimeCounter) * FactVolume
            CASE (f_iAR)  
                rValues(indxCol) = rTempValues(indxCol,iTimeCounter) * FactArea
            CASE (f_iLT)
                rValues(indxCol) = rTempValues(indxCol,iTimeCounter) * FactLength
          END SELECT
        END DO
        
        !Print results
        IF (TrackTime) THEN
            IF (lTimeToPrint) THEN
                CALL OutputFile%WriteData(cTime,rValues,FinalPrint=lFinalPrint)
                rValues          = 0.0
                lFirstAfterPrint = .TRUE.
                PrintTimeStamp   = IncrementTimeStamp(PrintTimeStamp,iPrintDeltaT_InMinutes,1)
            ELSE
                lFirstAfterPrint = .FALSE.
            END IF
        ELSE
            CALL OutputFile%WriteData(cTime,rValues,FinalPrint=lFinalPrint)
            lFirstAfterPrint = .TRUE.
        END IF
        
        !Increment time 
        IF (TrackTime) THEN
            CurrentDateAndTime = IncrementTimeStamp(CurrentDateAndTime,DeltaT_InMinutes,1)
        ELSE
            CurrentTime        = CurrentTime + DeltaT
        END IF
        
        !If Land & Water Use Budget, store Agrilcultural Shortage from previous timestep
        IF (lLWUBudget) rAgShortPrevious = rTempValues(iAgShortCol,iTimeCounter)
        
        !Increment time counter for data read from file and stored in memory
        IF (iTimeCounter .EQ. nReadTimes) THEN
            iTimeCounter = 1
        ELSE
            iTimeCounter = iTimeCounter + 1
        END IF

      END DO
      
      !For ASCII output print an empty line
      IF (iFileType .EQ. f_iTXT) THEN
          CALL OutputFile%WriteData(' '//NEW_LINE('x'))
      END IF
        
    END DO 
    
    !Close output file
    CALL OutputFile%Kill()
    
  END SUBROUTINE PrintResults_ReadFromHDFFile
  
  
  ! -------------------------------------------------------------
  ! --- PRINT OUT RESULTS TO OUTPUT FILE
  ! -------------------------------------------------------------
  SUBROUTINE PrintResults_ReadFromBinFile(Budget,cOutputFileName,Cache,FactLength,FactArea,FactVolume,LengthUnit,AreaUnit,VolumeUnit,PrintInterval,cPrintTimeStep,iLocations,iStat)
    TYPE(BudgetType),TARGET,INTENT(IN) :: Budget
    INTEGER,INTENT(IN)                 :: Cache
    CHARACTER(LEN=*),INTENT(IN)        :: cOutputFileName,LengthUnit,AreaUnit,VolumeUnit,cPrintTimeStep
    REAL(8),INTENT(IN)                 :: FactLength,FactArea,FactVolume
    TYPE(PrintIntervalType),INTENT(IN) :: PrintInterval
    INTEGER,OPTIONAL,INTENT(IN)        :: iLocations(:)
    INTEGER,INTENT(OUT)                :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+28),PARAMETER   :: ThisProcedure = ModName // 'PrintResults_ReadFromBinFile'
    INTEGER                                  :: iLocationsLocal(SIZE(iLocations)),iLoc,indxLocation,NTimeSteps,indxTime, &
                                                iCol,iFileType,indxS,indxL,NIntervals,DeltaT_InMinutes,ErrorCode,        &
                                                iDataTypes(SIZE(Budget%Header%DSSOutput%cPathNames)),iTotalStorUnits,    &
                                                NDataColumns(Budget%Header%NLocations),iPrintDeltaT_InMinutes,           &
                                                NPrintIntervals,indxCol,iAgShortCol,iAgSupReqCol,iAgPumpCol,iAgDivCol,   &
                                                iAgOtherInflowCol
    INTEGER(KIND=8)                          :: indxPos,indxPosRelative
    REAL(8)                                  :: CurrentTime,DeltaT,rPrintDeltaT,rAgShortPrevious,rAgSupReq_Modified
    REAL(8),ALLOCATABLE                      :: rTempValues(:),rValues(:)
    CHARACTER(LEN=f_iTimeStampLength)        :: CurrentDateAndTime,OutputTime,PrintTimeStamp
    CHARACTER(LEN=LEN(cPrintTimeStep))       :: cPrintTimeStepLocal
    CHARACTER(LEN=80),TARGET                 :: cPathNames(SIZE(Budget%Header%DSSOutput%cPathNames))
    TYPE(LocationDataType),POINTER           :: pLocationData
    TYPE(GenericFileType)                    :: OutputFile
    LOGICAL                                  :: TrackTime,lFinalPrint,lFirstAfterPrint,lTimeToPrint,lLWUBudget
    TYPE(PrintIntervalType)                  :: PrintIntervalLocal
    CHARACTER(LEN=f_iDSSDataUnitLen),TARGET  :: cDataUnits(MAXVAL(Budget%Header%Locations%NDataColumns))
    CHARACTER(LEN=f_iDSSDataUnitLen),POINTER :: pcDataUnits(:)
    CHARACTER(LEN=80),POINTER                :: pcPathNames(:)
    
    !Initialize
    iStat              = 0
    TrackTime          = Budget%Header%TimeStep%TrackTime
    DeltaT_InMinutes   = Budget%Header%TimeStep%DeltaT_InMinutes
    DeltaT             = Budget%Header%TimeStep%DeltaT
    PrintIntervalLocal = PrintInterval
    IF (cPrintTimeStep .EQ. '') THEN
      cPrintTimeStepLocal = Budget%Header%TimeStep%Unit
    ELSE
      cPrintTimeStepLocal = cPrintTimeStep
    END IF
    lLWUBudget         = .FALSE.
    
    !For time-tracking simulations, check that the print interval is consistent with the data time step
    IF (TrackTime) THEN
      CALL CTimeStep_To_RTimeStep(cPrintTimeStepLocal,rPrintDeltaT,iPrintDeltaT_InMinutes,ErrorCode)
      IF (ErrorCode .NE. 0) THEN
          CALL SetLastMessage(TRIM(cPrintTimeStepLocal)//' is not a recognized time interval for printing of '//TRIM(Budget%Header%cBudgetDescriptor)//'!',f_iFatal,ThisProcedure) 
          iStat = -1
          RETURN
      END IF
      IF (iPrintDeltaT_InMinutes .LT. DeltaT_InMinutes) THEN
        MessageArray(1) = 'Print interval ('//TRIM(cPrintTimeStepLocal)//') for '//TRIM(Budget%Header%cBudgetDescriptor)//' cannot be less than the'
        MessageArray(2) = 'simulation time step ('//TRIM(Budget%Header%TimeStep%Unit)//')!'
        CALL SetLastMessage(MessageArray(1:2),f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
      END IF
    END IF
    
    !Adjust print begin and end times
    CALL AdjustOutputTimes(Budget%Header,iPrintDeltaT_InMinutes,PrintIntervalLocal,iStat)  ;  IF (iStat .EQ. -1) RETURN
    IF (TrackTime) THEN
      NIntervals      = NPeriods(DeltaT_InMinutes,PrintIntervalLocal%PrintBeginDateAndTime,PrintIntervalLocal%PrintEndDateAndTime) + 1
      PrintTimeStamp  = IncrementTimeStamp(PrintIntervalLocal%PrintBeginDateAndTime,DeltaT_InMinutes,-1)
      NPrintIntervals = NPeriods(iPrintDeltaT_InMinutes,PrintTimeStamp,PrintIntervalLocal%PrintEndDateAndTime)
    ELSE
      NIntervals      = NPeriods(DeltaT,PrintIntervalLocal%PrintBeginTime,PrintIntervalLocal%PrintEndTime) + 1
      NPrintIntervals = NIntervals
    END IF
      
    !Open output file
    CALL OutputFile%New(FileName=cOutputFileName,InputFile=.FALSE.,IsTSFile=.TRUE.,Descriptor=Budget%Header%cBudgetDescriptor,iStat=iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Get the file type
    iFileType = OutputFile%iGetFileType()
    
    !Make sure that output file type is recognized
    IF (iFileType.NE.f_iTXT  .AND. iFileType.NE.f_iDSS) THEN
      MessageArray(1) = 'Currently budget tables can be printed only to text or DSS files!'
      MessageArray(2) = '('//TRIM(Budget%Header%cBudgetDescriptor)//')'
      CALL SetLastMessage(MessageArray(1:2),f_iFatal,ThisProcedure)
      iStat = -1
      RETURN
    END IF  
    
    !Set the output file specific data
    CALL SetCacheLimit(Cache)
    
    !ASCII output
    IF (iFileType .EQ. f_iTXT) THEN
      CALL OutputFile%SetCacheSize(MAXVAL(Budget%Header%Locations%NDataColumns),1,iStat)  ;  IF(iStat .EQ. -1) RETURN
      CALL OutputFile%SetPrintFormatSpec(Budget%Header%ASCIIOutput%cFormatSpec,iStat)     ;  IF(iStat .EQ. -1) RETURN
    !DSS output
    ELSE IF (iFileType .EQ. f_iDSS) THEN
      CALL GetArrayData(Budget%Header%DSSOutput%iDataTypes,iDataTypes,ThisProcedure,iStat)      ;  IF (iStat .EQ. -1) RETURN
      CALL GetArrayData(Budget%Header%Locations%NDataColumns,NDataColumns,ThisProcedure,iStat)  ;  IF (iStat .EQ. -1) RETURN
    END IF

    !Sort the location indices
    iLocationsLocal = iLocations
    CALL ShellSort(iLocationsLocal)
    
    !Relative position in file
    IF (TrackTime) THEN
        CALL StorageUnitsToSkip(Budget%Header,cDateAndTime=PrintIntervalLocal%PrintBeginDateAndTime,iUnits=indxPosRelative,iStat=iStat)
        indxPosRelative = Budget%Header%iPosition + indxPosRelative
    ELSE
        CALL StorageUnitsToSkip(Budget%Header,rTime=PrintIntervalLocal%PrintBeginTime,iUnits=indxPosRelative,iStat=iStat)
        indxPosRelative = Budget%Header%iPosition + indxPosRelative
    END IF
    IF (iStat .EQ. -1) RETURN

    !Loop over locations
    NTimeSteps = Budget%Header%NTimeSteps
    indxS      = 0
    DO indxLocation=1,SIZE(iLocationsLocal)
      iLoc = iLocationsLocal(indxLocation)
      
      !Get a pointer to the location data and calculate number of storage units for all locations for one time step
      IF (SIZE(Budget%Header%Locations) .EQ. 1) THEN
        pLocationData   => Budget%Header%Locations(1)
        iTotalStorUnits =  Budget%Header%NLocations * pLocationData%iStorUnitsInFile
        indxPos         =  indxPosRelative + (iLoc-1)*pLocationData%iStorUnitsInFile
      ELSE
        pLocationData   => Budget%Header%Locations(iLoc)
        iTotalStorUnits =  SUM(Budget%Header%Locations%iStorUnitsInFile)
        indxPos         =  indxPosRelative + SUM(Budget%Header%Locations(1:iLoc-1)%iStorUnitsInFile)
      END IF
    
      !Number of columns for the location
      iCol = pLocationData%NDataColumns
      
      !If this is Land & Water Use Budget, there will be special accumulation of some columns
      !Check if this Land & Water Use Budget, and identify special data columns
      IF (LocateInList(f_iVR_lwu_PotCUAW,pLocationData%iDataColumnTypes) .GT. 0) THEN
          iAgShortCol       = LocateInList(f_iVR_lwu_AgShort,pLocationData%iDataColumnTypes)
          iAgSupReqCol      = LocateInList(f_iVR_lwu_AgSupplyReq,pLocationData%iDataColumnTypes)
          iAgPumpCol        = LocateInList(f_iVR_lwu_AgPump,pLocationData%iDataColumnTypes)
          iAgDivCol         = LocateInList(f_iVR_lwu_AgDiv,pLocationData%iDataColumnTypes)
          iAgOtherInflowCol = LocateInList(f_iVR_lwu_AgOthIn,pLocationData%iDataColumnTypes)
          lLWUBudget        = .TRUE.
      END IF
      
      !Allocate work arrays
      DEALLOCATE (rTempValues , rValues ,STAT=ErrorCode)
      ALLOCATE (rTempValues(iCol) , rValues(iCol))
      
      !Initial print time
      IF (TrackTime) THEN
        CurrentDateAndTime = PrintIntervalLocal%PrintBeginDateAndTime
      ELSE
        CurrentTime        = PrintIntervalLocal%PrintBeginTime
      END IF
      
      !If the output is a DSS file, close and open the file, and set the parameters
      IF (iFileType .EQ. f_iDSS) THEN
        !Initialize file
        CALL OutputFile%Kill()
        CALL OutputFile%New(FileName=cOutputFileName,InputFile=.FALSE.,IsTSFile=.TRUE.,Descriptor=Budget%Header%cBudgetDescriptor,iStat=iStat)
        IF (iStat .EQ. -1) RETURN
        
        !Array indices
        indxS = SUM(NDataColumns(1:iLoc-1))+1
        indxL = indxS + NDataColumns(iLoc) - 1
        
        !Pointers
        pcDataUnits => cDataUnits(1:iCol)

        !Define data units
        WHERE (pLocationData%iDataColumnTypes .EQ. f_iLT)
          pcDataUnits = LengthUnit
        ELSE WHERE (pLocationData%iDataColumnTypes .EQ. f_iAR)
          pcDataUnits = AreaUnit
        ELSE WHERE (pLocationData%iDataColumnTypes .EQ. f_iVR  .OR.  &
                    pLocationData%iDataColumnTypes .EQ. f_iVLB .OR.  &
                    pLocationData%iDataColumnTypes .EQ. f_iVLE       )
          pcDataUnits = VolumeUnit
        END WHERE
        
        !Redefine the E part of the pathnames according to print-out interval specified by user
        IF (DeltaT_InMinutes .NE. iPrintDeltaT_InMinutes) THEN
          cPathNames(indxS:indxL) =  ModifyPathnameEPart(Budget%Header%DSSOutput%cPathNames(indxS:indxL),indxL-indxS+1,cPrintTimeStep)
          pcPathnames             => cPathNames(indxS:indxL)
        ELSE
          pcPathnames             => Budget%Header%DSSOutput%cPathNames(indxS:indxL)
        END IF
        
        !Set the parameters for DSS output
        CALL OutputFile%SetParametersForDSSFile(PathNames=pcPathNames                                         , &
                                                DataUnit=pcDataUnits                                          , &
                                                DataType=f_cDataTypes(iDataTypes(indxS:indxL))                , &
                                                SimulationStartTime=CurrentDateAndTime                        , &    
                                                NTimeSteps=NPrintIntervals                                    , &    
                                                NColumnsOfData=iCol                                           , &
                                                NRowsOfData=1                                                 , &
                                                iStat=iStat                                                   )
        IF(iStat .EQ. -1) RETURN
      END IF
      
      !Print titles if it is ASCII output
      IF (iFileType .EQ. f_iTXT) THEN
          CALL PrintASCIITitles(Budget%Header,iLoc,FactArea,LengthUnit,AreaUnit,VolumeUnit,OutputFile)  
      END IF
      
      !Initialize values for print-out
      rValues          = 0.0
      rAgShortPrevious = 0.0
      lFirstAfterPrint = .TRUE.
      IF (TrackTime) THEN
        PrintTimeStamp = IncrementTimeStamp(CurrentDateAndTime,DeltaT_InMinutes,-1)
        PrintTimeStamp = IncrementTimeStamp(PrintTimeStamp,iPrintDeltaT_InMinutes,1)
      END IF
      
      !Read values from input file, convert them to desired units and print out results
      DO indxTime=1,NIntervals
      
        !Is it time to print?
        IF (TrackTime) THEN
          IF (CurrentDateAndTime .EQ. PrintTimeStamp) THEN
            lTimeToPrint = .TRUE.
          ELSE
            lTimeToPrint = .FALSE.
          END IF
        ELSE
          lTimeToPrint = .TRUE.
        END IF
        
        !Is it the last print-out
        IF (indxTime .EQ. NIntervals) THEN
          lFinalPrint = .TRUE.
        ELSE
          lFinalPrint = .FALSE.
        END IF
        
        !Read
        CALL Budget%InputFile%ReadData(rTempValues,iStat,indxPos)  
        IF(iStat .EQ. -1) RETURN
        
        !If Land & Water Use Budget, compute modified agricultural supply requirement for special accumulation
        IF (lLWUBudget) rAgSupReq_Modified = ModifiedAgSupplyReq(rAgShortPrevious,rTempValues(iAgSupReqCol))
        
        !Convert units
        DO indxCol=1,iCol
          SELECT CASE (pLocationData%iDataColumnTypes(indxCol))
            CASE (f_iVR, f_iVR_lwu_AgPump, f_iVR_lwu_AgDiv, f_iVR_lwu_AgOthIn)
              rValues(indxCol) = rValues(indxCol) + rTempValues(indxCol) * FactVolume
            CASE (f_iVR_lwu_PotCUAW)
                IF (NIntervals .EQ. NPrintIntervals) THEN
                    rValues(indxCol) = rTempValues(indxCol) * FactVolume
                ELSE
                    !Special accumulation method 
                    IF (rTempValues(iAgSupReqCol) .NE. 0.0)  &
                        rValues(indxCol) = rValues(indxCol) + rAgSupReq_Modified * rTempValues(indxCol) / rTempValues(iAgSupReqCol) * FactVolume
                END IF
            CASE (f_iVR_lwu_AgSupplyReq)
                IF (NIntervals .EQ. NPrintIntervals) THEN
                    rValues(indxCol) = rTempValues(indxCol) * FactVolume
                ELSE
                    !Special accumulation method 
                    rValues(indxCol) = rValues(indxCol) + rAgSupReq_Modified * FactVolume
                END IF
            CASE (f_iVR_lwu_AgShort)
                IF (NIntervals .EQ. NPrintIntervals) THEN
                    rValues(indxCol) = rTempValues(indxCol) * FactVolume
                ELSE
                    !Special accumulation method 
                    rValues(indxCol) = rValues(indxCol) + (rAgSupReq_Modified - rTempValues(iAgPumpCol) - rTempValues(iAgDivCol) - rTempValues(iAgOtherInflowCol)) * FactVolume
                END IF
            CASE (f_iVLB)
              IF (lFirstAfterPrint) rValues(indxCol) = rTempValues(indxCol) * FactVolume
            CASE (f_iVLE)
              IF (lTimeToPrint) rValues(indxCol) = rTempValues(indxCol) * FactVolume
            CASE (f_iAR)  
              rValues(indxCol) = rTempValues(indxCol) * FactArea
            CASE (f_iLT)
              rValues(indxCol) = rTempValues(indxCol) * FactLength
          END SELECT
        END DO
        
        !Prepare time for print-out
        IF (TrackTime) THEN
          OutputTime = ADJUSTL(CurrentDateAndTime)
        ELSE
          WRITE (OutputTime,'(F16.2)') CurrentTime
        END IF
        
        !Print results
        IF (TrackTime) THEN
          IF (lTimeToPrint) THEN
            CALL OutputFile%WriteData(OutputTime,rValues,FinalPrint=lFinalPrint)
            rValues          = 0.0
            lFirstAfterPrint = .TRUE.
            PrintTimeStamp   = IncrementTimeStamp(PrintTimeStamp,iPrintDeltaT_InMinutes,1)
          ELSE
            lFirstAfterPrint = .FALSE.
          END IF
        ELSE
          CALL OutputFile%WriteData(OutputTime,rValues,FinalPrint=lFinalPrint)
          lFirstAfterPrint = .TRUE.
        END IF
        
        !Increment time 
        IF (TrackTime) THEN
          CurrentDateAndTime = IncrementTimeStamp(CurrentDateAndTime,DeltaT_InMinutes,1)
        ELSE
          CurrentTime        = CurrentTime + DeltaT
        END IF
        
        !Move position in file
        indxPos = indxPos + iTotalStorUnits 
        
        !If Land & Water Use Budget, store Agrilcultural Shortage from previous timestep
        IF (lLWUBudget) rAgShortPrevious = rTempValues(iAgShortCol)

      END DO
      
      !For ASCII output print an empty line
      IF (iFileType .EQ. f_iTXT) THEN
          CALL OutputFile%WriteData(' '//NEW_LINE('x'))
      END IF
        
    END DO 
    
    !Close output file
    CALL OutputFile%Kill()
    
  END SUBROUTINE PrintResults_ReadFromBinFile
  
  
  ! -------------------------------------------------------------
  ! --- READ BUDGET DATA FROM HDF FILE FOR ONE COLUMN AND ACCUMULATE TO A GIVEN INTERVAL
  ! --- Note: Assumes file is an HDF file
  ! -------------------------------------------------------------
  SUBROUTINE ReadData_OneColumn_FromHDFFile(Budget,iLocation,iCol,cOutputInterval,cOutputBeginDateAndTime,cOutputEndDateAndTime,rOutputInterval,rOutputBeginTime,rOutputEndTime,rFact_LT,rFact_AR,rFact_VL,iDataUnitType,nPopulatedValues,rDates,rValues,iStat)
    CLASS(BudgetType),TARGET,INTENT(IN) :: Budget
    INTEGER,INTENT(IN)                  :: iLocation,iCol
    CHARACTER(LEN=*),INTENT(IN)         :: cOutputInterval,cOutputBeginDateAndTime,cOutputEndDateAndTime
    REAL(8),INTENT(IN)                  :: rOutputInterval,rOutputBeginTime,rOutputEndTime,rFact_LT,rFact_AR,rFact_VL
    INTEGER,INTENT(OUT)                 :: iDataUnitType,nPopulatedValues   !Number of elements in rJulianDates and rValues arrays that are actually populated (can be less than or equal to the size of the rJulianDates and rValues arrays for accumulated or partial-time-range output)
    REAL(8),INTENT(OUT)                 :: rValues(:)                       !Output array for accumulated data with the highest possible size, preferably equal to the number of time steps in the simulation that created the budget
    REAL(8),INTENT(OUT)                 :: rDates(:)                        !Output array for dates of the output data with the highest possible size, preferably equal to the number of time steps in the simulation that created the budget
    INTEGER,INTENT(OUT)                 :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+36),PARAMETER :: ThisProcedure = ModName // 'ReadData_OneColumn_FromHDFFile'
    LOGICAL                                :: TrackTime
    INTEGER                                :: OutputDeltaT_InMinutes,DeltaT_InMinutes,indxTime,ErrorCode,iTimeCounter,   &
                                              iAgSupReqCol,iAgShortCol,iAgPumpCol,iAgDivCol,iAgOtherInflowCol,           &
                                              NAccumIntervals,NOutputIntervalsEstimate,iOutputTimeCounter
    LOGICAL                                :: lLWUBudget
    REAL(8)                                :: rOutputDeltaT,rAgShortPrevious,rAgSupReq_Modified,rOutputBeginTime_Work,   &
                                              DeltaT,rNextTime 
    CHARACTER(LEN=f_iTimeStampLength)      :: cNextDateAndTime,cTime,cOutputBeginDateAndTime_Work
    CHARACTER(:),ALLOCATABLE               :: cFileName
    TYPE(LocationDataType),POINTER         :: pLocationData
    TYPE(TimeStepType)                     :: DataTimeStep
    REAL(8),ALLOCATABLE                    :: rReadValues(:),rRead_AgShort(:),rRead_AgSupReq(:),rRead_AgPump(:),rRead_AgDiv(:), &
                                              rRead_AgOtherInflow(:)
    
    !Initialize
    iStat            = 0
    lLWUBudget       = .FALSE.
    rAgShortPrevious = 0.0
    DataTimeStep     = Budget%Header%TimeStep
    TrackTime        = DataTimeStep%TrackTime
    
    !Number of intervals over which the budget data is to be accumulated
    IF (TrackTime) THEN
        DeltaT_InMinutes = DataTimeStep%DeltaT_InMinutes
        CALL CTimeStep_To_RTimeStep(cOutputInterval,rOutputDeltaT,OutputDeltaT_InMinutes)
        cOutputBeginDateAndTime_Work = IncrementTimeStamp(cOutputBeginDateAndTime,DeltaT_InMinutes,-1)
        cNextDateAndTime = IncrementTimeStamp(cOutputBeginDateAndTime_Work,OutputDeltaT_InMinutes,1)
        IF (cNextDateAndTime .TSGT. cOutputEndDateAndTime) THEN
            nPopulatedValues = 0
            RETURN
        END IF
        NOutputIntervalsEstimate = NPeriods(DeltaT_InMinutes,cOutputBeginDateAndTime_Work,cOutputEndDateAndTime) 
    ELSE
        DeltaT                = DataTimeStep%DeltaT
        rOutputBeginTime_Work = rOutputBeginTime - DeltaT
        rNextTime             = rOutputBeginTime_Work + rOutputInterval 
        IF (rNextTime .GT. rOutputEndTime) THEN
            nPopulatedValues = 0
            RETURN
        END IF
        NOutputIntervalsEstimate = NPeriods(DeltaT,rOutputBeginTime_Work,rOutputEndTime) 
    END IF 
    
    !Get a pointer to the location data
    IF (SIZE(Budget%Header%Locations) .EQ. 1) THEN
        pLocationData => Budget%Header%Locations(1)
    ELSE
        pLocationData => Budget%Header%Locations(iLocation)
    END IF
        
    !If Land & Water Use Budget, set additional parameters
    IF (LocateInList(f_iVR_lwu_PotCUAW,pLocationData%iDataColumnTypes) .GT. 0) THEN
        iAgShortCol       = LocateInList(f_iVR_lwu_AgShort,pLocationData%iDataColumnTypes)
        iAgSupReqCol      = LocateInList(f_iVR_lwu_AgSupplyReq,pLocationData%iDataColumnTypes)
        iAgPumpCol        = LocateInList(f_iVR_lwu_AgPump,pLocationData%iDataColumnTypes)
        iAgDivCol         = LocateInList(f_iVR_lwu_AgDiv,pLocationData%iDataColumnTypes)
        iAgOtherInflowCol = LocateInList(f_iVR_lwu_AgOthIn,pLocationData%iDataColumnTypes)
        lLWUBudget        = .TRUE.
    END IF
    
    !Allocate memory for the arrays that will store data read from budget input file
    ALLOCATE (rReadValues(NOutputIntervalsEstimate))
    IF (lLWUBudget) THEN
        ALLOCATE(rRead_AgShort(NOutputIntervalsEstimate) , rRead_AgSupReq(NOutputIntervalsEstimate) , rRead_AgPump(NOutputIntervalsEstimate) , rRead_AgDiv(NOutputIntervalsEstimate) , rRead_AgOtherInflow(NOutputIntervalsEstimate))
    END IF
    
    !Read data for the column
    IF (TrackTime) THEN
        cTime = cOutputBeginDateAndTime
    ELSE
        WRITE (cTime,'(A)') rOutputBeginTime
    END IF
    CALL Budget%InputFile%ReadData(cTime,iLocation,iCol,rReadValues,ErrorCode,iStat)  
    IF (ErrorCode .NE. 0) THEN
        CALL Budget%InputFile%GetName(cFileName)
        CALL SetLastMessage('Error in reading data from file '//cFileName//'!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    IF (lLWUBudget) THEN
        CALL Budget%InputFile%ReadData(cTime,iLocation,iAgSupReqCol,rRead_AgSupReq,ErrorCode,iStat)  ;  IF(iStat .EQ. -1) RETURN
        CALL Budget%InputFile%ReadData(cTime,iLocation,iAgShortCol,rRead_AgShort,ErrorCode,iStat)    ;  IF(iStat .EQ. -1) RETURN
        IF (iCol .EQ. iAgShortCol) THEN
            CALL Budget%InputFile%ReadData(cTime,iLocation,iAgPumpCol,rRead_AgPump,ErrorCode,iStat)                ;  IF(iStat .EQ. -1) RETURN
            CALL Budget%InputFile%ReadData(cTime,iLocation,iAgDivCol,rRead_AgDiv,ErrorCode,iStat)                  ;  IF(iStat .EQ. -1) RETURN
            CALL Budget%InputFile%ReadData(cTime,iLocation,iAgOtherInflowCol,rRead_AgOtherInflow,ErrorCode,iStat)  ;  IF(iStat .EQ. -1) RETURN
        END IF
    END IF
    
    !Data unit type
    SELECT CASE (pLocationData%iDataColumnTypes(iCol))
        CASE (f_iVR, f_iVLB , f_iVLE , f_iVR_lwu_AgPump , f_iVR_lwu_AgDiv, f_iVR_lwu_AgOthIn , f_iVR_lwu_PotCUAW , f_iVR_lwu_AgSupplyReq , f_iVR_lwu_AgShort)
            iDataUnitType = f_iDataUnitType_Volume
        CASE (f_iAR)  
            iDataUnitType = f_iDataUnitType_Area
        CASE (f_iLT)
            iDataUnitType = f_iDataUnitType_Length
    END SELECT   
    
    !If data interval and output interval are the same no need to accumulate, convert units and return
    IF (DeltaT_InMinutes .EQ. OutputDeltaT_InMinutes) THEN
        cOutputBeginDateAndTime_Work = cOutputBeginDateAndTime
        rOutputBeginTime_Work        = rOutputBeginTime
        DO indxTime=1,NOutputIntervalsEstimate
            !Convert units
            SELECT CASE (pLocationData%iDataColumnTypes(iCol))
                CASE (f_iVR, f_iVLB , f_iVLE , f_iVR_lwu_AgPump , f_iVR_lwu_AgDiv , f_iVR_lwu_AgOthIn , f_iVR_lwu_PotCUAW , f_iVR_lwu_AgSupplyReq , f_iVR_lwu_AgShort)
                    rValues(indxTime) = rReadValues(indxTime) * rFact_VL
                CASE (f_iAR)  
                    rValues(indxTime) = rReadValues(indxTime) * rFact_AR
                CASE (f_iLT)
                    rValues(indxTime) = rReadValues(indxTime) * rFact_LT
            END SELECT   
            
            !Save the output date
            IF (TrackTime) THEN      
                rDates(indxTime)             = TimeStampToJulian(cOutputBeginDateAndTime_Work)
                cOutputBeginDateAndTime_Work = IncrementTimeStamp(cOutputBeginDateAndTime_Work,DeltaT_InMinutes,1)
            ELSE
                rDates(indxTime)      = rOutputBeginTime_Work
                rOutputBeginTime_Work = rOutputBeginTime_Work + DeltaT
            END IF
        END DO
        nPopulatedValues = NOutputIntervalsEstimate
        RETURN
    END IF
    
    !Convert to output units and accumulate
    iTimeCounter       = 0
    iOutputTimeCounter = 1
    DO
        !Initialize ouput array for the output time step
        rValues(iOutputTimeCounter) = 0.0
        
        !Calculate number of data intervals in output interval
        IF (TrackTime) THEN
            NAccumIntervals = NPeriods(DeltaT_InMinutes,cOutputBeginDateAndTime_Work,cNextDateAndTime)
        ELSE
            NAccumIntervals = NPeriods(DeltaT,rOutputBeginTime_Work,rNextTime)
        END IF
        
        !Read and accumulate data
        Accumulation_Loop:  &
        DO indxTime=1,NAccumIntervals
            !Increment data time counter
            iTimeCounter = iTimeCounter + 1
        
            !If Land & Water Use Budget, compute modified agricultural supply requirement for special accumulation
            IF (lLWUBudget) rAgSupReq_Modified = ModifiedAgSupplyReq(rAgShortPrevious,rRead_AgSupReq(iTimeCounter))

            !Convert units
            SELECT CASE (pLocationData%iDataColumnTypes(iCol))
                CASE (f_iVR , f_iVR_lwu_AgPump , f_iVR_lwu_AgDiv, f_iVR_lwu_AgOthIn)
                    rValues(iOutputTimeCounter) = rValues(iOutputTimeCounter) + rReadValues(iTimeCounter) * rFact_VL
                CASE (f_iVR_lwu_PotCUAW)
                    IF (NAccumIntervals .EQ. 1) THEN
                        rValues(iOutputTimeCounter) = rReadValues(iTimeCounter) * rFact_VL
                    ELSE
                        !Special accumulation method
                        IF (rRead_AgSupReq(iTimeCounter) .NE. 0.0) &
                            rValues(iOutputTimeCounter) = rValues(iOutputTimeCounter) + rAgSupReq_Modified * rReadValues(iTimeCounter) / rRead_AgSupReq(iTimeCounter) * rFact_VL
                    END IF
                CASE (f_iVR_lwu_AgSupplyReq)
                    IF (NAccumIntervals .EQ. 1) THEN
                        rValues(iOutputTimeCounter) = rReadValues(iTimeCounter) * rFact_VL
                    ELSE
                        !Special accumulation method 
                        rValues(iOutputTimeCounter) = rValues(iOutputTimeCounter) + rAgSupReq_Modified * rFact_VL
                    END IF
                CASE (f_iVR_lwu_AgShort)
                    IF (NAccumIntervals .EQ. 1) THEN
                        rValues(iOutputTimeCounter) = rReadValues(iTimeCounter) * rFact_VL
                    ELSE
                        !Special accumulation method 
                        rValues(iOutputTimeCounter) = rValues(iOutputTimeCounter) + (rAgSupReq_Modified - rRead_AgPump(iTimeCounter) - rRead_AgDiv(iTimeCounter) - rRead_AgOtherInflow(iTimeCounter)) * rFact_VL
                    END IF
                CASE (f_iVLB)
                    IF (indxTime .EQ. 1) rValues(iOutputTimeCounter) = rReadValues(iTimeCounter) * rFact_VL
                CASE (f_iVLE)
                    IF (indxTime .EQ. NAccumIntervals) rValues(iOutputTimeCounter) = rReadValues(iTimeCounter) * rFact_VL
                CASE (f_iAR)  
                    rValues(iOutputTimeCounter) = rReadValues(iTimeCounter) * rFact_AR
                CASE (f_iLT)
                    rValues(iOutputTimeCounter) = rReadValues(iTimeCounter) * rFact_LT
            END SELECT                
            
            !If Land & Water Budget, store ag short from previous time step
            IF (lLWUBudget) rAgShortPrevious = rRead_AgShort(indxTime)
            
        END DO Accumulation_Loop
    
        !Assign the time value 
        IF (TrackTime) THEN      
            rDates(iOutputTimeCounter) = TimeStampToJulian(cNextDateAndTime)
        ELSE
            rDates(iOutputTimeCounter) = rNextTime
        END IF

        !Increase time by the output interval; if passed output end time return
        IF (TrackTime) THEN
            cOutputBeginDateAndTime_Work = cNextDateAndTime
            cNextDateAndTime             = IncrementTimeStamp(cNextDateAndTime,OutputDeltaT_InMinutes,1)
            IF (cNextDateAndTime .TSGT. cOutputEndDateAndTime) EXIT
        ELSE
            rOutputBeginTime_Work = rNextTime
            rNextTime             = rNextTime + rOutputInterval
            IF (rNextTime .GT. rOutputEndTime) EXIT
        END IF
        
        !Increment output time counter
        iOutputTimeCounter = iOutputTimeCounter + 1

    END DO
  
    !Store number of output times
    nPopulatedValues = iOutputTimeCounter
    
  END SUBROUTINE ReadData_OneColumn_FromHDFFile
  
  
  ! -------------------------------------------------------------
  ! --- READ BUDGET DATA FROM HDF OR BIN FILE AND ACCUMULATE TO A GIVEN INTERVAL
  ! -------------------------------------------------------------
  SUBROUTINE ReadData_SelectedColumns(Budget,iLocation,iReadCols,cOutputInterval,cOutputBeginDateAndTime,cOutputEndDateAndTime,rOutputInterval,rOutputBeginTime,rOutputEndTime,rFact_LT,rFact_AR,rFact_VL,nPopulatedValues,rValues,iStat)
    CLASS(BudgetType),INTENT(IN) :: Budget
    INTEGER,INTENT(IN)           :: iLocation,iReadCols(:)
    CHARACTER(LEN=*),INTENT(IN)  :: cOutputInterval,cOutputEndDateAndTime
    CHARACTER(LEN=*),INTENT(IN)  :: cOutputBeginDateAndTime
    REAL(8),INTENT(IN)           :: rOutputBeginTime
    REAL(8),INTENT(IN)           :: rOutputInterval,rOutputEndTime,rFact_LT,rFact_AR,rFact_VL
    INTEGER,INTENT(OUT)          :: nPopulatedValues
    REAL(8),INTENT(OUT)          :: rValues(:,:)
    INTEGER,INTENT(OUT)          :: iStat
    
    IF (Budget%InputFile%iGetFileType() .EQ. f_iHDF) THEN
        CALL ReadData_SelectedColumns_FromHDFFile(Budget,iLocation,iReadCols,cOutputInterval,cOutputBeginDateAndTime,cOutputEndDateAndTime,rOutputInterval,rOutputBeginTime,rOutputEndTime,rFact_LT,rFact_AR,rFact_VL,nPopulatedValues,rValues,iStat)
    ELSE
        CALL ReadData_SelectedColumns_FromBinFile(Budget,iLocation,iReadCols,cOutputInterval,cOutputBeginDateAndTime,cOutputEndDateAndTime,rOutputInterval,rOutputBeginTime,rOutputEndTime,rFact_LT,rFact_AR,rFact_VL,nPopulatedValues,rValues,iStat)
    END IF
    
  END SUBROUTINE ReadData_SelectedColumns
  
  
  ! -------------------------------------------------------------
  ! --- READ BUDGET DATA FROM HDF FILE AND ACCUMULATE TO A GIVEN INTERVAL
  ! -------------------------------------------------------------
  SUBROUTINE ReadData_SelectedColumns_FromHDFFile(Budget,iLocation,iReadCols,cOutputInterval,cOutputBeginDateAndTime,cOutputEndDateAndTime,rOutputInterval,rOutputBeginTime,rOutputEndTime,rFact_LT,rFact_AR,rFact_VL,nPopulatedValues,rValues,iStat)
    TYPE(BudgetType),TARGET,INTENT(IN) :: Budget
    INTEGER,INTENT(IN)                 :: iLocation,iReadCols(:)
    CHARACTER(LEN=*),INTENT(IN)        :: cOutputInterval,cOutputEndDateAndTime
    CHARACTER(LEN=*),INTENT(IN)        :: cOutputBeginDateAndTime
    REAL(8),INTENT(IN)                 :: rOutputBeginTime
    REAL(8),INTENT(IN)                 :: rOutputInterval,rOutputEndTime,rFact_LT,rFact_AR,rFact_VL
    INTEGER,INTENT(OUT)                :: nPopulatedValues   !This is the number of elements in rValues array that are actually populated (can be less than or equal to the dimension of the rValues array for accumulated or partial-time-range output)
    REAL(8),INTENT(OUT)                :: rValues(:,:)       !This is the output array for (column,time) combination with the highest possible size on the second dimension, preferably equal to the number of time steps in the simulation that created the budget
    INTEGER,INTENT(OUT)                :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+36),PARAMETER :: ThisProcedure = ModName // 'ReadData_SelectedColumns_FromHDFFile'
    LOGICAL                                :: TrackTime
    INTEGER                                :: OutputDeltaT_InMinutes,DeltaT_InMinutes,NAccumIntervals,nReadColumns,      &
                                              indxTime,indxCol,iAgSupReqCol,iAgShortCol,iAgPumpCol,iAgDivCol,iCol,       &
                                              iAgOtherInflowCol,ErrorCode,iTimeCounter,NDataTimeSteps,iOutputTimeCounter
    LOGICAL                                :: lLWUBudget
    REAL(8)                                :: rOutputDeltaT,DeltaT,rOutputBeginTime_Work,rAgSupReq_Modified,rNextTime
    REAL(8)                                :: rAgShortPrevious  
    CHARACTER(LEN=f_iTimeStampLength)      :: cOutputBeginDateAndTime_Work,cTime,cNextDateAndTime
    CHARACTER(:),ALLOCATABLE               :: cFileName
    TYPE(LocationDataType),POINTER         :: pLocationData
    TYPE(TimeStepType)                     :: DataTimeStep
    REAL(8),ALLOCATABLE                    :: rTempValues(:,:)
    
    !Initialize
    iStat            = 0
    lLWUBudget       = .FALSE.
    nReadColumns     = SIZE(iReadCols)
    rAgShortPrevious = 0.0
    DataTimeStep     = Budget%Header%TimeStep
    TrackTime        = DataTimeStep%TrackTime
    
    !Number of intervals over which the budget data is to be accumulated
    IF (TrackTime) THEN
        DeltaT_InMinutes = DataTimeStep%DeltaT_InMinutes
        CALL CTimeStep_To_RTimeStep(cOutputInterval,rOutputDeltaT,OutputDeltaT_InMinutes)
        cOutputBeginDateAndTime_Work = IncrementTimeStamp(cOutputBeginDateAndTime,DeltaT_InMinutes,-1)
        cNextDateAndTime = IncrementTimeStamp(cOutputBeginDateAndTime_Work,OutputDeltaT_InMinutes,1)
        IF (cNextDateAndTime .TSGT. cOutputEndDateAndTime) THEN
            nPopulatedValues = 0
            RETURN
        END IF
        NDataTimeSteps = NPeriods(DeltaT_InMinutes,cOutputBeginDateAndTime,cOutputEndDateAndTime) + 1
    ELSE
        DeltaT                = DataTimeStep%DeltaT
        rOutputBeginTime_Work = rOutputBeginTime - DeltaT
        rNextTime             = rOutputBeginTime_Work + rOutputInterval 
        IF (rNextTime .GT. rOutputEndTime) THEN
            nPopulatedValues = 0
            RETURN
        END IF
        NDataTimeSteps = NPeriods(DeltaT,rOutputBeginTime,rOutputEndTime) + 1
    END IF 
    
    !Get a pointer to the location data
    IF (SIZE(Budget%Header%Locations) .EQ. 1) THEN
        pLocationData => Budget%Header%Locations(1)
    ELSE
        pLocationData => Budget%Header%Locations(iLocation)
    END IF
        
    !If Land & Water Use Budget, set additional parameters
    IF (LocateInList(f_iVR_lwu_PotCUAW,pLocationData%iDataColumnTypes) .GT. 0) THEN
        iAgShortCol       = LocateInList(f_iVR_lwu_AgShort,pLocationData%iDataColumnTypes)
        iAgSupReqCol      = LocateInList(f_iVR_lwu_AgSupplyReq,pLocationData%iDataColumnTypes)
        iAgPumpCol        = LocateInList(f_iVR_lwu_AgPump,pLocationData%iDataColumnTypes)
        iAgDivCol         = LocateInList(f_iVR_lwu_AgDiv,pLocationData%iDataColumnTypes)
        iAgOtherInflowCol = LocateInList(f_iVR_lwu_AgOthIn,pLocationData%iDataColumnTypes)
        lLWUBudget        = .TRUE.
    END IF
    
    !Allocate memory for the array that will store partial or full data read from budget input file and read data
    ALLOCATE (rTempValues(pLocationData%NDataColumns,NDataTimeSteps))  !First row is for time step
    IF (TrackTime) THEN
        cTime = cOutputBeginDateAndTime
    ELSE
        WRITE (cTime,'(A)') rOutputBeginTime
    END IF
    CALL Budget%InputFile%ReadData(cTime,iLocation,rTempValues,ErrorCode,iStat)  
    IF (ErrorCode .NE. 0) THEN
        CALL Budget%InputFile%GetName(cFileName)
        CALL SetLastMessage('Error in reading data from file '//cFileName//'!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    !If data interval and output interval are the same, no need to accumulate; convert units and return
    IF (DeltaT_InMinutes .EQ. OutputDeltaT_InMinutes) THEN
        cOutputBeginDateAndTime_Work = cOutputBeginDateAndTime
        rOutputBeginTime_Work        = rOutputBeginTime
        Time_Loop:  &
        DO indxTime=1,NDataTimeSteps
            !Convert units
            Processed_Columns_Loop1:   &
            DO indxCol=2,nReadColumns+1  !First column is reserved for time  
                iCol = iReadCols(indxCol-1)
                SELECT CASE (pLocationData%iDataColumnTypes(iCol))
                    CASE (f_iVR , f_iVLB , f_iVLE , f_iVR_lwu_AgPump , f_iVR_lwu_AgDiv , f_iVR_lwu_AgOthIn , f_iVR_lwu_PotCUAW , f_iVR_lwu_AgSupplyReq , f_iVR_lwu_AgShort)
                        rValues(indxCol,indxTime) = rTempValues(iCol,indxTime) * rFact_VL
                    CASE (f_iAR)  
                        rValues(indxCol,indxTime) = rTempValues(iCol,indxTime) * rFact_AR
                    CASE (f_iLT)
                        rValues(indxCol,indxTime) = rTempValues(iCol,indxTime) * rFact_LT
                END SELECT                
            END DO Processed_Columns_Loop1
     
            !Save the output date
            IF (TrackTime) THEN      
                rValues(1,indxTime)          = TimeStampToJulian(cOutputBeginDateAndTime_Work)
                cOutputBeginDateAndTime_Work = IncrementTimeStamp(cOutputBeginDateAndTime_Work,DeltaT_InMinutes,1)
            ELSE
                rValues(1,indxTime)   = rOutputBeginTime_Work
                rOutputBeginTime_Work = rOutputBeginTime_Work + DeltaT
            END IF
        END DO Time_Loop
        nPopulatedValues = NDataTimeSteps
        RETURN
    END IF
    
    !Convert to output units and accumulate
    iTimeCounter                 = 0
    iOutputTimeCounter           = 0
    cOutputBeginDateAndTime_Work = cOutputBeginDateAndTime
    rOutputBeginTime_Work        = rOutputBeginTime
    DO
        !Increment output time counter
        iOutputTimeCounter = iOutputTimeCounter + 1

        !Initialize ouput array for the output time step
        rValues(:,iOutputTimeCounter) = 0.0
        
        !Calculate number of data intervals in output interval
        IF (TrackTime) THEN
            NAccumIntervals = NPeriods(DeltaT_InMinutes,cOutputBeginDateAndTime_Work,cNextDateAndTime) + 1
        ELSE
            NAccumIntervals = NPeriods(DeltaT,rOutputBeginTime_Work,rNextTime) + 1
        END IF
        
        !Read and accumulate data
        Accumulation_Loop:  &
        DO indxTime=1,NAccumIntervals    
            !Increment data time counter
            iTimeCounter = iTimeCounter + 1
            
            !If Land & Water Use Budget, compute modified agricultural supply requirement for special accumulation
            IF (lLWUBudget) rAgSupReq_Modified = ModifiedAgSupplyReq(rAgShortPrevious,rTempValues(iAgSupReqCol,iTimeCounter))        

            !Convert units
            Processed_Columns_Loop:   &
            DO indxCol=2,nReadColumns+1  !First column is reserved for time  
                iCol = iReadCols(indxCol-1)
                SELECT CASE (pLocationData%iDataColumnTypes(iCol))
                    CASE (f_iVR , f_iVR_lwu_AgPump , f_iVR_lwu_AgDiv , f_iVR_lwu_AgOthIn)
                        rValues(indxCol,iOutputTimeCounter) = rValues(indxCol,iOutputTimeCounter) + rTempValues(iCol,iTimeCounter) * rFact_VL
                    CASE (f_iVR_lwu_PotCUAW)
                        IF (NAccumIntervals .EQ. 1) THEN
                            rValues(indxCol,iOutputTimeCounter) = rTempValues(iCol,iTimeCounter) * rFact_VL
                        ELSE
                            !Special accumulation method
                            IF (rTempValues(iAgSupReqCol,iTimeCounter) .NE. 0.0) &
                                rValues(indxCol,iOutputTimeCounter) = rValues(indxCol,iOutputTimeCounter) + rAgSupReq_Modified * rTempValues(iCol,iTimeCounter) / rTempValues(iAgSupReqCol,iTimeCounter) * rFact_VL
                        END IF
                    CASE (f_iVR_lwu_AgSupplyReq)
                        IF (NAccumIntervals .EQ. 1) THEN
                            rValues(indxCol,iOutputTimeCounter) = rTempValues(indxCol,iOutputTimeCounter) * rFact_VL
                        ELSE
                            !Special accumulation method 
                            rValues(indxCol,iOutputTimeCounter) = rValues(indxCol,iOutputTimeCounter) + rAgSupReq_Modified * rFact_VL
                        END IF
                    CASE (f_iVR_lwu_AgShort)
                        IF (NAccumIntervals .EQ. 1) THEN
                            rValues(indxCol,iOutputTimeCounter) = rTempValues(iCol,iTimeCounter) * rFact_VL
                        ELSE
                            !Special accumulation method 
                            rValues(indxCol,iOutputTimeCounter) = rValues(indxCol,iOutputTimeCounter) + (rAgSupReq_Modified - rTempValues(iAgPumpCol,iTimeCounter) - rTempValues(iAgDivCol,iTimeCounter) - rTempValues(iAgOtherInflowCol,iTimeCounter)) * rFact_VL
                        END IF
                    CASE (f_iVLB)
                        IF (indxTime .EQ. 1) rValues(indxCol,iOutputTimeCounter) = rTempValues(iCol,iTimeCounter) * rFact_VL
                    CASE (f_iVLE)
                        IF (indxTime .EQ. NAccumIntervals) rValues(indxCol,iOutputTimeCounter) = rTempValues(iCol,iTimeCounter) * rFact_VL
                    CASE (f_iAR)  
                        rValues(indxCol,iOutputTimeCounter) = rTempValues(iCol,iTimeCounter) * rFact_AR
                    CASE (f_iLT)
                        rValues(indxCol,iOutputTimeCounter) = rTempValues(iCol,iTimeCounter) * rFact_LT
                END SELECT                
            END DO Processed_Columns_Loop
            
            !If Land & Water Budget, store ag short from previous time step
            IF (lLWUBudget) rAgShortPrevious = rTempValues(iAgShortCol,iTimeCounter)
        
        END DO Accumulation_Loop
    
        !Assign the time value 
        IF (TrackTime) THEN      
            rValues(1,iOutputTimeCounter) = TimeStampToJulian(cNextDateAndTime)
            cOutputBeginDateAndTime_Work  = IncrementTimeStamp(cNextDateAndTime,DeltaT_InMinutes,1)
            cNextDateAndTime              = IncrementTimeStamp(cNextDateAndTime,OutputDeltaT_InMinutes,1)
            IF (cNextDateAndTime .TSGT. cOutputEndDateAndTime) EXIT
        ELSE
            rValues(1,iOutputTimeCounter) = rNextTime
            rOutputBeginTime_Work         = rNextTime + DeltaT
            rNextTime                     = rNextTime + rOutputInterval
            IF (rNextTime .GT. rOutputEndTime) EXIT
        END IF

    END DO
  
    !Store number of output times
    nPopulatedValues = iOutputTimeCounter
    
  END SUBROUTINE ReadData_SelectedColumns_FromHDFFile
  
  
  ! -------------------------------------------------------------
  ! --- READ BUDGET DATA FROM BINARY FILE AND ACCUMULATE TO A GIVEN INTERVAL
  ! -------------------------------------------------------------
  SUBROUTINE ReadData_SelectedColumns_FromBinFile(Budget,iLocation,iReadCols,cOutputInterval,cOutputBeginDateAndTime,cOutputEndDateAndTime,rOutputInterval,rOutputBeginTime,rOutputEndTime,rFact_LT,rFact_AR,rFact_VL,nPopulatedValues,rValues,iStat)
    TYPE(BudgetType),TARGET,INTENT(IN) :: Budget
    INTEGER,INTENT(IN)                 :: iLocation,iReadCols(:)
    CHARACTER(LEN=*),INTENT(IN)        :: cOutputInterval,cOutputEndDateAndTime
    CHARACTER(LEN=*),INTENT(IN)        :: cOutputBeginDateAndTime
    REAL(8),INTENT(IN)                 :: rOutputBeginTime
    REAL(8),INTENT(IN)                 :: rOutputInterval,rOutputEndTime,rFact_LT,rFact_AR,rFact_VL
    INTEGER,INTENT(OUT)                :: nPopulatedValues   !This is the number of elements in rValues array that are actually populated (can be less than or equal to the dimension of the rValues array for accumulated or partial-time-range output)
    REAL(8),INTENT(OUT)                :: rValues(:,:)       !This is the output array for (column,time) combination with the highest possible size on the second dimension, preferably equal to the number of time steps in the simulation that created the budget
    INTEGER,INTENT(OUT)                :: iStat
    
    !Local variables
    LOGICAL                           :: TrackTime
    INTEGER                           :: iTotalStorUnits,OutputDeltaT_InMinutes,DeltaT_InMinutes,NAccumIntervals,    &
                                         indxTime,indxCol,iAgSupReqCol,iAgShortCol,iAgPumpCol,iAgDivCol,iCol,        &
                                         iAgOtherInflowCol,nReadColumns,iTimeCounter
    LOGICAL                           :: lLWUBudget
    INTEGER(KIND=8)                   :: indxPos,indxPosRelative
    REAL(8)                           :: rOutputDeltaT,DeltaT,rOutputBeginTime_Work,rAgSupReq_Modified,rNextTime,    &
                                         rAgShortPrevious  
    CHARACTER(LEN=f_iTimeStampLength) :: cOutputBeginDateAndTime_Work,cNextDateAndTime
    TYPE(LocationDataType),POINTER    :: pLocationData
    TYPE(TimeStepType)                :: DataTimeStep
    REAL(8),ALLOCATABLE               :: rTempValues(:)
    
    !Initialize
    iStat            = 0
    lLWUBudget       = .FALSE.
    nReadColumns     = SIZE(iReadCols)
    rAgShortPrevious = 0.0
    DataTimeStep     = Budget%Header%TimeStep
    TrackTime        = DataTimeStep%TrackTime
    
    !Number of intervals over which the budget data is to be accumulated
    IF (TrackTime) THEN
        DeltaT_InMinutes = DataTimeStep%DeltaT_InMinutes
        CALL CTimeStep_To_RTimeStep(cOutputInterval,rOutputDeltaT,OutputDeltaT_InMinutes)
        cOutputBeginDateAndTime_Work = IncrementTimeStamp(cOutputBeginDateAndTime,DeltaT_InMinutes,-1)
        cNextDateAndTime = IncrementTimeStamp(cOutputBeginDateAndTime_Work,OutputDeltaT_InMinutes,1)
        IF (cNextDateAndTime .TSGT. cOutputEndDateAndTime) THEN
            nPopulatedValues = 0
            RETURN
        END IF
    ELSE
        DeltaT                = DataTimeStep%DeltaT
        rOutputBeginTime_Work = rOutputBeginTime - DeltaT
        rNextTime             = rOutputBeginTime_Work + rOutputInterval 
        IF (rNextTime .GT. rOutputEndTime) THEN
            nPopulatedValues = 0
            RETURN
        END IF
    END IF 

    !Relative position in file after skipping for time steps that is not required for printing
    IF (TrackTime) THEN
        CALL StorageUnitsToSkip(Budget%Header,cDateAndTime=cOutputBeginDateAndTime,iUnits=indxPosRelative,iStat=iStat)
        indxPosRelative = indxPosRelative + Budget%Header%iPosition
    ELSE
        CALL StorageUnitsToSkip(Budget%Header,rTime=rOutputBeginTime,iUnits=indxPosRelative,iStat=iStat)
        indxPosRelative = indxPosRelative + Budget%Header%iPosition 
    END IF
    IF (iStat .EQ. -1) RETURN
    
    !Get a pointer to the location data and calculate number of storage units for all locations for one time step
    IF (SIZE(Budget%Header%Locations) .EQ. 1) THEN
        pLocationData   => Budget%Header%Locations(1)
        iTotalStorUnits =  Budget%Header%NLocations * pLocationData%iStorUnitsInFile
        indxPos         =  indxPosRelative + (iLocation-1)*pLocationData%iStorUnitsInFile
    ELSE
        pLocationData   => Budget%Header%Locations(iLocation)
        iTotalStorUnits =  SUM(Budget%Header%Locations%iStorUnitsInFile)
        indxPos         =  indxPosRelative + SUM(Budget%Header%Locations(1:iLocation-1)%iStorUnitsInFile)
    END IF
    
    !Allocate memeory for temporary array to read data from binary budget file
    ALLOCATE (rTempValues(pLocationData%NDataColumns))
    
    !If Land & Water Use Budget, set additional parameters
    IF (LocateInList(f_iVR_lwu_PotCUAW,pLocationData%iDataColumnTypes) .GT. 0) THEN
        iAgShortCol       = LocateInList(f_iVR_lwu_AgShort,pLocationData%iDataColumnTypes)
        iAgSupReqCol      = LocateInList(f_iVR_lwu_AgSupplyReq,pLocationData%iDataColumnTypes)
        iAgPumpCol        = LocateInList(f_iVR_lwu_AgPump,pLocationData%iDataColumnTypes)
        iAgDivCol         = LocateInList(f_iVR_lwu_AgDiv,pLocationData%iDataColumnTypes)
        iAgOtherInflowCol = LocateInList(f_iVR_lwu_AgOthIn,pLocationData%iDataColumnTypes)
        lLWUBudget        = .TRUE.
    END IF
    
    !Read data, convert to output units and accumulate
    iTimeCounter                 = 0
    cOutputBeginDateAndTime_Work = cOutputBeginDateAndTime
    rOutputBeginTime_Work        = rOutputBeginTime
    DO
        !Increment time counter
        iTimeCounter = iTimeCounter + 1

        !Initialize ouput array for the output time step
        rValues(:,iTimeCounter) = 0.0

        !Calculate number of data intervals in output interval
        IF (TrackTime) THEN
            NAccumIntervals = NPeriods(DeltaT_InMinutes,cOutputBeginDateAndTime_Work,cNextDateAndTime) + 1
        ELSE
            NAccumIntervals = NPeriods(DeltaT,rOutputBeginTime_Work,rNextTime) + 1
        END IF
        
        !Read and accumulate data
        Accumulation_Loop: &
        DO indxTime=1,NAccumIntervals
            !Read
            CALL Budget%InputFile%ReadData(rTempValues,iStat,indxPos)  
            IF(iStat .EQ. -1) RETURN
        
            !If Land & Water Use Budget, compute modified agricultural supply requirement for special accumulation
            IF (lLWUBudget) rAgSupReq_Modified = ModifiedAgSupplyReq(rAgShortPrevious,rTempValues(iAgSupReqCol))

            !Convert units
            Process_Column_Loop:  &
            DO indxCol=2,nReadColumns+1  !First index is reserved for time
                iCol = iReadCols(indxCol-1)
                SELECT CASE (pLocationData%iDataColumnTypes(iCol))
                    CASE (f_iVR , f_iVR_lwu_AgPump , f_iVR_lwu_AgDiv , f_iVR_lwu_AgOthIn)
                        rValues(indxCol,iTimeCounter) = rValues(indxCol,iTimeCounter) + rTempValues(iCol) * rFact_VL
                    CASE (f_iVR_lwu_PotCUAW)
                        IF (NAccumIntervals .EQ. 1) THEN
                            rValues(indxCol,iTimeCounter) = rTempValues(iCol) * rFact_VL
                        ELSE
                            !Special accumulation method
                            IF (rTempValues(iAgSupReqCol) .NE. 0.0) &
                                rValues(indxCol,iTimeCounter) = rValues(indxCol,iTimeCounter) + rAgSupReq_Modified * rTempValues(iCol) / rTempValues(iAgSupReqCol) * rFact_VL
                        END IF
                    CASE (f_iVR_lwu_AgSupplyReq)
                        IF (NAccumIntervals .EQ. 1) THEN
                            rValues(indxCol,iTimeCounter) = rTempValues(iCol) * rFact_VL
                        ELSE
                            !Special accumulation method 
                            rValues(indxCol,iTimeCounter) = rValues(indxCol,iTimeCounter) + rAgSupReq_Modified * rFact_VL
                        END IF
                    CASE (f_iVR_lwu_AgShort)
                        IF (NAccumIntervals .EQ. 1) THEN
                            rValues(indxCol,iTimeCounter) = rTempValues(iCol) * rFact_VL
                        ELSE
                            !Special accumulation method 
                            rValues(indxCol,iTimeCounter) = rValues(indxCol,iTimeCounter) + (rAgSupReq_Modified - rTempValues(iAgPumpCol) - rTempValues(iAgDivCol) - rTempValues(iAgOtherInflowCol)) * rFact_VL
                        END IF
                    CASE (f_iVLB)
                        IF (indxTime .EQ. 1) rValues(indxCol,iTimeCounter) = rTempValues(iCol) * rFact_VL
                    CASE (f_iVLE)
                        IF (indxTime .EQ. NAccumIntervals) rValues(indxCol,iTimeCounter) = rTempValues(iCol) * rFact_VL
                    CASE (f_iAR)  
                        rValues(indxCol,iTimeCounter) = rTempValues(iCol) * rFact_AR
                    CASE (f_iLT)
                        rValues(indxCol,iTimeCounter) = rTempValues(iCol) * rFact_LT
                END SELECT                
            END DO Process_Column_Loop
            
            !Move position in file
            indxPos = indxPos + iTotalStorUnits 

            !If Land & Water Budget, store ag short from previous time step
            IF (lLWUBudget) rAgShortPrevious = rTempValues(iAgShortCol)
          
        END DO Accumulation_Loop
    
        !Assign the time value 
        IF (TrackTime) THEN      
            rValues(1,iTimeCounter)       = TimeStampToJulian(cNextDateAndTime)
            cOutputBeginDateAndTime_Work  = IncrementTimeStamp(cNextDateAndTime,DeltaT_InMinutes,1)
            cNextDateAndTime              = IncrementTimeStamp(cNextDateAndTime,OutputDeltaT_InMinutes,1)
            IF (cNextDateAndTime .TSGT. cOutputEndDateAndTime) EXIT
        ELSE
            rValues(1,iTimeCounter) = rNextTime
            rOutputBeginTime_Work   = rNextTime + DeltaT
            rNextTime               = rNextTime + rOutputInterval
            IF (rNextTime .GT. rOutputEndTime) EXIT
        END IF
        
    END DO
    
    !Store number of output times
    nPopulatedValues = iTimeCounter
    
  END SUBROUTINE ReadData_SelectedColumns_FromBinFile

  
  ! -------------------------------------------------------------
  ! --- PRINT OUT ASCII FILE TITLES
  ! -------------------------------------------------------------
  SUBROUTINE PrintASCIITitles(OutputData,iLocation,FactArea,LengthUnit,AreaUnit,VolumeUnit,File)
    TYPE(BudgetHeaderType),TARGET,INTENT(IN) :: OutputData
    INTEGER,INTENT(IN)                       :: iLocation
    REAL(8),INTENT(IN)                       :: FactArea
    CHARACTER(LEN=*),INTENT(IN)              :: LengthUnit,AreaUnit,VolumeUnit
    TYPE(GenericFileType)                    :: File
    
    !Local variables
    INTEGER                                        :: TitleLenArray(OutputData%ASCIIOutput%NTitles),indxLine
    CHARACTER(LEN=OutputData%ASCIIOutput%TitleLen) :: cTitles(OutputData%ASCIIOutput%NTitles),cColumnHeaderLine
    CHARACTER(LEN=f_iColumnHeaderLen),ALLOCATABLE  :: cColumnHeaders(:,:),cTempHeaders(:)
    CHARACTER(LEN=15)                              :: AreaChar
    TYPE(LocationDataType),POINTER                 :: pLocation
    
    !Initialize
    TitleLenArray = OutputData%ASCIIOutput%TitleLen
    cTitles       = OutputData%ASCIIOutput%cTitles
    IF (SIZE(OutputData%Locations) .EQ. 1) THEN
      pLocation => OutputData%Locations(1)
    ELSE
      pLocation => OutputData%Locations(iLocation)
    END IF
    ALLOCATE (cColumnHeaders(pLocation%NDataColumns+1,OutputData%ASCIIOutput%NColumnHeaderLines) , &
              cTempHeaders(pLocation%NDataColumns+1)                                             )  
    cColumnHeaders = pLocation%cColumnHeaders
    
    !Modify the titles for location name, area, area unit, volume unit and length unit
    CALL InsertTextToTitles(OutputData%cLocationNames(iLocation),f_cLocationNameMarker,TitleLenArray,cTitles,f_iCenter)
    CALL InsertTextToTitles(LengthUnit,f_cLengthUnitMarker,TitleLenArray,cTitles,f_iCenter)
    CALL InsertTextToTitles(AreaUnit,f_cAreaUnitMarker,TitleLenArray,cTitles,f_iCenter)
    CALL InsertTextToTitles(VolumeUnit,f_cVolumeUnitMarker,TitleLenArray,cTitles,f_iCenter)
    IF (OutputData%NAreas .GT. 0) THEN
      WRITE (AreaChar,'(F15.2)') OutputData%Areas(iLocation)*FactArea
      CALL InsertTextToTitles(TRIM(ADJUSTL(AreaChar)),f_cAreaMarker,TitleLenArray,cTitles,f_iCenter)
    END IF
    
    !Modify column headers for area unit, volume unit and length unit
    DO indxLine=1,OutputData%ASCIIOutput%NColumnHeaderLines
      CALL InsertTextToTitles(LengthUnit,f_cLengthUnitMarker,pLocation%iColWidth,cColumnHeaders(:,indxLine),f_iRight)
      CALL InsertTextToTitles(AreaUnit,f_cAreaUnitMarker,pLocation%iColWidth,cColumnHeaders(:,indxLine),f_iRight)
      CALL InsertTextToTitles(VolumeUnit,f_cVolumeUnitMarker,pLocation%iColWidth,cColumnHeaders(:,indxLine),f_iRight)
    END DO
    
    !Print titles
    CALL File%WriteData(cTitles)
    
    !Prepare column header lines and print
    DO indxLine=1,OutputData%ASCIIOutput%NColumnHeaderLines
      cTempHeaders = cColumnHeaders(:,indxLine)
      WRITE (cColumnHeaderLine,TRIM(pLocation%cColumnHeadersFormatSpec(indxLine))) cTempHeaders 
      CALL File%WriteData(cColumnHeaderLine)
    END DO
    
  END SUBROUTINE PrintASCIITitles




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
  ! ---IS BUDGET RAW FILE DEFINED?
  ! -------------------------------------------------------------
  FUNCTION IsDefined(Budget) RESULT(lDefined)
    CLASS(BudgetType),INTENT(IN) :: Budget
    LOGICAL                      :: lDefined
    
    IF (Budget%InputFile%iGetFileType() .EQ. f_iUNKNOWN) THEN
        lDefined = .FALSE.
    ELSE
        lDefined = .TRUE.
    END IF
  
  END FUNCTION IsDefined
  
  
  ! -------------------------------------------------------------
  ! --- CALCULATE NUMBER OF BYTES TO SKIP TO GET TO A SPECIFIED TIME STEP IN BINARY FILE
  ! -------------------------------------------------------------
  SUBROUTINE StorageUnitsToSkip(OutputData,cDateAndTime,rTime,iUnits,iStat)
    TYPE(BudgetHeaderType),INTENT(IN)                     :: OutputData
    CHARACTER(LEN=f_iTimeStampLength),OPTIONAL,INTENT(IN) :: cDateAndTime
    REAL(8),OPTIONAL,INTENT(IN)                           :: rTime
    INTEGER(KIND=8),INTENT(OUT)                           :: iUnits
    INTEGER,INTENT(OUT)                                   :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+18) :: ThisProcedure = ModName // 'StorageUnitsToSkip'
    INTEGER                      :: iTime,iStorUnitsInFile(OutputData%NLocations)
    
    !Initilaize
    iStat = 0
    
    !Find the number of time steps to skip
    IF (OutputData%TimeStep%TrackTime) THEN
      !Make sure the right data is supplied
      IF (.NOT. PRESENT(cDateAndTime)) THEN
          CALL SetLastMessage('Print-out date and time for time-tracked budget output is missing!',f_iFatal,ThisProcedure) 
          iStat = -1
          RETURN
      END IF
      !Compute
      iTime = NPeriods(OutputData%TimeStep%DeltaT_InMinutes,OutputData%TimeStep%CurrentDateAndTime,cDateAndTime) 
    ELSE
      !Make sure the right data is supplied
      IF (.NOT. PRESENT(rTime)) THEN
          CALL SetLastMessage('Print-out time for non-time-tracked budget output is missing!',f_iFatal,ThisProcedure) 
          iStat = -1
          RETURN
      END IF
      !Compute
      iTime = NPeriods(OutputData%TimeStep%DeltaT,OutputData%TimeStep%CurrentTime,rTime)
    END IF
    
    !Get the storage units for each location
    CALL GetArrayData(OutputData%Locations%iStorUnitsInFile,iStorUnitsInFile,ThisProcedure,iStat)  ;  IF (iStat .EQ. -1) RETURN
    
    iUnits = iTime * SUM(iStorUnitsInFile)

  END SUBROUTINE StorageUnitsToSkip

  
  ! -------------------------------------------------------------
  ! --- MODIFY AGRICULTURAL SUPPLY REQUIREMENT FOR PROPER ACCUMULATION
  ! -------------------------------------------------------------
  FUNCTION ModifiedAgSupplyReq(rAgShortPrevious,rAgSupReq) RESULT(rAgSupReqMod)
    REAL(8),INTENT(IN) :: rAgShortPrevious,rAgSupReq
    REAL(8)            :: rAgSupReqMod
    
    IF (rAgShortPrevious .LE. 0.0) THEN
        rAgSupReqMod = rAgSupReq
    ELSE
        IF (rAgSupReq .GT. rAgShortPrevious) THEN
            rAgSupReqMod = rAgSupReq - rAgShortPrevious
        ELSE
            rAgSupReqMod = rAgSupReq
        END IF
    END IF
    
  END FUNCTION ModifiedAgSupplyReq

  
  ! -------------------------------------------------------------
  ! --- MODIFY ASCII TITLES FOR LOCATION NAMES AND OUTPUT UNITS
  ! -------------------------------------------------------------
  SUBROUTINE ModifyASCIITitles(Budget,iLocation,LengthUnit,AreaUnit,VolumeUnit,FactArea,cTitles,cAlternativeLocationName)
    CLASS(BudgetType),INTENT(IN)         :: Budget
    INTEGER,INTENT(IN)                   :: iLocation
    CHARACTER(LEN=*),INTENT(IN)          :: LengthUnit,AreaUnit,VolumeUnit
    REAL(8),INTENT(IN)                   :: FactArea
    CHARACTER(LEN=*)                     :: cTitles(:)
    CHARACTER(LEN=*),OPTIONAL,INTENT(IN) :: cAlternativeLocationName    !Used to override the original location name
    
    !Local variables
    INTEGER   :: TitleLenArray(Budget%Header%ASCIIOutput%NTitles)
    CHARACTER :: AreaChar*15
    
    !Initialize
    TitleLenArray = Budget%Header%ASCIIOutput%TitleLen
    
    !Insert data location name and output units into titles
    IF (PRESENT(cAlternativeLocationName)) THEN
        CALL InsertTextToTitles(TRIM(cAlternativeLocationName),f_cLocationNameMarker,TitleLenArray,cTitles,f_iCenter)
    ELSE
        CALL InsertTextToTitles(Budget%Header%cLocationNames(iLocation),f_cLocationNameMarker,TitleLenArray,cTitles,f_iCenter)
    END IF
    CALL InsertTextToTitles(LengthUnit,f_cLengthUnitMarker,TitleLenArray,cTitles,f_iCenter)
    CALL InsertTextToTitles(AreaUnit,f_cAreaUnitMarker,TitleLenArray,cTitles,f_iCenter)
    CALL InsertTextToTitles(VolumeUnit,f_cVolumeUnitMarker,TitleLenArray,cTitles,f_iCenter)
    IF (Budget%Header%NAreas .GT. 0) THEN
        WRITE (AreaChar,'(F15.2)') Budget%Header%Areas(iLocation)*FactArea
        CALL InsertTextToTitles(TRIM(ADJUSTL(AreaChar)),f_cAreaMarker,TitleLenArray,cTitles,f_iCenter)
    END IF
       
  END SUBROUTINE ModifyASCIITitles
  

  ! -------------------------------------------------------------
  ! --- MODIFY FULL COLUMN HEADERS FOR OUTPUT UNITS
  ! -------------------------------------------------------------
  SUBROUTINE ModifyFullColumnHeaders(LengthUnit,AreaUnit,VolumeUnit,cFullColumnHeaders)
    CHARACTER(LEN=*),INTENT(IN) :: LengthUnit,AreaUnit,VolumeUnit
    CHARACTER(LEN=*)            :: cFullColumnHeaders(:)
    
    !Local variables
    INTEGER   :: iColWidthArray(SIZE(cFullColumnHeaders))
    
    !Initialize
    iColWidthArray = f_iColumnHeaderLen
    
    !Insert output units into column headers
    CALL InsertTextToTitles(LengthUnit,f_cLengthUnitMarker,iColWidthArray,cFullColumnHeaders,f_iRight)
    CALL InsertTextToTitles(AreaUnit,f_cAreaUnitMarker,iColWidthArray,cFullColumnHeaders,f_iRight)
    CALL InsertTextToTitles(VolumeUnit,f_cVolumeUnitMarker,iColWidthArray,cFullColumnHeaders,f_iRight)

  END SUBROUTINE ModifyFullColumnHeaders
  

  ! -------------------------------------------------------------
  ! --- MODIFY THE E-PART OF A SET OF PATHNAMES
  ! -------------------------------------------------------------
  FUNCTION ModifyPathnameEPart(cPathNames,iDim,cNewEPart) RESULT(cPathNamesNew)
    INTEGER,INTENT(IN)                :: iDim
    CHARACTER(LEN=*),INTENT(IN)       :: cPathNames(iDim),cNewEPart
    CHARACTER(LEN=LEN(cPathNames(1))) :: cPathNamesNew(iDim)
    
    !Local variables
    CHARACTER(LEN=LEN(cPathNames(1)))  :: cPathNameTemp
    CHARACTER(LEN=LEN(cNewEPart))      :: cEPart
    INTEGER                            :: indx,iLen,indxLoc,iStart
    
    !Initialize
    iLen   = LEN(cPathNameTemp)
    cEPart = UpperCase(cNewEPart)
    
    !Modify E part
    DO indx=1,iDim
      cPathNameTemp = cPathNames(indx)
      iStart        = 2
      
      !Find the index of "/" before E part
      DO indxLoc=1,4
        iStart = iStart + FirstLocation('/',cPathNameTemp(iStart:iLen))
      END DO 
      iStart = iStart - 1
      
      !Copy the part of pathnames before E part and add the new E part
      cPathNamesNew(indx) = cPathNameTemp(1:iStart) // TRIM(cEPart)
      
      !Find the index of "/" after E part
      iStart = iStart + 1
      iStart = iStart + FirstLocation('/',cPathNameTemp(iStart:iLen)) - 1
      
      !Copy the rest of the pathname
      cPathNamesNew(indx) = TRIM(cPathNamesNew(indx)) // cPathNameTemp(iStart:iLen)
    END DO
    
  END FUNCTION ModifyPathnameEPart
  
  
  ! -------------------------------------------------------------
  ! --- INSERT A TEXT INTO TITLES
  ! -------------------------------------------------------------
  SUBROUTINE InsertTextToTitles(cText,cMarker,iLenArray,cTitles,iOrientation)
    CHARACTER(LEN=*),INTENT(IN) :: cText,cMarker
    INTEGER,INTENT(IN)          :: iLenArray(:)
    CHARACTER(LEN=*)            :: cTitles(:)
    INTEGER,INTENT(IN)          :: iOrientation
    
    !Local variables
    INTEGER                        :: indx,BeginLocation
    CHARACTER(LEN=LEN(cTitles(1))) :: cTitleBack
    
    DO indx=1,SIZE(cTitles)
      CALL FindSubStringInString(cMarker,cTitles(indx),BeginLocation)
      DO 
        IF (BeginLocation .EQ. 0) EXIT
        cTitleBack = cTitles(indx)(BeginLocation+LEN(cMarker):LEN(cTitles(indx)))
        cTitles(indx) = cTitles(indx)(1:BeginLocation-1) // TRIM(cText) // TRIM(cTitleBack)
        SELECT CASE(iOrientation)
          CASE (f_iLeft)
            cTitles(indx) = ADJUSTL(cTitles(indx))
          
          CASE (f_iCenter)
            cTitles(indx) = ArrangeText(TRIM(cTitles(indx)),iLenArray(indx))
            
          CASE (f_iRight)
            cTitles(indx) = ADJUSTR(cTitles(indx)(1:iLenArray(indx)))
        END SELECT
        CALL FindSubStringInString(cMarker,cTitles(indx),BeginLocation)
      END DO
    END DO
    
  END SUBROUTINE InsertTextToTitles
  
  
  ! -------------------------------------------------------------
  ! --- ADJUST THE OUTPUT BEGIN AND END TIMES IF NECESSARY
  ! -------------------------------------------------------------
  SUBROUTINE AdjustOutputTimes(OutputData,iPrintDeltaT_InMinutes,PrintInterval,iStat)
    TYPE(BudgetHeaderType),INTENT(IN) :: OutputData
    INTEGER,INTENT(IN)                :: iPrintDeltaT_InMinutes
    TYPE(PrintIntervalType)           :: PrintInterval
    INTEGER,INTENT(OUT)               :: iStat

    !Local variables
    CHARACTER(LEN=ModNameLen+17)      :: ThisProcedure = ModName // 'AdjustOutputTimes'
    REAL(8)                           :: DataEndTime,TempTime,rStartTime,DeltaT
    CHARACTER(LEN=f_iTimeStampLength) :: DataEndDateAndTime,TempTimeStamp,cStartDateAndTime
    INTEGER                           :: NIntervals,DeltaT_InMinutes,NTimeSteps
    
    !Initialize
    iStat      = 0
    NTimeSteps = OutputData%NTimeSteps
    
    IF (OutputData%TimeStep%TrackTime) THEN
      !Initialize
      cStartDateAndTime  = OutputData%TimeStep%CurrentDateAndTime
      DeltaT_InMinutes   = OutputData%TimeStep%DeltaT_InMinutes
      DataEndDateAndTime = IncrementTimeStamp(cStartDateAndTime,DeltaT_InMinutes,NTimeSteps-1)
    
      !Make sure that PrintBeginDateAndTime is less than or equal to PrintEndDateAndTime
      IF (PrintInterval%PrintBeginDateAndTime .TSGT. PrintInterval%PrintEndDateAndTime) THEN
        MessageArray(1)='Starting date and time for budget table print-out for '//TRIM(OutputData%cBudgetDescriptor)
        MessageArray(2)='cannot be less than the ending date and time!'
        CALL SetLastMessage(MessageArray(1:2),f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
      END IF
      
      !Make sure output begin date and time is not less than data begin date and time
      IF (PrintInterval%PrintBeginDateAndTime .TSLT. cStartDateAndTime) PrintInterval%PrintBeginDateAndTime = cStartDateAndTime
      
      !Make sure output end date and time is not greater than data end date and time
      IF (PrintInterval%PrintEndDateAndTime .TSGT. DataEndDateAndTime) PrintInterval%PrintEndDateAndTime = DataEndDateAndTime
      
      !Allign the budget output begin date and time with data intervals
      NIntervals                          = NPeriods(DeltaT_InMinutes,cStartDateAndTime,PrintInterval%PrintBeginDateAndTime)
      PrintInterval%PrintBeginDateAndTime = IncrementTimeStamp(cStartDateAndTime,DeltaT_InMinutes,NIntervals)
     
      !Allign the budget output end date and time with data intervals
      TempTimeStamp                     = IncrementTimeStamp(PrintInterval%PrintBeginDateAndTime,DeltaT_InMinutes,-1)
      NIntervals                        = NPeriods(DeltaT_InMinutes,TempTimeStamp,PrintInterval%PrintEndDateAndTime)
      PrintInterval%PrintEndDateAndTime = IncrementTimeStamp(TempTimeStamp,DeltaT_InMinutes,NIntervals)
      
      !Adjust the output end date and time w.r.t. the desired output interval
      IF (iPrintDeltaT_InMinutes .GT. DeltaT_InMinutes) THEN
        TempTimeStamp                     = IncrementTimeStamp(PrintInterval%PrintBeginDateAndTime,DeltaT_InMinutes,-1)
        NIntervals                        = NPeriods(iPrintDeltaT_InMinutes,TempTimeStamp,PrintInterval%PrintEndDateAndTime)
        PrintInterval%PrintEndDateAndTime = IncrementTimeStamp(TempTimeStamp,iPrintDeltaT_InMinutes,NIntervals)
      END IF
      
    ELSE
      !Initialize
      rStartTime  = OutputData%TimeStep%CurrentTime
      DeltaT      = OutputData%TimeStep%DeltaT
      DataEndTime = rStartTime + REAL(NTimeSteps-1,8) * DeltaT
    
      !Make sure that PrintBeginTime is less than or equal to PrintEndTime
      IF (PrintInterval%PrintBeginTime .GT. PrintInterval%PrintEndTime) THEN
        MessageArray(1)='Starting time for budget table print-out '//TRIM(OutputData%cBudgetDescriptor)
        MessageArray(2)='cannot be less than the ending time!'
        CALL SetLastMessage(MessageArray(1:2),f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
      END IF
      
      !Make sure output begin time is not less than data begin time
      IF (PrintInterval%PrintBeginTime .LT. rStartTime) PrintInterval%PrintBeginTime = rStartTime
      
      !Make sure output end time is not greater than data end time
      IF (PrintInterval%PrintEndTime .GT. DataEndTime) PrintInterval%PrintEndTime = DataEndTime
      
      !Allign the budget output begin time with data intervals
      NIntervals                   = NPeriods(DeltaT,rStartTime,PrintInterval%PrintBeginTime)
      PrintInterval%PrintBeginTime = rStartTime + REAL(NIntervals,8)*DeltaT
       
      !Allign the budget output end time with data intervals
      TempTime                   = PrintInterval%PrintBeginTime-DeltaT
      NIntervals                 = NPeriods(DeltaT,TempTime,PrintInterval%PrintEndTime)
      PrintInterval%PrintEndTime = TempTime + REAL(NIntervals,8)*DeltaT

    END IF

  END SUBROUTINE AdjustOutputTimes

END MODULE