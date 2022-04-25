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
MODULE Class_ZBudget
  USE,INTRINSIC :: ISO_C_BINDING , ONLY: C_INT                     , &
                                         C_CHAR                  
  USE MessageLogger              , ONLY: LogMessage                , &
                                         SetLastMessage            , &
                                         EchoProgress              , &
                                         MessageArray              , &
                                         f_iFatal                  , &
                                         f_iWarn                   , &
                                         f_iMessage                , &
                                         f_iSCREEN_FILE               
  USE GeneralUtilities           , ONLY: IntToText                 , &
                                         LocateInList              , &
                                         ArrangeText               , &
                                         UpperCase                 , &
                                         FirstLocation             , &
                                         ReplaceString             , &
                                         GetFileDirectory          , &
                                         StripFileNameFromPath     , &
                                         f_cLineFeed               , &
                                         String_Copy_F_C
  USE TimeSeriesUtilities        , ONLY: TimeStepType              , &
                                         IncrementTimeStamp        , &
                                         NPeriods                  , &
                                         TimeStampToJulian         , &
                                         CTimeStep_To_RTimeStep    , &
                                         OPERATOR(.TSLT.)          , &
                                         OPERATOR(.TSGT.)          , &
                                         f_iTimeStampLength
  USE IOInterface                , ONLY: GenericFileType           , &
                                         iGetFileType_FromName     , &
                                         f_iHDF                    , &
                                         f_iTXT                    , &
                                         f_iDSS                    , &
                                         f_iUNKNOWN                , &
                                         f_iMaxDatasetNameLen    
  USE Package_Misc               , ONLY: f_iDataUnitType_Area      , &
                                         f_iDataUnitType_Volume
  USE Class_SystemData           , ONLY: SystemDataType
  USE Class_ZBudgetHeader        , ONLY: ZBudgetHeaderType          
  USE Class_ZoneList             , ONLY: ZoneType                  , &
                                         AdjacentZoneType          , &
                                         ZoneListType
  USE ZBudget_Parameters         , ONLY: f_iElemDataType           , &
                                         f_iVerticalFlowType       , &
                                         f_iFaceFlowType           , &
                                         f_iStorageType            , &
                                         f_iUndefinedZone          , &
                                         f_iZoneVertical           , &
                                         f_iColumnHeaderLen        , &
                                         f_cMarkerChar             , &
                                         f_cAreaUnitMarker         , &
                                         f_cVolumeUnitMarker       , &
                                         f_cAttributesDir          , &
                                         ModifiedAgSupplyReq       , &
                                         f_iAR                     , &
                                         f_iVR                     , &
                                         f_iVR_lwu_PotCUAW         , &
                                         f_iVR_lwu_AgSupplyReq     , &
                                         f_iVR_lwu_AgPump          , &
                                         f_iVR_lwu_AgDiv           , &
                                         f_iVR_lwu_AgOthIn         , &
                                         f_iVR_lwu_AgShort         , &
                                         f_iVLB                    , &
                                         f_iVLE                       
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
  PUBLIC :: ZBudgetType          , &
            Abstract_CallbackFun
  
  
  ! -------------------------------------------------------------
  ! --- Z-BUDGET DATA TYPE
  ! -------------------------------------------------------------
  TYPE ZBudgetType
      TYPE(GenericFileType)   :: File
      TYPE(ZBudgetHeaderType) :: Header
      TYPE(SystemDataType)    :: SystemData
  CONTAINS
      PROCEDURE,PASS   :: Create_ZBudget
      PROCEDURE,PASS   :: Open_ZBudget
      PROCEDURE,PASS   :: Kill 
      PROCEDURE,PASS   :: GetFileName
      PROCEDURE,PASS   :: GetTimeStepRelatedData
      PROCEDURE,NOPASS :: GetNTitleLines
      PROCEDURE,PASS   :: GetTitleLines
      PROCEDURE,PASS   :: GetNDiversifiedColumns
      PROCEDURE,PASS   :: GetFullColumnHeaders
      PROCEDURE,PASS   :: WriteDataAtElements
      PROCEDURE,PASS   :: ReadData_SelectedColumns_ForAZone
      PROCEDURE,PASS   :: ReadData_SelectedColumns_ForSomeZones_ForAnInterval
      PROCEDURE,PASS   :: PrintZoneData
      GENERIC          :: New       => Create_ZBudget                                        , &
                                       Open_ZBudget
      GENERIC          :: ReadData  => ReadData_SelectedColumns_ForAZone                     , &
                                       ReadData_SelectedColumns_ForSomeZones_ForAnInterval
      GENERIC          :: WriteData => WriteDataAtElements
  END TYPE ZBudgetType
  
  
  ! -------------------------------------------------------------
  ! --- INTERFACE FOR A C-STYLE CALLBACK FUNCTION TO UPDATE A STRING VARIABLE
  ! -------------------------------------------------------------
  ABSTRACT INTERFACE
     SUBROUTINE Abstract_CallBackFun(iLenText,cText_C) BIND(C)
       !DEC$ ATTRIBUTES STDCALL :: Abstract_CallBackFun
       IMPORT                          :: C_INT,C_CHAR
       INTEGER(C_INT),INTENT(IN)       :: iLenText
       CHARACTER(C_CHAR),INTENT(INOUT) :: cText_C(iLenText)
     END SUBROUTINE Abstract_CallbackFun
  END INTERFACE

  
  ! -------------------------------------------------------------
  ! --- MISC. ENTITIES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                   :: ModNameLen = 15
  CHARACTER(LEN=ModNameLen),PARAMETER :: ModName    = 'Class_ZBudget::'
  
  
  
  
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
  ! --- CREATE NEW Z-BUDGET
  ! -------------------------------------------------------------
  SUBROUTINE Create_ZBudget(ZBudget,cFileName,NTimeSteps,TimeStep,Header,SystemData,iStat)  
    CLASS(ZBudgetType)                 :: ZBudget
    CHARACTER(LEN=*),INTENT(IN)        :: cFileName
    INTEGER,INTENT(IN)                 :: NTimeSteps
    TYPE(TimeStepType),INTENT(IN)      :: TimeStep
    TYPE(ZBudgetHeaderType),INTENT(IN) :: Header
    TYPE(SystemDataType),INTENT(IN)    :: SystemData
    INTEGER,INTENT(OUT)                :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+14),PARAMETER :: ThisProcedure = ModName // 'Create_ZBudget'
    INTEGER                                :: indxLayer,indxData,iCount,nDataColumns((Header%iNData+3)*SystemData%NLayers),NNodes,NElements,NLayers,NFaces
    CHARACTER(LEN=f_iMaxDatasetNameLen)    :: cDatasetNames((Header%iNData+3)*SystemData%NLayers)
    
    !Initialize
    iStat     = 0
    NNodes    = SystemData%NNodes
    NElements = SystemData%NElements
    NFaces    = SystemData%NFaces
    NLayers   = SystemData%NLayers
    
    !Make sure that there are at least 3 lines of ASCII column titles
    IF (ZBudget%Header%ASCIIOutput%iNTitles .LT. 3) THEN
        CALL SetLastMessage('There has to be at least 3 lines for column titles for Z-Budget ASCII output!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    !Store header in persistent memory
    ZBudget%Header = Header
    
    !Return if a file name for Z-Budget is not defined
    IF (cFileName .EQ. '') RETURN
    
    !Make sure that file is an HDF5 file
    IF (iGetFileType_FromName(cFileName) .NE. f_iHDF) THEN
        CALL SetLastMessage('File '//TRIM(ADJUSTL(cFileName))//' must be an HDF5 file for Z-Budget output!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    !Open file
    CALL ZBudget%File%New(FileName=ADJUSTL(cFileName),InputFile=.FALSE.,Descriptor=Header%cDescriptor,iStat=iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Write header
    CALL Header%WriteToFile(SystemData,ZBudget%File)
    
    !Write system data
    CALL SystemData%WriteToFile(ZBudget%File)
    
    !Create groups
    DO indxLayer=1,NLayers
        CALL ZBudget%File%CreateHDFGroup('/Layer_'//TRIM(IntToText(indxLayer)))
    END DO
    
    !Data paths for each dataset
    iCount = 0
    DO indxLayer=1,NLayers
        !Data columns
        DO indxData=1,Header%iNData
            iCount                = iCount + 1
            nDataColumns(iCount)  = Header%iNDataElems(indxData,indxLayer)
            cDatasetNames(iCount) = '/Layer_' // TRIM(IntToText(indxLayer)) // '/' // Header%cDataHDFPaths(indxData)
        END DO        
    END DO
        
    !Vertical flows
    DO indxLayer=1,NLayers
        IF (indxLayer .LT. NLayers) THEN
            iCount                = iCount + 1
            IF (Header%lVertFlows_DefinedAtNode) THEN
                nDataColumns(iCount) = SystemData%NNodes
            ELSE
                nDataColumns(iCount) = SystemData%NElements
            END IF
            cDatasetNames(iCount) = '/Layer_' // TRIM(IntToText(indxLayer)) // '/VerticalFlows'
        END IF
    END DO
    
    !Face flows
    IF (Header%lFaceFlows_Defined) THEN
        DO indxLayer=1,NLayers
            iCount                = iCount + 1
            nDataColumns(iCount)  = SystemData%NFaces
            cDatasetNames(iCount) = '/Layer_' // TRIM(IntToText(indxLayer)) // '/FaceFlows'
        END DO
    END IF
    
    !Storage
    IF (Header%lStorages_Defined) THEN
        DO indxLayer=1,NLayers
            iCount                = iCount + 1
            nDataColumns(iCount)  = SystemData%NElements
            cDatasetNames(iCount) = '/Layer_' // TRIM(IntToText(indxLayer)) // '/Storage'
        END DO
    END IF
    
    !Create datasets
    CALL ZBudget%File%CreateHDFDataSet(cDatasetNames(1:iCount),nDataColumns(1:iCount),NTimeSteps,TimeStep,DataType=0d0,iStat=iStat)  
        
  END SUBROUTINE Create_ZBudget
  
  
  ! -------------------------------------------------------------
  ! --- OPEN AN EXISTING Z-BUDGET DATASET
  ! -------------------------------------------------------------
  SUBROUTINE Open_ZBudget(ZBudget,cFileName,iStat)  
    CLASS(ZBudgetType)          :: ZBudget
    CHARACTER(LEN=*),INTENT(IN) :: cFileName
    INTEGER,INTENT(OUT)         :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+12),PARAMETER :: ThisProcedure = ModName // 'Open_ZBudget'
    
    !Initialize
    iStat = 0
    
    !Make sure that file is an HDF5 file
    IF (iGetFileType_FromName(cFileName) .NE. f_iHDF) THEN
        CALL SetLastMessage('File '//TRIM(ADJUSTL(cFileName))//' must be an HDF5 file for Z-Budget processing!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    !Open file
    CALL ZBudget%File%New(FileName=ADJUSTL(cFileName),InputFile=.TRUE.,iStat=iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Read system data
    CALL ZBudget%SystemData%ReadFromFile(ZBudget%File,iStat)
    IF (iStat .EQ. -1) RETURN
       
    !Read header
    CALL ZBudget%Header%ReadFromFile(ZBudget%SystemData,ZBudget%File,iStat)  
    
  END SUBROUTINE Open_ZBudget
 
  
  
  
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
  ! --- KILL Z-BUDGET DATA
  ! -------------------------------------------------------------
  SUBROUTINE Kill(ZBudget)
    CLASS(ZBudgetType) :: ZBudget
    
    !Kill the header
    CALL ZBudget%Header%Kill()
    
    !Kill system data
    CALL ZBudget%SystemData%Kill()
    
    !Close file
    CALL ZBudget%File%Kill()
  
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
  ! --- GET RAW Z-BUDGET FILE NAME
  ! -------------------------------------------------------------
  SUBROUTINE GetFileName(ZBudget,cFileName)
    CLASS(ZBudgetType),INTENT(IN)        :: ZBudget
    CHARACTER(:),ALLOCATABLE,INTENT(OUT) :: cFileName   
    
    !Local variables
    INTEGER :: ErrorCode
    
    DEALLOCATE (cFileName , STAT=ErrorCode)
    
    !Is the file available?
    IF (ZBudget%File%iGetFileType() .EQ. f_iUNKNOWN) RETURN
    
    !Get the filename
    CALL ZBudget%File%GetName(cFileName)
        
  END SUBROUTINE GetFileName
  
  
  ! -------------------------------------------------------------
  ! --- GET TIME STEP RELATED DATA FROM THE HEADER
  ! -------------------------------------------------------------
  SUBROUTINE GetTimeStepRelatedData(ZBudget,NTimeSteps,TimeStep)
    CLASS(ZBudgetType),INTENT(IN) :: ZBudget
    INTEGER,INTENT(OUT)           :: NTImeSteps
    TYPE(TimeStepType)            :: TimeStep
    
    CALL ZBudget%File%GetTimeStepRelatedData(NTimeSteps,TimeStep)
  
  END SUBROUTINE GetTimeStepRelatedData
  
  
  ! -------------------------------------------------------------
  ! --- GET NUMBER OF DIVERSIFIED COLUMNS CORRESPONDING TO A SET OF COLUMN NUMBERS
  ! --- *Note: For zones that interact with each other, this number 
  ! ---        can be larger than the non-diversified number of columns 
  ! ---        where inflows/outflows between zones are lumped.
  ! -------------------------------------------------------------
  SUBROUTINE GetNDiversifiedColumns(ZBudget,ZoneList,iZone,iColumnList,iNCols,iStat) 
    CLASS(ZBudgetType),INTENT(IN) :: ZBudget
    TYPE(ZoneListType),INTENT(IN) :: ZoneList
    INTEGER,INTENT(IN)            :: iZone,iColumnList(:)
    INTEGER,INTENT(OUT)           :: iNCols,iStat
    
    !Local variables
    CHARACTER(LEN=f_iColumnHeaderLen),ALLOCATABLE :: cColumnHeaders(:)
    CHARACTER                                     :: cDummy*10
    INTEGER,ALLOCATABLE                           :: iColumnListDiversified(:)
    
    !Initialize
    iNCols = 0

    !Get the diversified column titles to figure out the number of diversified columns
    CALL ZBudget%GetFullColumnHeaders(cDummy,cDummy,cColumnHeaders,iStat,ZoneList,iZone,iColumnList,iColumnListDiversified)
    IF (iStat .EQ. -1) RETURN
    
    iNCols = SIZE(cColumnHeaders)
    
  END SUBROUTINE GetNDiversifiedColumns
  
    
  ! -------------------------------------------------------------
  ! --- GET COLUMN HEADERS
  ! -------------------------------------------------------------
  SUBROUTINE GetFullColumnHeaders(ZBudget,cUnit_AR,cUnit_VL,cColumnHeaders,iStat,ZoneList,iZone,iColumnList,iColumnListDiversified)
    CLASS(ZBudgetType),INTENT(IN)                 :: ZBudget
    CHARACTER(LEN=*),INTENT(IN)                   :: cUnit_AR,cUnit_VL
    CHARACTER(LEN=f_iColumnHeaderLen),ALLOCATABLE :: cColumnHeaders(:)
    INTEGER,INTENT(OUT)                           :: iStat
    TYPE(ZoneListType),OPTIONAL,INTENT(IN)        :: ZoneList                   !The other OPTIONAL parameter must be provided if one is provided
    INTEGER,OPTIONAL,INTENT(IN)                   :: iZone,iColumnList(:)       !iColumnList lists the indices of columns for which the column headers are required; used to diversify the headers for inflows/outflows between zones
    INTEGER,ALLOCATABLE,OPTIONAL,INTENT(INOUT)    :: iColumnListDiversified(:)  !This is the diversified list of column indices corresponding to iColumnList
    
    !Local variables
    INTEGER                                       :: ErrorCode,iNColumns,iCount,indx,iLocE,iLocS,indxAdjZone,iAdjZone,iCol,iColumnListDivsfd_Local(500)
    CHARACTER(LEN=f_iColumnHeaderLen)             :: cTemp_In(1),cTemp_Out(1),cDiversifiedColumnHeaders(500)
    CHARACTER(LEN=f_iColumnHeaderLen),ALLOCATABLE :: cColumnHeaders_Work(:)
    CLASS(*),POINTER                              :: pZone
    LOGICAL                                       :: lAdjZoneInflow,lAdjZoneOutflow,lAdjZoneFlowsDefined
    
    !Initialize
    iStat           = 0
    lAdjZoneInflow  = .FALSE.
    lAdjZoneOutflow = .FALSE.
    IF (ZBudget%SystemData%NLayers .GT. 1  .OR.  ZBudget%Header%lFaceFlows_Defined) THEN
        lAdjZoneFlowsDefined = .TRUE.
    ELSE
        lAdjZoneFlowsDefined = .FALSE.
    END IF
    
    !First deallocate array
    DEALLOCATE (cColumnHeaders , STAT=ErrorCode)
    
    !Number of data columns
    iNColumns = ZBudget%Header%iNData + 1  !+1 for Time column
    IF (ZBudget%Header%lStorages_Defined) iNColumns = iNColumns + 1
    IF (ZBudget%Header%lComputeError) iNColumns = iNColumns + 1
    IF (lAdjZoneFlowsDefined) iNColumns = iNColumns + 2
    
    !Allocate work array
    ALLOCATE (cColumnHeaders_Work(iNColumns))
    cColumnHeaders_Work = ''
    
    !Assign column headers
    iCount                        = ZBudget%Header%iNData + 1  !+1 for Time column
    cColumnHeaders_Work(1)        = 'Time'
    cColumnHeaders_Work(2:iCount) = ZBudget%Header%cFullDataNames
    IF (lAdjZoneFlowsDefined) THEN
        cColumnHeaders_Work(iCount+1:iCount+2) = ['Inflow from adjacent zones (+)','Outflow to adjacent zones (-)']
        iCount                                 = iCount + 2
    END IF
    IF (ZBudget%Header%lComputeError) THEN
        iCount                      = iCount + 1
        cColumnHeaders_Work(iCount) = 'Discrepancy (=)'
    END IF
    IF (ZBudget%Header%lStorages_Defined) THEN
        iCount                      = iCount + 1
        cColumnHeaders_Work(iCount) = 'Absolute Storage'
    END IF
    
    !If column headers for a specific zone are required, diversify the columns for interaction among zones
    IF (PRESENT(iZone)) THEN
        !Check if inflows and outflows between zones are asked to be diversified
        DO indx=1,SIZE(iColumnList)
            iCol = iColumnList(indx)
            IF (TRIM(cColumnHeaders_Work(iCol)) .EQ. 'Inflow from adjacent zones (+)')  lAdjZoneInflow  = .TRUE.
            IF (TRIM(cColumnHeaders_Work(iCol)) .EQ. 'Outflow to adjacent zones (-)')   lAdjZoneOutflow = .TRUE.
        END DO
        
        !Get a pointer to the zone data
        pZone => ZoneList%GetPointerToNode(iZone)
        
        SELECT TYPE (pZone)
            TYPE IS (ZoneType)
                !Keep regular columns as is and diversify lumped zone intrecation columns
                iCount = 0
                DO indx=1,SIZE(iColumnList) 
                    iCol = iColumnList(indx)
                    IF (TRIM(cColumnHeaders_Work(iCol)) .EQ. 'Inflow from adjacent zones (+)') THEN 
                        IF (lAdjZoneOutflow) THEN
                            DO indxAdjZone=1,pZone%NAdjacentZones
                                iAdjZone                                     = pZone%AdjacentZoneNumbers(indxAdjZone)
                                cDiversifiedColumnHeaders(iCount+1:iCount+2) = ['Inflow from zone '//TRIM(IntToText(iAdjZone))//' (+)' , 'Outflow to zone '//TRIM(IntToText(iAdjZone))//' (-)']
                                iColumnListDivsfd_Local(iCount+1:iCount+2)   = [ZBudget%Header%iNData+1+2*(indxAdjZone-1)+1 , ZBudget%Header%iNData+1+2*indxAdjZone]
                                iCount                                       = iCount + 2
                            END DO
                        ELSE
                            DO indxAdjZone=1,pZone%NAdjacentZones
                                iAdjZone                          = pZone%AdjacentZoneNumbers(indxAdjZone)
                                iCount                            = iCount + 1
                                cDiversifiedColumnHeaders(iCount) = 'Inflow from zone '//TRIM(IntToText(iAdjZone))//' (+)'
                                iColumnListDivsfd_Local(iCount)   = ZBudget%Header%iNData +1 + (indxAdjZone-1)*2+1
                            END DO
                        END IF
                    ELSE IF (TRIM(cColumnHeaders_Work(iCol)) .EQ. 'Outflow to adjacent zones (-)') THEN
                        IF (.NOT.lAdjZoneInflow  .AND.  lAdjZoneOutflow) THEN
                            DO indxAdjZone=1,pZone%NAdjacentZones
                                iAdjZone                          = pZone%AdjacentZoneNumbers(indxAdjZone)
                                iCount                            = iCount + 1
                                cDiversifiedColumnHeaders(iCount) = 'Outflow to zone '//TRIM(IntToText(iAdjZone))//' (-)'
                                iColumnListDivsfd_Local(iCount)   = ZBudget%Header%iNData + 1 + indxAdjZone*2
                            END DO
                        END IF
                    ELSE
                        iCount = iCount + 1
                        IF (iCol .LE. ZBudget%Header%iNData+1) THEN
                            cDiversifiedColumnHeaders(iCount) = cColumnHeaders_Work(iCol)
                            iColumnListDivsfd_Local(iCount)   = iCol
                        ELSEIF (TRIM(cColumnHeaders_Work(iCol)) .EQ. 'Discrepancy (=)') THEN
                            cDiversifiedColumnHeaders(iCount) = cColumnHeaders_Work(iCol)
                            IF (lAdjZoneFlowsDefined) THEN
                                iColumnListDivsfd_Local(iCount) = ZBudget%Header%iNData + 1 + pZone%NAdjacentZones*2 + 1
                            ELSE
                                iColumnListDivsfd_Local(iCount) = ZBudget%Header%iNData + 1 + 2
                            END IF
                        ELSEIF (TRIM(cColumnHeaders_Work(iCol)) .EQ. 'Absolute Storage') THEN
                            cDiversifiedColumnHeaders(iCount) = cColumnHeaders_Work(iCol)
                            IF (lAdjZoneFlowsDefined) THEN
                                iColumnListDivsfd_Local(iCount) = ZBudget%Header%iNData + 1 + pZone%NAdjacentZones*2 + 2
                            ELSE
                                iColumnListDivsfd_Local(iCount) = ZBudget%Header%iNData + 1 + 2
                            END IF
                        END IF
                    END IF
                END DO
        END SELECT
            
        !Final number of columns and column headers
        iNColumns = iCount
        DEALLOCATE (cColumnHeaders_Work ,STAT=ErrorCode)
        ALLOCATE (cColumnHeaders_Work(iNColumns) , iColumnListDiversified(iNColumns))
        cColumnHeaders_Work    = cDiversifiedColumnHeaders(1:iCount)
        iColumnListDiversified = iColumnListDivsfd_Local(1:iCount)
    END IF
    
    !Allocate memory for the return array
    ALLOCATE (cColumnHeaders(iNColumns))
    
    !Replace unit markers
    DO indx=1,iNColumns
        iLocS = FirstLocation(f_cMarkerChar,cColumnHeaders_Work(indx))
        IF (iLocS .EQ. 0) THEN
            cColumnHeaders(indx) = cColumnHeaders_Work(indx)
            CYCLE
        END IF
        iLocE = FirstLocation(f_cMarkerChar,cColumnHeaders_Work(indx)(iLocS+1:)) + iLocS
        IF (iLocE-iLocS .LT. 1) THEN
            cTemp_Out(1) = ''
        ELSE
            cTemp_In(1) = cColumnHeaders_Work(indx)(iLocS:iLocE)
            CALL ReplaceMarkers(cTemp_In,cUnit_AR,cUnit_VL,cTemp_Out,iStat)
            IF (iStat .EQ. -1) RETURN
        END IF
        cColumnHeaders(indx) = cColumnHeaders_Work(indx)(1:iLocS-1) // ' ' // TRIM(ADJUSTL(cTemp_Out(1)))
    END DO
    
  END SUBROUTINE GetFullColumnHeaders
  
  
  ! -------------------------------------------------------------
  ! --- GET NUMBER OF TITLE LINES
  ! -------------------------------------------------------------
  FUNCTION GetNTitleLines() RESULT(iNTitles)
    INTEGER :: iNTitles
    
    iNTitles = 3
    
  END FUNCTION GetNTitleLines
  
  
  ! -------------------------------------------------------------
  ! --- GET TITLE LINES
  ! -------------------------------------------------------------
  SUBROUTINE GetTitleLines(ZBudget,iZone,rZoneArea,cZoneName,iLenTitles,cUnit_AR,cUnit_VL,cTitles)
    CLASS(ZBudgetType),INTENT(IN) :: ZBudget
    INTEGER,INTENT(IN)            :: iZone,iLenTitles
    REAL(8),INTENT(IN)            :: rZoneArea
    CHARACTER(LEN=*),INTENT(IN)   :: cZoneName,cUnit_AR,cUnit_VL
    CHARACTER(LEN=*)              :: cTitles(3)
    
    !Local variables
    CHARACTER :: cText*20 
    
    !First line
    cTitles(1) = ArrangeText(TRIM(ZBudget%Header%cSoftwareVersion),iLenTitles)
      
    !Second line
    cTitles(2) = ArrangeText(TRIM(UpperCase(ZBudget%Header%cDescriptor))//' IN '//TRIM(cUnit_VL)//' FOR ZONE '//TRIM(IntToText(iZone))//' ('//TRIM(ADJUSTL(cZoneName))//')',iLenTitles)
      
    !Third line
    WRITE (cText,'(F20.2)') rZoneArea 
    cTitles(3) = ArrangeText('ZONE AREA: '//TRIM(ADJUSTL(cText))//' '//ADJUSTL(cUnit_AR),iLenTitles)
    
  END SUBROUTINE GetTitleLines
  
  
  
  
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
  ! --- WRITE DATA DEFINED AT ALL ELEMENTS TO HDF FILE
  ! -------------------------------------------------------------
  SUBROUTINE WriteDataAtElements(ZBudget,NLayers,iFlowType,iData,iLayer,Data)
    CLASS(ZBudgetType) :: ZBudget
    INTEGER,INTENT(IN) :: NLayers,iFlowType,iData,iLayer
    REAL(8),INTENT(IN) :: Data(:,:)       !Data comes in for (column,row) format; row being timesteps
    
    !Local variables
    INTEGER :: iDataset
    
    iDataset = DatasetIndex(iFlowType,NLayers,ZBudget%Header%lFaceFlows_Defined,ZBudget%Header%iNData,iData,iLayer)
    CALL ZBudget%File%WriteData(Data,iDataset)
    
  END SUBROUTINE WriteDataAtElements
  
  
  
  
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
  ! --- READ Z-BUDGET DATA FOR SELECTED COLUMNS ACCUMULATED TO A GIVEN INTERVAL
  ! -------------------------------------------------------------
  SUBROUTINE ReadData_SelectedColumns_ForSomeZones_ForAnInterval(ZBudget,ZoneList,iZones,iReadCols,cOutputInterval,cOutputBeginDateAndTime,rFact_AR,rFact_VL,rValues,iStat)
    CLASS(ZBudgetType),INTENT(IN) :: ZBudget
    TYPE(ZoneListType),INTENT(IN) :: ZoneList
    INTEGER,INTENT(IN)            :: iZones(:),iReadCols(:,:)    !iReadCols comes in (column,zone) format
    CHARACTER(LEN=*),INTENT(IN)   :: cOutputBeginDateAndTime,cOutputInterval
    REAL(8),INTENT(IN)            :: rFact_AR,rFact_VL
    REAL(8),INTENT(OUT)           :: rValues(:,:)                !rValues is in (column,zone) format
    INTEGER,INTENT(OUT)           :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+52),PARAMETER :: ThisProcedure = ModName // 'ReadData_SelectedColumns_ForSomeZones_ForAnInterval'
    INTEGER                                :: indxZone,NTimeSteps,iNOutputIntervals,NStorageCol,NErrorCol,iZone,iNZoneDataCols,iCol,indxCol
    REAL(8)                                :: rZoneFlows(200)  !Allows maximum 200 columns of zone data
    REAL(8)                                :: rJulian
    TYPE(TimeStepType)                     :: TimeStep
    CLASS(*),POINTER                       :: pZone
    
    !Initialize
    iStat = 0
    
    !Get the timestep and simulation period characteristics
    CALL ZBudget%GetTimeStepRelatedData(NTimeSteps,TimeStep)
    
    !Calculate the number of intervals between output dates
    iNOutputIntervals = OutputIntervals(TimeStep%DeltaT_InMinutes,cOutputBeginDateAndTime,cOutputInterval,ZBudget%Header%cDescriptor)
    
    !Final time stamp to julian
    rJulian = TimeStampToJulian(IncrementTimeStamp(IncrementTimeStamp(cOutputBeginDateAndTime,TimeStep%DeltaT_InMinutes,-1),TimeStep%DeltaT_InMinutes,iNOutputIntervals))

    !Does the zone budget output have a storage column
    IF (ZBudget%Header%lStorages_Defined) THEN
        NStorageCol = 1
    ELSE
        NStorageCol = 0
    END IF
    
    !Is mass balance error computed and printed as a column
    IF (ZBudget%Header%lComputeError) THEN
        NErrorCol = 1
    ELSE
        NErrorCol = 0
    END IF
    
    !Compile zone flows
    CALL CompileZoneData(ZBudget,ZoneList,iZones,cOutputBeginDateAndTime,iNOutputIntervals,rFact_AR,rFact_VL,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Retrieve data for each zone
    DO indxZone=1,SIZE(iZones)
        iZone =  iZones(indxZone)
        pZone => ZoneList%GetPointerToNode(iZone)
        
        SELECT TYPE (pZone)
            TYPE IS (ZoneType)
                !Number of columns for the zone
                iNZoneDataCols = ZBudget%Header%iNData + 2*pZone%NAdjacentZones + NErrorCol + NStorageCol
                !Get zone flows all packed into an array
                CALL pZone%GetPackedZoneData(ZBudget%Header%iNData,ZBudget%Header%lStorages_Defined,ZBudget%Header%lComputeError,ZBudget%Header%iErrorInCols,ZBudget%Header%iErrorOutCols,rZoneFlows(1:iNZoneDataCols))
        END SELECT

        !Time column
        rValues(1,indxZone) = rJulian  
        
        !Other columns
        DO indxCol=1,SIZE(iReadCols,DIM=1)
            iCol = iReadCols(indxCol,indxZone)
            IF (iCol.LT.1  .OR.  iCol.GT.iNZoneDataCols) EXIT
            rValues(indxCol+1,indxZone) = rZoneFlows(iCol) 
        END DO
    END DO
    
  END SUBROUTINE ReadData_SelectedColumns_ForSomeZones_ForAnInterval
    

  ! -------------------------------------------------------------
  ! --- READ Z-BUDGET DATA FOR SELECTED COLUMNS ACCUMULATED TO A GIVEN INTERVAL FOR A ZONE
  ! -------------------------------------------------------------
  SUBROUTINE ReadData_SelectedColumns_ForAZone(ZBudget,ZoneList,iZone,iReadCols,cOutputInterval,cOutputBeginDateAndTime,cOutputEndDateAndTime,rFact_AR,rFact_VL,iDataUnitTypes,nPopulatedValues,rValues,iStat,pCallbackFun)
    CLASS(ZBudgetType),INTENT(IN)                               :: ZBudget
    TYPE(ZoneListType),INTENT(IN)                               :: ZoneList
    INTEGER,INTENT(IN)                                          :: iZone,iReadCols(:)
    CHARACTER(LEN=*),INTENT(IN)                                 :: cOutputInterval,cOutputEndDateAndTime
    CHARACTER(LEN=*),INTENT(IN)                                 :: cOutputBeginDateAndTime
    REAL(8),INTENT(IN)                                          :: rFact_AR,rFact_VL
    INTEGER,INTENT(OUT)                                         :: iDataUnitTypes(:),nPopulatedValues
    REAL(8),INTENT(OUT)                                         :: rValues(:,:)
    INTEGER,INTENT(OUT)                                         :: iStat
    PROCEDURE(Abstract_CallbackFun),POINTER,OPTIONAL,INTENT(IN) :: pCallbackFun
    
    !Local variables
    CHARACTER(LEN=ModNameLen+33),PARAMETER :: ThisProcedure = ModName // 'ReadData_SelectedColumns_ForAZone'
    INTEGER                                :: NTimeSteps,iNOutputIntervals,NStorageCol,NErrorCol,iNZoneDataCols,iZoneArray(1),iCol,indx
    REAL(8),ALLOCATABLE                    :: rZoneFlows(:)
    CHARACTER(LEN=f_iTimeStampLength)      :: cAdjPrintBeginDateAndTime,cAdjPrintEndDateAndTime,cPrintDateAndTime,cCurrentDateAndTime
    CHARACTER(C_CHAR)                      :: cPrintDateAndTime_C(f_iTimeStampLength)
    LOGICAL                                :: lFinalTime
    TYPE(TimeStepType)                     :: TimeStep
    CLASS(*),POINTER                       :: pZone
    
    !Initialize
    iStat            = 0
    nPopulatedValues = 0
    iZoneArray       = iZone
    
    !If iZone equals -99 (undefined zone), generate an error and return
    IF (iZone .EQ. f_iUndefinedZone) THEN
        CALL SetLastMessage('It is not allowed to generate zone budget for zone -99 (undefined zone)!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    !Get the timestep and simulation period characteristics
    CALL ZBudget%GetTimeStepRelatedData(NTimeSteps,TimeStep)
    
    !Adjust output period if necessary
    CALL AdjustPrintDates(TimeStep,NTimeSteps,cOutputBeginDateAndTime,cOutputEndDateAndTime,cAdjPrintBeginDateAndTime,cAdjPrintEndDateAndTime)

    !Calculate the number of intervals between output dates
    iNOutputIntervals = OutputIntervals(TimeStep%DeltaT_InMinutes,TimeStep%CurrentDateAndTime,cOutputInterval,ZBudget%Header%cDescriptor)
    
    !Does the zone budget output have a storage column
    IF (ZBudget%Header%lStorages_Defined) THEN
        NStorageCol = 1
    ELSE
        NStorageCol = 0
    END IF
    
    !Is mass balance error computed and printed as a column
    IF (ZBudget%Header%lComputeError) THEN
        NErrorCol = 1
    ELSE
        NErrorCol = 0
    END IF
    
    !Return if there are not enough timesteps to be printed
    cPrintDateAndTime = IncrementTimeStamp(cAdjPrintBeginDateAndTime,TimeStep%DeltaT_InMinutes,iNOutputIntervals-1)
    IF (cPrintDateAndTime .TSGT. cAdjPrintEndDateAndTime) THEN
        MessageArray(1) = 'There are not enough print-out timesteps between specified time period.'
        MessageArray(2) = 'Printing of Z-Budget for '//TRIM(ZBudget%Header%cDescriptor)//' is supressed.'
        CALL LogMessage(MessageArray(1:2),f_iWarn,ThisProcedure)
    END IF
    
    !Allocate array to hold the packed zone flows
    pZone => ZoneList%GetPointerToNode(iZone)
    IF (.NOT. ASSOCIATED(pZone)) THEN
        CALL SetLastMessage('Zone number '//TRIM(IntToText(iZone))//' cannot be located in the list of zones!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    SELECT TYPE (pZone)
        TYPE IS (ZoneType)
            iNZoneDataCols = ZBudget%Header%iNData + 2*pZone%NAdjacentZones + NErrorCol + NStorageCol
    END SELECT
    ALLOCATE (rZoneFlows(iNZoneDataCols))
    
    !Time stamps for output
    cCurrentDateAndTime = cAdjPrintBeginDateAndTime
    cPrintDateAndTime   = IncrementTimeStamp(cCurrentDateAndTime,TimeStep%DeltaT_InMinutes,iNOutputIntervals-1)
    
    !Data unit types of the columns read
    DO indx=1,SIZE(iReadCols)
        iCol = iReadCols(indx)
        IF (iCol .LE. ZBudget%Header%iNData) THEN
            SELECT CASE (ZBudget%Header%iDataTypes(iCol))
                CASE (f_iAR)
                    iDataUnitTypes(indx) = f_iDataUnitType_Area
                CASE DEFAULT
                    iDataUnitTypes(indx) = f_iDataUnitType_Volume
            END SELECT
        ELSE
            iDataUnitTypes(indx) = f_iDataUnitType_Volume
        END IF
    END DO
    
    !Process zones through time
    DO
        !If callback function provided call it
        IF (PRESENT(pCallbackFun)) THEN
            CALL String_Copy_F_C(cPrintDateAndTime,cPrintDateAndTime_C)
            CALL pCallbackFun(INT(f_iTimeStampLength,C_INT),cPrintDateAndTime_C)
        END IF
        
        !Compile zone flows
        CALL CompileZoneData(ZBudget,ZoneList,iZoneArray,cCurrentDateAndTime,iNOutputIntervals,rFact_AR,rFact_VL,iStat)
        IF (iStat .EQ. -1) RETURN
        
        !Is this the last time for printing?
        iNOutputIntervals = OutputIntervals(TimeStep%DeltaT_InMinutes,IncrementTimeStamp(cPrintDateAndTime,TimeStep%DeltaT_InMinutes,1),cOutputInterval,ZBudget%Header%cDescriptor)
        IF (IncrementTimeStamp(cPrintDateAndTime,TimeStep%DeltaT_InMinutes,iNOutputIntervals) .TSGT. cAdjPrintEndDateAndTime) THEN
            lFinalTime = .TRUE.
        ELSE
            lFinalTime = .FALSE.
        END IF
        
        !Get zone flows all packed into an array
        SELECT TYPE (pZone)
            TYPE IS (ZoneType)
                CALL pZone%GetPackedZoneData(ZBudget%Header%iNData,ZBudget%Header%lStorages_Defined,ZBudget%Header%lComputeError,ZBudget%Header%iErrorInCols,ZBudget%Header%iErrorOutCols,rZoneFlows)
        END SELECT
            
        !Pick the requested columns and store in the return array
        nPopulatedValues = nPopulatedValues + 1
        
        !Time column
        rValues(1,nPopulatedValues) = TimeStampToJulian(cPrintDateAndTime)  
        
        !Other columns
        rValues(2:,nPopulatedValues) = rZoneFlows(iReadCols)  
            
        !Exit time loop if reached end of printing time
        IF (lFinalTime) EXIT
        
        !Move in time
        cCurrentDateAndTime = IncrementTimeStamp(cPrintDateAndTime,TimeStep%DeltaT_InMinutes,1)
        iNOutputIntervals   = OutputIntervals(TimeStep%DeltaT_InMinutes,cCurrentDateAndTime,cOutputInterval,ZBudget%Header%cDescriptor)
        cPrintDateAndTime   = IncrementTimeStamp(cCurrentDateAndTime,TimeStep%DeltaT_InMinutes,iNOutputIntervals-1)
        
    END DO

  END SUBROUTINE ReadData_SelectedColumns_ForAZone
  
  
  ! -------------------------------------------------------------
  ! --- COMPILE AND PRINT ZONE FLOWS
  ! -------------------------------------------------------------
  SUBROUTINE PrintZoneData(ZBudget,ZoneList,cOutputFileName,cPrintBeginDateAndTime,cPrintEndDateAndTime,cPrintInterval,iPrintZones,rFact_VL,rFact_AR,cUnit_VL,cUnit_AR,iStat)
    CLASS(ZBudgetType),INTENT(IN) :: ZBudget
    TYPE(ZoneListType)            :: ZoneList
    CHARACTER(LEN=*),INTENT(IN)   :: cOutputFileName,cPrintBeginDateAndTime,cPrintEndDateAndTime,cPrintInterval,cUnit_VL,cUnit_AR
    INTEGER,INTENT(IN)            :: iPrintZones(:)
    REAL(8),INTENT(IN)            :: rFact_VL,rFact_AR
    INTEGER,INTENT(OUT)           :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+13),PARAMETER :: ThisProcedure = ModName // 'PrintZoneData'
    INTEGER                                :: iNOutputIntervals,NTimeSteps,indxPrint,iZPrint,NStorageCol,ErrorCode,NErrorCol,iNPrintZones, &
                                              iNZoneDataCols(SIZE(iPrintZones))
    REAL(8),ALLOCATABLE                    :: rZoneFlows(:)
    CHARACTER                              :: cFile1*500,cFile2*500
    CHARACTER(LEN=f_iTimeStampLength)      :: cAdjPrintBeginDateAndTime,cAdjPrintEndDateAndTime,cCurrentDateAndTime,cPrintDateAndTime
    CHARACTER(:),ALLOCATABLE               :: cOutDirectory,cPureOutFileName
    TYPE(TimeStepType)                     :: TimeStep
    TYPE(GenericFileType)                  :: TempZoneOutFiles(SIZE(iPrintZones)),DummyFile
    CLASS(*),POINTER                       :: pZone
    LOGICAL                                :: lPrintToTextFile,lFinalPrint
    
    !Initialize
    iStat        = 0
    iNPrintZones = SIZE(iPrintZones)
    
    !Get the timestep and simulation period characteristics
    CALL ZBudget%GetTimeStepRelatedData(NTimeSteps,TimeStep)
    
    !Adjust output period if necessary
    CALL AdjustPrintDates(TimeStep,NTimeSteps,cPrintBeginDateAndTime,cPrintEndDateAndTime,cAdjPrintBeginDateAndTime,cAdjPrintEndDateAndTime)
          
    !Calculate the number of intervals between output dates
    iNOutputIntervals = OutputIntervals(TimeStep%DeltaT_InMinutes,TimeStep%CurrentDateAndTime,cPrintInterval,ZBudget%Header%cDescriptor)
    
    !Does the zone budget output have a storage column
    IF (ZBudget%Header%lStorages_Defined) THEN
        NStorageCol = 1
    ELSE
        NStorageCol = 0
    END IF
    
    !Is mass balance error computed and printed as a column
    IF (ZBudget%Header%lComputeError) THEN
        NErrorCol = 1
    ELSE
        NErrorCol = 0
    END IF
    
    !Return if there are not enough timesteps to be printed
    cPrintDateAndTime = IncrementTimeStamp(cAdjPrintBeginDateAndTime,TimeStep%DeltaT_InMinutes,iNOutputIntervals-1)
    IF (cPrintDateAndTime .TSGT. cAdjPrintEndDateAndTime) THEN
        MessageArray(1) = 'There are not enough print-out timesteps between specified time period.'
        MessageArray(2) = 'Printing of Z-Budget for '//TRIM(ZBudget%Header%cDescriptor)//' is supressed.'
        CALL LogMessage(MessageArray(1:2),f_iWarn,ThisProcedure)
    END IF
    
    !Prepare output file(s)
    SELECT CASE (iGetFileType_FromName(cOutputFileName))
        !If output is text, prepare temporary files for each zone which will be merged all together later
        CASE (f_iTXT)
            lPrintToTextFile = .TRUE.
            CALL GetFileDirectory(cOutputFileName,cOutDirectory)
            CALL StripFileNameFromPath(cOutputFileName,cPureOutFileName)
            DO indxPrint=1,iNPrintZones
                iZPrint =  iPrintZones(indxPrint)
                pZone   => ZoneList%GetPointerToNode(iZPrint)
                SELECT TYPE (pZone)
                    TYPE IS (ZoneType)
                        !Prepare output file
                        CALL PrepareZoneFlowOutFile(ZBudget,cOutDirectory // 'Zone'//TRIM(IntToText(iZPrint))//'.dat',iZPrint,pZone%AdjacentZoneNumbers,pZone%Area*rFact_AR,pZone%cName,cUnit_AR,cUnit_VL,cAdjPrintBeginDateAndTime,cAdjPrintEndDateAndTime,cPrintInterval,TempZoneOutFiles(indxPrint),iStat)
                        IF (iStat .EQ. -1) RETURN
                END SELECT    
            END DO
    
        !If output file is a DSS file    
        CASE (f_iDSS)
            lPrintToTextFile = .FALSE.
            DO indxPrint=1,iNPrintZones
                iZPrint =  iPrintZones(indxPrint)
                pZone   => ZoneList%GetPointerToNode(iZPrint)
                SELECT TYPE (pZone)
                    TYPE IS (ZoneType)
                        !Prepare output file
                        CALL PrepareZoneFlowOutFile(ZBudget,TRIM(cOutputFileName),iZPrint,pZone%AdjacentZoneNumbers,pZone%Area*rFact_AR,pZone%cName,cUnit_AR,cUnit_VL,cAdjPrintBeginDateAndTime,cAdjPrintEndDateAndTime,cPrintInterval,TempZoneOutFiles(indxPrint),iStat)
                        IF (iStat .EQ. -1) RETURN
                END SELECT    
            END DO
            
        !Outwise, error message
        CASE DEFAULT
            CALL SetLastMessage('File '//TRIM(cOutputFileName)//' is not a recognized filetype for Z-Budget output!',f_iFatal,ThisProcedure) 
            iStat = -1
            RETURN
    END SELECT
        
    !Allocate array to hold the packed zone flows
    DO indxPrint=1,iNPrintZones
        iZPrint =  iPrintZones(indxPrint)
        pZone   => ZoneList%GetPointerToNode(iZPrint)
        SELECT TYPE (pZone)
            TYPE IS (ZoneType)
                !Number of columns for the zone
                iNZoneDataCols(indxPrint) = ZBudget%Header%iNData + 2*pZone%NAdjacentZones + NErrorCol + NStorageCol
        END SELECT    
    END DO
    DEALLOCATE (rZoneFlows , STAT=ErrorCode)
    ALLOCATE (rZoneFlows(MAXVAL(iNZoneDataCols)))
    
    !Time stamps for printing
    cCurrentDateAndTime = cAdjPrintBeginDateAndTime
    cPrintDateAndTime   = IncrementTimeStamp(cCurrentDateAndTime,TimeStep%DeltaT_InMinutes,iNOutputIntervals-1)

    !Process zones through time
    DO
        !Inform user
        CALL LogMessage(cPrintDateAndTime,f_iMessage,'',Destination=f_iSCREEN_FILE)
        
        !Compile zone flows
        CALL CompileZoneData(ZBudget,ZoneList,iPrintZones,cCurrentDateAndTime,iNOutputIntervals,rFact_AR,rFact_VL,iStat)
        IF (iStat .EQ. -1) RETURN
        
        !Is this the last time for printing?
        iNOutputIntervals = OutputIntervals(TimeStep%DeltaT_InMinutes,IncrementTimeStamp(cPrintDateAndTime,TimeStep%DeltaT_InMinutes,1),cPrintInterval,ZBudget%Header%cDescriptor)
        IF (IncrementTimeStamp(cPrintDateAndTime,TimeStep%DeltaT_InMinutes,iNOutputIntervals) .TSGT. cAdjPrintEndDateAndTime) THEN
            lFinalPrint = .TRUE.
        ELSE
            lFinalPrint = .FALSE.
        END IF

        !Print zone flows
        DO indxPrint=1,iNPrintZones
            iZPrint =  iPrintZones(indxPrint)
            pZone   => ZoneList%GetPointerToNode(iZPrint)
            SELECT TYPE (pZone)
                TYPE IS (ZoneType)
                    !Get zone flows all packed into an array
                    CALL pZone%GetPackedZoneData(ZBudget%Header%iNData,ZBudget%Header%lStorages_Defined,ZBudget%Header%lComputeError,ZBudget%Header%iErrorInCols,ZBudget%Header%iErrorOutCols,rZoneFlows(1:iNZoneDataCols(indxPrint)))
                                        
                    !Print zone flows
                    CALL TempZoneOutFiles(indxPrint)%WriteData(cPrintDateAndTime,rZoneFlows(1:iNZoneDataCols(indxPrint)),FinalPrint=lFinalPrint)
            END SELECT
        END DO
        
        !Exit time loop if reached end of printing time
        IF (lFinalPrint) EXIT
        
        !Move in time
        cCurrentDateAndTime = IncrementTimeStamp(cPrintDateAndTime,TimeStep%DeltaT_InMinutes,1)
        iNOutputIntervals   = OutputIntervals(TimeStep%DeltaT_InMinutes,cCurrentDateAndTime,cPrintInterval,ZBudget%Header%cDescriptor)
        cPrintDateAndTime   = IncrementTimeStamp(cCurrentDateAndTime,TimeStep%DeltaT_InMinutes,iNOutputIntervals-1)
        
    END DO
    
    !If the output is to text file, merge temp files
    IF (lPrintToTextFile) THEN
        cFile1 = cOutDirectory // 'Zone' // TRIM(IntToText(iPrintZones(1))) // '.dat'
        CALL TempZoneOutFiles(1)%Kill()
!DIR$ IF (linux .NE. 1)
        DO indxPrint=2,iNPrintZones
            cFile2 = cOutDirectory // 'Zone'// TRIM(IntToText(iPrintZones(indxPrint))) // '.dat'
            CALL EXECUTE_COMMAND_LINE('ECHO. >> ' // TRIM(cFile1))
            CALL EXECUTE_COMMAND_LINE('ECHO. >> ' // TRIM(cFile1))
            CALL EXECUTE_COMMAND_LINE('TYPE ' // TRIM(cFile2) // ' >> ' // TRIM(cFile1))
            CALL TempZoneOutFiles(indxPrint)%Kill('DELETE')
        END DO
        CALL DummyFile%New(FileName=TRIM(cOutputFileName),InputFile=.FALSE.,iStat=iStat)
        IF (iStat .EQ. -1) RETURN  
        CALL DummyFile%Kill('DELETE')
        CALL EXECUTE_COMMAND_LINE('RENAME ' // TRIM(cFile1) // ' ' // cPureOutFileName)
!DIR$ ELSE
        DO indxPrint=2,iNPrintZones
            cFile2 = 'Zone'// TRIM(IntToText(iPrintZones(indxPrint))) // '.dat'
            CALL EXECUTE_COMMAND_LINE('echo >> ' // TRIM(cFile1))
            CALL EXECUTE_COMMAND_LINE('echo >> ' // TRIM(cFile1))
            CALL EXECUTE_COMMAND_LINE('cat ' // TRIM(cFile2) // ' >> ' // TRIM(cFile1))
            CALL TempZoneOutFiles(indxPrint)%Kill('DELETE')
        END DO
        CALL EXECUTE_COMMAND_LINE('mv -f ' // TRIM(cFile1) // ' ' // TRIM(cOutputFileName))
!DIR$ END IF
    END IF
    
    !Close output files
    DO indxPrint=1,iNPrintZones
        CALL  TempZoneOutFiles(indxPrint)%Kill()
    END DO

  END SUBROUTINE PrintZoneData
  
    
  ! -------------------------------------------------------------
  ! --- PREPARE ZONE BUDGET OUTPUT FILE TO PRINT OUT COMPILED ZONE FLOWS
  ! -------------------------------------------------------------
  SUBROUTINE PrepareZoneFlowOutFile(ZBudget,cOutFileName,iZone,iAdjZoneNumbers,Area,cZoneName,cUnit_AR,cUnit_VL,cPrintBeginDateAndTime,cPrintEndDateAndTime,cPrintInterval,OutFile,iStat)
    TYPE(ZBudgetType),INTENT(IN) :: ZBudget
    CHARACTER(LEN=*),INTENT(IN)  :: cOutFileName,cZoneName,cUnit_AR,cUnit_VL,cPrintBeginDateAndTime,cPrintEndDateAndTime,cPrintInterval
    INTEGER,INTENT(IN)           :: iZone,iAdjZoneNumbers(:)
    REAL(8),INTENT(IN)           :: Area
    TYPE(GenericFileType)        :: OutFile
    INTEGER,INTENT(OUT)          :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+22),PARAMETER :: ThisProcedure = ModName // 'PrepareZoneFlowOutFile'
    INTEGER                                :: iFileType,NDataColumns,ErrorCode,NPrint,PrintDeltaT_InMinutes,NAdjZones,iCount,indxAdjZone,indxData
    REAL(8)                                :: rDummy
    CHARACTER                              :: cAccessType*10,cFormatSpec*300
    CHARACTER(LEN=8),ALLOCATABLE           :: cDataTypes(:),cDataUnits(:)
    CHARACTER(LEN=80),ALLOCATABLE          :: cDSSPathNames(:)
    
    !Initialize
    iStat = 0
    
    !Number of adjacent zones
    NAdjZones = SIZE(iAdjZoneNumbers)
    
    
    !Get the output file type
    iFileType = OutFile%iGetFileType()
    
    
    !If the output file is already open for a previous zone, close it to re-open it to set the different column numbers
    IF (iFileType .NE. f_iUNKNOWN) THEN
        IF (iFileType .EQ. f_iTXT) CALL OutFile%WriteData(f_cLineFeed)
        CALL OutFile%Kill()
        cAccessType = 'APPEND'
    !Oterwise, make sure that output file is either ASCII or DSS
    ELSE
        cAccessType = 'SEQUENTIAL'
        !Make sure file is ASCII or DSS
        iFileType = iGetFileType_FromName(cOutFileName)
        IF (iFileType.NE.f_iTXT  .AND.  iFileType.NE.f_iDSS) THEN
            CALL SetLastMessage('Z-Budget output file ('//TRIM(cOutFileName)//') must be either an ASCII or DSS file!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
    END IF
    
    
    !Open file
    IF (iFileType .EQ. f_iTXT) THEN
        CALL OutFile%New(FileName=TRIM(cOutFileName),InputFile=.FALSE.,IsTSFile=.TRUE.,Descriptor='output file for '//TRIM(ZBudget%Header%cDescriptor),AccessType=cAccessType,iStat=iStat)
    ELSE
        CALL OutFile%New(FileName=TRIM(cOutFileName),InputFile=.FALSE.,IsTSFile=.TRUE.,Descriptor='output file for '//TRIM(ZBudget%Header%cDescriptor),iStat=iStat)
    END IF
    IF (iStat .EQ. -1) RETURN 
    
    
    !Number of data columns
    NDataColumns                                       = ZBudget%Header%iNData + 2*NAdjZones          !Includes element data, inflows and outflows from adjacent zones
    IF (ZBudget%Header%lComputeError)     NDataColumns = NDataColumns + 1                             !Mass balance error
    IF (ZBudget%Header%lStorages_Defined) NDataColumns = NDataColumns + 1                             !If necessary include the stoarge term
    
    
    !Set parameters for ASCII output
    IF (iFileType .EQ. f_iTXT) THEN
        !Cache size and print format
        CALL OutFile%SetCacheSize(NDataColumns,iStat=iStat)  ;  IF (iStat .EQ. -1) RETURN
        cFormatSpec = ZBudget%Header%ASCIIOutput%cNumberFormat(1:LEN_TRIM(ZBudget%Header%ASCIIOutput%cNumberFormat)-1) // ',' // TRIM(IntTotext(2*NAdjZones)) // '(2X,F14.2)'
        IF (ZBudget%Header%lComputeError)     cFormatSpec = TRIM(cFormatSpec) // ',1(2X,F14.2)'
        IF (ZBudget%Header%lStorages_Defined) cFormatSpec = TRIM(cFormatSpec) // ',1(2X,F14.2)'
        cFormatSpec = TRIM(cFormatSpec) // ')'
        CALL OutFile%SetPrintFormatSpec(TRIM(cFormatSpec),iStat)  ;  IF (iStat .EQ. -1) RETURN
        
        !Zone titles
        CALL PrintASCIITitles(iStat)
        IF (iStat .EQ. -1) RETURN
        
    !... and for DSS output
    ELSEIF (iFileType .EQ. f_iDSS) THEN
        !Data units
        DEALLOCATE (cDataUnits , STAT=ErrorCode)  ;  ALLOCATE (cDataUnits(NDataColumns))
        WHERE (ZBudget%Header%iDataTypes .EQ. f_iAR)
            cDataUnits(1:ZBudget%Header%iNData) = cUnit_AR
        ELSE WHERE (ZBudget%Header%iDataTypes .EQ. f_iVR  .OR.  &
                    ZBudget%Header%iDataTypes .EQ. f_iVLE .OR.  &
                    ZBudget%Header%iDataTypes .EQ. f_iVLB       )
            cDataUnits(1:ZBudget%Header%iNData) = cUnit_VL
        END WHERE
        IF (NDataColumns .GT. ZBudget%Header%iNData) cDataUnits(ZBudget%Header%iNData+1:) = cUnit_VL
        
        !Data types
        DEALLOCATE (cDataTypes , STAT=ErrorCode)  ;  ALLOCATE (cDataTypes(NDataColumns))
        WHERE (ZBudget%Header%iDataTypes .EQ. f_iAR  .OR. &
               ZBudget%Header%iDataTypes .EQ. f_iVLE .OR. &
               ZBudget%Header%iDataTypes .EQ. f_iVLB      )
            cDataTypes(1:ZBudget%Header%iNData) = 'INST-VAL'
        ELSE WHERE (ZBudget%Header%iDataTypes .EQ. f_iVR)
            cDataTypes(1:ZBudget%Header%iNData) = 'PER-CUM'
        END WHERE
        IF (ZBudget%Header%lStorages_Defined) THEN
            cDataTypes(NDataColumns)   = 'INST-VAL'
            IF (ZBudget%Header%lComputeError) cDataTypes(NDataColumns-1) = 'PER-CUM'
        ELSE
            IF (ZBudget%Header%lComputeError) cDataTypes(NDataColumns) = 'PER-CUM'
        END IF
        
        !Pathnames
        DEALLOCATE (cDSSPathNames , STAT=ErrorCode)  ;  ALLOCATE (cDSSPathNames(NDataColumns))
        DO indxData=1,ZBudget%Header%iNData
            SELECT CASE (ZBudget%Header%iDataTypes(indxData))
                CASE (f_iAR)
                    cDSSPathNames(indxData) = '/IWFM_ZBUD/ZONE:' // TRIM(IntToText(iZone)) // '/AREA//' // TRIM(cPrintInterval) // '/' // TRIM(ZBudget%Header%cDSSFParts(indxData)) // '/'
                CASE DEFAULT
                    cDSSPathNames(indxData) = '/IWFM_ZBUD/ZONE:' // TRIM(IntToText(iZone)) // '/VOLUME//' // TRIM(cPrintInterval) // '/' // TRIM(ZBudget%Header%cDSSFParts(indxData)) // '/'
            END SELECT
        END DO
        iCount = ZBudget%Header%iNData + 1
        DO indxAdjZone=1,NAdjZones
            cDSSPathNames(iCount)   = '/IWFM_ZBUD/ZONE:' // TRIM(IntToText(iZone)) // '/VOLUME//' // TRIM(cPrintInterval) // '/FLOW_FROM_ZONE_' // TRIM(IntToText(iAdjZoneNumbers(indxAdjZone))) // '/'
            cDSSPathNames(iCount+1) = '/IWFM_ZBUD/ZONE:' // TRIM(IntToText(iZone)) // '/VOLUME//' // TRIM(cPrintInterval) // '/FLOW_TO_ZONE_' // TRIM(IntToText(iAdjZoneNumbers(indxAdjZone))) // '/'
            iCount                  = iCount + 2
        END DO
        IF (ZBudget%Header%lStorages_Defined) THEN
            cDSSPathNames(NDataColumns) = '/IWFM_ZBUD/ZONE:' // TRIM(IntToText(iZone)) // '/VOLUME//' // TRIM(cPrintInterval) // '/ABS_STORAGE/'
            IF (ZBudget%Header%lComputeError) cDSSPathNames(NDataColumns-1) = '/IWFM_ZBUD/ZONE:' // TRIM(IntToText(iZone)) // '/VOLUME//' // TRIM(cPrintInterval) // '/DISCREPANCY/'
        ELSE
            IF (ZBudget%Header%lComputeError) cDSSPathNames(NDataColumns) = '/IWFM_ZBUD/ZONE:' // TRIM(IntToText(iZone)) // '/VOLUME//' // TRIM(cPrintInterval) // '/DISCREPANCY/'
        END IF
        
        !Number of print dates
        CALL CTimeStep_To_RTimeStep(cPrintInterval,rDummy,PrintDeltaT_InMinutes)
        NPrint = NPeriods(PrintDeltaT_InMinutes,cPrintBeginDateAndTime,cPrintEndDateAndTime) + 1

        CALL OutFile%SetParametersForDSSFile(PathNames           = cDSSPathNames            , &
                                             DataUnit            = cDataUnits               , &
                                             DataType            = cDataTypes               , &
                                             SimulationStartTime = cPrintBeginDateAndTime   , &    
                                             NTimeSteps          = NPrint                   , &    
                                             NColumnsOfData      = NDataColumns             , &
                                             NRowsOfData         = 1                        , &
                                             iStat               = iStat                    )
    END IF
    
    
  CONTAINS
   
  
    ! #############################################################
    ! ### PRINT ZONE TITLES TO ASCII FILE
    ! #############################################################
    SUBROUTINE PrintASCIITitles(iStat)
      INTEGER,INTENT(OUT) :: iStat
    
      !Local variables
      INTEGER                                                    :: iLoc,indxFlowID,iFlowID,indxAdjZone,iNDash,indxTitle,ErrorCode
      CHARACTER(:),ALLOCATABLE                                   :: cText
      CHARACTER(LEN=ZBudget%Header%ASCIIOutput%iLenColumnTitles) :: cTempColumnTitles(ZBudget%Header%ASCIIOutput%iNTitles)
      CHARACTER(LEN=ZBudget%Header%ASCIIOutput%iLenTitles)       :: cPersistentTitles(3)
      
      !Initialize
      iStat  = 0
      iNDash = ZBudget%Header%ASCIIOutput%iLenColumnTitles + 2*NAdjZones*16
      IF (ZBudget%Header%lComputeError)     iNDash = iNDash + 16
      IF (ZBudget%Header%lStorages_Defined) iNDash = iNDash + 16
      
      !Print persistent titles (first three lines)
      CALL ZBudget%GetTitleLines(iZone,Area,cZoneName,ZBudget%Header%ASCIIOutput%iLenTitles,cUnit_AR,cUnit_VL,cPersistentTitles)
      CALL OutFile%WriteData(cPersistentTitles)
      
      !Fourth line
      CALL OutFile%WriteData(REPEAT('-',iNDash))
      
      !Next two line (not included if no adjacent zones)
      IF (NAdjZones .GT. 0) THEN
          CALL OutFile%WriteData(REPEAT(' ',ZBudget%Header%ASCIIOutput%iLenColumnTitles+2)//ArrangeText('Zone Exchange Flows',2*NAdjZones*16-2))
          CALL OutFile%WriteData(REPEAT(' ',ZBudget%Header%ASCIIOutput%iLenColumnTitles+2)//REPEAT('-',2*NAdjZones*16-2))
      END IF
      
      !Column titles
      CALL ReplaceMarkers(ZBudget%Header%ASCIIOutput%cColumnTitles,cUnit_AR,cUnit_VL,cTempColumnTitles,iStat)  ;  IF (iStat .EQ. -1) RETURN
      DO indxTitle=1,ZBudget%Header%ASCIIOutput%iNTitles-3
          CALL OutFile%WriteData(TRIM(cTempColumnTitles(indxTitle)))
      END DO          
      ALLOCATE (CHARACTER(LEN=iNDash) :: cText)
      DO indxTitle=ZBudget%Header%ASCIIOutput%iNTitles-2,ZBudget%Header%ASCIIOutput%iNTitles
          cText = REPEAT(" ",iNDash)
          cText(1:LEN(cTempColumnTitles(indxTitle))) = cTempColumnTitles(indxTitle)
          IF (indxTitle .EQ. ZBudget%Header%ASCIIOutput%iNTitles-2) THEN
             iLoc = ZBudget%Header%ASCIIOutput%iLenColumnTitles + 2
              DO indxAdjZone=1,NAdjZones
                  cText(iLoc+1:iLoc+30) = ArrangeText('Zones '//TRIM(IntToText(iZone))//' and '//TRIM(IntToText(iAdjZoneNumbers(indxAdjZone))),30)
                  iLoc                  = iLoc + 32
              END DO
          ELSEIF (indxTitle .EQ. ZBudget%Header%ASCIIOutput%iNTitles-1) THEN
              iLoc = ZBudget%Header%ASCIIOutput%iLenColumnTitles + 2
              DO indxAdjZone=1,NAdjZones
                  cText(iLoc+1:iLoc+30) = '            IN             OUT'
                  iLoc                  = iLoc + 32
              END DO
              IF (ZBudget%Header%lComputeError) THEN
                  cText(iLoc+1:iLoc+14) = '   Discrepancy'
                  iLoc                  = iLoc + 16
              END IF
              IF (ZBudget%Header%lStorages_Defined) THEN
                  cText(iLoc+1:iLoc+13) = '     Absolute'
              END IF
          ELSE
              iLoc = ZBudget%Header%ASCIIOutput%iLenColumnTitles + 2
              DO indxAdjZone=1,NAdjZones
                  cText(iLoc+1:iLoc+30) = '           (+)             (-)'
                  iLoc                  = iLoc + 32
              END DO
              IF (ZBudget%Header%lComputeError) THEN
                  cText(iLoc+1:iLoc+14) = '      (=)     '
                  iLoc                  = iLoc + 16
              END IF
              IF (ZBudget%Header%lStorages_Defined) THEN
                  cText(iLoc+1:iLoc+13) = '     Storage '
              END IF
          END IF
          CALL OutFile%WriteData(TRIM(cText))
      END DO
      
      !Final line with dashes under titles
      CALL OutFile%WriteData(REPEAT('-',iNDash))
      
      !Clear memory
      DEALLOCATE (cText ,STAT=ErrorCode)
            
    END SUBROUTINE PrintASCIITitles
    
  END SUBROUTINE PrepareZoneFlowOutFile
  
  
  ! -------------------------------------------------------------
  ! --- COMPILE FLOWS FOR A ZONE
  ! -------------------------------------------------------------
  SUBROUTINE CompileZoneData(ZBudget,ZoneList,iPrintZones,cBeginDateAndTime,iNOutputIntervals,rFact_AR,rFact_VL,iStat)
    TYPE(ZBudgetType)           :: ZBudget
    TYPE(ZoneListType)          :: ZoneList
    CHARACTER(LEN=*),INTENT(IN) :: cBeginDateAndTime
    INTEGER,INTENT(IN)          :: iPrintZones(:),iNOutputIntervals
    REAL(8),INTENT(IN)          :: rFact_AR,rFact_VL
    INTEGER,INTENT(OUT)         :: iStat
    
    !Local data type to keep LWU data information
    TYPE LWUGroupType
        INTEGER             :: iMaxIndex               = 0
        INTEGER             :: iAgSupplyReqDataIndex   = 0
        INTEGER             :: iAgShortDataIndex       = 0
        INTEGER             :: iAgPumpDataIndex        = 0
        INTEGER             :: iAgDiverDataIndex       = 0
        INTEGER             :: iAgOtherInflowDataIndex = 0
        REAL(8),ALLOCATABLE :: rAgSupplyReqRead(:,:)
        REAL(8),ALLOCATABLE :: rAgShortRead(:,:)
        REAL(8),ALLOCATABLE :: rAgPumpRead(:,:)
        REAL(8),ALLOCATABLE :: rAgDiverRead(:,:)
        REAL(8),ALLOCATABLE :: rAgOtherInflowRead(:,:)
    END TYPE LWUGroupType
    
    !Local variables
    INTEGER             :: indxLayer,iDataset,ErrorCode,indxElem,iElem,iCol,indxZone,iZone,indxLWUGroup,indxS,     &
                           NLayers,iNDataColumns,NElements,indxData,iNPrintZones,NLWUGroups,iLWUGroup
    TYPE(LWUGroupType)  :: LWUGroup(3)
    LOGICAL             :: lStorages_Defined,lFaceFlows_Defined
    REAL(8),ALLOCATABLE :: FlowsRead(:,:)
    INTEGER,PARAMETER   :: iDummyDataIndex = 1
    CLASS(*),POINTER    :: pZone
    
    !Initialize
    iStat              = 0
    NElements          = ZBudget%SystemData%NElements
    NLayers            = ZBudget%SystemData%NLayers
    iNDataColumns      = ZBudget%Header%iNData
    lStorages_Defined  = ZBudget%Header%lStorages_Defined
    lFaceFlows_Defined = ZBudget%Header%lFaceFlows_Defined
    iNPrintZones       = SIZE(iPrintZones)
    NLWUGroups         = 0
    
    !Zero out zone flows
    DO indxZone=1,iNPrintZones
        iZone = iPrintZones(indxZone)
        pZone => ZoneList%GetPointerToNode(iZone)
        SELECT TYPE (pZone)
            TYPE IS (ZoneType)
                !Zero out zone flows
                CALL pZone%ZeroOutFlows()
        END SELECT
    END DO
    
    !If this is Land & Water Use ZBudget, get the data index number for ag supply requirement and ag supply shortage and number of LWU groups
    indxS = 1
    DO indxLWUGroup=1,SIZE(LWUGroup)
        ASSOCIATE (pLWUGroup => LWUGroup(indxLWUGroup))
            pLWUGroup%iAgSupplyReqDataIndex   = LocateInList(f_iVR_lwu_AgSupplyReq,ZBudget%Header%iDataTypes(indxS:)) + indxS - 1
            pLWUGroup%iAgShortDataIndex       = LocateInList(f_iVR_lwu_AgShort,ZBudget%Header%iDataTypes(indxS:)) + indxS - 1
            pLWUGroup%iAgPumpDataIndex        = LocateInList(f_iVR_lwu_AgPump,ZBudget%Header%iDataTypes(indxS:)) + indxS - 1
            pLWUGroup%iAgDiverDataIndex       = LocateInList(f_iVR_lwu_AgDiv,ZBudget%Header%iDataTypes(indxS:)) + indxS - 1
            pLWUGroup%iAgOtherInflowDataIndex = LocateInList(f_iVR_lwu_AgOthIn,ZBudget%Header%iDataTypes(indxS:)) + indxS - 1
            IF (pLWUGroup%iAgSupplyReqDataIndex .EQ. 0) EXIT
            NLWUGroups          = NLWUGroups + 1
            pLWUGroup%iMaxIndex = MAX(pLWUGroup%iAgSupplyReqDataIndex  , &
                                      pLWUGroup%iAgShortDataIndex      , &
                                      pLWUGroup%iAgPumpDataIndex       , &
                                      pLWUGroup%iAgDiverDataIndex      , &
                                      pLWUGroup%iAgOtherInflowDataIndex) 
            indxS               = pLWUGroup%iMaxIndex + 1
        END ASSOCIATE
    END DO
    
    DO indxLayer=1,NLayers
        !Allocate memory for LWU ag supply requirement data, ag supply shortage data and read them
        DO indxLWUGroup=1,NLWUGroups
            ASSOCIATE (pLWUGroup => LWUGroup(indxLWUGroup))
                DEALLOCATE (pLWUGroup%rAgSupplyReqRead , pLWUGroup%rAgShortRead , pLWUGroup%rAgPumpRead , pLWUGroup%rAgDiverRead , pLWUGroup%rAgOtherInflowRead , STAT=ErrorCode)
                ALLOCATE (pLWUGroup%rAgSupplyReqRead(ZBudget%Header%iNDataElems(pLWUGroup%iAgSupplyReqDataIndex,indxLayer),iNOutputIntervals)     , &
                          pLWUGroup%rAgShortRead(ZBudget%Header%iNDataElems(pLWUGroup%iAgShortDataIndex,indxLayer),iNOutputIntervals)             , &
                          pLWUGroup%rAgPumpRead(ZBudget%Header%iNDataElems(pLWUGroup%iAgPumpDataIndex,indxLayer),iNOutputIntervals)               , &
                          pLWUGroup%rAgDiverRead(ZBudget%Header%iNDataElems(pLWUGroup%iAgDiverDataIndex,indxLayer),iNOutputIntervals)             , &
                          pLWUGroup%rAgOtherInflowRead(ZBudget%Header%iNDataElems(pLWUGroup%iAgOtherInflowDataIndex,indxLayer),iNOutputIntervals) )

                !Ag supply requirement
                iDataset = DatasetIndex(f_iElemDataType,NLayers,lFaceFlows_Defined,iNDataColumns,pLWUGroup%iAgSupplyReqDataIndex,indxLayer)
                CALL ZBudget%File%ReadData(cBeginDateAndTime,iDataset,pLWUGroup%rAgSupplyReqRead,ErrorCode,iStat)  ;  IF (iStat .EQ. -1) RETURN

                !Ag pumping
                iDataset = DatasetIndex(f_iElemDataType,NLayers,lFaceFlows_Defined,iNDataColumns,pLWUGroup%iAgPumpDataIndex,indxLayer)
                CALL ZBudget%File%ReadData(cBeginDateAndTime,iDataset,pLWUGroup%rAgPumpRead,ErrorCode,iStat)  ;  IF (iStat .EQ. -1) RETURN
                
                !Ag diversion
                iDataset = DatasetIndex(f_iElemDataType,NLayers,lFaceFlows_Defined,iNDataColumns,pLWUGroup%iAgDiverDataIndex,indxLayer)
                CALL ZBudget%File%ReadData(cBeginDateAndTime,iDataset,pLWUGroup%rAgDiverRead,ErrorCode,iStat)  ;  IF (iStat .EQ. -1) RETURN
                
                !Ag surface inflows from upstream elements
                iDataset = DatasetIndex(f_iElemDataType,NLayers,lFaceFlows_Defined,iNDataColumns,pLWUGroup%iAgOtherInflowDataIndex,indxLayer)
                CALL ZBudget%File%ReadData(cBeginDateAndTime,iDataset,pLWUGroup%rAgOtherInflowRead,ErrorCode,iStat)  ;  IF (iStat .EQ. -1) RETURN
                
                !Ag short
                iDataset = DatasetIndex(f_iElemDataType,NLayers,lFaceFlows_Defined,iNDataColumns,pLWUGroup%iAgShortDataIndex,indxLayer)
                CALL ZBudget%File%ReadData(cBeginDateAndTime,iDataset,pLWUGroup%rAgShortRead,ErrorCode,iStat)  ;  IF (iStat .EQ. -1) RETURN
            END ASSOCIATE
        END DO
        
        !Read element data and aggregate them for the zones
        iLWUGroup = 1
        DO indxData=1,iNDataColumns
            !If there are no elements with this type of data, cycle
            IF (ZBudget%Header%iNDataElems(indxData,indxLayer) .EQ. 0) CYCLE
            
            !Which LWU group is this (if there are any)?
            IF (iLWUGroup .LT. NLWUGroups ) THEN
                IF (indxData .GT. LWUGroup(iLWUGroup)%iMaxIndex) iLWUGroup = iLWUGroup + 1
            END IF
            
            !Allocate memory to read data for all elements
            DEALLOCATE (FlowsRead , STAT=ErrorCode)
            ALLOCATE (FlowsRead(ZBudget%Header%iNDataElems(indxData,indxLayer),iNOutputIntervals))
            
            !Read data
            iDataset = DatasetIndex(f_iElemDataType,NLayers,lFaceFlows_Defined,iNDataColumns,indxData,indxLayer)
            CALL ZBudget%File%ReadData(cBeginDateAndTime,iDataset,FlowsRead,ErrorCode,iStat)  ;  IF (iStat .EQ. -1) RETURN
            
            !Aggregate
            DO indxZone=1,iNPrintZones
                iZone =  iPrintZones(indxZone)
                pZone => ZoneList%GetPointerToNode(iZone)
                SELECT TYPE (pZone)
                    TYPE IS (ZoneType)
                        DO indxElem=1,SIZE(pZone%LayerZoneElements(indxLayer)%Elements)
                            iElem = pZone%LayerZoneElements(indxLayer)%Elements(indxElem)
                            iCol  = ZBudget%Header%iElemDataColumns(iElem,indxData,indxLayer)
                            IF (iCol .LT. 1) CYCLE
                            CALL AccumulateData(FlowsRead(iCol,:),ZBudget%Header%iDataTypes(indxData),rFact_AR,rFact_VL,pZone%AccumulatedData(indxData))
                        END DO
                END SELECT
            END DO
        END DO
        
        !If necessary read vertical flows and aggregate them as vertical flows between adjacent zones
        IF (NLayers .GT. 0) THEN                                     !Process vertical flows if there are more than 1 layer
            IF (ZoneList%iZoneExtent .EQ. f_iZoneVertical) THEN        !                   ... if the zonation is different in vertical 
                IF (indxLayer .LT. NLayers) THEN                     !                   ... if we are not processing the last layer
                    IF (ZBudget%Header%lVertFlows_DefinedAtNode) THEN
                        CALL CompileVerticalFlows_DefinedAtNode(indxLayer,iStat)
                    ELSE
                        CALL CompileVerticalFlows_DefinedAtElement(indxLayer,iStat)
                    END IF
                    IF (iStat .EQ. -1) RETURN
                END IF
            END IF
        END IF

        !If necessary read face flows and aggregate them as horizontal flows between adjacent zones
        IF (lFaceFlows_Defined) THEN
            CALL CompileFaceFlows(indxLayer,iStat)
            IF (iStat .EQ. -1) RETURN
        END IF
        
        !If necessary read storage and aggregate them for the zone
        IF (lStorages_Defined) THEN
            CALL CompileStorage(indxLayer,iStat)  
            IF (iStat .EQ. -1) RETURN
        END IF
        
    END DO
        
    
  CONTAINS
  
  
    !#######################################################################
    !### ACCUMULATE DATA BASED ON DATA TYPE
    !#######################################################################
    SUBROUTINE AccumulateData(rData,iDataType,rFact_AR,rFact_VL,AccumulatedData)
      REAL(8),INTENT(IN) :: rData(:),rFact_AR,rFact_VL
      INTEGER,INTENT(IN) :: iDataType
      REAL(8)            :: AccumulatedData
      
      !Local variables
      INTEGER :: indxTime,iElemColAgSupplyReq,iElemColAgShort,iElemColAgPump,iElemColAgDiver,iElemColAgOtherInflow
      REAL(8) :: rAgSupReq_Modified,rAgShortPrevious
      
      SELECT CASE (iDataType)
          CASE (f_iVR , f_iVR_lwu_AgPump , f_iVR_lwu_AgDiv , f_iVR_lwu_AgOthIn)
              AccumulatedData = AccumulatedData + SUM(rData) * rFact_VL
              
          CASE (f_iAR)
              AccumulatedData = AccumulatedData + rData(iNOutputIntervals) * rFact_AR
              
          CASE (f_iVLB)
              AccumulatedData = AccumulatedData + rData(1) * rFact_VL
              
          CASE (f_iVLE)
              AccumulatedData = AccumulatedData + rData(iNOutputIntervals) * rFact_VL
              
          CASE (f_iVR_lwu_PotCUAW)
              IF (iNOutputIntervals .EQ. 1) THEN
                  AccumulatedData = AccumulatedData + rData(iNOutputIntervals) * rFact_VL
              ELSE
                  !Special accumulation method
                  iElemColAgSupplyReq = ZBudget%Header%iElemDataColumns(iElem,LWUGroup(iLWUGroup)%iAgSupplyReqDataIndex,indxLayer)
                  iElemColAgShort     = ZBudget%Header%iElemDataColumns(iElem,LWUGroup(iLWUGroup)%iAgShortDataIndex,indxLayer)
                  rAgShortPrevious    = 0.0
                  DO indxTime=1,iNOutputIntervals
                      rAgSupReq_Modified = ModifiedAgSupplyReq(rAgShortPrevious,LWUGroup(iLWUGroup)%rAgSupplyReqRead(iElemColAgSupplyReq,indxTime))                      
                      IF (LWUGroup(iLWUGroup)%rAgSupplyReqRead(iElemColAgSupplyReq,indxTime) .NE. 0.0)  &
                          AccumulatedData = AccumulatedData + rAgSupReq_Modified * rData(indxTime) / LWUGroup(iLWUGroup)%rAgSupplyReqRead(iElemColAgSupplyReq,indxTime) * rFact_VL
                      rAgShortPrevious = LWUGroup(iLWUGroup)%rAgShortRead(iElemColAgShort,indxTime)
                  END DO
              END IF

          CASE (f_iVR_lwu_AgSupplyReq)
              IF (iNOutputIntervals .EQ. 1) THEN
                  AccumulatedData = AccumulatedData + rData(iNOutputIntervals) * rFact_VL
              ELSE
                  !Special accumulation method 
                  iElemColAgSupplyReq = ZBudget%Header%iElemDataColumns(iElem,LWUGroup(iLWUGroup)%iAgSupplyReqDataIndex,indxLayer)
                  iElemColAgShort     = ZBudget%Header%iElemDataColumns(iElem,LWUGroup(iLWUGroup)%iAgShortDataIndex,indxLayer)
                  rAgShortPrevious    = 0.0
                  DO indxTime=1,iNOutputIntervals
                      rAgSupReq_Modified = ModifiedAgSupplyReq(rAgShortPrevious,LWUGroup(iLWUGroup)%rAgSupplyReqRead(iElemColAgSupplyReq,indxTime))                      
                      AccumulatedData    = AccumulatedData + rAgSupReq_Modified * rFact_VL
                      rAgShortPrevious   = LWUGroup(iLWUGroup)%rAgShortRead(iElemColAgShort,indxTime)
                  END DO
              END IF
              
          CASE (f_iVR_lwu_AgShort)
              IF (iNOutputIntervals .EQ. 1) THEN
                  AccumulatedData = AccumulatedData + rData(iNOutputIntervals) * rFact_VL
              ELSE
                  !Special accumulation method 
                  iElemColAgSupplyReq   = ZBudget%Header%iElemDataColumns(iElem,LWUGroup(iLWUGroup)%iAgSupplyReqDataIndex,indxLayer)
                  iElemColAgShort       = ZBudget%Header%iElemDataColumns(iElem,LWUGroup(iLWUGroup)%iAgShortDataIndex,indxLayer)
                  iElemColAgPump        = ZBudget%Header%iElemDataColumns(iElem,LWUGroup(iLWUGroup)%iAgPumpDataIndex,indxLayer)
                  iElemColAgDiver       = ZBudget%Header%iElemDataColumns(iElem,LWUGroup(iLWUGroup)%iAgDiverDataIndex,indxLayer)
                  iElemColAgOtherInflow = ZBudget%Header%iElemDataColumns(iElem,LWUGroup(iLWUGroup)%iAgOtherInflowDataIndex,indxLayer)
                  rAgShortPrevious      = 0.0
                  DO indxTime=1,iNOutputIntervals
                      rAgSupReq_Modified = ModifiedAgSupplyReq(rAgShortPrevious,LWUGroup(iLWUGroup)%rAgSupplyReqRead(iElemColAgSupplyReq,indxTime))                      
                      AccumulatedData    = AccumulatedData + (rAgSupReq_Modified - LWUGroup(iLWUGroup)%rAgPumpRead(iElemColAgPump,indxTime) - LWUGroup(iLWUGroup)%rAgDiverRead(iElemColAgDiver,indxTime)) * rFact_VL
                      rAgShortPrevious   = LWUGroup(iLWUGroup)%rAgShortRead(iElemColAgShort,indxTime)
                  END DO
                  IF (iElemColAgOtherInflow .GT. 0) THEN
                      AccumulatedData = AccumulatedData - SUM(LWUGroup(iLWUGroup)%rAgOtherInflowRead(iElemColAgOtherInflow,:)) * rFact_VL
                  END IF
              END IF
      END SELECT
      
    END SUBROUTINE AccumulateData
  
    
    !#######################################################################
    !### READ AND AGGREGATE VERTICAL FLOWS DEFINED AT NODES
    !#######################################################################
    SUBROUTINE CompileVerticalFlows_DefinedAtNode(iLayer,iStat)
      INTEGER,INTENT(IN)  :: iLayer
      INTEGER,INTENT(OUT) :: iStat
      
      !Local variables
      INTEGER          :: iDataset,ErrorCode,indxElem,NVertex,Vertex(4),indxVertex,iActiveLayerBelow,iAdjZone, &
                          iNode,iZone,iLocZone,iLocAdjZone
      REAL(8)          :: VertFlowsRead(ZBudget%SystemData%NNodes,iNOutputIntervals),rVertFlowSum_PLUS,rVertFlowSum_NEGATIVE
      CLASS(*),POINTER :: pZone,pAdjZone
      
      !Read vertical flows
      iDataset = DatasetIndex(f_iVerticalFlowType,NLayers,lFaceFlows_Defined,iNDataColumns,iDummyDataIndex,iLayer)
      CALL ZBudget%File%ReadData(cBeginDateAndTime,iDataset,VertFlowsRead,ErrorCode,iStat)  ;  IF (iStat .EQ. -1) RETURN
      
      !Process vertical flows
      DO indxElem=1,ZBudget%SystemData%NElements
          iZone   = ZoneList%ElemZones(indxElem,iLayer)
          NVertex = ZBudget%SystemData%iElementNNodes(indxElem)
          Vertex  = ZBudget%SystemData%iElementNodes(:,indxElem)
          DO indxVertex=1,NVertex
              iNode             = Vertex(indxVertex)
              iActiveLayerBelow = ZBudget%SystemData%iActiveLayerBelow(iNode,iLayer)
              IF (iActiveLayerBelow .GT. 0) THEN
                  iAdjZone = ZoneList%ElemZones(indxElem,iActiveLayerBelow)
                  !If both zones are the same no flows between different zones; cycle
                  IF (iAdjZone .EQ. iZone) CYCLE
                  !If both zones are not asked for z-budget compilation, cycle
                  iLocZone    = LocateInList(iZone,iPrintZones)
                  iLocAdjZone = LocateInList(iAdjZone,iPrintZones)
                  IF (iLocZone .EQ. 0) THEN
                      IF (iLocAdjZone .EQ. 0) CYCLE
                  END IF
                  !Calculate total upward vertical flows 
                  rVertFlowSum_PLUS     = SUM(VertFlowsRead(iNode,:) , MASK=VertFlowsRead(iNode,:).GT.0.0) * ZBudget%SystemData%rElementNodeAreas(indxVertex,indxElem) / ZBudget%SystemData%rNodeAreas(iNode) * rFact_VL
                  !Calculate total downward vertical flows 
                  rVertFlowSum_NEGATIVE = SUM(VertFlowsRead(iNode,:) , MASK=VertFlowsRead(iNode,:).LT.0.0) * ZBudget%SystemData%rElementNodeAreas(indxVertex,indxElem) / ZBudget%SystemData%rNodeAreas(iNode) * rFact_VL
                  !Add flows to zone that the element belongs
                  IF (iLocZone .GT. 0) THEN
                      pZone => ZoneList%GetPointerToNode(iZone)
                      SELECT TYPE (pZone)
                          TYPE IS (ZoneType)
                              CALL pZone%AddAdjacentZoneFlow(iAdjZone,rVertFlowSum_PLUS)
                              CALL pZone%AddAdjacentZoneFlow(iAdjZone,rVertFlowSum_NEGATIVE)                              
                      END SELECT
                  END IF
                  !Add flows to adjacent zone that is below the current element
                  IF (iLocAdjZone .GT. 0) THEN
                      pZone => ZoneList%GetPointerToNode(iAdjZone)
                      SELECT TYPE (pZone)
                          TYPE IS (ZoneType)
                              CALL pZone%AddAdjacentZoneFlow(iZone,-rVertFlowSum_PLUS)
                              CALL pZone%AddAdjacentZoneFlow(iZone,-rVertFlowSum_NEGATIVE)                              
                      END SELECT
                  END IF
              END IF
          END DO
      END DO
            
    END SUBROUTINE CompileVerticalFlows_DefinedAtNode
      
      
    !#######################################################################
    !### READ AND AGGREGATE VERTICAL FLOWS DEFINED AT ELEMENTS
    !#######################################################################
    SUBROUTINE CompileVerticalFlows_DefinedAtElement(iLayer,iStat)
      INTEGER,INTENT(IN)  :: iLayer
      INTEGER,INTENT(OUT) :: iStat
      
      !Local variables
      INTEGER :: iDataset,ErrorCode,indxElem,iZone,iAdjZone,iLocZone,iLocAdjZone
      REAL(8) :: VertFlowsRead(ZBudget%SystemData%NElements,iNOutputIntervals),rVertFlowSum_UP,rVertFlowSum_DOWN
      
      !Read vertical flows
      iDataset = DatasetIndex(f_iVerticalFlowType,NLayers,lFaceFlows_Defined,iNDataColumns,iDummyDataIndex,iLayer)
      CALL ZBudget%File%ReadData(cBeginDateAndTime,iDataset,VertFlowsRead,ErrorCode,iStat)  ;  IF (iStat .EQ. -1) RETURN
      
      !Process vertical flows
      DO indxElem=1,ZBudget%SystemData%NElements
          iZone    = ZoneList%ElemZones(indxElem,iLayer)
          iAdjZone = ZoneList%ElemZones(indxElem,iLayer+1)
          
          !Cycle if both elements belong to the same zone
          IF (iZone .EQ. iAdjZone) CYCLE
          
          !If both zones are not asked for z-budget compilation, cycle
          iLocZone    = LocateInList(iZone,iPrintZones)
          iLocAdjZone = LocateInList(iAdjZone,iPrintZones)
          IF (iLocZone .EQ. 0) THEN
              IF (iLocAdjZone .EQ. 0) CYCLE
          END IF
          
          !Calculate total upward vertical flows 
          rVertFlowSum_UP = SUM(VertFlowsRead(indxElem,:) , MASK=VertFlowsRead(indxElem,:).GT.0.0) * rFact_VL
          
          !Calculate total downward vertical flows 
          rVertFlowSum_DOWN = SUM(VertFlowsRead(indxElem,:) , MASK=VertFlowsRead(indxElem,:).LT.0.0) * rFact_VL
          
          !Add flows to zone that the element belongs
          IF (iLocZone .GT. 0) THEN
              pZone => ZoneList%GetPointerToNode(iZone)
              SELECT TYPE (pZone)
                  TYPE IS (ZoneType)
                      CALL pZone%AddAdjacentZoneFlow(iAdjZone,rVertFlowSum_UP)
                      CALL pZone%AddAdjacentZoneFlow(iAdjZone,rVertFlowSum_DOWN)                              
              END SELECT
          END IF
          
          !Add flows to adjacent zone that is below the current element
          IF (iLocAdjZone .GT. 0) THEN
              pZone => ZoneList%GetPointerToNode(iAdjZone)
              SELECT TYPE (pZone)
                  TYPE IS (ZoneType)
                      CALL pZone%AddAdjacentZoneFlow(iZone,-rVertFlowSum_UP)
                      CALL pZone%AddAdjacentZoneFlow(iZone,-rVertFlowSum_DOWN)                              
              END SELECT
          END IF
      END DO

    END SUBROUTINE CompileVerticalFlows_DefinedAtElement
    
    
    !#######################################################################
    !### READ AND AGGREGATE HORIZONTAL FLOW EXCHANGE BETWEEN ZONE AND ADJACENT ZONES
    !#######################################################################
    SUBROUTINE CompileFaceFlows(iLayer,iStat)
      INTEGER,INTENT(IN)  :: iLayer
      INTEGER,INTENT(OUT) :: iStat
      
      !Local variables
      INTEGER             :: indxElem,iDataset,iElem,ErrorCode,indxAdjZone,iAdjZone,indxFace,iFace,iElem1,iElem2,iZone,indxZone
      REAL(8)             :: FaceFlowsRead(ZBudget%SystemData%NFaces,iNOutputIntervals),FaceFlowSum_PLUS,FaceFlowSum_NEGATIVE
      INTEGER,ALLOCATABLE :: iFaceList(:)
      CLASS(*),POINTER    :: pAdjZone
      
      !Read face flows
      iDataset = DatasetIndex(f_iFaceFlowType,NLayers,lFaceFlows_Defined,iNDataColumns,iDummyDataIndex,iLayer)
      CALL ZBudget%File%ReadData(cBeginDateAndTime,iDataset,FaceFlowsRead,ErrorCode,iStat)  ;  IF (iStat .EQ. -1) RETURN
      
      !Aggregate face flows
      DO indxZone=1,iNPrintZones
          iZone =  iPrintZones(indxZone)
          pZone => ZoneList%GetPointerToNode(iZone)
          SELECT TYPE (pZone)
              TYPE IS (ZoneType)
                  !If no elements in this layer belong to this zone, cycle
                  IF (SIZE(pZone%LayerZoneElements(iLayer)%Elements) .EQ. 0) CYCLE
                  DO indxAdjZone=1,pZone%NAdjacentZones
                      iAdjZone =  pZone%AdjacentZoneNumbers(indxAdjZone)
                      pAdjZone => pZone%AdjacentZoneList%GetPointerToNode(iAdjZone)
                      SELECT TYPE (pAdjZone)
                          TYPE IS (AdjacentZoneType)
                              IF (.NOT. ASSOCIATED(pAdjZone%iFaceNumberList)) CYCLE
                              CALL pAdjZone%iFaceNumberList%GetOrderedKeyList(iFaceList)
                              DO indxFace=1,SIZE(iFaceList)
                                  iFace  = iFaceList(indxFace)
                                  iElem1 = ZBudget%SystemData%iFaceElems(1,iFace)
                                  iElem2 = ZBudget%SystemData%iFaceElems(2,iFace)
                                  !Determine the direction of flow based on elements and sign of the flow
                                  !First element is in zone being considered (then second element is in adjacent zone)
                                  IF (ZoneList%ElemZones(iElem1,iLayer) .EQ. iZone) THEN 
                                      !Is the second element in adjacent zone?
                                      IF (ZoneList%ElemZones(iElem2,iLayer) .EQ. iAdjZone) THEN
                                          FaceFlowSum_PLUS     = SUM(FaceFlowsRead(iFace,:) , MASK=FaceFlowsRead(iFace,:).GT.0.0) * rFact_VL
                                          FaceFlowSum_NEGATIVE = SUM(FaceFlowsRead(iFace,:) , MASK=FaceFlowsRead(iFace,:).LT.0.0) * rFact_VL
                                          CALL pZone%AddAdjacentZoneFlow(iAdjZone,FaceFlowSum_PLUS)
                                          CALL pZone%AddAdjacentZoneFlow(iAdjZone,FaceFlowSum_NEGATIVE)
                                      END IF
                                      
                                  !Second element is in zone being considered (then first element is in adjacent zone)
                                  ELSE
                                      !Is the first element in adjacent zone?
                                      IF (ZoneList%ElemZones(iElem1,iLayer) .EQ. iAdjZone) THEN
                                          FaceFlowSum_PLUS     = SUM(FaceFlowsRead(iFace,:) , MASK=FaceFlowsRead(iFace,:).GT.0.0) * rFact_VL
                                          FaceFlowSum_NEGATIVE = SUM(FaceFlowsRead(iFace,:) , MASK=FaceFlowsRead(iFace,:).LT.0.0) * rFact_VL
                                          CALL pZone%AddAdjacentZoneFlow(iAdjZone,-FaceFlowSum_PLUS)
                                          CALL pZone%AddAdjacentZoneFlow(iAdjZone,-FaceFlowSum_NEGATIVE)
                                      END IF
                                  END IF
                              END DO
                      END SELECT
                  END DO
          END SELECT
      END DO 
      
    END SUBROUTINE CompileFaceFlows
    
    
    !#######################################################################
    !### READ AND USE THE STORAGE AT THE LAST TIMESTAMP (CAN'T AGGREGATE STORAGES SINCE IT IS NOT FLOW RATE)
    !#######################################################################
    SUBROUTINE CompileStorage(iLayer,iStat)
      INTEGER,INTENT(IN)  :: iLayer
      INTEGER,INTENT(OUT) :: iStat
      
      !Local variables
      INTEGER            :: iDataset,ErrorCode,NTimeSteps,iZone,indxZone
      REAL(8)            :: StorageRead(ZBudget%SystemData%NElements,1)
      CHARACTER          :: cStorageDateAndTime*f_iTimeStampLength
      TYPE(TimeStepType) :: TimeStep
      
      !Go to the last time of the interval
      CALL ZBudget%GetTimeStepRelatedData(NTimeSteps,TimeStep)
      cStorageDateAndTime = IncrementTimeStamp(cBeginDateAndTime,TimeStep%DeltaT_InMinutes,iNOutputIntervals-1)
      
      !Read storages at the end of period
      iDataset = DatasetIndex(f_iStorageType,NLayers,lFaceFlows_Defined,iNDataColumns,iDummyDataIndex,iLayer)
      CALL ZBudget%File%ReadData(cStorageDateAndTime,iDataset,StorageRead,ErrorCode,iStat)  ;  IF (iStat .EQ. -1) RETURN
      
      !Accumulate storages for the zone
      DO indxZone=1,iNPrintZones
          iZone =  iPrintZones(indxZone)
          pZone => ZoneList%GetPointerToNode(iZone)
          SELECT TYPE (pZone)
              TYPE IS (ZoneType)
                  pZone%Storage = pZone%Storage + SUM(StorageRead(pZone%LayerZoneElements(iLayer)%Elements,1)) * rFact_VL
          END SELECT
      END DO
      
    END SUBROUTINE CompileStorage

  END SUBROUTINE CompileZoneData
  
  
  ! -------------------------------------------------------------
  ! --- CALCULATE DATASET INDEX IN THE HDF FILE FOR A FLOW TYPE
  ! -------------------------------------------------------------
  PURE FUNCTION DatasetIndex(iFlowType,NLayers,lFaceFlows_Defined,iNDataColumns,iData,iLayer) RESULT(iDataset)
    INTEGER,INTENT(IN) :: iFlowType,NLayers,iNDataColumns,iData,iLayer
    LOGICAL,INTENT(IN) :: lFaceFlows_Defined
    INTEGER            :: iDataset
    
    SELECT CASE (iFlowType)
        CASE (f_iElemDataType)
            iDataset = (iLayer-1) * iNDataColumns + iData
            
        CASE (f_iVerticalFlowType)
            iDataset = NLayers * iNDataColumns + iLayer 

        CASE (f_iFaceFlowType)
            iDataset = NLayers * iNDataColumns                     !Element data
            IF (NLayers .GT. 0) iDataset = iDataset + NLayers - 1  !Vertical flows
            iDataset = iDataset + iLayer
            
        CASE (f_iStorageType)
            iDataset = NLayers * iNDataColumns                     !Element data
            IF (NLayers .GT. 0) iDataset = iDataset + NLayers - 1  !Vertical flows
            IF (lFaceFlows_Defined) iDataset = iDataset + NLayers  !Face flows
            iDataset = iDataset + iLayer
            
    END SELECT

  END FUNCTION DatasetIndex
  
  
  ! -------------------------------------------------------------
  ! --- ADJUST Z-BUDGET PRINT BEGIN AND END DATES BASED ON Z-BUDGET DATA 
  ! -------------------------------------------------------------
  SUBROUTINE AdjustPrintDates(TimeStep,NTimeSteps,cPrintBeginDateAndTime,cPrintEndDateAndTime,cAdjPrintBeginDateAndTime,cAdjPrintEndDAteAndTime)
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    INTEGER,INTENT(IN)            :: NTimeSteps
    CHARACTER(LEN=*),INTENT(IN)   :: cPrintBeginDateAndTime,cPrintEndDateAndTime
    CHARACTER(LEN=*),INTENT(OUT)  :: cAdjPrintBeginDateAndTime,cAdjPrintEndDateAndTime
    
    IF (cPrintBeginDateAndTime .TSLT. TimeStep%CurrentDateAndTime) THEN
        cAdjPrintBeginDateAndTime = TimeStep%CurrentDateAndTime
    ELSE
        cAdjPrintBeginDateAndTime = cPrintBeginDateAndTime
    END IF
    cAdjPrintEndDateAndTime = IncrementTimeStamp(TimeStep%CurrentDateAndTime,TimeStep%DeltaT_InMinutes,NTimeSteps-1)
    IF (cPrintEndDateAndTime .TSLT. cAdjPrintEndDateAndTime) cAdjPrintEndDateAndTime = cPrintEndDateAndTime
    
  END SUBROUTINE AdjustPrintDates
  
  
  ! -------------------------------------------------------------
  ! --- CALCULATE INTERVALS BETWEEN TWO PRINT-OUT DATES 
  ! -------------------------------------------------------------
  FUNCTION OutputIntervals(DeltaT_InMinutes,cCurrentDateAndTime,cPrintInterval,cDescriptor) RESULT(iNIntervals)
    INTEGER,INTENT(IN)            :: DeltaT_InMinutes
    CHARACTER(LEN=*),INTENT(IN)   :: cCurrentDateAndTime,cPrintInterval,cDescriptor
    INTEGER                       :: iNIntervals
  
    !LOcal variables
    CHARACTER(LEN=ModNameLen+15),PARAMETER :: ThisProcedure = ModName // 'OutputIntervals'
    INTEGER                                :: iPrintDeltaT_InMinutes
    REAL(8)                                :: rDummy
    CHARACTER(LEN=f_iTimeStampLength)      :: cEndDateTime,cDateZero
    
    
    IF (cPrintInterval .EQ. '') THEN
        iNIntervals = 1
    ELSE
        CALL CTimeStep_To_RTimeStep(TRIM(cPrintInterval),rDummy,iPrintDeltaT_InMinutes)
        IF (iPrintDeltaT_InMinutes .LT. DeltaT_InMinutes) THEN
            MessageArray(1) = 'Z-Budget output interval cannot be less than the simulation timestep!'
            MessageArray(2) = 'Adjusting the output interval to be equal to the simulation timestep for '//TRIM(cDescriptor)//'.'
            CALL LogMessage(MessageArray(1:2),f_iWarn,ThisProcedure)
            iPrintDeltaT_InMinutes = DeltaT_InMinutes
        END IF
        cDateZero    = IncrementTimeStamp(cCurrentDateAndTime,DeltaT_InMinutes,-1)  ! This method of calculation includes the cCurrentDateandTime in
        cEndDateTime = IncrementTimeStamp(cDateZero,iPrintDeltaT_InMinutes,1)       ! the calculation of intervals
        iNIntervals  = NPeriods(DELTAT_InMinutes,cDateZero,cEndDateTime)           !
    END IF

  END FUNCTION OutputIntervals
  
  
  ! -------------------------------------------------------------
  ! --- REPLACE SPECIALS MARKERS IN ASCII COLUMN TITLES WITH APPROPRIATE DATA
  ! -------------------------------------------------------------
  SUBROUTINE ReplaceMarkers(cColumnTitles,cUnit_AR,cUnit_VL,cModColumnTitles,iStat)
    CHARACTER(LEN=*),INTENT(IN) :: cColumnTitles(:),cUnit_AR,cUnit_VL
    CHARACTER(LEN=*)            :: cModColumnTitles(:)
    INTEGER,INTENT(OUT)         :: iStat
    
    !Local variables
    INTEGER                  :: iLocS,iLocE,indxTitle,iLen,iLenWork,iLenUnit,iLocMarker,ErrorCode,indx
    CHARACTER(:),ALLOCATABLE :: cWorkText,cWorkText1
    
    !Initialize
    iStat            = 0
    cModColumnTitles = cColumnTitles
    
    !Search for consecutive marker characters
    DO indxTitle=1,SIZE(cColumnTitles)
        iLen = LEN_TRIM(cColumnTitles(indxTitle))
        DO
            iLocS = FirstLocation(f_cMarkerChar,cModColumnTitles(indxTitle))
            IF (iLocS .EQ. 0) EXIT
            iLocE    = FirstLocation(f_cMarkerChar,cModColumnTitles(indxTitle)(iLocS+1:iLen)) + iLocS
            iLenWork = iLocE - iLocS - 1
            IF (iLenWork .LT. 1) CYCLE
            DEALLOCATE (cWorkText , cWorkText1 , STAT=ErrorCode)
            ALLOCATE (CHARACTER(LEN=iLenWork) :: cWorkText , cWorkText1)
            cWorkText = cModColumnTitles(indxTitle)(iLocS+1:iLocE-1)  
            !Replace area unit marker
            IF (FirstLocation(f_cAreaUnitMarker,cWorkText) .GT. 0) THEN
                iLenUnit                                     = LEN_TRIM(cUnit_AR)
                iLocMarker                                   = FirstLocation(f_cAreaUnitMarker,cWorkText)
                indx                                         = iLocMarker - iLenUnit
                cWorkText1(1:indx)                           = cWorkText(iLenUnit:iLocMarker-1)
                cWorkText1(indx+1:indx+iLenUnit)             = TRIM(cUnit_AR)
                indx                                         = indx + iLenUnit
                cWorkText1(indx+1:iLenWork)                  = cWorkText(iLocMarker+1:iLenWork)
                cModColumnTitles(indxTitle)(iLocS+1:iLocE-1) = cWorkText1
                CALL ReplaceString(cModColumnTitles(indxTitle)(iLocS:iLocE),f_cMarkerChar,' ',iStat)  
                IF (iStat .EQ. -1) RETURN
            !Replace volume unit marker
            ELSE IF (FirstLocation(f_cVolumeUnitMarker,cWorkText) .GT. 0) THEN
                iLenUnit                                     = LEN_TRIM(cUnit_VL)
                iLocMarker                                   = FirstLocation(f_cVolumeUnitMarker,cWorkText)
                indx                                         = iLocMarker - iLenUnit
                cWorkText1(1:indx)                           = cWorkText(iLenUnit:iLocMarker-1)
                cWorkText1(indx+1:indx+iLenUnit)             = TRIM(cUnit_VL)
                indx                                         = indx + iLenUnit
                cWorkText1(indx+1:iLenWork)                  = cWorkText(iLocMarker+1:iLenWork)
                cModColumnTitles(indxTitle)(iLocS+1:iLocE-1) = cWorkText1
                CALL ReplaceString(cModColumnTitles(indxTitle)(iLocS:iLocE),f_cMarkerChar,' ',iStat)  
                IF (iStat .EQ. -1) RETURN
            END IF
        END DO
    END DO
    
  END SUBROUTINE ReplaceMarkers
  

END MODULE