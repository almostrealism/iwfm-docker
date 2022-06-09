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
MODULE IWFM_ZBudget_Exports
  USE,INTRINSIC :: ISO_C_BINDING , ONLY: C_INT                   , &
                                         C_DOUBLE                , &
                                         C_CHAR                  
  USE MessageLogger              , ONLY: SetLastMessage          , &
                                         f_iFatal
  USE TimeSeriesUtilities        , ONLY: TimeStepType            , &
                                         IncrementTimeStamp      , &
                                         f_iTimeStampLength
  USE GeneralUtilities           , ONLY: LocateInList            , &
                                         IntToText               , &
                                         String_Copy_C_F         , &
                                         String_Copy_F_C
  USE Package_ZBudget            , ONLY: ZBudgetType             , &
                                         ZoneListType            , &
                                         ZoneType                , &
                                         f_iColumnHeaderLen      , &
                                         f_iUndefinedZone        , &
                                         Abstract_CallbackFun
  IMPLICIT NONE
  
  
  ! -------------------------------------------------------------
  ! --- PUBLIC VARIABLES
  ! -------------------------------------------------------------
  PUBLIC

  
  ! -------------------------------------------------------------
  ! --- VARIABLES
  ! -------------------------------------------------------------
  TYPE(ZBudgetType),SAVE  :: ZBudget
  TYPE(ZoneListType),SAVE :: ZoneList 
  LOGICAL,SAVE            :: lZBudget_Instantiated = .FALSE.
  
  
  ! -------------------------------------------------------------
  ! --- MISC. ENTITIES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                   :: ModNameLen = 22
  CHARACTER(LEN=ModNameLen),PARAMETER :: ModName    = 'IWFM_ZBudget_Exports::'
  

CONTAINS


    

! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** CONSTRUCTOR
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************
    
  ! -------------------------------------------------------------
  ! --- OPEN Z-BUDGET HDF5 FILE
  ! -------------------------------------------------------------
  SUBROUTINE IW_ZBudget_OpenFile(cFileName,iLen,iStat) BIND(C,NAME='IW_ZBudget_OpenFile')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_ZBudget_OpenFile
    INTEGER(C_INT),INTENT(IN)    :: iLen
    CHARACTER(C_CHAR),INTENT(IN) :: cFileName(iLen)
    INTEGER(C_INT),INTENT(OUT)   :: iStat
    
    !Local variables
    CHARACTER :: cFileName_F*iLen
    
    CALL String_Copy_C_F(cFileName,cFileName_F)
    
    !If a Z-Budget file is already open, close it
    IF (lZBudget_Instantiated) THEN
      CALL ZBudget%Kill()
      lZBudget_Instantiated = .FALSE.
    END IF
    
    !Open file
    CALL ZBudget%New(cFileName_F,iStat)
    IF (iStat .EQ. -1) THEN
        CALL ZBudget%Kill()
        RETURN
    END IF
    lZBudget_Instantiated = .TRUE.
    
  END SUBROUTINE IW_ZBudget_OpenFile
  
  
  ! -------------------------------------------------------------
  ! --- GENERATE ZONE LIST FROM AN ASCII FILE
  ! --- Note: Assumes ZBudget is already instantiated
  ! -------------------------------------------------------------
  SUBROUTINE IW_ZBudget_GenerateZoneList_FromFile(cFileName,iLen,iStat) BIND(C,NAME='IW_ZBudget_GenerateZoneList_FromFile')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_ZBudget_GenerateZoneList_FromFile
    INTEGER(C_INT),INTENT(IN)    :: iLen
    CHARACTER(C_CHAR),INTENT(IN) :: cFileName(iLen)
    INTEGER(C_INT),INTENT(OUT)   :: iStat
  
    !Local variables
    CHARACTER :: cFileName_F*iLen
    
    CALL String_Copy_C_F(cFileName,cFileName_F)
    
    !First, kill zone list for clean start
    CALL ZoneList%Kill()
    
    !Then create the zone list
    CALL ZoneList%New(ZBudget%Header%iNData,ZBudget%Header%lFaceFlows_Defined,ZBudget%SystemData,TRIM(cFileName_F),iStat)
    
  END SUBROUTINE IW_ZBudget_GenerateZoneList_FromFile
  
  
  ! -------------------------------------------------------------
  ! --- GENERATE ZONE LIST WITH DATA PROVIDED
  ! --- Note: Assumes ZBudget is already instantiated
  ! -------------------------------------------------------------
  SUBROUTINE IW_ZBudget_GenerateZoneList(iZExtent,iNElems,iElems,iLayers,iZones,nZonesWithNames,iZonesWithNames,iLenZoneNames,cZoneNames,iLocArray,iStat) BIND(C,NAME='IW_ZBudget_GenerateZoneList')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_ZBudget_GenerateZoneList
    INTEGER(C_INT),INTENT(IN)    :: iZExtent,iNElems,nZonesWithNames,iElems(iNElems),iLayers(iNElems),iZones(iNElems),iZonesWithNames(nZonesWithNames),iLenZoneNames,iLocArray(nZonesWithNames)
    CHARACTER(C_CHAR),INTENT(IN) :: cZoneNames(iLenZoneNames)
    INTEGER(C_INT),INTENT(OUT)   :: iStat
    
    !LOcal variables
    INTEGER                      :: indx
    CHARACTER(LEN=50)            :: cZoneNamesArray(nZonesWithNames)
    CHARACTER(LEN=iLenZoneNames) :: cZoneNames_F
    
    !First, kill zone list for clean start
    CALL ZoneList%Kill()
    
    !Convert zone names from C to Fortran
    CALL String_Copy_C_F(cZoneNames,cZoneNames_F)
    
    !Turn the zone names into array
    DO indx=1,nZonesWithNames-1
        cZoneNamesArray(indx) = cZoneNames_F(iLocArray(indx):iLocArray(indx+1)-1)
    END DO
    cZoneNamesArray(nZonesWithNames) = cZoneNames_F(iLocArray(nZonesWithNames):iLenZoneNames)
    
    !Then create the zone list
    CALL ZoneList%New(ZBudget%Header%iNData,ZBudget%Header%lFaceFlows_Defined,ZBudget%SystemData,iZExtent,iElems,iLayers,iZones,iZonesWithNames,cZoneNamesArray,iStat)
    
  END SUBROUTINE IW_ZBudget_GenerateZoneList

  
  
  
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
  ! --- CLOSE Z-BUDGET HDF5 FILE
  ! -------------------------------------------------------------
  SUBROUTINE IW_ZBudget_CloseFile(iStat) BIND(C,NAME='IW_ZBudget_CloseFile')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_ZBudget_CloseFile
    INTEGER(C_INT),INTENT(OUT) :: iStat

    iStat = 0
    
    CALL ZBudget%Kill()
    
  END SUBROUTINE IW_ZBudget_CloseFile
  


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
  ! --- GET THE Z-BUDGET DATA FOR A SET OF ZONES
  ! --- Note: iNDiversifiedReadColsMax must count the Time column
  ! ---       iDiversifiedReadCols must include the Time column as column no. 1
  ! -------------------------------------------------------------
  SUBROUTINE IW_ZBudget_GetValues_ForSomeZones_ForAnInterval(iNZones,iZones,iNDiversifiedReadColsMax,iDiversifiedReadCols,cDateAndTimeBegin,iLenDateAndTime,cOutputInterval,iLenInterval,rFact_AR,rFact_VL,rValues,iStat) BIND(C,NAME='IW_ZBudget_GetValues_ForSomeZones_ForAnInterval') !) 
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_ZBudget_GetValues_ForSomeZones_ForAnInterval
    INTEGER(C_INT),INTENT(IN)    :: iNZones,iZones(iNZones),iNDiversifiedReadColsMax,iDiversifiedReadCols(iNDiversifiedReadColsMax,iNZones),iLenDateAndTime,iLenInterval
    CHARACTER(C_CHAR),INTENT(IN) :: cDateAndTimeBegin(iLenDateAndTime),cOutputInterval(iLenInterval)
    REAL(C_DOUBLE),INTENT(IN)    :: rFact_AR,rFact_VL
    REAL(C_DOUBLE),INTENT(OUT)   :: rValues(iNDiversifiedReadColsMax,iNZones)
    INTEGER(C_INT),INTENT(OUT)   :: iStat
    
    !!Local variables
    CHARACTER :: cDateAndTimeBegin_F*iLenDateAndTime,cOutputInterval_F*iLenInterval
    INTEGER   :: iDiversifiedReadCols_Work(iNDiversifiedReadColsMax-1,iNZones)
    
    !C strings to Fortran strings
    CALL String_Copy_C_F(cDateAndTimeBegin,cDateAndTimeBegin_F)
    CALL String_Copy_C_F(cOutputInterval,cOutputInterval_F)
    
    !Remove time column from columns to be read (ZBudget does not expect Time column as one of the columns to read) and decrease column numbers by 1
    iDiversifiedReadCols_Work = iDiversifiedReadCols(2:,:) -1
    
    !Read data 
    CALL ZBudget%ReadData(ZoneList,iZones,iDiversifiedReadCols_Work,cOutputInterval_F,cDateAndTimeBegin_F,rFact_AR,rFact_VL,rValues,iStat)
    
    !Convert Julian time to Excel-style Julian time
    rValues(1,:) = rValues(1,:) - 2415020d0
    
  END SUBROUTINE IW_ZBudget_GetValues_ForSomeZones_ForAnInterval
  
    
  ! -------------------------------------------------------------
  ! --- GET THE Z-BUDGET DATA FOR A ZONE
  ! --- Note: iNDiversifiedReadCols must count the Time column
  ! ---       iDiversifiedReadCols must include the Time column as column no. 1
  ! -------------------------------------------------------------
  SUBROUTINE IW_ZBudget_GetValues_ForAZone(iZone,iNDiversifiedReadCols,iDiversifiedReadCols,cDateAndTimeBegin,cDateAndTimeEnd,iLenDateAndTime,cOutputInterval,iLenInterval,rFact_AR,rFact_VL,iNTimes_In,rValues,iNTimes_Out,iStat) BIND(C,NAME='IW_ZBudget_GetValues_ForAZone')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_ZBudget_GetValues_ForAZone
    INTEGER(C_INT),INTENT(IN)    :: iZone,iNDiversifiedReadCols,iDiversifiedReadCols(iNDiversifiedReadCols),iLenDateAndTime,iLenInterval,iNTimes_In
    CHARACTER(C_CHAR),INTENT(IN) :: cDateAndTimeBegin(iLenDateAndTime),cDateAndTimeEnd(iLenDateAndTime),cOutputInterval(iLenInterval)
    REAL(C_DOUBLE),INTENT(IN)    :: rFact_AR,rFact_VL
    REAL(C_DOUBLE),INTENT(OUT)   :: rValues(iNDiversifiedReadCols,iNTimes_In)
    INTEGER(C_INT),INTENT(OUT)   :: iNTimes_Out,iStat
    
    !Local variables
    CHARACTER :: cDateAndTimeBegin_F*iLenDateAndTime,cDateAndTimeEnd_F*iLenDateAndTime,cOutputInterval_F*iLenInterval
    INTEGER   :: iDummyArray(iNDiversifiedReadCols-1)
    
    !C strings to Fortran strings
    CALL String_Copy_C_F(cDateAndTimeBegin,cDateAndTimeBegin_F)
    CALL String_Copy_C_F(cDateAndTimeEnd,cDateAndTimeEnd_F)
    CALL String_Copy_C_F(cOutputInterval,cOutputInterval_F)
    
    !Read data (iDiversifiedReadCols is modified according to how ZBudget expecting it: Time column is ignored so column numbers are decresed by 1)
    CALL ZBudget%ReadData(ZoneList,iZone,iDiversifiedReadCols(2:)-1,cOutputInterval_F,cDateAndTimeBegin_F,cDateAndTimeEnd_F,rFact_AR,rFact_VL,iDummyArray,iNTimes_Out,rValues,iStat)

    !Convert Julian time to Excel-style Julian time
    rValues(1,:) = rValues(1,:) - 2415020d0
    
  END SUBROUTINE IW_ZBudget_GetValues_ForAZone
  
    
  ! -------------------------------------------------------------
  ! --- GET THE Z-BUDGET DATA FOR A LOCATION
  ! --- Note: iNDiversifiedReadCols must count the Time column
  ! ---       iDiversifiedReadCols must include the Time column as column no. 1
  ! -------------------------------------------------------------
  SUBROUTINE IW_ZBudget_GetValues_WithCallback(pCallbackFun_C,iZone,iNDiversifiedReadCols,iDiversifiedReadCols,cDateAndTimeBegin,cDateAndTimeEnd,iLenDateAndTime,cOutputInterval,iLenInterval,rFact_AR,rFact_VL,iNTimes_In,rValues,iNTimes_Out,iStat) BIND(C,NAME='IW_ZBudget_GetValues_WithCallback')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_ZBudget_GetValues_WithCallback
    PROCEDURE(Abstract_CallbackFun) :: pCallbackFun_C
    INTEGER(C_INT),INTENT(IN)       :: iZone,iNDiversifiedReadCols,iDiversifiedReadCols(iNDiversifiedReadCols),iLenDateAndTime,iLenInterval,iNTimes_In
    CHARACTER(C_CHAR),INTENT(IN)    :: cDateAndTimeBegin(iLenDateAndTime),cDateAndTimeEnd(iLenDateAndTime),cOutputInterval(iLenInterval)
    REAL(C_DOUBLE),INTENT(IN)       :: rFact_AR,rFact_VL
    REAL(C_DOUBLE),INTENT(OUT)      :: rValues(iNDiversifiedReadCols,iNTimes_In)
    INTEGER(C_INT),INTENT(OUT)      :: iNTimes_Out,iStat
    
    !Local variables
    CHARACTER                               :: cDateAndTimeBegin_F*iLenDateAndTime,cDateAndTimeEnd_F*iLenDateAndTime,cOutputInterval_F*iLenInterval
    INTEGER                                 :: iDummyArray(iNDiversifiedReadCols-1)
    PROCEDURE(Abstract_CallbackFun),POINTER :: pCallbackFun_F
    
    !Fortran function pointer to callback function
    pCallbackFun_F => pCallBackFun_C
    
    !C strings to Fortran strings
    CALL String_Copy_C_F(cDateAndTimeBegin,cDateAndTimeBegin_F)
    CALL String_Copy_C_F(cDateAndTimeEnd,cDateAndTimeEnd_F)
    CALL String_Copy_C_F(cOutputInterval,cOutputInterval_F)
    
    !Read data (iDiversifiedReadCols is modified according to how ZBudget expecting it: Time column is ignored so column numbers are decresed by 1)
    CALL ZBudget%ReadData(ZoneList,iZone,iDiversifiedReadCols(2:)-1,cOutputInterval_F,cDateAndTimeBegin_F,cDateAndTimeEnd_F,rFact_AR,rFact_VL,iDummyArray,iNTimes_Out,rValues,iStat,pCallbackFun_F)

    !Convert Julian time to Excel-style Julian time
    rValues(1,:) = rValues(1,:) - 2415020d0
    
    !Clear pointer from memory
    NULLIFY(pCallbackFun_F)
    
  END SUBROUTINE IW_ZBudget_GetValues_WithCallback
  
    
  ! -------------------------------------------------------------
  ! --- GET NUMBER OF TIME STEPS
  ! -------------------------------------------------------------
  SUBROUTINE IW_ZBudget_GetNTimeSteps(NTimeSteps,iStat) BIND(C,NAME='IW_ZBudget_GetNTimeSteps')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_ZBudget_GetNTimeSteps
    INTEGER(C_INT),INTENT(OUT) :: NTimeSteps,iStat
    
    !Local variables
    TYPE(TimeStepType) :: TimeStep
    
    iStat = 0
    
    CALL ZBudget%GetTimeStepRelatedData(NTimeSteps,TimeStep)
    
  END SUBROUTINE IW_ZBudget_GetNTimeSteps
 
  
  ! -------------------------------------------------------------
  ! --- GET DATA BEGIN DATE-TIME, END DATE-TIME AND INTERVAL
  ! -------------------------------------------------------------
  SUBROUTINE IW_ZBudget_GetTimeSpecs(cDataDatesAndTimes,iLenDates,cInterval,iLenInterval,NData,iLocArray,iStat) BIND(C,NAME='IW_ZBudget_GetTimeSpecs')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_ZBudget_GetTimeSpecs
    INTEGER(C_INT),INTENT(IN)     :: iLenDates,iLenInterval,NData
    CHARACTER(C_CHAR),INTENT(OUT) :: cDataDatesAndTimes(iLenDates)
    CHARACTER(C_CHAR),INTENT(OUT) :: cInterval(iLenInterval)
    INTEGER(C_INT),INTENT(OUT)    :: iLocArray(NData),iStat
    
    !Local variables
    TYPE(TimeStepType) :: TimeStep
    CHARACTER          :: cDateAndTime*f_iTimeStampLength,cDataDatesAndTimes_F*iLenDates,cInterval_F*iLenInterval
    INTEGER            :: indx,NTimeSteps
    
    !Initialize
    iStat                = 0
    cDataDatesAndTimes_F = ''
    
    !Get the time step data
    CALL ZBudget%GetTimeStepRelatedData(NTimeSteps,TimeStep)
    
    !Data interval
    cInterval_F = TimeStep%Unit
    
    !First entry
    cDataDatesAndTimes_F = TimeStep%CurrentDateAndTime
    iLocArray(1)         = 1
    
    !Compile data
    cDateAndTime = TimeStep%CurrentDateAndTime
    DO indx=2,NData
        iLocArray(indx)      = LEN_TRIM(cDataDatesAndTimes_F) + 1
        cDateAndTime         = IncrementTimeStamp(cDateAndTime,TimeStep%DeltaT_InMinutes,1)
        cDataDatesAndTimes_F = TRIM(cDataDatesAndTimes_F) // TRIM(cDateAndTime)
    END DO
    
    !Fortran strings to C strings
    CALL String_Copy_F_C(cDataDatesAndTimes_F,cDataDatesAndTimes)
    CALL String_Copy_F_C(cInterval_F,cInterval)
        
  END SUBROUTINE IW_ZBudget_GetTimeSpecs

  
  ! -------------------------------------------------------------
  ! --- GET FULL DATA COLUMN HEADERS FOR ANY ZONE (INFLOW/OUTFLOW BETWEEN ZONES ARE SHOWN AS LUMPED) 
  ! -------------------------------------------------------------
  SUBROUTINE IW_ZBudget_GetColumnHeaders_General(NColumnsMax,AreaUnit,VolumeUnit,iLenUnit,iLenColumnHeaders,cColumnHeaders,NColumns,iLocArray,iStat) BIND(C,NAME='IW_ZBudget_GetColumnHeaders_General')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_ZBudget_GetColumnHeaders_General
    INTEGER(C_INT),INTENT(IN)     :: iLenColumnHeaders,NColumnsMax,iLenUnit
    CHARACTER(C_CHAR),INTENT(IN)  :: AreaUnit(iLenUnit),VolumeUnit(iLenUnit)
    CHARACTER(C_CHAR),INTENT(OUT) :: cColumnHeaders(iLenColumnHeaders)
    INTEGER(C_INT),INTENT(OUT)    :: NColumns,iLocArray(NColumnsMax),iStat
    
    !Local variables
    INTEGER                                       :: indx
    CHARACTER(LEN=f_iColumnHeaderLen),ALLOCATABLE :: cHeaders_Work(:)
    CHARACTER                                     :: AreaUnit_F*iLenUnit,VolumeUnit_F*iLenUnit,cColumnHeaders_F*iLenColumnHeaders
    
    !Initialize
    iStat            = 0
    cColumnHeaders_F = ''
    CALL String_Copy_C_F(AreaUnit,AreaUnit_F)
    CALL String_Copy_C_F(VolumeUnit,VolumeUnit_F)    
    
    !Get the column headers
    CALL ZBudget%GetFullColumnHeaders(AreaUnit_F,VolumeUnit_F,cHeaders_Work,iStat=iStat)
    IF (iStat .EQ. -1) THEN
        CALL ZBudget%Kill()
        RETURN
    END IF
    
    !Number of columns
    NColumns = SIZE(cHeaders_Work) 
    
    !Compile column headers into return variable
    DO indx=1,NColumns
        iLocArray(indx)  = LEN_TRIM(cColumnHeaders_F) + 1
        cColumnHeaders_F = TRIM(cColumnHeaders_F) // ADJUSTL(cHeaders_Work(indx))
    END DO
    
    !Fortran string to C string
    CALL String_Copy_F_C(cColumnHeaders_F,cColumnHeaders)
    
  END SUBROUTINE IW_ZBudget_GetColumnHeaders_General
  
  
  ! -------------------------------------------------------------
  ! --- GET FULL DATA COLUMN HEADERS FOR ANY ZONE (INFLOW/OUTFLOW BETWEEN ZONES ARE SHOWN SEPARETELY FOR EACH NEIGHBORING ZONE) 
  ! -------------------------------------------------------------
  SUBROUTINE IW_ZBudget_GetColumnHeaders_ForAZone(iZone,NColumnsList,iColumnsList,NColumnsMax,AreaUnit,VolumeUnit,iLenUnit,iLenColumnHeaders,cColumnHeaders,NColumns,iLocArray,iColumnsListDiversified,iStat) BIND(C,NAME='IW_ZBudget_GetColumnHeaders_ForAZone')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_ZBudget_GetColumnHeaders_ForAZone
    INTEGER(C_INT),INTENT(IN)     :: iZone,iLenColumnHeaders,NColumnsMax,iLenUnit,NColumnsList,iColumnsList(NColumnsList)
    CHARACTER(C_CHAR),INTENT(IN)  :: AreaUnit(iLenUnit),VolumeUnit(iLenUnit)
    CHARACTER(C_CHAR),INTENT(OUT) :: cColumnHeaders(iLenColumnHeaders)
    INTEGER(C_INT),INTENT(OUT)    :: NColumns,iLocArray(NColumnsMax),iColumnsListDiversified(NColumnsMax),iStat
    
    !Local variables
    INTEGER                                       :: indx
    CHARACTER(LEN=f_iColumnHeaderLen),ALLOCATABLE :: cHeaders_Work(:)
    INTEGER,ALLOCATABLE                           :: iColumnsDiversfd_Local(:)
    CHARACTER                                     :: AreaUnit_F*iLenUnit,VolumeUnit_F*iLenUnit,cColumnHeaders_F*iLenColumnHeaders
    
    !Initialize
    iStat            = 0
    cColumnHeaders_F = ''
    CALL String_Copy_C_F(AreaUnit,AreaUnit_F)
    CALL String_Copy_C_F(VolumeUnit,VolumeUnit_F)    
    
    !Get the column headers
    CALL ZBudget%GetFullColumnHeaders(AreaUnit_F,VolumeUnit_F,cHeaders_Work,iStat,ZoneList,iZone,iColumnsList,iColumnsDiversfd_Local)
    IF (iStat .EQ. -1) THEN
        CALL ZBudget%Kill()
        RETURN
    END IF
    
    !Number of columns
    NColumns = SIZE(cHeaders_Work) 
    
    !List of diversified column indices
    iColumnsListDiversified(1:NColumns) = iColumnsDiversfd_Local
    
    !Compile column headers into return variable
    DO indx=1,NColumns
        iLocArray(indx)  = LEN_TRIM(cColumnHeaders_F) + 1
        cColumnHeaders_F = TRIM(cColumnHeaders_F) // ADJUSTL(cHeaders_Work(indx))
    END DO
    
    !Fortran string to C string
    CALL String_Copy_F_C(cColumnHeaders_F,cColumnHeaders)
    
  END SUBROUTINE IW_ZBudget_GetColumnHeaders_ForAZone
  
  
  ! -------------------------------------------------------------
  ! --- GET NUMBER OF ZONES
  ! -------------------------------------------------------------
  SUBROUTINE IW_ZBudget_GetNZones(iNZones,iStat) BIND(C,NAME='IW_ZBudget_GetNZones')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_ZBudget_GetNZones
    INTEGER(C_INT),INTENT(OUT) :: iNZones,iStat
    
    !Local variables
    INTEGER,ALLOCATABLE :: iZoneList(:)
    
    iStat = 0

    !Get the zone list
    CALL ZoneList%GetOrderedKeyList(iZoneList)
    
    !If there are no zones defined
    IF (SIZE(iZoneList) .EQ. 0) THEN
        iNZones = 0
        RETURN
    END IF
    
    !Do not count the undefined zone
    IF (LocateInList(f_iUndefinedZone,iZoneList) .GT. 0) THEN
        iNZones = SIZE(iZoneList) - 1
    ELSE
        iNZones = SIZE(iZoneList)
    END IF
    
  END SUBROUTINE IW_ZBudget_GetNZones
  

  ! -------------------------------------------------------------
  ! --- GET ZONE LIST
  ! --- Note: Assumes iNZones does not include the undefined zone number, -99
  ! -------------------------------------------------------------
  SUBROUTINE IW_ZBudget_GetZoneList(iNZones,iZoneList,iStat) BIND(C,NAME='IW_ZBudget_GetZoneList')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_ZBudget_GetZoneList
    INTEGER(C_INT),INTENT(IN)  :: iNZones
    INTEGER(C_INT),INTENT(OUT) :: iZoneList(iNZones),iStat
    
    !Local variables
    INTEGER             :: ErrorCode,iLoc
    INTEGER,ALLOCATABLE :: iZoneList_Local(:)
    
    !Initialize
    iStat = 0
    
    !Get the zone list
    CALL ZoneList%GetOrderedKeyList(iZoneList_Local)
    
    !Eliminate undefined zone from list
    iLoc = LocateInList(f_iUndefinedZone,iZoneList_Local)
    IF (iLoc .GT. 0) THEN
        iZoneList(1:iLoc-1)     = iZoneList_Local(1:iLoc-1)
        iZoneList(iLoc:iNZones) = iZoneList_Local(iLoc+1:)
    ELSE
        iZoneList = iZoneList_Local
    END IF
    
    DEALLOCATE (iZoneList_Local , STAT=ErrorCode)
    
  END SUBROUTINE IW_ZBudget_GetZoneList
  
  
  ! -------------------------------------------------------------
  ! --- GET ZONE NAMES
  ! -------------------------------------------------------------
  SUBROUTINE IW_ZBudget_GetZoneNames(iNZones,iLenZoneNames,cZoneNames,iLocArray,iStat) BIND(C,NAME='IW_ZBudget_GetZoneNames')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_ZBudget_GetZoneNames
    INTEGER(C_INT),INTENT(IN)     :: iNZones,iLenZoneNames
    CHARACTER(C_CHAR),INTENT(OUT) :: cZoneNames(iLenZoneNames)
    INTEGER(C_INT),INTENT(OUT)    :: iLocArray(iNZones),iStat
    
    !Local variables
    INTEGER                       :: indx,iCount
    CHARACTER(LEN=50),ALLOCATABLE :: cZoneNamesArray(:)
    CHARACTER                     :: cZoneNames_F*iLenZoneNames
    INTEGER,ALLOCATABLE           :: iZoneList_Local(:)
    
    !Initialize
    iStat        = 0
    cZoneNames_F = ''
    
    !Get the zone list
    CALL ZoneList%GetOrderedKeyList(iZoneList_Local)
    
    !Get names
    CALL ZoneList%GetNames(cZoneNamesArray)
    
    !Compile return variables but eliminate the undefined zone
    iCount = 0
    DO indx=1,SIZE(cZoneNamesArray)
        IF (iZoneList_Local(indx) .EQ. f_iUndefinedZone) CYCLE
        iCount            = iCount + 1
        iLocArray(iCount) = LEN_TRIM(cZoneNames_F) + 1
        cZoneNames_F      = TRIM(cZoneNames_F) // ADJUSTL(cZoneNamesArray(indx))
    END DO
    
    !Convert Fortran string to C string
    CALL String_Copy_F_C(cZoneNames_F,cZoneNames)
        
  END SUBROUTINE IW_ZBudget_GetZoneNames
  
  
  ! -------------------------------------------------------------
  ! --- GET NUMBER OF TITLES
  ! -------------------------------------------------------------
  SUBROUTINE IW_ZBudget_GetNTitleLines(iNTitles,iStat) BIND(C,NAME='IW_ZBudget_GetNTitleLines')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_ZBudget_GetNTitleLines
    INTEGER(C_INT),INTENT(OUT) :: iNTitles,iStat
    
    iStat    = 0
    iNTitles = ZBudget%GetNTitleLines()
    
  END SUBROUTINE IW_ZBudget_GetNTitleLines
  
  
  ! -------------------------------------------------------------
  ! --- GET TITLES
  ! -------------------------------------------------------------
  SUBROUTINE IW_ZBudget_GetTitleLines(iNTitles,iZone,rFact_AR,cUnit_AR,cUnit_VL,iLenUnit,cTitles,iLenTitles,iLocArray,iStat) BIND(C,NAME='IW_ZBudget_GetTitleLines')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_ZBudget_GetTitleLines
    INTEGER(C_INT),INTENT(IN)     :: iNTitles,iZone,iLenUnit,iLenTitles
    REAL(C_DOUBLE),INTENT(IN)     :: rFact_AR
    CHARACTER(C_CHAR),INTENT(IN)  :: cUnit_AR(iLenUnit),cUnit_VL(iLenUnit)
    CHARACTER(C_CHAR),INTENT(OUT) :: cTitles(iLenTitles)
    INTEGER(C_INT),INTENT(OUT)    :: iLocArray(iNTitles),iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+24),PARAMETER :: ThisProcedure = ModName // 'IW_ZBudget_GetTitleLines'
    INTEGER                                :: indx
    CHARACTER                              :: cUnit_AR_F*iLenUnit,cUnit_VL_F*iLenUnit,cTitles_Work(iNTitles)*(iLenTitles/iNTitles),cTitles_F*iLenTitles
    CLASS(*),POINTER                       :: pZone
    
    !Initialize
    iStat     = 0
    cTitles_F = ''
    cTitles   = ''
    
    !Convert units to Fortran strings
    CALL String_Copy_C_F(cUnit_AR, cUnit_AR_F)
    CALL String_Copy_C_F(cUnit_VL, cUnit_VL_F)
    
    !Get the titles
    pZone => ZoneList%GetPointerToNode(iZone)
    SELECT TYPE (pZone)
        TYPE IS (ZoneType)
            CALL ZBudget%GetTitleLines(iZone,pZone%Area*rFact_AR,pZone%cName,ZBudget%Header%ASCIIOutput%iLenTitles,cUnit_AR_F,cUnit_VL_F,cTitles_Work)
        CLASS DEFAULT
            CALL SetLastMessage(TRIM(IntToText(iZone)) // ' cannot be located in the zone list!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
    END SELECT
        
    !Compile return data
    DO indx=1,iNTitles
        iLocArray(indx) = LEN_TRIM(cTitles_F) + 1
        cTitles_F       = TRIM(cTitles_F) // TRIM(ADJUSTL(cTitles_Work(indx)))
    END DO
    
    !Convert Fortran string to C string
    CALL String_Copy_F_C(TRIM(cTitles_F),cTitles)
    
  END SUBROUTINE IW_ZBudget_GetTitleLines

  
END MODULE
