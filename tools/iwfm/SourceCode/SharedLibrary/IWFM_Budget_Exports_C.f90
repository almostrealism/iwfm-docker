!***********************************************************************
!  Integrated Water Flow Model (IWFM)
!  Copyright (C) 2005-2022  
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
MODULE IWFM_Budget_Exports
  USE,INTRINSIC :: ISO_C_BINDING , ONLY: C_INT                                   , &
                                         C_DOUBLE                                , &
                                         C_CHAR
  USE TimeSeriesUtilities        , ONLY: TimeStepType                            , &
                                         IncrementTimeStamp                      , & 
                                         f_iTimeStampLength                      
  USE GeneralUtilities           , ONLY: String_Copy_C_F                         , &
                                         String_Copy_F_C
  USE Package_Budget             , ONLY: BudgetType                              , &
                                         f_iMaxLocationNameLen                   , &
                                         f_iColumnHeaderLen
  IMPLICIT NONE
  

  ! -------------------------------------------------------------
  ! --- PUBLIC VARIABLES
  ! -------------------------------------------------------------
  PUBLIC

  
  ! -------------------------------------------------------------
  ! --- VARIABLES
  ! -------------------------------------------------------------
  TYPE(BudgetType),SAVE    :: Budget
  LOGICAL,SAVE             :: lBudget_Instantiated = .FALSE.
  
  
  

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
  ! --- OPEN BUDGET BINARY FILE
  ! -------------------------------------------------------------
  SUBROUTINE IW_Budget_OpenFile(cFileName,iLen,iStat) BIND(C,NAME='IW_Budget_OpenFile')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Budget_OpenFile
    INTEGER(C_INT),INTENT(IN)         :: iLen
    CHARACTER(KIND=C_CHAR),INTENT(IN) :: cFileName(iLen)
    INTEGER(C_INT),INTENT(OUT)        :: iStat
    
    !Local variables
    CHARACTER :: cFileName_F*iLen
    
    !Initialize
    iStat = 0
    
    CALL String_Copy_C_F(cFileName,cFileName_F)
    
    !If a budget file is already open, close it
    IF (lBudget_Instantiated) THEN
      CALL Budget%Kill()
      lBudget_Instantiated = .FALSE.
    END IF
    
    !Open file
    CALL Budget%New(cFileName_F,iStat)
    IF (iStat .EQ. -1) THEN
        CALL Budget%Kill()
        RETURN
    END IF
    lBudget_Instantiated = .TRUE.
    
  END SUBROUTINE IW_Budget_OpenFile
  
  
  
  
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
  ! --- CLOSE BUDGET BINARY FILE
  ! -------------------------------------------------------------
  SUBROUTINE IW_Budget_CloseFile(iStat) BIND(C,NAME='IW_Budget_CloseFile')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Budget_CloseFile
    INTEGER(C_INT),INTENT(OUT) :: iStat
    
    iStat = 0
    CALL Budget%Kill()
    
  END SUBROUTINE IW_Budget_CloseFile
  


  
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
  ! --- GET NUMBER OF BUDGET LOCATIONS FROM A BUDGET BINARY FILE
  ! -------------------------------------------------------------
  SUBROUTINE IW_Budget_GetNLocations(NLocations,iStat) BIND(C,NAME='IW_Budget_GetNLocations')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Budget_GetNLocations
    INTEGER(C_INT),INTENT(OUT) :: NLocations,iStat
    
    iStat = 0
    
    !Number of locations
    NLocations = Budget%GetNLocations()
    
  END SUBROUTINE IW_Budget_GetNLocations

  
  ! -------------------------------------------------------------
  ! --- GET BUDGET LOCATION NAMES FROM BUDGET BINARY FILE
  ! -------------------------------------------------------------
  SUBROUTINE IW_Budget_GetLocationNames(cLocNames,iLenLocNames,NLocations,iLocArray,iStat) BIND(C,NAME='IW_Budget_GetLocationNames')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Budget_GetLocationNames
    INTEGER(C_INT),INTENT(IN)          :: iLenLocNames,NLocations
    CHARACTER(KIND=C_CHAR),INTENT(OUT) :: cLocNames(iLenLocNames)
    INTEGER(C_INT),INTENT(OUT)         :: iLocArray(NLocations),iStat
    
    !Local variables
    INTEGER                              :: indx
    CHARACTER(LEN=f_iMaxLocationNameLen) :: cNames(NLocations)
    CHARACTER                            :: cLocNames_F*iLenLocNames
    
    !Initialize
    iStat        = 0
    cLocNames_F  = ''
    
    !Get the names
    cNames = Budget%GetLocationNames(NLocations)
    
    !Compile location names
    DO indx=1,NLocations
        iLocArray(indx) = LEN_TRIM(cLocNames_F) + 1
        cLocNames_F     = TRIM(cLocNames_F) // TRIM(cNames(indx)) 
    END DO
    
    !Fortran string to C string
    CALL String_Copy_F_C(cLocNames_F,cLocNames)
    
  END SUBROUTINE IW_Budget_GetLocationNames
  
  
  ! -------------------------------------------------------------
  ! --- GET NUMBER OF TIME STEPS
  ! -------------------------------------------------------------
  SUBROUTINE IW_Budget_GetNTimeSteps(NTimeSteps,iStat) BIND(C,NAME='IW_Budget_GetNTimeSteps')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Budget_GetNTimeSteps
    INTEGER(C_INT),INTENT(OUT) :: NTimeSteps,iStat
    
    iStat = 0
    NTimeSteps = Budget%GetNTimeSteps()
    
  END SUBROUTINE IW_Budget_GetNTimeSteps
 
  
  ! -------------------------------------------------------------
  ! --- GET DATA BEGIN DATE-TIME, END DATE-TIME, ALL TIMESTAMPS IN BETWEEN AND SIMULATION INTERVAL
  ! -------------------------------------------------------------
  SUBROUTINE IW_Budget_GetTimeSpecs(cDataDatesAndTimes,iLenDates,cInterval,iLenInterval,NData,iLocArray,iStat) BIND(C,NAME='IW_Budget_GetTimeSpecs')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Budget_GetTimeSpecs
    INTEGER(C_INT),INTENT(IN)          :: iLenDates,iLenInterval,NData
    CHARACTER(KIND=C_CHAR),INTENT(OUT) :: cDataDatesAndTimes(iLenDates)
    CHARACTER(KIND=C_CHAR),INTENT(OUT) :: cInterval(iLenInterval)
    INTEGER(C_INT),INTENT(OUT)         :: iLocArray(NData),iStat
    
    !Local variables
    TYPE(TimeStepType) :: TimeStep
    CHARACTER          :: cDateAndTime*f_iTimeStampLength,cDataDatesAndTimes_F*iLenDates,cInterval_F*iLenInterval
    INTEGER            :: indx
    
    !Initialize
    iStat                = 0
    cDataDatesAndTimes_F = ''
    
    !Get the time step data
    TimeStep = Budget%GetTimeSpecs()
    
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
        
  END SUBROUTINE IW_Budget_GetTimeSpecs

  
  
  ! -------------------------------------------------------------
  ! --- GET NUMBER OF BUDGET TITLE LINES
  ! -------------------------------------------------------------
  SUBROUTINE IW_Budget_GetNTitleLines(NTitles,iStat) BIND(C,NAME='IW_Budget_GetNTitleLines')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Budget_GetNTitleLines
    INTEGER(C_INT),INTENT(OUT) :: NTitles,iStat
    
    iStat = 0
    
    !Get the number of titles
    NTitles = Budget%GetNPersistentTitles()
    
  END SUBROUTINE IW_Budget_GetNTitleLines

  
  ! -------------------------------------------------------------
  ! --- GET BUDGET TITLE LENGTH
  ! -------------------------------------------------------------
  SUBROUTINE IW_Budget_GetTitleLength(iLen,iStat) BIND(C,NAME='IW_Budget_GetTitleLength')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Budget_GetTitleLength
    INTEGER(C_INT),INTENT(OUT) :: iLen,iStat
    
    iStat = 0
        
    !Get title length
    iLen = Budget%GetTitleLen()
    
  END SUBROUTINE IW_Budget_GetTitleLength

  
  ! -------------------------------------------------------------
  ! --- GET BUDGET TITLE LINES
  ! -------------------------------------------------------------
  SUBROUTINE IW_Budget_GetTitleLines(NTitles,iLocation,FactArea,LengthUnit,AreaUnit,VolumeUnit,iLenUnit,cAltLocName,iLenAltLocName,cTitles,iLenTitles,iLocArray,iStat) BIND(C,NAME='IW_Budget_GetTitleLines')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Budget_GetTitleLines
    INTEGER(C_INT),INTENT(IN)          :: NTitles,iLocation,iLenUnit,iLenTitles,iLenAltLocName  
    REAL(C_DOUBLE),INTENT(IN)          :: FactArea
    CHARACTER(KIND=C_CHAR),INTENT(IN)  :: LengthUnit(iLenUnit),AreaUnit(iLenUnit),VolumeUnit(iLenUnit) 
    CHARACTER(KIND=C_CHAR),INTENT(IN)  :: cAltLocName(iLenAltLocName)
    CHARACTER(KIND=C_CHAR),INTENT(OUT) :: cTitles(iLenTitles)    
    INTEGER(C_INT),INTENT(OUT)         :: iLocArray(NTitles),iStat 
    
    !Local variables
    CHARACTER(LEN=iLenTitles/NTitles) :: cTitles_Work(NTitles)
    CHARACTER                         :: LengthUnit_F*iLenUnit,AreaUnit_F*iLenUnit,VolumeUnit_F*iLenUnit,cAltLocName_F*iLenAltLocName,cTitles_F*iLenTitles 
    INTEGER                           :: indx
    
    !Initialize
    iStat     = 0
    cTitles_F = ''
    CALL String_Copy_C_F(LengthUnit,LengthUnit_F)
    CALL String_Copy_C_F(AreaUnit,AreaUnit_F)
    CALL String_Copy_C_F(VolumeUnit,VolumeUnit_F)
    CALL String_Copy_C_F(cAltLocName,cAltLocName_F)
    
    !Get titles
    cTitles_Work = Budget%GetPersistentTitles(NTitles)
    
    !Modify titles based on location name and output units
    IF (LEN_TRIM(cAltLocName_F) .EQ. 0) THEN
        CALL Budget%ModifyASCIITitles(iLocation,LengthUnit_F,AreaUnit_F,VolumeUnit_F,FactArea,cTitles_Work)
    ELSE
        CALL Budget%ModifyASCIITitles(iLocation,LengthUnit_F,AreaUnit_F,VolumeUnit_F,FactArea,cTitles_Work,TRIM(cAltLocName_F))
    END IF
    
    !Compile return data
    DO indx=1,NTitles
        iLocArray(indx) = LEN_TRIM(cTitles_F) + 1
        cTitles_F       = TRIM(cTitles_F) // TRIM(ADJUSTL(cTitles_Work(indx)))
    END DO
    
    !Fortran string to C string
    CALL String_Copy_F_C(cTitles_F,cTitles)
    
  END SUBROUTINE IW_Budget_GetTitleLines
  
  
  ! -------------------------------------------------------------
  ! --- GET NUMBER OF DATA COLUMNS FOR THE BUDGET LOCATION
  ! -------------------------------------------------------------
  SUBROUTINE IW_Budget_GetNColumns(iLoc,NColumns,iStat) BIND(C,NAME='IW_Budget_GetNColumns')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Budget_GetNColumns
    INTEGER(C_INT),INTENT(IN)  :: iLoc
    INTEGER(C_INT),INTENT(OUT) :: NColumns,iStat
    
    CALL Budget%GetNDataColumns(iLoc,NColumns,iStat)
    IF (iStat .EQ. -1) CALL Budget%Kill()
    
  END SUBROUTINE IW_Budget_GetNColumns
  

  ! -------------------------------------------------------------
  ! --- GET FULL DATA COLUMN HEADERS FOR THE BUDGET LOCATION
  ! -------------------------------------------------------------
  SUBROUTINE IW_Budget_GetColumnHeaders(iLoc,cColumnHeaders,iLenColumnHeaders,NColumns,LengthUnit,AreaUnit,VolumeUnit,iLenUnit,iLocArray,iStat) BIND(C,NAME='IW_Budget_GetColumnHeaders')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Budget_GetColumnHeaders
    INTEGER(C_INT),INTENT(IN)          :: iLoc,iLenColumnHeaders,NColumns,iLenUnit
    CHARACTER(KIND=C_CHAR),INTENT(IN)  :: LengthUnit(iLenUnit),AreaUnit(iLenUnit),VolumeUnit(iLenUnit)
    CHARACTER(KIND=C_CHAR),INTENT(OUT) :: cColumnHeaders(iLenColumnHeaders)
    INTEGER(C_INT),INTENT(OUT)         :: iLocArray(NColumns),iStat
    
    !Local variables
    INTEGER                           :: indx,NDataColumns
    CHARACTER(LEN=f_iColumnHeaderLen) :: cHeaders_Work(NColumns)
    CHARACTER                         :: LengthUnit_F*iLenUnit,AreaUnit_F*iLenUnit,VolumeUnit_F*iLenUnit,cColumnHeaders_F*iLenColumnHeaders
    
    !Initialize
    iStat            = 0
    cColumnHeaders_F = ''
    CALL Budget%GetNDataColumns(iLoc,NDataColumns,iStat)  
    IF (iStat .EQ. -1) THEN
        CALL Budget%Kill()
        RETURN
    END IF
    CALL String_Copy_C_F(LengthUnit,LengthUnit_F)
    CALL String_Copy_C_F(AreaUnit,AreaUnit_F)
    CALL String_Copy_C_F(VolumeUnit,VolumeUnit_F)    
    
    !Get the column headers
    cHeaders_Work = Budget%GetFullColumnHeaders(iLoc,NDataColumns)
    
    !Insert output units into column headers
    CALL Budget%ModifyFullColumnHeaders(LengthUnit_F,AreaUnit_F,VolumeUnit_F,cHeaders_Work)
    
    !Compile column headers into return variable
    DO indx=1,NColumns
        iLocArray(indx)  = LEN_TRIM(cColumnHeaders_F) + 1
        cColumnHeaders_F = TRIM(cColumnHeaders_F) // ADJUSTL(cHeaders_Work(indx))
    END DO
    
    !Fortran string to C string
    CALL String_Copy_F_C(cColumnHeaders_F,cColumnHeaders)
    
  END SUBROUTINE IW_Budget_GetColumnHeaders
  
  
  ! -------------------------------------------------------------
  ! --- GET THE BUDGET DATA FOR A LOCATION
  ! -------------------------------------------------------------
  SUBROUTINE IW_Budget_GetValues(iLoc,nReadCols,iReadCols,cDateAndTimeBegin,cDateAndTimeEnd,iLenDateAndTime,cOutputInterval,iLenInterval,rFact_LT,rFact_AR,rFact_VL,nTimes_In,Values,nTimes_Out,iStat) BIND(C,NAME='IW_Budget_GetValues')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Budget_GetValues
    INTEGER(C_INT),INTENT(IN)         :: iLoc,nReadCols,iReadCols(nReadCols),iLenDateAndTime,iLenInterval,nTimes_In
    CHARACTER(KIND=C_CHAR),INTENT(IN) :: cDateAndTimeBegin(iLenDateAndTime),cDateAndTimeEnd(iLenDateAndTime),cOutputInterval(iLenInterval)
    REAL(C_DOUBLE),INTENT(IN)         :: rFact_LT,rFact_AR,rFact_VL
    REAL(C_DOUBLE),INTENT(OUT)        :: Values(nReadCols+1,nTimes_In)
    INTEGER(C_INT),INTENT(OUT)        :: nTimes_Out,iStat
    
    !Local variables
    REAL(8)   :: rDummy
    CHARACTER :: cDateAndTimeBegin_F*iLenDateAndTime,cDateAndTimeEnd_F*iLenDateAndTime,cOutputInterval_F*iLenInterval
    
    !C strings to Fortran strings
    CALL String_Copy_C_F(cDateAndTimeBegin,cDateAndTimeBegin_F)
    CALL String_Copy_C_F(cDateAndTimeEnd,cDateAndTimeEnd_F)
    CALL String_Copy_C_F(cOutputInterval,cOutputInterval_F)
    
    !Read data
    CALL Budget%ReadData(iLoc,iReadCols,cOutputInterval_F,cDateAndTimeBegin_F,cDateAndTimeEnd_F,rDummy,rDummy,rDummy,rFact_LT,rFact_AR,rFact_VL,nTimes_Out,Values,iStat)
    IF (iStat .EQ. -1) THEN
        CALL Budget%Kill()
        RETURN
    END IF

    !Convert Julian time to Excel-style Julian time
    Values(1,:) = Values(1,:) - 2415020d0
    
  END SUBROUTINE IW_Budget_GetValues
  
    
  ! -------------------------------------------------------------
  ! --- GET THE BUDGET DATA FOR A LOCATION FOR A COLUMN FROM AN HDF FILE
  ! --- Note: Assumes Budget input file is an HDF file
  ! -------------------------------------------------------------
  SUBROUTINE IW_Budget_GetValues_ForAColumn(iLoc,iCol,cOutputInterval,iLenInterval,cOutputBeginDateAndTime,cOutputEndDateAndTime,iLenDateAndTime,rFact_LT,rFact_AR,rFact_VL,iDim_In,iDim_Out,Dates,Values,iStat) BIND(C,NAME='IW_Budget_GetValues_ForAColumn')
    !DEC$ ATTRIBUTES STDCALL, DLLEXPORT :: IW_Budget_GetValues_ForAColumn
    INTEGER(C_INT),INTENT(IN)         :: iLoc,iCol,iLenInterval,iLenDateAndTime,iDim_In
    CHARACTER(KIND=C_CHAR),INTENT(IN) :: cOutputInterval(iLenInterval),cOutputBeginDateAndTime(iLenDateAndTime),cOutputEndDateAndTime(iLenDateAndTime)
    REAL(C_DOUBLE),INTENT(IN)         :: rFact_LT,rFact_AR,rFact_VL
    INTEGER(C_INT),INTENT(OUT)        :: iDim_Out,iStat
    REAL(C_DOUBLE),INTENT(OUT)        :: Dates(iDim_In),Values(iDim_In)  
    
    !Local variables
    INTEGER :: iDataUnitType
    REAL(8) :: rDummy
    CHARACTER :: cOutputInterval_F*iLenInterval,cOutputBeginDateAndTime_F*iLenDateAndTime,cOutputEndDateAndTime_F*iLenDateAndTime
    
    !C strings to Fortran strings
    CALL String_Copy_C_F(cOutputInterval,cOutputInterval_F)
    CALL String_Copy_C_F(cOutputBeginDateAndTime,cOutputBeginDateAndTime_F)
    CALL String_Copy_C_F(cOutputEndDateAndTime,cOutputEndDateAndTime_F)
    
    !Read data
    CALL Budget%ReadData(iLoc,iCol,cOutputInterval_F,cOutputBeginDateAndTime_F,cOutputEndDateAndTime_F,rDummy,rDummy,rDummy,rFact_LT,rFact_AR,rFact_VL,iDataUnitType,iDim_Out,Dates,Values,iStat)
    IF (iStat .EQ. -1) THEN
        CALL Budget%Kill()
        RETURN
    END IF
    
    !Convert dates to Excel-style Julian date
    Dates = Dates - 2415020D0
    
  END SUBROUTINE IW_Budget_GetValues_ForAColumn
  
END MODULE