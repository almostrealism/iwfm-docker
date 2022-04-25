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
MODULE Class_TSBCDataFile
  USE GeneralUtilities    , ONLY: AllocArray
  USE TimeSeriesUtilities , ONLY: TimeStepType
  USE MessageLogger       , ONLY: SetLastMessage        , &
                                  f_iFatal
  USE Package_Misc        , ONLY: RealTSDataInFileType  , &
                                  ReadTSData
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
  PUBLIC :: TSBCDataFileType         , &
            TSBCDataFile_New         , &
            TSBCDataFile_Kill        , &
            TSBCDataFile_ReadTSData 
  
  
  ! -------------------------------------------------------------
  ! --- TIME SERIES B.C. DATA FILE TYPE
  ! -------------------------------------------------------------
  TYPE,EXTENDS(RealTSDataInFileType) :: TSBCDataFileType
      PRIVATE
      LOGICAL             :: lDefined            = .FALSE.  !Flag to check if time series data is specified
      INTEGER             :: NTSFlowBCColumns    = 0        !Number of columns to be used for time series flow b.c.
      INTEGER,ALLOCATABLE :: iTSFlowBCColumns(:)            !List of column numbers that store time series flow b.c.
      REAL(8)             :: Factor_TSHeadBC     = 1.0      !Factor to convert time series head b.c. to simulation units
      REAL(8)             :: Factor_TSFlowBC     = 1.0      !Factor to convert time series flow b.c. to simulation units
  END TYPE TSBCDataFileType
  
  
  ! -------------------------------------------------------------
  ! --- MISC. ENTITIES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                   :: ModNameLen = 20
  CHARACTER(LEN=ModNameLen),PARAMETER :: ModName    = 'Class_TSBCDataFile::'



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
  ! --- INSTANTIATE TIME SERIES BOUNDARY CONDITIONS DATA FILE
  ! -------------------------------------------------------------
  SUBROUTINE TSBCDataFile_New(cFileName,cWorkingDirectory,iTSFlowBCColumns,TimeStep,TSBCDataFile,iStat)
    CHARACTER(LEN=*),INTENT(IN)   :: cFileName,cWorkingDirectory
    INTEGER,INTENT(IN)            :: iTSFlowBCColumns(:)
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    TYPE(TSBCDataFileType)        :: TSBCDataFile
    INTEGER,INTENT(OUT)           :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+16) :: ThisProcedure = ModName // 'TSBCDataFile_New'
    CHARACTER                    :: cErrorMsg*200
    INTEGER                      :: ErrorCode
    REAL(8)                      :: rFactor(2)
    LOGICAL,ALLOCATABLE          :: RateTypeDataArray(:)
    
    !Initialize
    iStat = 0

    !If no filename return
    IF (cFileName .EQ. '') RETURN
    
    !Allocate memory and store the flow b.c. column list
    IF (SIZE(iTSFlowBCColumns) .GT. 0)  THEN
        CALL AllocArray(RateTypeDataArray,MAXVAL(iTSFlowBCColumns),ThisProcedure,iStat)  ;  IF (iStat .EQ. -1) RETURN
        TSBCDataFile%NTSFlowBCColumns = SIZE(iTSFlowBCColumns)
        ALLOCATE (TSBCDataFile%iTSFlowBCColumns(SIZE(iTSFlowBCColumns)) , STAT=ErrorCode , ERRMSG=cErrorMsg)
        IF (ErrorCode .NE. 0) THEN
            CALL SetLastMessage('Error in allocating memory for the time series flow boundary conditions.'//NEW_LINE('')//TRIM(cErrorMsg),f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        TSBCDataFile%iTSFlowBCColumns = iTSFlowBCColumns
    END IF
    
    
    !Assign .TRUE. to RateTypeDataArray for specified flow type boundary conditions
    IF (ALLOCATED(RateTypeDataArray)) THEN
        RateTypeDataArray                   = .FALSE.
        RateTypeDataArray(iTSFlowBCColumns) = .TRUE.
    END IF

    !Open file
    IF (ALLOCATED(RateTypeDataArray)) THEN
        CALL TSBCDataFile%Init(cFileName,cWorkingDirectory,'time-series boundary conditions data file',TimeStep%TrackTime,1,.TRUE.,rFactor,RateTypeDataArray,iStat=iStat)
    ELSE
        CALL TSBCDataFile%Init(cFileName,cWorkingDirectory,'time-series boundary conditions data file',TimeStep%TrackTime,1,.TRUE.,rFactor,iStat=iStat)
    END IF
    IF (iStat .EQ. -1) RETURN
    TSBCDataFile%Factor_TSHeadBC = rFactor(1)
    TSBCDataFile%Factor_TSFlowBC = rFactor(2)
    
    !If made this far, set the flag
    TSBCDataFile%lDefined = .TRUE.
    
    !Read the time series b.c. for the first time step to compute groundwater storage
    CALL TSBCDataFile_ReadTSData(TimeStep,TSBCDataFile,iStat)
    
    !Free memory
    DEALLOCATE (RateTypeDataArray , STAT=ErrorCode)
    
  END SUBROUTINE TSBCDataFile_New
  
  
  
  
! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** DESTRUCTOR
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- KILL TIME SERIES BOUNDARY CONDITIONS DATA FILE
  ! -------------------------------------------------------------
  SUBROUTINE TSBCDataFile_Kill(TSBCDataFile)
    TYPE(TSBCDataFileType) :: TSBCDataFile
    
    !Local variables
    INTEGER :: ErrorCode
    
    CALL TSBCDataFile%Close()
    DEALLOCATE(TSBCDataFile%iTSFLowBCColumns , STAT=ErrorCode)
    TSBCDataFile%NTSFlowBCColumns = 0
    TSBCDataFile%Factor_TSHeadBC  = 1.0
    TSBCDataFile%Factor_TSFlowBC  = 1.0    
    TSBCDataFile%lDefined         = .FALSE.
    
  END SUBROUTINE TSBCDataFile_Kill
  
  
  
  
! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** DATA READER
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- READ TIME SERIES B.C. DATA
  ! -------------------------------------------------------------
  SUBROUTINE TSBCDataFile_ReadTSData(TimeStep,TSBCDataFile,iStat)
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    TYPE(TSBCDataFileType)        :: TSBCDataFile
    INTEGER,INTENT(OUT)           :: iStat
    
    !Local variables
    INTEGER :: FileReadCode
    
    !Initailize
    iStat = 0 
    
    !Return if time series b.c. data is not defined
    IF (.NOT. TSBCDataFile%lDefined) RETURN
    
    !Read data
    CALL ReadTSData(TimeStep,'timeseries boundary conditions data',TSBCDataFile%RealTSDataInFileType,FileReadCode,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Return if data reading was not successful
    IF (FileReadCode .NE. 0) RETURN
    
    !First multiply all columns with head conversion factor
    TSBCDataFile%rValues = TSBCDataFile%rValues * TSBCDataFile%Factor_TSHeadBC
    
    !Then multiply the flow b.c. columns with flow conversion factor and divide by head conversion factor for correction from the previous step
    IF (TSBCDataFile%NTSFlowBCColumns .GT. 0)  &
        TSBCDataFile%rValues(TSBCDataFile%iTSFlowBCColumns) = TSBCDataFile%rValues(TSBCDataFile%iTSFlowBCColumns) / TSBCDataFile%Factor_TSHeadBC * TSBCDataFile%Factor_TSFlowBC
       
  END SUBROUTINE TSBCDataFile_ReadTSData

  
END MODULE 