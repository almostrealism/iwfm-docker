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
MODULE Class_StrmEvap
  USE MessageLogger           , ONLY: SetLastMessage           , &
                                      MessageArray             , &   
                                      f_iFatal   
  USE GeneralUtilities        , ONLY: StripTextUntilCharacter  , &
                                      CleanSpecialCharacters   , &
                                      IntToText                , &  
                                      LocateInList
  USE IOInterface             , ONLY: GenericFileType          , &
                                      RealTSDataInFileType
  USE TimeSeriesUtilities     , ONLY: TimeStepType
  USE Package_PrecipitationET , ONLY: ETType
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
  PUBLIC :: StrmEvapType   
 
  
  ! -------------------------------------------------------------
  ! --- STREAM EVAPORATION DATA TYPE
  ! -------------------------------------------------------------
  TYPE StrmEvapType
      LOGICAL                    :: lComputeEvap      = .FALSE.  !Flag to check if stream evaportaion will be computed
      LOGICAL                    :: lAreaFile_Defined = .FALSE.  !Flag to check if stream surface area file is defined 
      TYPE(RealTSDataInFileType) :: StrmAreaFile                 !Stream surface area file
      REAL(8)                    :: rAreaFactor       = 1d0      !Conversion factor for stream surface area
      INTEGER,ALLOCATABLE        :: iAreaCol(:)                  !Stream surface area column number for each (node) 
      INTEGER,ALLOCATABLE        :: iEvapCol(:)                  !Stream evaporation column number for each (node)
      REAL(8),ALLOCATABLE        :: rEvap(:)                     !Potential stream evaporation at each (node)
  CONTAINS
      PROCEDURE,PASS :: New
      PROCEDURE,PASS :: ReadTSData
  END TYPE StrmEvapType

  
  ! -------------------------------------------------------------
  ! --- MISC. ENTITIES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                   :: ModNameLen      = 16
  CHARACTER(LEN=ModNameLen),PARAMETER :: ModName         = 'Class_StrmEvap::'

  
  
  
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
  ! --- INSTANTIATE STREAM EVAPORATION DATA
  ! -------------------------------------------------------------
  SUBROUTINE New(StrmEvap,InFile,TimeStep,ETData,cWorkingDirectory,iNStrmNodes,iStrmNodeIDs,iStat)
    CLASS(StrmEvapType)           :: StrmEvap
    TYPE(GenericFileType)         :: InFile
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    TYPE(ETType),INTENT(IN)       :: ETData
    CHARACTER(LEN=*),INTENT(IN)   :: cWorkingDirectory
    INTEGER,INTENT(IN)            :: iNStrmNodes,iStrmNodeIDs(iNStrmNodes)
    INTEGER,INTENT(OUT)           :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+3) :: ThisProcedure = ModName // 'New'
    INTEGER                     :: ID,indx,iStrmNode,iDummyArray(3),iMaxFileCol,iMaxPointer 
    REAL(8)                     :: rFactor(1)
    CHARACTER                   :: cAreaFile*1000
    LOGICAL                     :: lProcessed(iNStrmNodes)
    
    !Initialize
    iStat      = 0
    lProcessed = .FALSE.
    
    !Allocate memory
    ALLOCATE (StrmEvap%iEvapCol(iNStrmNodes) , StrmEvap%iAreaCol(iNStrmNodes) , StrmEvap%rEvap(iNStrmNodes))

    !Read stream area filename
    !Backward compatibility: Check if this data even exists
    CALL InFile%ReadData(cAreaFile,iStat)
    IF (iStat .EQ. 0) THEN
        cAreaFile = StripTextUntilCharacter(cAreaFile,'/') 
        CALL CleanSpecialCharacters(cAreaFile)
        IF (LEN_TRIM(cAreaFile) .NE. 0) THEN
            CALL StrmEvap%StrmAreaFile%Init(TRIM(ADJUSTL(cAreaFile)),cWorkingDirectory,'stream surface area file',TimeStep%TrackTime,1,.TRUE.,rFactor,[.FALSE.],iStat=iStat) 
            IF (iStat .EQ. -1) RETURN
            StrmEvap%rAreaFactor       = rFactor(1)
            StrmEvap%lAreaFile_Defined = .TRUE.
        END IF
    ELSE
        StrmEvap%iEvapCol = 0
        StrmEvap%iAreaCol = 0
        StrmEvap%rEvap    = 0.0
        iStat             = 0
        RETURN
    END IF
    
    !Read data to simulate evaporation
    !Backward compatibility: Check if this data even exists
    CALL InFile%ReadData(iDummyArray,iStat)
    IF (iStat .NE. 0) THEN
        StrmEvap%iEvapCol = 0
        StrmEvap%iAreaCol = 0
        StrmEvap%rEvap    = 0.0
        iStat             = 0
        RETURN
    END IF
    
    !Set flag that evap is simulated
    StrmEvap%lComputeEvap = .TRUE.
    
    !Read and process data (first of data is read above, so backspace file)
    CALL InFile%BackspaceFile() 
    DO indx=1,iNStrmNodes
        CALL InFile%ReadData(iDummyArray,iStat)  ;  IF (iStat .NE. 0) RETURN
        
        !Make sure stream node ID is recognized
        ID        = iDummyArray(1)
        iStrmNode = LocateInList(ID,iStrmNodeIDs)
        IF (iStrmNode .LT. 1) THEN
            CALL SetLastMessage('Stream node ID '//TRIM(IntToText(ID))//' listed for stream surface evaporation data is not in the model!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        
        !Make sure same node is not entered more than once
        IF (lProcessed(iStrmNode)) THEN
            CALL SetLastMessage('Stream node ID '//TRIM(IntToText(ID))//' for stream surface evaporation data is listed more than once!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        
        !Store information
        StrmEvap%iEvapCol(iStrmNode) = iDummyArray(2)
        StrmEvap%iAreaCol(iStrmNode) = iDummyArray(3)
        
        !Tag the node as processed
        lProcessed(iStrmNode) = .TRUE.

    END DO
    
    !If all evap columns are zero, then evaporation is not simulated
    IF (ALL(StrmEvap%iEvapCol .EQ. 0)) THEN
        StrmEvap%lComputeEvap = .FALSE.
        RETURN
    END IF
    
    !Make sure that ET data file is available
    IF (ETData%GetNDataColumns() .EQ. 0) THEN
        CALL SetLastMessage('Evapotranspiration rate input data file must be defined to simulate stream evaporation!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    !Check that ET data columns are referred properly
    iMaxFileCol = ETData%GetNDataColumns()
    iMaxPointer = MAXVAL(StrmEvap%iEvapCol)
    IF (iMaxPointer .GT. iMaxFileCol) THEN
        MessageArray(1) = 'There are not enough data columns in evapotranspiration file'
        MessageArray(2) = 'for the simulation of stream evaporation!'
        MessageArray(3) = 'Number of columns in file        = '//TRIM(IntToText(iMaxFileCol))
        MessageArray(4) = 'Highest column number referenced = '//TRIM(IntToText(iMaxPointer))
        CALL SetLastMessage(MessageArray(1:4),f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    !Check that area file is defined, if needed
    IF (ANY(StrmEvap%iAreaCol .GT. 0)) THEN
        IF (.NOT. StrmEvap%lAreaFile_Defined) THEN
            MessageArray(1) = 'Stream surface area file must be defined when one or more area column'
            MessageArray(2) = 'pointers are non-zero for the simulation of stream evaporation!'
            CALL SetLastMessage(MessageArray(1:2),f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
    END IF
    
    !Check that area column pointers are set properly
    IF (StrmEvap%lAreaFile_Defined) THEN
        iMaxFileCol = StrmEvap%StrmAreaFile%GetNDataColumns()
        iMaxPointer = MAXVAL(StrmEvap%iAreaCol)
        IF (iMaxPointer .GT. iMaxFileCol) THEN
            MessageArray(1) = 'There are not enough data columns in stream surface area file'
            MessageArray(2) = ' provided for the simulation of stream evaporation!'
            MessageArray(3) = 'Number of columns in file        = '//TRIM(IntToText(iMaxFileCol))
            MessageArray(4) = 'Highest column number referenced = '//TRIM(IntToText(iMaxPointer))
            CALL SetLastMessage(MessageArray(1:4),f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
    END IF
    
  END SUBROUTINE New
  
  
  
  
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
  ! --- READ TIMESERIES INPUT DATA
  ! -------------------------------------------------------------
  SUBROUTINE ReadTSData(StrmEvap,TimeStep,iStat)
    CLASS(StrmEvapType)           :: StrmEvap
    TYPE(TImeStepType),INTENT(IN) :: TimeStep
    INTEGER,INTENT(OUT)           :: iStat
    
    !Local variables
    INTEGER :: iFileReadCode
    
    !Initialize
    iStat = 0
    
    !Return if evaporation is not simulated
    IF (.NOT. StrmEvap%lComputeEvap) RETURN
    
    !Read surface area data, if defined
    IF (StrmEvap%lAreaFile_Defined) THEN
        CALL StrmEvap%StrmAreaFile%ReadTSData(TimeStep,'stream surface area',iFileReadCode,iStat)
        IF (iStat .EQ. -1) RETURN
        IF (iFileReadCode .EQ. 0) StrmEvap%StrmAreaFile%rValues = StrmEvap%StrmAreaFile%rValues * StrmEvap%rAreaFactor
    END IF
    
  END SUBROUTINE ReadTSData

END MODULE Class_StrmEvap