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
MODULE Class_GenericMoistureData
  USE MessageLogger       , ONLY: SetLastMessage        , &
                                  EchoProgress          , &
                                  f_iFatal
  USE TimeSeriesUtilities , ONLY: TimeStepType
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
  PUBLIC :: GenericMoistureDataType        
  
  
  ! -------------------------------------------------------------
  ! --- GENERIC MOISTURE DATA TYPE
  ! -------------------------------------------------------------
  TYPE,EXTENDS(RealTSDataInFileType) :: GenericMoistureDataType
    REAL(8)             :: Fact  = 1.0               !Conversion factor for generic moisture
    INTEGER,ALLOCATABLE :: iColGenericMoisture(:,:)  !Column number in the generic moisture data file for each (soil,location) combination
    REAL(8),ALLOCATABLE :: rGenericMoisture(:,:)     !Generic moisture read from file for each (soil,location) combination
  CONTAINS
    PROCEDURE,PASS :: New
    PROCEDURE,PASS :: Kill
    PROCEDURE,PASS :: ReadTSData => GenericMoistureData_ReadTSData
  END TYPE GenericMoistureDataType


  ! -------------------------------------------------------------
  ! --- MISC. ENTITIES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                   :: ModNameLen = 27
  CHARACTER(LEN=ModNameLen),PARAMETER :: ModName    = 'Class_GenericMoistureData::'


  
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
  ! --- NEW GENERIC MOISTURE DATA
  ! -------------------------------------------------------------
  SUBROUTINE New(GenericMoistureData,cFileName,cWorkingDirectory,NSoils,NLocations,iColGenericMoisture,TrackTime,iStat) 
    CLASS(GenericMoistureDataType) :: GenericMoistureData
    CHARACTER(LEN=*),INTENT(IN)    :: cFileName,cWorkingDirectory
    INTEGER,INTENT(IN)             :: NSoils,NLocations,iColGenericMoisture(NSoils,NLocations)
    LOGICAL,INTENT(IN)             :: TrackTime
    INTEGER,INTENT(OUT)            :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+3) :: ThisProcedure = ModName // 'New'
    INTEGER                     :: ErrorCode,iCheckColPointers(NSoils*NLocations)
    REAL(8)                     :: Factor(1)
    
    !initialize
    iStat = 0
    
    !Allocate memory for rGenericMoisture no matter what
    ALLOCATE (GenericMoistureData%rGenericMoisture(NSoils,NLocations) , STAT=ErrorCode)
    IF (ErrorCode .NE. 0) THEN
        CALL SetLastMessage('Error in allocating memory for generic moisture data!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    !Initialize rGenericMoisture
    GenericMoistureData%rGenericMoisture = 0.0
    
    !If filename is not defined, return
    IF (cFileName .EQ. '') RETURN
  
    !Open generic moisture data file
    CALL GenericMoistureData%Init(ADJUSTL(cFileName),cWorkingDirectory,'generic moisture data file',TrackTime,BlocksToSkip=1,lFactorDefined=.TRUE.,Factor=Factor,RateTypeData=[.TRUE.],iStat=iStat)  
    IF (iStat .EQ. -1) RETURN
    GenericMoistureData%Fact                 = Factor(1)
    ALLOCATE (GenericMoistureData%iColGenericMoisture(NSoils,NLocations))
    GenericMoistureData%iColGenericMoisture  = iColGenericMoisture
  
    !Make sure there are enough columns of data in Generic Moisture file
    iCheckColPointers = PACK(GenericMoistureData%iColGenericMoisture , MASK=.TRUE.)
    CALL GenericMoistureData%CheckColNum('Generic moisture file',iCheckColPointers,.TRUE.,iStat)

  END SUBROUTINE New
  
  
  
  
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
  ! --- KILL GENERIC MOISTURE DATA
  ! -------------------------------------------------------------
  SUBROUTINE Kill(GenericMoistureData)
    CLASS(GenericMoistureDataType) :: GenericMoistureData
    
    !Local variables
    INTEGER :: ErrorCode
    
    CALL GenericMoistureData%Close()
    DEALLOCATE (GenericMoistureData%iColGenericMoisture , &
                GenericMoistureData%rGenericMoisture    , &
                STAT = ErrorCode                        )
    
    GenericMoistureData%Fact = 1.0
    
  END SUBROUTINE Kill
  
  
  
  
! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** DATA READERS
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************
  SUBROUTINE GenericMoistureData_ReadTSData(GenericMoistureData,TimeStep,iStat)
    CLASS(GenericMoistureDataType) :: GenericMoistureData
    TYPE(TimeStepType),INTENT(IN)  :: TimeStep
    INTEGER,INTENT(OUT)            :: iStat
    
    !Local variables
    INTEGER :: FileReadCode,indxLocation
    
    !Inform user about progress
    CALL EchoProgress('Reading generic moisture time series data')
    
    !Read data
    CALL ReadTSData(TimeStep,'Generic moisture data',GenericMoistureData%RealTSDataInFileType,FileReadCode,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Compile rGenericMoisture
    IF (FileReadCode .EQ. 0) THEN
      DO indxLocation=1,SIZE(GenericMoistureData%iColGenericMoisture,DIM=2)
          GenericMoistureData%rGenericMoisture(:,indxLocation) = GenericMoistureData%rValues(GenericMoistureData%iColGenericMoisture(:,indxLocation)) * GenericMoistureData%Fact
      END DO
    END IF

  END SUBROUTINE GenericMoistureData_ReadTSData


END MODULE 