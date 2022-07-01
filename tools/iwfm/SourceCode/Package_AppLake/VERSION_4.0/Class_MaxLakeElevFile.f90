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
MODULE Class_MaxLakeElevFile
  USE TSDFileHandler        , ONLY: RealTSDataInFileType  , &
                                    ReadTSData
  USE TimeSeriesUtilities   , ONLY: TimeStepType
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
  PUBLIC :: MaxLakeElevFileType        


  ! -------------------------------------------------------------
  ! --- MAXIMUM LAKE ELEVATIONS FILE DATA TYPE
  ! -------------------------------------------------------------
  TYPE,EXTENDS(RealTSDataInFileType) :: MaxLakeElevFileType
      REAL(8) :: Fact     = 1.0
  CONTAINS
      PROCEDURE,PASS :: New
      PROCEDURE,PASS :: Kill
      PROCEDURE,PASS :: ReadTSData => MaxLakeELevFile_ReadTSData
  END TYPE MaxLakeElevFileType
  
  
  ! -------------------------------------------------------------
  ! --- MISC. ENTITIES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                   :: ModNameLen = 23
  CHARACTER(LEN=ModNameLen),PARAMETER :: ModName    = 'Class_MaxLakeElevFile::'

  
  
  
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
  ! --- NEW MAXIMUM LAKE ELEVATIONS DATA FILE
  ! -------------------------------------------------------------
  SUBROUTINE New(MaxLakeElevFile,cFileName,cWorkingDirectory,TimeStep,iStat)
    CLASS(MaxLakeElevFileType)    :: MaxLakeElevFile
    CHARACTER(LEN=*),INTENT(IN)   :: cFileName,cWorkingDirectory
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    INTEGER,INTENT(OUT)           :: iStat
    
    REAL(8) :: Factor(1)
    LOGICAL :: DummyArray(1) = [.FALSE.]
    
    !Initialize
    iStat = 0

    !If no filename return
    IF (cFileName .EQ. '') RETURN

    !Open file
    CALL MaxLakeElevFile%Init(cFileName,cWorkingDirectory,'Maximum lake elevations data file',TimeStep%TrackTime,1,.TRUE.,Factor,DummyArray,iStat=iStat)
    MaxLakeElevFile%Fact = Factor(1)
    
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
  ! --- KILL MAXIMUM LAKE ELEVATIONS DATA FILE OBJECT
  ! -------------------------------------------------------------
  SUBROUTINE Kill(MaxLakeElevFile)
    CLASS(MaxLakeElevFileType) :: MaxLakeElevFile
    
    !Local varibles
    TYPE(MaxLakeElevFileType) :: Dummy
    
    !Close file
    CALL MaxLakeElevFile%Close()
    
    !Set attributes to their default values
    SELECT TYPE (MaxLakeElevFile)
        TYPE IS (MaxLakeElevFileType)
            MaxLakeElevFile = Dummy
    END SELECT
        
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

  ! -------------------------------------------------------------
  ! --- READ MAX LAKE ELEV DATA
  ! -------------------------------------------------------------
  SUBROUTINE MaxLakeELevFile_ReadTSData(MaxLakeElevFile,TimeStep,iStat)
    CLASS(MaxLakeElevFileType)    :: MaxLakeElevFile
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    INTEGER,INTENT(OUT)           :: iStat
    
    !Local variables
    INTEGER :: FileReadCode
    
    !Read data
    CALL ReadTSData(TimeStep,'Maximum lake elevation data',MaxLakeElevFile%RealTSDataInFileType,FileReadCode,iStat)
    IF (iStat .EQ. -1) RETURN

    !If error code returned was zero (data read successfully), scale maximum lake elevations
    IF (FileReadCode .EQ. 0) MaxLakeElevFile%rValues = MaxLakeElevFile%rValues * MaxLakeElevFile%Fact
  
  END SUBROUTINE MaxLakeELevFile_ReadTSData
  
END MODULE