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
MODULE Class_IrigFracFile
  USE TimeSeriesUtilities  , ONLY: TimeStepType
  USE Package_Misc         , ONLY: RealTSDataInFileType , &
                                   ReadTSData
  USE Package_AppGW        , ONLY: AppGWType
  USE Package_AppStream    , ONLY: AppStreamType
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
  PUBLIC  :: IrigFracFileType   
  
  
  ! -------------------------------------------------------------
  ! --- IRRIGATION FRACTIONS DATA FILE TYPE
  ! -------------------------------------------------------------
  TYPE,EXTENDS(RealTSDataInFileType) :: IrigFracFileType
    PRIVATE
  CONTAINS
    PROCEDURE,PASS :: New 
    PROCEDURE,PASS :: Kill
    PROCEDURE,PASS :: ReadTSData =>  IrigFracFile_ReadTSData  
  END TYPE
    
  

CONTAINS


  ! -------------------------------------------------------------
  ! --- NEW IRRIGATION FRACTIONS FILE
  ! -------------------------------------------------------------
  SUBROUTINE New(IrigFracFile,cFileName,cWorkingDirectory,TimeStep,iStat) 
    CLASS(IrigFracFileType),INTENT(OUT) :: IrigFracFile
    CHARACTER(LEN=*),INTENT(IN)         :: cFileName,cWorkingDirectory
    TYPE(TimeStepType),INTENT(IN)       :: TimeStep
    INTEGER,INTENT(OUT)                 :: iStat
    
    !Local variables
    REAL(8) :: DummyFactor(1)
    
    !Initialize
    iStat = 0
    
    !If no file name is specified, return
    IF (cFileName .EQ. '') RETURN
    
    !Instantiate the data type
    CALL IrigFracFile%Init(cFileName,cWorkingDirectory,'irrigation fractions data file',TimeStep%TrackTime,BlocksToSkip=1,lFactorDefined=.FALSE.,Factor=DummyFactor,iStat=iStat)
    
  END SUBROUTINE New
  
  
  ! -------------------------------------------------------------
  ! --- KILL IRRIGATION FRACTIONS FILE
  ! -------------------------------------------------------------
  SUBROUTINE Kill(IrigFracFile)
    CLASS(IrigFracFileType) :: IrigFracFile
    
    CALL IrigFracFile%Close()
    
  END SUBROUTINE Kill
    
  
  ! -------------------------------------------------------------
  ! --- READ IRRIGATION FRACTIONS DATA
  ! -------------------------------------------------------------
  SUBROUTINE IrigFracFile_ReadTSData(IrigFracFile,AppStream,lDiversionAdjusted,AppGW,lPumpingAdjusted,TimeStep,iStat)
    CLASS(IrigFracFileType)       :: IrigFracFile
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    TYPE(AppStreamType)           :: AppStream
    TYPE(AppGWType)               :: AppGW
    LOGICAL,INTENT(IN)            :: lDiversionAdjusted,lPumpingAdjusted
    INTEGER,INTENT(OUT)           :: iStat
    
    !Local variables
    INTEGER :: FileReadCode
    
    !Read time series data
    CALL ReadTSData(TimeStep,'Irrigations fractions data',IrigFracFile%RealTSDataInFileType,FileReadCode,iStat)
    IF (iStat .EQ. -1) RETURN
        
    !Update the supply irrigtaion fractions if read with no problems
    SELECT CASE (FileReadCode)
        !It wasn't time to read
        CASE (-1)
            IF (lDiversionAdjusted) CALL AppStream%ResetIrigFracs()
            IF (lPumpingAdjusted) CALL AppGW%ResetIrigFracs()
          
        !Data was read with sucess
        CASE (0)
            !Fraction of diversion to be used for agricultural purposes
            CALL AppStream%SetIrigFracsRead(IrigFracFile%rValues)
            !Fraction of pumping to be used for agricultural purposes
            CALL AppGW%SetIrigFracsRead(IrigFracFile%rValues)
    END SELECT

  END SUBROUTINE IrigFracFile_ReadTSData
  
  
END MODULE