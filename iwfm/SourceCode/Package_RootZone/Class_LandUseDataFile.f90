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
MODULE Class_LandUseDataFile
  USE MessageLogger          , ONLY: SetLastMessage          , &
                                     MessageArray            , &
                                     f_iFatal
  USE GeneralUtilities       , ONLY: ConvertID_To_Index      , &
                                     LowerCase               , &
                                     IntToText
  USE TimeSeriesUtilities    , ONLY: TimeStepType
  USE TSDFileHandler         , ONLY: Real2DTSDataInFileType  , &
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
  PUBLIC :: LandUseDataFileType        


  ! -------------------------------------------------------------
  ! --- LAND USE DATA FILE TYPE
  ! -------------------------------------------------------------
  TYPE,EXTENDS(Real2DTSDataInFileType) :: LandUseDataFileType
    REAL(8) :: Fact  = 1.0     !Conversion factor for land use areas
  CONTAINS
    PROCEDURE,PASS :: New        => LandUseDataFile_New
    PROCEDURE,PASS :: Kill       => LandUseDataFile_Kill  
    PROCEDURE,PASS :: ReadTSData => LandUseDataFile_ReadTSData
  END TYPE LandUseDataFileType
  
  
  ! -------------------------------------------------------------
  ! --- MISC. ENTITIES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                   :: ModNameLen = 23
  CHARACTER(LEN=ModNameLen),PARAMETER :: ModName    = 'Class_LandUseDataFile::'


  
  
CONTAINS




  ! -------------------------------------------------------------
  ! --- NEW LAND USE DATA FILE
  ! -------------------------------------------------------------
  SUBROUTINE LandUseDataFile_New(LandUseDataFile,cFileName,cWorkingDirectory,cDescriptor,NLocations,NLands,TrackTime,iStat) 
    CLASS(LandUseDataFileType)  :: LandUseDataFile
    CHARACTER(LEN=*),INTENT(IN) :: cFileName,cWorkingDirectory,cDescriptor
    INTEGER,INTENT(IN)          :: NLocations,NLands
    LOGICAL,INTENT(IN)          :: TrackTime
    INTEGER,INTENT(OUT)         :: iStat
    
    !Local variables
    REAL(8) :: Factor(1)
    
    !Initailize
    iStat = 0
    
    !Return if no file name is specified
    IF (cFileName .EQ. '') RETURN
    
    !Instantiate
    !** Note: 1 column is added to NLands to allow reading of element numbers.
    CALL LandUseDataFile%Init(cFileName,cWorkingDirectory,cDescriptor,TrackTime,BlocksToSkip=1,lFactorDefined=.TRUE.,lReadDims=.FALSE.,nRow=NLocations,nCol=NLands+1,Factor=Factor,iStat=iStat)
    LandUseDataFile%Fact = Factor(1)

  END SUBROUTINE LandUseDataFile_New

    
  ! -------------------------------------------------------------
  ! --- KILL LAND USE DATA FILE
  ! -------------------------------------------------------------
  SUBROUTINE LandUseDataFile_Kill(LandUseDataFile)
    CLASS(LandUseDataFileType) :: LandUseDataFile
    
    !Local variables
    TYPE(LandUseDataFileType) :: Dummy
    
    CALL LandUseDataFile%Real2DTSDataInFileType%Close()
    SELECT TYPE (p => LandUseDataFile)
        TYPE IS (LandUseDataFileType)
           p = Dummy
    END SELECT
    
  END SUBROUTINE LandUseDataFile_Kill
    
    
  ! -------------------------------------------------------------
  ! --- SUBROUTINE TO READ LAND USE DATA FOR A TIME STAMP
  ! -------------------------------------------------------------
  SUBROUTINE LandUseDataFile_ReadTSData(LandUseDataFile,cDescriptor,TimeStep,Area,iLocationIDs,iStat)
    CLASS(LandUseDataFileType)    :: LandUseDataFile
    CHARACTER(LEN=*),INTENT(IN)   :: cDescriptor
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    REAL(8),INTENT(IN)            :: Area(:)
    INTEGER,INTENT(IN)            :: iLocationIDs(:)
    INTEGER,INTENT(OUT)           :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+26) :: ThisProcedure = ModName // 'LandUseDataFile_ReadTSData'
    INTEGER                      :: indxLocation,FileReadCode,NLocations,iLocation,iLocationID
    REAL(8)                      :: rLandUse_Work(LandUseDataFile%nRow,LandUseDataFile%nCol)
    LOGICAL                      :: lProcessed(LandUseDataFile%nRow)
    
    !Initialize
    iStat      = 0
    NLocations = LandUseDataFile%nRow
    
    !Read data
    CALL ReadTSData(TimeStep,cDescriptor,LandUseDataFile%Real2DTSDataInFileType,FileReadCode,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Return if new data is not read
    IF (FileReadCode .NE. 0) RETURN
        
    !Check for errors and process data
    lProcessed = .FALSE.
    DO indxLocation=1,NLocations
        iLocationID = INT(LandUseDataFile%rValues(indxLocation,1))
        
        !Check that listed element/subregion is modeled
        CALL ConvertID_To_Index(iLocationID,iLocationIDs,iLocation)
        IF (iLocation .EQ. 0) THEN
            CALL SetLastMessage('Element or subregion number '//TRIM(IntToText(iLocationID))//' listed for '//TRIM(LowerCase(cDescriptor))//' is not in the model!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        
        !Make sure location number is not listed more than once
        IF (lProcessed(iLocation)) THEN
            CALL SetLastMessage('Element or subregion number '//TRIM(IntToText(iLocationID))//' listed for '//TRIM(LowerCase(cDescriptor))//' is listed more than once!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        
        !Process data
        lProcessed(iLocation)       = .TRUE.
        rLandUse_Work(iLocation,1)  = iLocation
        rLandUse_Work(iLocation,2:) = LandUseDataFile%rValues(indxLocation,2:)
        
    END DO
        
    !Store data in return argument and perform unit conversions
    IF (LandUseDataFile%Fact .EQ. 0.0) THEN
        DO indxLocation=1,NLocations
            LandUseDataFile%rValues(indxLocation,1)  = rLandUse_Work(indxLocation,1)
            LandUseDataFile%rValues(indxLocation,2:) = rLandUse_Work(indxLocation,2:) * Area(indxLocation)
        END DO
    ELSE
        LandUseDataFile%rValues(:,1)  = rLandUse_Work(:,1)
        LandUseDataFile%rValues(:,2:) = rLandUse_Work(:,2:) * LandUseDataFile%Fact
    END IF
     
  END SUBROUTINE LandUseDataFile_ReadTSData
  
END MODULE