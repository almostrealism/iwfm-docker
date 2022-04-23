!***********************************************************************
!  Integrated Water Flow Model (IWFM)
!  Copyright (C) 2005-2018  
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
  USE Class_AppGrid
  USE MessageLogger          , ONLY: SetLastMessage          , &
                                     MessageArray            , &
                                     iFatal
  USE GeneralUtilities
  USE TimeSeriesUtilities
  USE IOInterface
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
  SUBROUTINE LandUseDataFile_New(LandUseDataFile,cFileName,cDescriptor,NLocations,NLands,TrackTime,iStat) 
    CLASS(LandUseDataFileType)  :: LandUseDataFile
    CHARACTER(LEN=*),INTENT(IN) :: cFileName,cDescriptor
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
    CALL LandUseDataFile%Init(cFileName,cDescriptor,TrackTime,BlocksToSkip=1,lFactorDefined=.TRUE.,lReadDims=.FALSE.,nRow=NLocations,nCol=NLands+1,Factor=Factor,iStat=iStat)
    LandUseDataFile%Fact = Factor(1)

  END SUBROUTINE LandUseDataFile_New

    
  ! -------------------------------------------------------------
  ! --- KILL LAND USE DATA FILE
  ! -------------------------------------------------------------
  SUBROUTINE LandUseDataFile_Kill(LandUseDataFile)
    CLASS(LandUseDataFileType) :: LandUseDataFile
    
    !Local variables
    TYPE(LandUseDataFileType) :: Dummy
    
    CALL LandUseDataFile%Close()
    LandUsedataFile%Fact = Dummy%Fact
    
  END SUBROUTINE LandUseDataFile_Kill
    
    
  ! -------------------------------------------------------------
  ! --- SUBROUTINE TO READ LAND USE DATA FOR A TIME STAMP
  ! -------------------------------------------------------------
  SUBROUTINE LandUseDataFile_ReadTSData(LandUseDataFile,cDescriptor,TimeStep,Area,iStat)
    CLASS(LandUseDataFileType)    :: LandUseDataFile
    CHARACTER(LEN=*),INTENT(IN)   :: cDescriptor
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    REAL(8),INTENT(IN)            :: Area(:)
    INTEGER,INTENT(OUT)           :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+26) :: ThisProcedure = ModName // 'LandUseDataFile_ReadTSData'
    INTEGER                      :: indxLocation,FileReadCode,NLocations,iLocation
    
    !Initialize
    iStat      = 0
    NLocations = LandUseDataFile%nRow
    
    !Read data
    CALL ReadTSData(TimeStep,cDescriptor,LandUseDataFile%Real2DTSDataInFileType,FileReadCode,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Return if new data is not read
    IF (FileReadCode .NE. 0) RETURN
        
    !Check for errors and process data
    DO indxLocation=1,NLocations
        
      !Make sure land use data is entered sequentially for locations
      iLocation = INT(LandUseDataFile%rValues(indxLocation,1))
      IF (iLocation .NE. indxLocation) THEN
        MessageArray(1) = 'Land use data for each location (elements, subregions, etc.) should be entered sequentially.'
        MessageArray(2) = 'Expected location ID='//TRIM(IntTotext(indxLocation))  
        MessageArray(3) = 'Entered location ID ='//TRIM(IntTotext(iLocation))
        CALL SetLastMessage(MessageArray(1:3),iFatal,ThisProcedure)
        iStat = -1
        RETURN
      END IF 
        
    END DO
        
    !Conversion
    IF (LandUseDataFile%Fact .EQ. 0.0) THEN
      DO indxLocation=1,NLocations
        LandUseDataFile%rValues(indxLocation,2:) = LandUseDataFile%rValues(indxLocation,2:) * Area(indxLocation)
      END DO
    ELSE
      LandUseDataFile%rValues(:,2:) = LandUseDataFile%rValues(:,2:) * LandUseDataFile%Fact
    END IF
     
  END SUBROUTINE LandUseDataFile_ReadTSData
  
  
END MODULE