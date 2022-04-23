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
MODULE Util_Package_RootZone
  USE IOInterface
  IMPLICIT NONE
  
  
  ! -------------------------------------------------------------
  ! --- PUBLIC ENTITIES
  ! -------------------------------------------------------------
  PUBLIC 

  
  ! -------------------------------------------------------------
  ! --- SUPPLY RELATED DATA TYPE
  ! -------------------------------------------------------------
  TYPE WaterSupplyType
    REAL(8) :: Diversion_Ag     = 0.0     !Water supply from diversions for ag lands
    REAL(8) :: Diversion_Urb    = 0.0     !Water supply from diversions for urban lands
    REAL(8) :: Pumping_Ag       = 0.0     !Water supply from pumping for ag lands
    REAL(8) :: Pumping_Urb      = 0.0     !Water supply from pumping for urban lands
    REAL(8) :: UpstrmRunoff     = 0.0     !Water supply from runoff/return flow from upstream areas
  END TYPE WaterSupplyType
  
  
  ! -------------------------------------------------------------
  ! --- IRRIGATION PERIOD FLAGS
  ! -------------------------------------------------------------
  INTEGER,PARAMETER :: NoIrigPeriod  = 0  , &
                       IrigPeriod    = 1  , &
                       PreIrigPeriod = 2
  INTEGER,PARAMETER :: IrigPeriodFlags(3) = [NoIrigPeriod , IrigPeriod , PreIrigPeriod]
  
  
  ! -------------------------------------------------------------
  ! --- FLAGS FOR BEGINNING OR ENDING SOIL MOISTURE FOR AG DEMAND CALCULATION
  ! -------------------------------------------------------------
  INTEGER,PARAMETER :: iDemandFromMoistAtBegin = 0 , &
                       iDemandFromMoistAtEnd   = 1

                                           
  
  
  
CONTAINS



  ! -------------------------------------------------------------
  ! --- SUBROUTINE TO READ DATA FROM PARAMETER FILE
  ! -------------------------------------------------------------
  SUBROUTINE ReadPointerData(File,iRow,iCol,DummyIntArray,iStat)
    TYPE(GenericFileType)           :: File
    INTEGER,INTENT(IN)              :: iRow,iCol
    INTEGER,ALLOCATABLE,INTENT(OUT) :: DummyIntArray(:,:)
    INTEGER,INTENT(OUT)             :: iStat
      
    !Local variables
    INTEGER :: indxElem,FirstLine(iCol),ErrorCode
      
    !Allocate memory for DummyIntArray
    IF (ALLOCATED(DummyIntArray)) DEALLOCATE(DummyIntArray,STAT=ErrorCode)
    ALLOCATE (DummyIntArray(iRow,iCol))
    
    !Read first line 
    CALL File%ReadData(FirstLine,iStat)  
    IF (iStat .EQ. -1) RETURN
      
    !If Element number in first line is zero, apply the data to all DummyIntArray
    IF (FirstLine(1) .EQ. 0) THEN
      FORALL (indxElem=1:iRow) DummyIntArray(indxElem,:) = FirstLine
      
    !Otherwise, read data for each element seperately
    ELSE
      DummyIntArray(1,:) = FirstLine
      CALL File%ReadData(DummyIntArray(2:,:),iStat)  
    END IF
            
  END SUBROUTINE ReadPointerData
    

  ! -------------------------------------------------------------
  ! --- SUBROUTINE TO READ REAL DATA FROM PARAMETER FILE
  ! -------------------------------------------------------------
  SUBROUTINE ReadRealData(File,iRow,iCol,DummyRealArray,iStat)
    TYPE(GenericFileType)           :: File
    INTEGER,INTENT(IN)              :: iRow,iCol
    REAL(8),ALLOCATABLE,INTENT(OUT) :: DummyRealArray(:,:)
    INTEGER,INTENT(OUT)             :: iStat
    
    !Local variables
    INTEGER :: indxElem,ErrorCode
    REAL(8) :: FirstLine(iCol)
    
    !Allocate memory for DummyRealArray
    IF (ALLOCATED(DummyRealArray)) DEALLOCATE(DummyRealArray,STAT=ErrorCode)
    ALLOCATE (DummyRealArray(iRow,iCol))
    
    !Read first line 
    CALL File%ReadData(FirstLine,iStat)  
    IF (iStat .EQ. -1) RETURN
    
    !If Element number in first line is zero, apply the data to all DummyIntArray
    IF (FirstLine(1) .EQ. 0) THEN
      FORALL (indxElem=1:iRow) DummyRealArray(indxElem,:) = FirstLine
    
    !Otherwise, read data for each element seperately
    ELSE
      DummyRealArray(1,:) = FirstLine
      CALL File%ReadData(DummyRealArray(2:,:),iStat)  
      IF (iStat .EQ. -1) RETURN
    END IF
    
  END SUBROUTINE ReadRealData
  
  
  ! -------------------------------------------------------------
  ! --- SUBROUTINE TO ADD A STRING TO AN ARRAY OF STRINGS
  ! -------------------------------------------------------------
  SUBROUTINE AddStringToStringList(cStringToAdd,cStringList)
    CHARACTER(LEN=*),INTENT(IN)  :: cStringToAdd
    CHARACTER(LEN=*),ALLOCATABLE :: cStringList(:)
    
    !Local variables
    INTEGER                                     :: iDim
    CHARACTER(LEN=LEN(cStringList)),ALLOCATABLE :: cTempList(:)
    
    IF (ALLOCATED(cStringList)) THEN
        iDim = SIZE(cStringList)
    ELSE
        iDim = 0
    END IF
    ALLOCATE (cTempList(iDim+1))
    cTempList(1:iDim) = cStringList(1:iDim)
    iDim              = iDim + 1
    cTempList(iDim)   = cStringToAdd
    CALL MOVE_ALLOC(cTempList , cStringList)
    
  END SUBROUTINE AddStringToStringList 
  

END MODULE