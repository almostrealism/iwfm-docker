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
MODULE Util_Package_RootZone
  USE MessageLogger    , ONLY: SetLastMessage     , &
                               f_iFatal             
  USE GeneralUtilities , ONLY: LowerCase          , &
                               ConvertID_To_Index
  USE IOInterface      , ONLY: GenericFileType
  USE Package_Misc     , ONLY: f_iRootZoneComp
  IMPLICIT NONE
  
  
  ! -------------------------------------------------------------
  ! --- PUBLIC ENTITIES
  ! -------------------------------------------------------------
  PRIVATE
  PUBLIC :: WaterSupplyType                      , &
            ReadRealData                         , & 
            ReadPointerData                      , &
            AddStringToStringList                , &
            f_iNoIrigPeriod                      , &
            f_iIrigPeriod                        , &
            f_iPreIrigPeriod                     , &
            f_iDemandFromMoistAtBegin            , &
            f_iDemandFromMoistAtEnd              , &
            f_iIrigPeriodFlags                   , &
            f_iBudgetType_LWU                    , &
            f_iBudgetType_RootZone               , &
            f_iBudgetType_NonPondedCrop_LWU      , &
            f_iBudgetType_NonPondedCrop_RZ       , & 
            f_iBudgetType_PondedCrop_LWU         , &
            f_iBudgetType_PondedCrop_RZ          , & 
            f_cDescription_LWUBudget             , &
            f_cDescription_RootZoneBudget        , &
            f_cDescription_NPCrop_LWUBudget      , &
            f_cDescription_NPCrop_RootZoneBudget , &
            f_cDescription_PCrop_LWUBudget       , &
            f_cDescription_PCrop_RootZoneBudget  , &
            f_iZBudgetType_RootZone              , &
            f_iZBudgetType_LWU                   , &
            f_cDescription_RootZoneZBudget       , &           
            f_cDescription_LWUZBudget      

  
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
  INTEGER,PARAMETER :: f_iNoIrigPeriod  = 0  , &
                       f_iIrigPeriod    = 1  , &
                       f_iPreIrigPeriod = 2
  INTEGER,PARAMETER :: f_iIrigPeriodFlags(3) = [f_iNoIrigPeriod , f_iIrigPeriod , f_iPreIrigPeriod]
  
  
  ! -------------------------------------------------------------
  ! --- FLAGS FOR BEGINNING OR ENDING SOIL MOISTURE FOR AG DEMAND CALCULATION
  ! -------------------------------------------------------------
  INTEGER,PARAMETER :: f_iDemandFromMoistAtBegin = 0 , &
                       f_iDemandFromMoistAtEnd   = 1

                                           
  ! -------------------------------------------------------------
  ! ---FLAGS FOR BUDGET FILES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER           :: f_iBudgetType_LWU                    = f_iRootZoneComp*1000 + 1 , &
                                 f_iBudgetType_RootZone               = f_iRootZoneComp*1000 + 2 , &
                                 f_iBudgetType_NonPondedCrop_LWU      = f_iRootZoneComp*1000 + 3 , &
                                 f_iBudgetType_NonPondedCrop_RZ       = f_iRootZoneComp*1000 + 4 , & 
                                 f_iBudgetType_PondedCrop_LWU         = f_iRootZoneComp*1000 + 5 , &
                                 f_iBudgetType_PondedCrop_RZ          = f_iRootZoneComp*1000 + 6  
  CHARACTER(LEN=25),PARAMETER :: f_cDescription_LWUBudget             = 'Land and water use budget'                          
  CHARACTER(LEN=16),PARAMETER :: f_cDescription_RootZoneBudget        = 'Root zone budget'                                   
  CHARACTER(LEN=50),PARAMETER :: f_cDescription_NPCrop_LWUBudget      = 'Non-ponded-crop specific land and water use budget' 
  CHARACTER(LEN=41),PARAMETER :: f_cDescription_NPCrop_RootZoneBudget = 'Non-ponded-crop specific root zone budget'          
  CHARACTER(LEN=46),PARAMETER :: f_cDescription_PCrop_LWUBudget       = 'Ponded-crop specific land and water use budget'     
  CHARACTER(LEN=37),PARAMETER :: f_cDescription_PCrop_RootZoneBudget  = 'Ponded-crop specific root zone budget'              
  
  
  ! -------------------------------------------------------------
  ! --- FLAGS FOR ZONE BUDGET FILES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER           :: f_iZBudgetType_RootZone        = f_iRootZoneComp*1000 + 1 , &
                                 f_iZBudgetType_LWU             = f_iRootZoneComp*1000 + 2
  CHARACTER(LEN=21),PARAMETER :: f_cDescription_RootZoneZBudget = 'Root zone zone budget'           
  CHARACTER(LEN=30),PARAMETER :: f_cDescription_LWUZBudget      = 'Land and water use zone budget'  

                                           
  ! -------------------------------------------------------------
  ! --- MISC. ENTITIES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                   :: ModNameLen = 23
  CHARACTER(LEN=ModNameLen),PARAMETER :: ModName    = 'Util_Package_RootZone::'
  
  
  
  
CONTAINS



  ! -------------------------------------------------------------
  ! --- SUBROUTINE TO READ DATA FROM PARAMETER FILE
  ! -------------------------------------------------------------
  SUBROUTINE ReadPointerData(File,cDescription,cFeatures,iRow,iCol,iFeatureIDs,DummyIntArray,iStat)
    TYPE(GenericFileType)           :: File
    CHARACTER(LEN=*),INTENT(IN)     :: cDescription,cFeatures
    INTEGER,INTENT(IN)              :: iRow,iCol,iFeatureIDs(iRow)
    INTEGER,ALLOCATABLE,INTENT(OUT) :: DummyIntArray(:,:)
    INTEGER,INTENT(OUT)             :: iStat
      
    !Local variables
    CHARACTER(LEN=ModNameLen+15),PARAMETER :: ThisProcedure = ModName // 'ReadPointerData'
    INTEGER                                :: indxElem,FirstLine(iCol),ErrorCode,IDs(iRow),iIndices(iRow)
      
    !Allocate memory for DummyIntArray
    IF (ALLOCATED(DummyIntArray)) DEALLOCATE(DummyIntArray,STAT=ErrorCode)
    ALLOCATE (DummyIntArray(iRow,iCol))
    
    !Read first line 
    CALL File%ReadData(FirstLine,iStat)  
    IF (iStat .EQ. -1) RETURN
      
    !If Element number in first line is zero, apply the data to all DummyIntArray
    IF (FirstLine(1) .EQ. 0) THEN
        DO indxElem=1,iRow
            DummyIntArray(indxElem,1)  = indxElem
            DummyIntArray(indxElem,2:) = FirstLine(2:)
        END DO
      
    !Otherwise, read data for each element seperately
    ELSE
        DummyIntArray(1,:) = FirstLine
        CALL File%ReadData(DummyIntArray(2:,:),iStat)  
        IF (iStat .EQ. -1) RETURN
        
        !Convert IDs to indices
        IDs = DummyIntArray(:,1)
        CALL ConvertID_To_Index(IDs,iFeatureIDs,iIndices)
        IF (ANY(iIndices.EQ.0)) THEN
            CALL SetLastMessage('One or more '//TRIM(cFeatures)//' listed for '//TRIM(LowerCase(cDescription))//' are not in the model!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        DummyIntArray(:,1) = iIndices
    END IF
            
  END SUBROUTINE ReadPointerData
    

  ! -------------------------------------------------------------
  ! --- SUBROUTINE TO READ REAL DATA FROM PARAMETER FILE
  ! -------------------------------------------------------------
  SUBROUTINE ReadRealData(File,cDescription,cFeatures,iRow,iCol,iFeatureIDs,DummyRealArray,iStat)
    TYPE(GenericFileType)           :: File
    CHARACTER(LEN=*),INTENT(IN)     :: cDescription,cFeatures
    INTEGER,INTENT(IN)              :: iRow,iCol,iFeatureIDs(iRow)
    REAL(8),ALLOCATABLE,INTENT(OUT) :: DummyRealArray(:,:)
    INTEGER,INTENT(OUT)             :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+12),PARAMETER :: ThisProcedure = ModName // 'ReadRealData'
    INTEGER                                :: indxElem,ErrorCode,IDs(iRow),iIndices(iRow)
    REAL(8)                                :: FirstLine(iCol)
    
    !Allocate memory for DummyRealArray
    IF (ALLOCATED(DummyRealArray)) DEALLOCATE(DummyRealArray,STAT=ErrorCode)
    ALLOCATE (DummyRealArray(iRow,iCol))
    
    !Read first line 
    CALL File%ReadData(FirstLine,iStat)  
    IF (iStat .EQ. -1) RETURN
    
    !If Element number in first line is zero, apply the data to all DummyIntArray
    IF (FirstLine(1) .EQ. 0) THEN
        DO indxElem=1,iRow
            DummyRealArray(indxElem,1)  = indxElem
            DummyRealArray(indxElem,2:) = FirstLine(2:)
        END DO
    
    !Otherwise, read data for each element seperately
    ELSE
        DummyRealArray(1,:) = FirstLine
        CALL File%ReadData(DummyRealArray(2:,:),iStat)  
        IF (iStat .EQ. -1) RETURN
        
        !Convert IDs to indices
        IDs = DummyRealArray(:,1)
        CALL ConvertID_To_Index(IDs,iFeatureIDs,iIndices)
        IF (ANY(iIndices.EQ.0)) THEN
            CALL SetLastMessage('One or more '//TRIM(cFeatures)//' listed for '//TRIM(LowerCase(cDescription))//' are not in the model!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        DummyRealArray(:,1) = iIndices
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