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
MODULE Class_Well
  USE MessageLogger               , ONLY: SetLastMessage  , &
                                          MessageArray    , &
                                          iFatal
  USE GeneralUtilities
  USE IOInterface
  USE Package_ComponentConnectors , ONLY: Supply_New            
  USE Class_Pumping
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
  PUBLIC :: WellType                    ,  &
            Well_New                    


  ! -------------------------------------------------------------
  ! --- WELL DATA TYPE
  ! -------------------------------------------------------------
  TYPE,EXTENDS(PumpingType) :: WellType
    REAL(8)             :: X                 = 0.0    !x-coordinate of well location
    REAL(8)             :: Y                 = 0.0    !y-coordinate of well location
    REAL(8)             :: R                 = 0.0    !Well radius
    REAL(8)             :: PerfTop           = 0.0    !Elevation of top of perforation
    REAL(8)             :: PerfBottom        = 0.0    !Elevation of bottom of perforation
    REAL(8),ALLOCATABLE :: rNodeFactor(:)             !Factor of well pumping that is imposed on the surrounding node 
  END TYPE WellType
  
  
  ! -------------------------------------------------------------
  ! --- MISC. ENTITIES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER           :: ModNameLen = 12
  CHARACTER(LEN=12),PARAMETER :: ModName = 'Class_Well::'

  
  
  
CONTAINS




! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** CONSTRUCTORS
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- NEW WELL SET
  ! -------------------------------------------------------------
  SUBROUTINE Well_New(cFileName,AppGrid,Stratigraphy,Wells,iStat)
    CHARACTER(LEN=*),INTENT(IN)            :: cFileName
    TYPE(AppGridType),INTENT(IN)           :: AppGrid
    TYPE(StratigraphyType),INTENT(IN)      :: Stratigraphy
    TYPE(WellType),ALLOCATABLE,INTENT(OUT) :: Wells(:)
    INTEGER,INTENT(OUT)                    :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+8),PARAMETER :: ThisProcedure = ModName // 'Well_New'
    INTEGER                               :: NWell,ErrorCode,indxWell,ID,Element,NLayers,iElem,iRegion,  &
                                             iNGroup,indxGroup,NElem,indxElem,iDest
    REAL(8)                               :: FactXY,FactR,FactLT,DummyArray(6),X,Y,R,PerfBottom,PerfTop, &
                                             DummyArray1(10)
    CHARACTER                             :: ALine*2000
    INTEGER,ALLOCATABLE                   :: Nodes(:),TempArray(:),iColIrigFrac(:),iColAdjust(:)
    REAL(8),ALLOCATABLE                   :: rFactor(:)
    TYPE(GenericFileType)                 :: WellDataFile
    TYPE(FlowDestinationType),ALLOCATABLE :: WellDest(:)
    TYPE(ElemGroupType),ALLOCATABLE       :: ElemGroups(:)
    
    !Initialize
    NLayers = Stratigraphy%NLayers
    
    !Open file
    CALL WellDataFile%New(FileName=cFileName , InputFile=.TRUE. , iStat=iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Read number of wells
    CALL WellDataFile%ReadData(NWell,iStat)  ;  IF (iStat .EQ. -1) RETURN
    IF (NWell .EQ. 0) THEN
      CALL WellDataFile%Kill()
      RETURN
    END IF
    
    !Read conversion factors
    CALL WellDataFile%ReadData(FactXY,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL WellDataFile%ReadData(FactR,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL WellDataFile%ReadData(FactLT,iStat)  ;  IF (iStat .EQ. -1) RETURN
    
    !Allocate memory
    ALLOCATE (Wells(NWell) , WellDest(NWell) , iColIrigFrac(NWell) , iColAdjust(NWell) , STAT=ErrorCode)
    IF (ErrorCode .NE. 0) THEN
        CALL SetLastMessage('Error in allocating memory for the wells!',iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    !Read structural and location-related well data 
    DO indxWell=1,NWell
        CALL WellDataFile%ReadData(DummyArray,iStat)  ;  IF (iStat .EQ. -1) RETURN
        ID = INT(DummyArray(1))
        IF (ID .NE. indxWell) THEN
            MessageArray(1) = 'Well data should be entered sequentially!'
            MessageArray(2) = 'Well ID expected = ' // TRIM(IntToText(indxWell))
            MessageArray(3) = 'Well ID entered  = ' // TRIM(IntToText(ID))
            CALL SetLastMessage(MessageArray(1:3),iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        X          = DummyArray(2) * FactXY       
        Y          = DummyArray(3) * FactXY       
        R          = DummyArray(4) * FactR / 2.0  
        PerfTop    = DummyArray(5) * FactLT       
        PerfBottom = DummyArray(6) * FactLT      
        
        !Find the element number that the well belongs to
        CALL AppGrid%FEInterpolate(X,Y,Element,Nodes,rFactor)
        IF (Element .LT. 1) THEN
            CALL SetLastMessage('Well '// TRIM(IntToText(ID)) // 'is outside the model domain!',iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        
        !Instantiate well 
        CALL Well_(indxWell,X,Y,R,PerfTop,PerfBottom,rFactor,Nodes,Element,AppGrid,Stratigraphy,Wells(indxWell),iStat)
        IF (iStat .EQ. -1) RETURN
      
    END DO
    
    !Read pumping specs for wells
    DO indxWell=1,NWell
      CALL WellDataFile%ReadData(DummyArray1,iStat)  ;  IF (iStat .EQ. -1) RETURN
      ID = INT(DummyArray1(1))
      IF (ID .NE. indxWell) THEN
          MessageArray(1) = 'Pumping spec data for wells data should be entered sequentially!'
          MessageArray(2) = 'Well ID expected = ' // TRIM(IntToText(indxWell))
          MessageArray(3) = 'Well ID entered  = ' // TRIM(IntToText(ID))
          CALL SetLastMessage(MessageArray(1:3),iFatal,ThisProcedure)
          iStat = -1
          RETURN
      END IF
      Wells(indxWell)%iColPump      = INT(DummyArray1(2))
      Wells(indxWell)%rFPumpColRaw  =     DummyArray1(3)
      Wells(indxWell)%iDistMethod   = INT(DummyArray1(4))
      WellDest(indxWell)%iDestType  = INT(DummyArray1(5))  
      WellDest(indxWell)%iDest      = INT(DummyArray1(6))  
      iColIrigFrac(indxWell)        = INT(DummyArray1(7))
      iColAdjust(indxWell)          = INT(DummyArray1(8))
      Wells(indxWell)%iColPumpMax   = INT(DummyArray1(9))
      Wells(indxWell)%rFPumpMaxCol  =     DummyArray1(10)
      
      !Make sure that a non-zero irrigation fraction column is supplied if pumping is delievred within the model domain
      IF (WellDest(indxWell)%iDestType .NE. FlowDest_Outside) THEN
          IF (iColIrigFrac(indxWell) .LE. 0) THEN
              MessageArray(1) = 'Irrigation fraction column number for well ID '//TRIM(IntTotext(ID))
              MessageArray(2) = 'must be larger than zero when pumping is delivered within the model domain!'
              MessageArray(3) = 'Alternatively, pumping can be delivered outside the model domain.'
              CALL SetLastMessage(MessageArray(1:3),iFatal,ThisProcedure)
              iStat = -1
              RETURN
          END IF
      END IF
      
      !Make sure that iDistMethod is an acceptable value
      IF (.NOT. ANY(Wells(indxWell)%iDistMethod .EQ. iDistTypeArray)) THEN
          CALL SetLastMessage('Pumping distribution option (IOPTWL) for well ' // TRIM(IntToText(indxWell)) // ' is not recognized!',iFatal,ThisProcedure)
          iStat = -1
          RETURN
      END IF
      
      !Make sure that destination type is recognized
      IF (.NOT. ANY(WellDest(indxWell)%iDestType .EQ. DestTypeArray)) THEN
          CALL SetLastMessage('Destination type for well ID '//TRIM(IntToText(indxWell))//' is not recognized!',iFatal,ThisProcedure)
          iStat = -1
          RETURN
      END IF

      !Delivery region
      IF (WellDest(indxWell)%iDestType .LT. 0) THEN
        !Delivery to its own element
        iElem                          = Wells(indxWell)%Element
        WellDest(indxWell)%iDestType   = FlowDest_Element
        WellDest(indxWell)%iDest       = iElem
        WellDest(indxWell)%iDestRegion = AppGrid%AppElement(iElem)%Subregion
      
      !Otherwise
      ELSE
        SELECT CASE (WellDest(indxWell)%iDestType)
          CASE (FlowDest_Outside)
            !Do nothing
          CASE (FlowDest_Element)
            iElem = WellDest(indxWell)%iDest
            IF (iElem .GT. 0) WellDest(indxWell)%iDestRegion = AppGrid%AppElement(iElem)%Subregion
          CASE (FlowDest_Subregion)
            iRegion = WellDest(indxWell)%iDest
            IF (iRegion .GT. 0) WellDest(indxWell)%iDestRegion = iRegion
          CASE (FlowDest_ElementSet)
            !Do nothing for now. Will do more processing when element groups are read
        END SELECT
      END IF

    END DO
    
    !Read element group data served by well pumping
    CALL WellDataFile%ReadData(iNGroup,iStat)  ;  IF (iStat .EQ. -1) RETURN
    ALLOCATE (ElemGroups(iNGroup))
    DO indxGroup=1,iNGroup
        CALL WellDataFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN
        READ (ALine,*) ID,NElem,iElem
        
        !Make sure groups are entered sequentially
        IF (ID .NE. indxGroup) THEN
            MessageArray(1) = 'Element group IDs for well pumping delivery locations must be entered sequentially!'
            MessageArray(2) = 'Group ID expected = '//TRIM(IntToText(indxGroup))
            MessageArray(3) = 'Group ID entered  = '//TRIM(IntToText(ID))
            CALL SetLastMessage(MessageArray(1:3),iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        
        !Cycle if no elements are listed
        IF (NElem .LE. 0) CYCLE
        
        !Allocate memory and store the initial readings
        ALLOCATE (ElemGroups(indxGroup)%iElems(NElem))
        ElemGroups(indxGroup)%NElems    = NElem
        ElemGroups(indxGroup)%iElems(1) = iElem
        IF (iElem .GT. 0) iRegion = AppGrid%AppElement(iElem)%Subregion
        
        !Read the rest of the elements 
        DO indxElem=2,NElem
            CALL WellDataFile%ReadData(iElem,iStat)  ;  IF (iStat .EQ. -1) RETURN
            ElemGroups(indxGroup)%iElems(indxElem) = iElem
        END DO     
        
        !Order the element numbers
        CALL ShellSort(ElemGroups(indxGroup)%iElems)
        
        !Get rid of the dublicates
        CALL GetUniqueArrayComponents(ElemGroups(indxGroup)%iElems,TempArray)
        IF (SIZE(TempArray) .NE. ElemGroups(indxGroup)%NElems) THEN
            ElemGroups(indxGroup)%NElems = SIZE(TempArray)
            CALL MOVE_ALLOC(TempArray , ElemGroups(indxGroup)%iElems)
        END IF
        DEALLOCATE (TempArray , STAT=ErrorCode)
        
    END DO

    !Assign element groups to wells
    DO indxWell=1,NWell
       IF (WellDest(indxWell)%iDestType .EQ. FlowDest_ElementSet) THEN
           iDest = WellDest(indxWell)%iDest
           
           !Make sure element group is defined
           IF (iDest.LT.1  .OR.  iDest.GT.iNGroup) THEN
               CALL SetLastMessage('Element group number '//TRIM(IntToText(iDest))//' to which well number '//TRIM(IntToText(indxWell))//' is delivered is not defined!',iFatal,ThisProcedure) 
               iStat = -1
               RETURN
           END IF
           
           !Make sure there is at least one element in the group
           IF (ElemGroups(iDest)%NElems .EQ. 0) THEN
               CALL SetLastMessage('Element group '//TRIM(IntToText(iDest))//' as destination for well pumping '//TRIM(IntToText(indxWell))//' has no elements listed!',iFatal,ThisProcedure)
               iStat = -1
               RETURN
           END IF
       
           !Assign element group to well
           WellDest(indxWell)%iDestRegion = AppGrid%AppElement(ElemGroups(iDest)%iElems(1))%Subregion
           WellDest(indxWell)%iDestElems  = ElemGroups(iDest)
       END IF
    END DO

    !Compile information regarding the elements served by the wells
    CALL Supply_New(iColIrigFrac,iColAdjust,WellDest,Wells)
    
    !Normalize the read pumping distribution fractions
    CALL NormalizerFPumpColRaw(Wells%iColPump,Wells%rFPumpColRaw)
    
    !Set dynamic pumping distribution fractioons to read values
    Wells%rFPumpCol = Wells%rFPumpColRaw
    
    !Clear memory
    DEALLOCATE (WellDest , ElemGroups , Nodes , TempArray , iColIrigFrac , iColAdjust , STAT=ErrorCode)
    
    !Kill file
    CALL WellDataFile%Kill()
    
  END SUBROUTINE Well_New


  ! -------------------------------------------------------------
  ! --- NEW WELL
  ! -------------------------------------------------------------
  SUBROUTINE Well_(WellID,X,Y,R,PerfTop,PerfBottom,rFactor,Nodes,iElement,AppGrid,Stratigraphy,Well,iStat)
    REAL(8),INTENT(IN)                :: X,Y,R,PerfTop,PerfBottom,rFactor(:)
    INTEGER,INTENT(IN)                :: WellID,Nodes(:),iElement
    TYPE(AppGridType),INTENT(IN)      :: AppGrid
    TYPE(StratigraphyType),INTENT(IN) :: Stratigraphy
    TYPE(WellType)                    :: Well
    INTEGER,INTENT(OUT)               :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+7),PARAMETER :: ThisProcedure = ModName // 'Well_'
    REAL(8),PARAMETER                     :: PiHalf =  3.141592654d0/2d0
    INTEGER                               :: NLayers,PerfTopLayer,PerfBottomLayer,indxLayer,NVertex
    REAL(8)                               :: GSElev,PerfTopWork,PerfBottomWork,BottomMax,Top,Bottom,AquiferTop,AquiferBot,  &
                                             ScreenLength,AquiferThick,ScreenFrac(Stratigraphy%NLayers)                                        
    
    !Initialize
    iStat   = 0
    NLayers = Stratigraphy%NLayers
    NVertex = SIZE(Nodes)
    
    !Allocate memory for the (node,layer) distribution fractions
    ALLOCATE (Well%rLayerFactor(NLayers) , Well%rNodePumpFactor(NVertex,NLayers))
    
    !GSElev and BottomMax at well
    GSElev    = SUM(Stratigraphy%GSElev(Nodes) * rFactor)
    BottomMax = SUM(Stratigraphy%BottomElev(Nodes,NLayers) * rFactor)
    
    !Convert screen intervals to elevations if necessary
    IF (PerfBottom .GT. PerfTop) THEN
      PerfBottomWork = GSElev - PerfBottom
      PerfTopWork    = GSElev - PerfTop
    ELSE
      PerfBottomWork = PerfBottom
      PerfTopWork    = PerfTop
    END IF

    !Find the aquifer layers that top and bottom of the perforation belong
    PerfTopLayer    = Stratigraphy%GetLayerNumberForElevation(PerfTopWork,X,Y,AppGrid)
    PerfBottomLayer = Stratigraphy%GetLayerNumberForElevation(PerfBottomWork,X,Y,AppGrid)
    IF (PerfTopLayer .EQ. 0) THEN 
        WRITE (MessageArray(1),'(A,F10.4)') 'Top elevation of screen for well '//TRIM(IntToText(WellID))//' is not in the vertical extend of the aquifer!'
        WRITE (MessageArray(2),'(A,F10.4)') 'Top elevation of well screen = ',PerfTopWork
        WRITE (MessageArray(3),'(A,F10.4)') 'Ground surface elevation     = ',GSElev       
        WRITE (MessageArray(4),'(A,F10.4)') 'Elevation of aquifer bottom  = ',BottomMax
        CALL SetLastMessage(MessageArray(1:4),iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    IF (PerfBottomLayer .EQ. 0) THEN 
        WRITE (MessageArray(1),'(A,F10.4)') 'Bottom elevation of screen for well '//TRIM(IntToText(WellID))//' is not in the vertical extend of the aquifer!'
        WRITE (MessageArray(2),'(A,F10.4)') 'Bottom elevation of well screen = ',PerfBottomWork
        WRITE (MessageArray(3),'(A,F10.4)') 'Ground surface elevation        = ',GSElev       
        WRITE (MessageArray(4),'(A,F10.4)') 'Elevation of aquifer bottom     = ',BottomMax
        CALL SetLastMessage(MessageArray(1:4),iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    !Compute screen length as a fraction of aquifer thickness and vertical pumping distribution factors
    ASSOCIATE (pBottomElev => Stratigraphy%BottomElev  ,  &
               pTopElev    => Stratigraphy%TopElev     )
      ScreenFrac           = 0.0
      Well%rLayerFactor    = 0.0
      Top                  = PerfTopWork
      AquiferTop           = SUM(pTopElev(Nodes,1) * rFactor)
      DO indxLayer=PerfTopLayer,PerfBottomLayer
        AquiferBot   = SUM(pBottomElev(Nodes,indxLayer) * rFactor)
        Bottom       = MAX(PerfBottomWork , AquiferBot)
        ScreenLength = Top - Bottom
        AquiferThick = AquiferTop - AquiferBot
        IF (AquiferThick .GT. 0.0) THEN
            ScreenFrac(indxLayer)        = ScreenLength / AquiferThick
            Well%rLayerFactor(indxLayer) = ScreenFrac(indxLayer) *(1.0 + 7.0 *SQRT(R/(2.0*ScreenLength)) * COS(PiHalf*ScreenFrac(indxLayer)))
        END IF
        Top        = Bottom
        AquiferTop = AquiferBot
      END DO
    END ASSOCIATE
    
    !Instantiate object
    Well%Element     = iElement
    Well%X           = X
    Well%Y           = Y
    Well%R           = R
    Well%PerfTop     = PerfTopWork
    Well%PerfBottom  = PerfBottomWork
    Well%rNodeFactor = rFactor
    
  END SUBROUTINE Well_
  
  
END MODULE