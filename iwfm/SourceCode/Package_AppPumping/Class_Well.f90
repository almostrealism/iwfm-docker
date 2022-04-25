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
MODULE Class_Well
  USE MessageLogger               , ONLY: SetLastMessage           , &
                                          MessageArray             , &
                                          f_iFatal                   
  USE GeneralUtilities            , ONLY: ConvertID_To_Index       , &
                                          IntToText                , &
                                          ShellSort                , &
                                          GetUniqueArrayComponents , &
                                          LocateInList
  USE Package_Discretization      , ONLY: AppGridType              , &
                                          StratigraphyType         
  USE IOInterface                 , ONLY: GenericFileType          
  USE Package_Misc                , ONLY: ElemGroupType            , &
                                          FlowDestinationType      , &
                                          f_iFlowDest_Outside      , &
                                          f_iFlowDest_Element      , &
                                          f_iFlowDest_Subregion    , &
                                          f_iFlowDest_ElementSet      
  USE Package_ComponentConnectors , ONLY: Supply_New                
  USE Class_Pumping               , ONLY: PumpingType              , &
                                          NormalizerFPumpColRaw    , &
                                          f_iDistTypeArray         , &
                                          f_iDestTypeArray
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
    INTEGER                               :: NWell,ErrorCode,indxWell,Element,NLayers,iElem,iRegion,ID,       &
                                             iNGroup,indxGroup,NElem,indxElem,iDest,indxWell1,iWell,iElemID,  &
                                             iElemIDs(AppGrid%NElements),iSubregionIDs(AppGrid%NSubregions),  &
                                             iRegionID,indxGroup1,iDestID
    REAL(8)                               :: FactXY,FactR,FactLT,DummyArray(6),X,Y,R,PerfBottom,PerfTop,      &
                                             DummyArray1(10)
    CHARACTER                             :: ALine*2000
    INTEGER,ALLOCATABLE                   :: Nodes(:),TempArray(:),iColIrigFrac(:),iColAdjust(:),iWellIDs(:), &
                                             Indices(:)
    REAL(8),ALLOCATABLE                   :: rFactor(:)
    TYPE(GenericFileType)                 :: WellDataFile
    TYPE(FlowDestinationType),ALLOCATABLE :: WellDest(:)
    TYPE(ElemGroupType),ALLOCATABLE       :: ElemGroups(:)
    LOGICAL,ALLOCATABLE                   :: lProcessed(:)
    
    !Initialize
    NLayers       = Stratigraphy%NLayers
    iElemIDs      = AppGrid%AppElement%ID
    iSubregionIDs = AppGrid%AppSubregion%ID
    
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
    ALLOCATE (Wells(NWell) , WellDest(NWell) , iColIrigFrac(NWell) , iColAdjust(NWell) , lProcessed(NWell) , iWellIDs(NWell) , STAT=ErrorCode)
    IF (ErrorCode .NE. 0) THEN
        CALL SetLastMessage('Error in allocating memory for the wells!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    !Read structural and location-related well data 
    DO indxWell=1,NWell
        CALL WellDataFile%ReadData(DummyArray,iStat)  ;  IF (iStat .EQ. -1) RETURN
        iWellIDs(indxWell) = INT(DummyArray(1))
        X                  = DummyArray(2) * FactXY       
        Y                  = DummyArray(3) * FactXY       
        R                  = DummyArray(4) * FactR / 2.0  
        PerfTop            = DummyArray(5) * FactLT       
        PerfBottom         = DummyArray(6) * FactLT  
        
        !Make sure well ID is not used more than once
        DO indxWell1=1,indxWell-1
            IF (iWellIDs(indxWell) .EQ. iWellIDs(indxWell1)) THEN
                CALL SetLastMessage('Well ID '//TRIM(IntToText(iWellIDs(indxWell)))//' is assigned to more than one well!',f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
        END DO
        
        !Find the element number that the well belongs to
        CALL AppGrid%FEInterpolate(X,Y,Element,Nodes,rFactor)
        IF (Element .LT. 1) THEN
            CALL SetLastMessage('Well '// TRIM(IntToText(iWellIDs(indxWell))) // ' is outside the model domain!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        
        !Instantiate well 
        CALL Well_(iWellIDs(indxWell),X,Y,R,PerfTop,PerfBottom,rFactor,Nodes,Element,AppGrid,Stratigraphy,Wells(indxWell),iStat)
        IF (iStat .EQ. -1) RETURN
      
    END DO
    
    !Read pumping specs for wells
    lProcessed = .FALSE.
    DO indxWell=1,NWell
        CALL WellDataFile%ReadData(DummyArray1,iStat)  ;  IF (iStat .EQ. -1) RETURN
        ID = INT(DummyArray1(1))
        
        !Make sure well ID is recognized
        CALL ConvertID_To_Index(ID,iWellIDs,iWell)
        IF (iWell .EQ. 0) THEN
            CALL SetLastMessage('Well ID '//TRIM(IntToText(ID))//' listed for well pumping characteristics is not recognized!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        
        !Make sure same well is not entered more than once
        IF (lProcessed(iWell)) THEN
            CALL SetLastMessage('Well ID '//TRIM(IntToText(ID))//' specified for well pumping characteristics is listed more than once!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        lProcessed(iWell) = .TRUE.
        
        !Process data
        Wells(iWell)%iColPump      = INT(DummyArray1(2))
        Wells(iWell)%rFPumpColRaw  =     DummyArray1(3)
        Wells(iWell)%iDistMethod   = INT(DummyArray1(4))
        WellDest(iWell)%iDestType  = INT(DummyArray1(5))  
        WellDest(iWell)%iDest      = INT(DummyArray1(6))  
        iColIrigFrac(iWell)        = INT(DummyArray1(7))
        iColAdjust(iWell)          = INT(DummyArray1(8))
        Wells(iWell)%iColPumpMax   = INT(DummyArray1(9))
        Wells(iWell)%rFPumpMaxCol  =     DummyArray1(10)
        
        !Make sure that a non-zero irrigation fraction column is supplied if pumping is delivered within the model domain
        IF (WellDest(iWell)%iDestType .NE. f_iFlowDest_Outside) THEN
            IF (iColIrigFrac(iWell) .LE. 0) THEN
                MessageArray(1) = 'Irrigation fraction column number for well ID '//TRIM(IntTotext(ID))
                MessageArray(2) = 'must be larger than zero when pumping is delivered within the model domain!'
                MessageArray(3) = 'Alternatively, pumping can be delivered outside the model domain.'
                CALL SetLastMessage(MessageArray(1:3),f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
        END IF
        
        !Make sure that iDistMethod is an acceptable value
        IF (.NOT. ANY(Wells(iWell)%iDistMethod .EQ. f_iDistTypeArray)) THEN
            CALL SetLastMessage('Pumping distribution option (IOPTWL) for well ID ' // TRIM(IntToText(ID)) // ' is not recognized!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        
        !Make sure that destination type is recognized
        IF (.NOT. ANY(WellDest(iWell)%iDestType .EQ. f_iDestTypeArray)) THEN
            CALL SetLastMessage('Destination type for well ID '//TRIM(IntToText(ID))//' is not recognized!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        
        !Delivery region
        IF (WellDest(iWell)%iDestType .LT. 0) THEN
            !Delivery to its own element
            iElem                       = Wells(iWell)%Element
            WellDest(iWell)%iDestType   = f_iFlowDest_Element
            WellDest(iWell)%iDest       = iElem
            WellDest(iWell)%iDestRegion = AppGrid%AppElement(iElem)%Subregion
        
        !Otherwise
        ELSE
            SELECT CASE (WellDest(iWell)%iDestType)
                CASE (f_iFlowDest_Outside)
                    !Do nothing
                
                CASE (f_iFlowDest_Element)
                    iElemID = WellDest(iWell)%iDest
                    CALL ConvertID_To_Index(iElemID,iElemIDs,iElem)
                    IF (iElem .EQ. 0) THEN
                        CALL SetLastMessage('Destination element '//TRIM(IntToText(iElemID))//' listed for well '//TRIM(IntToText(ID))//' is not in the model!',f_iFatal,ThisProcedure)
                        iStat = -1
                        RETURN
                    END IF
                    WellDest(iWell)%iDest       = iElem
                    WellDest(iWell)%iDestRegion = AppGrid%AppElement(iElem)%Subregion
                    
                CASE (f_iFlowDest_Subregion)
                    iRegionID = WellDest(iWell)%iDest
                    CALL ConvertID_To_Index(iRegionID,iSubregionIDs,iRegion)
                    IF (iRegion.EQ. 0) THEN
                        CALL SetLastMessage('Destination subregion '//TRIM(IntToText(iRegionID))//' listed for well '//TRIM(IntToText(ID))//' is not in the model!',f_iFatal,ThisProcedure)
                        iStat = -1
                        RETURN
                    END IF
                    WellDest(iWell)%iDest       = iRegion
                    WellDest(iWell)%iDestRegion = iRegion
                    
                CASE (f_iFlowDest_ElementSet)
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
        ElemGroups(indxGroup)%ID = ID
        
        !Make sure same element group ID is not used more than once
        DO indxGroup1=1,indxGroup-1
            IF (ID .EQ. ElemGroups(indxGroup1)%ID) THEN
                CALL SetLastMessage('Element group ID '//TRIM(IntToText(ID))//' for well pumping destinations is specified more than once!',f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
        END DO
        
        !Cycle if no elements are listed
        IF (NElem .LE. 0) CYCLE
        
        !Allocate memory and store the initial readings
        ALLOCATE (ElemGroups(indxGroup)%iElems(NElem))
        DEALLOCATE (Indices, STAT=ErrorCode)  ;  ALLOCATE (Indices(NElem))
        ElemGroups(indxGroup)%NElems    = NElem
        ElemGroups(indxGroup)%iElems(1) = iElem
        
        !Read the rest of the elements 
        DO indxElem=2,NElem
            CALL WellDataFile%ReadData(iElem,iStat)  ;  IF (iStat .EQ. -1) RETURN
            ElemGroups(indxGroup)%iElems(indxElem) = iElem
        END DO     
        
        !Order the element numbers
        CALL ShellSort(ElemGroups(indxGroup)%iElems)
        
        !Make sure elements are in the model
        CALL ConvertID_To_Index(ElemGroups(indxGroup)%iElems,iElemIDs,Indices)
        IF (ANY(Indices.EQ.0)) THEN
            CALL SetLastMessage('One or more elements listed in element group ID '//TRIM(IntToText(ID))//' listed for well pumping destination are not in the model!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        ElemGroups(indxGroup)%iElems = Indices
        
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
       IF (WellDest(indxWell)%iDestType .EQ. f_iFlowDest_ElementSet) THEN
           iDestID = WellDest(indxWell)%iDest
           
           !Make sure element group is defined
           iDest = LocateInList(iDestID,ElemGroups%ID)
           IF (iDest .EQ. 0) THEN
               ID = Wells(indxWell)%ID
               CALL SetLastMessage('Element group number '//TRIM(IntToText(iDestID))//' to which well ID '//TRIM(IntToText(ID))//' is delivered is not defined!',f_iFatal,ThisProcedure) 
               iStat = -1
               RETURN
           END IF
           WellDest(indxWell)%iDest = iDest
           
           !Make sure there is at least one element in the group
           IF (ElemGroups(iDest)%NElems .EQ. 0) THEN
               ID = Wells(indxWell)%ID
               CALL SetLastMessage('Element group '//TRIM(IntToText(iDest))//' as destination for well pumping '//TRIM(IntToText(ID))//' has no elements listed!',f_iFatal,ThisProcedure)
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
    DEALLOCATE (WellDest , ElemGroups , Nodes , TempArray , iColIrigFrac , iColAdjust , Indices , STAT=ErrorCode)
    
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
                                             ScreenLength,AquiferThick,ScreenFrac                                        
    
    !Initialize
    iStat   = 0
    NLayers = Stratigraphy%NLayers
    NVertex = SIZE(Nodes)
    
    !Allocate memory for the (node,layer) distribution fractions
    ALLOCATE (Well%rLayerFactor(NLayers) , Well%rNodePumpRequired(NVertex,NLayers) , Well%rNodePumpActual(NVertex,NLayers))
    
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
        CALL SetLastMessage(MessageArray(1:4),f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    IF (PerfBottomLayer .EQ. 0) THEN 
        WRITE (MessageArray(1),'(A,F10.4)') 'Bottom elevation of screen for well '//TRIM(IntToText(WellID))//' is not in the vertical extend of the aquifer!'
        WRITE (MessageArray(2),'(A,F10.4)') 'Bottom elevation of well screen = ',PerfBottomWork
        WRITE (MessageArray(3),'(A,F10.4)') 'Ground surface elevation        = ',GSElev       
        WRITE (MessageArray(4),'(A,F10.4)') 'Elevation of aquifer bottom     = ',BottomMax
        CALL SetLastMessage(MessageArray(1:4),f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    !Compute screen length as a fraction of aquifer thickness and vertical pumping distribution factors
    ASSOCIATE (pBottomElev => Stratigraphy%BottomElev  ,  &
               pTopElev    => Stratigraphy%TopElev     )
      Well%rLayerFactor = 0.0
      Top               = PerfTopWork
      AquiferTop        = SUM(pTopElev(Nodes,1) * rFactor)
      DO indxLayer=PerfTopLayer,PerfBottomLayer
        AquiferBot   = SUM(pBottomElev(Nodes,indxLayer) * rFactor)
        Bottom       = MAX(PerfBottomWork , AquiferBot)
        ScreenLength = Top - Bottom
        AquiferThick = AquiferTop - AquiferBot
        IF (AquiferThick .GT. 0.0) THEN
            ScreenFrac = ScreenLength / AquiferThick
            IF (ScreenFrac .GT. 0.0) &
                Well%rLayerFactor(indxLayer) = ScreenFrac *(1.0 + 7.0 *SQRT(R/(2.0*ScreenLength)) * COS(PiHalf*ScreenFrac))
        END IF
        Top        = Bottom
        AquiferTop = AquiferBot
      END DO
    END ASSOCIATE
    
    !Instantiate object
    Well%ID          = WellID
    Well%Element     = iElement
    Well%X           = X
    Well%Y           = Y
    Well%R           = R
    Well%PerfTop     = PerfTopWork
    Well%PerfBottom  = PerfBottomWork
    Well%rNodeFactor = rFactor
    
  END SUBROUTINE Well_
  
  
END MODULE