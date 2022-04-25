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
MODULE Class_ElementPumping
  USE MessageLogger               , ONLY: SetLastMessage           , &
                                          MessageArray             , &
                                          f_iFatal                   
  USE GeneralUtilities            , ONLY: ConvertID_To_Index       , &
                                          IntToText                , &
                                          LocateInList             , &
                                          GetUniqueArrayComponents , &
                                          ShellSort                , &
                                          NormalizeArray
  USE IOInterface                 , ONLY: GenericFileType           
  USE Package_Discretization      , ONLY: AppGridType              , &
                                          StratigraphyType
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
  PUBLIC :: ElemPumpType , &
            ElemPump_New


  ! -------------------------------------------------------------
  ! --- ELEMENT PUMP DATA TYPE
  ! -------------------------------------------------------------
  TYPE,EXTENDS(PumpingType) :: ElemPumpType
  END TYPE ElemPumpType
  
  
  ! -------------------------------------------------------------
  ! --- MISC. ENTITIES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                   :: ModNameLen = 22
  CHARACTER(LEN=ModNameLen),PARAMETER :: ModName = 'Class_ElementPumping::'




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
  ! --- NEW ELEMENT PUMPING SET
  ! -------------------------------------------------------------
  SUBROUTINE ElemPump_New(cFileName,AppGrid,Stratigraphy,ElemPump,iStat)
    CHARACTER(LEN=*),INTENT(IN)                :: cFileName
    TYPE(AppGridType),TARGET,INTENT(IN)        :: AppGrid
    TYPE(StratigraphyType),INTENT(IN)          :: Stratigraphy
    TYPE(ElemPumpType),ALLOCATABLE,INTENT(OUT) :: ElemPump(:)
    INTEGER,INTENT(OUT)                        :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+12)          :: ThisProcedure = ModName // 'ElemPump_New'
    INTEGER                               :: NSink,indxSink,ErrorCode,iElem,indx,NLayers,indxLayer,NVertex,   &
                                             Vertex(4),iNGroup,indxGroup,ID,NElem,indxElem,iDest,iElemID,     &
                                             iElemIDs(AppGrid%NElements),iSubregionIDs(AppGrid%NSubregions),  &
                                             iDestID,indxGroup1
    REAL(8)                               :: DummyArray(10+Stratigraphy%NLayers)
    INTEGER,ALLOCATABLE                   :: TempArray(:),iColIrigFrac(:),iColAdjust(:),Indices(:)
    CHARACTER                             :: ALine*2000
    TYPE(GenericFileType)                 :: ElemPumpDataFile
    TYPE(FlowDestinationType),ALLOCATABLE :: ElemPumpDest(:)
    TYPE(ElemGroupType),ALLOCATABLE       :: ElemGroups(:)
  
    !Initialize
    iStat         = 0
    iElemIDs      = AppGrid%AppElement%ID
    iSubregionIDs = AppGrid%AppSubregion%ID
    NLayers       = Stratigraphy%NLayers
    indx          = 4+NLayers
    
    !Open file
    CALL ElemPumpDataFile%New(FileName=cFileName , InputFile=.TRUE. , iStat=iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Read number of data entries
    CALL ElemPumpDataFile%ReadData(NSink,iStat)  ;  IF (iStat .EQ. -1) RETURN
    
    !Return if NSink is set to zero
    IF (NSink .EQ. 0) THEN
        CALL ElemPumpDataFile%Kill()
        RETURN
    END IF
    
    !Allocate memory
    ALLOCATE (ElemPump(NSink) , ElemPumpDest(NSink) , iColIrigFrac(NSink) , iColAdjust(NSink) , STAT=ErrorCode)
    IF (ErrorCode .NE. 0) THEN
        CALL SetLastMessage('Error in allocating memory for element pumping specifications!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    !Process data
    DO indxSink=1,NSink
        !Read data
        CALL ElemPumpDataFile%ReadData(DummyArray,iStat)  ;  IF (iStat .EQ. -1) RETURN
        
        !Element
        iElemID = INT(DummyArray(1))
        
        !Make sure element is legit
        CALL ConvertID_To_Index(iElemID,iElemIDS,iElem)
        IF (iElem .EQ. 0) THEN
            CALL SetLastMessage('Element '//TRIM(IntToText(iElemID))//' listed for element pumping is not in the model!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        
        !Vertex information for element
        NVertex = AppGrid%NVertex(iElem)
        Vertex  = AppGrid%Vertex(:,iElem)
        
        !Allocate memory for vertical and to-individual-node distribution fractions
        ALLOCATE (ElemPump(indxSink)%rLayerFactor(NLayers) , ElemPump(indxSink)%rNodePumpRequired(NVertex,NLayers) , ElemPump(indxSink)%rNodePumpActual(NVertex,NLayers) , STAT=ErrorCode)
        IF (ErrorCode .NE. 0) THEN
            CALL SetLastMessage('Error in allocating memory for the vertical distribution fractions for pumping at element ' // TRIM(IntToText(iElemID)) // '!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        ElemPump(indxSink)%rLayerFactor      = 0.0
        ElemPump(indxSink)%rNodePumpRequired = 0.0
        ElemPump(indxSink)%rNodePumpActual   = 0.0
        
        !Store information
        ElemPump(indxSink)%ID            = iElemID
        ElemPump(indxSink)%Element       = iElem
        ElemPump(indxSink)%iColPump      = INT(DummyArray(2))
        ElemPump(indxSink)%rFPumpColRaw  =     DummyArray(3)
        ElemPump(indxSink)%iDistMethod   = INT(DummyArray(4))
        ElemPump(indxSink)%rLayerFactor  =     DummyArray(5:indx)
        ElemPumpDest(indxSink)%iDestType = INT(DummyArray(indx+1))  
        ElemPumpDest(indxSink)%iDest     = INT(DummyArray(indx+2))  
        iColIrigFrac(indxSink)           = INT(DummyArray(indx+3))
        iColAdjust(indxSink)             = INT(DummyArray(indx+4))
        ElemPump(indxSink)%iColPumpMax   = INT(DummyArray(indx+5))
        ElemPump(indxSink)%rFPumpMaxCol  =     DummyArray(indx+6)
        
        !Make sure that a non-zero irrigation fraction column is supplied if pumping is delievred withion the model domain
        IF (ElemPumpDest(indxSink)%iDestType .NE. f_iFlowDest_Outside) THEN
            IF (iColIrigFrac(indxSink) .LE. 0) THEN
                MessageArray(1) = 'Irrigation fraction column number for element pumping at element '//TRIM(IntTotext(iElemID))
                MessageArray(2) = 'must be larger than zero when pumping is delivered within the model domain!'
                MessageArray(3) = 'Alternatively, pumping can be delivered outside the model domain.'
                CALL SetLastMessage(MessageArray(1:3),f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
        END IF
        
        !Make sure that iDistMethod is an acceptable value
        IF (.NOT. ANY(ElemPump(indxSink)%iDistMethod .EQ. f_iDistTypeArray)) THEN
            CALL SetLastMessage('Pumping distribution option (IOPTSK) for element ' // TRIM(IntToText(iElemID)) // ' is not recognized!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        
        !Make sure that destination type is recognized
        IF (.NOT. ANY(ElemPumpDest(indxSink)%iDestType .EQ. f_iDestTypeArray)) THEN
            CALL SetLastMessage('Destination type for element '//TRIM(IntToText(iElemID))//' is not recognized!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        
        !Delivery region
        IF (ElemPumpDest(indxSink)%iDestType .LT. 0) THEN
            !Delivery to its own element
            ElemPumpDest(indxSink)%iDestType   = f_iFlowDest_Element
            ElemPumpDest(indxSink)%iDest       = iElem
            ElemPumpDest(indxSink)%iDestRegion = AppGrid%AppElement(iElem)%Subregion
        
        !Otherwise
        ELSE
            SELECT CASE (ElemPumpDest(indxSink)%iDestType)
                CASE (f_iFlowDest_Outside)
                    !Do nothing
                CASE (f_iFlowDest_Element)
                    iDestID = ElemPumpDest(indxSink)%iDest
                    CALL ConvertID_To_Index(iDestID,iElemIDs,iDest)
                    IF (iDest .EQ. 0) THEN
                        CALL SetLastMessage('Destination element '//TRIM(IntToText(iDestID))//' listed for pumping at element '//TRIM(IntToText(iElemID))//' is not in the model!',f_iFatal,ThisProcedure)
                        iStat = -1
                        RETURN
                    END IF
                    ElemPumpDest(indxSink)%iDest       = iDest
                    ElemPumpDest(indxSink)%iDestRegion = AppGrid%AppElement(iDest)%Subregion
                CASE (f_iFlowDest_Subregion)
                    iDestID = ElemPumpDest(indxSink)%iDest
                    CALL ConvertID_To_Index(iDestID,iSubregionIDs,iDest)
                    IF (iDest .EQ. 0) THEN
                        CALL SetLastMessage('Subregion '//TRIM(IntToText(iDestID))//' listed as the destination for pumping from element '//TRIM(IntToText(iElemID))//' is not in the model!',f_iFatal,ThisProcedure)
                        iStat = -1
                        RETURN
                    END IF
                    ElemPumpDest(indxSink)%iDest       = iDest
                    ElemPumpDest(indxSink)%iDestRegion = iDest
                CASE (f_iFlowDest_ElementSet)
                    !Do nothing for now. Will do more processing when element groups are read
            END SELECT
        END IF
        
        !Normalize vertical pumping distribution factors; make sure no pumping is assigned to a layer with all inactive nodes
        DO indxLayer=1,NLayers
            IF (ALL(Stratigraphy%ActiveNode(Vertex(1:NVertex),indxLayer) .EQ. .FALSE.))   &
                ElemPump(indxSink)%rLayerFactor(indxLayer) = 0.0
        END DO
        CALL NormalizeArray(ElemPump(indxSink)%rLayerFactor)
      
    END DO
    
    !Read element group data served by element pumping
    CALL ElemPumpDataFile%ReadData(iNGroup,iStat)  ;  IF (iStat .EQ. -1) RETURN
    ALLOCATE (ElemGroups(iNGroup))
    DO indxGroup=1,iNGroup
        CALL ElemPumpDataFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN
        READ (ALine,*) ID,NElem,iElem
        ElemGroups(indxGroup)%ID = ID
        
        !Make sure same element group ID is not used more than once
        DO indxGroup1=1,indxGroup-1
            IF (ID .EQ. ElemGroups(indxGroup1)%ID) THEN
                CALL SetLastMessage('Element group ID '//TRIM(IntToText(ID))//' for element pumping destinations is specified more than once!',f_iFatal,ThisProcedure)
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
            CALL ElemPumpDataFile%ReadData(iElem,iStat)  ;  IF (iStat .EQ. -1) RETURN
            ElemGroups(indxGroup)%iElems(indxElem) = iElem
        END DO     
        
        !Order the element numbers
        CALL ShellSort(ElemGroups(indxGroup)%iElems)
        
        !Make sure elements are in the model
        CALL ConvertID_To_Index(ElemGroups(indxGroup)%iElems,iElemIDs,Indices)
        IF (ANY(Indices.EQ.0)) THEN
            CALL SetLastMessage('One or more elements listed in element group ID '//TRIM(IntToText(ID))//' listed for element pumping destination are not in the model!',f_iFatal,ThisProcedure)
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

    !Assign element groups to element pumps
    DO indxSink=1,NSink
       IF (ElemPumpDest(indxSink)%iDestType .EQ. f_iFlowDest_ElementSet) THEN
           iDestID = ElemPumpDest(indxSink)%iDest
           
           iDest = LocateInList(iDestID,ElemGroups%ID)
           IF (iDest .EQ. 0) THEN
               ID = ElemPump(indxSink)%ID
               CALL SetLastMessage('Element group number '//TRIM(IntToText(iDestID))//' to which pumping from element '//TRIM(IntToText(ID))//' is delivered is not defined!',f_iFatal,ThisProcedure)  
               iStat = -1
               RETURN
           END IF
           ElemPumpDest(indxSink)%iDest = iDest
           
           !Make sure there is at least one element in the group
           IF (ElemGroups(iDest)%NElems .EQ. 0) THEN
               ID = ElemPump(indxSink)%ID
               CALL SetLastMessage('Element group '//TRIM(IntToText(iDestID))//' as destination for pumping from element '//TRIM(IntToText(ID))//' has no elements listed!',f_iFatal,ThisProcedure)
               iStat = -1
               RETURN
           END IF
      
           !Assign element group to element pumping
           ElemPumpDest(indxSink)%iDestRegion = AppGrid%AppElement(ElemGroups(iDest)%iElems(1))%Subregion
           ElemPumpDest(indxSink)%iDestElems  = ElemGroups(iDest)
       END IF
    END DO

    !Compile information regarding the elements served by the pumps
    CALL Supply_New(iColIrigFrac,iColAdjust,ElemPumpDest,ElemPump)
    
    !Normalize the pumping distribution fractions
    CALL NormalizerFPumpColRaw(ElemPump%iColPump,ElemPump%rFPumpColRaw)
    
    !Initialize the dynamic pumping distribution fractions to be equal to the ones defined by the user
    ElemPump%rFPumpCol = ElemPump%rFPumpColRaw
    
    !Clear memory
    DEALLOCATE (ElemPumpDest , ElemGroups , TempArray , iColIrigFrac , iColAdjust , Indices , STAT=ErrorCode)
    
    !Close file
    CALL ElemPumpDataFile%Kill()
    
  END SUBROUTINE ElemPump_New
  
  
END MODULE