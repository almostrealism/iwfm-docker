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
MODULE Class_ElementPumping
  USE MessageLogger               , ONLY: SetLastMessage   , &
                                          MessageArray     , &
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
                                             Vertex(4),iDestElem,iRegion,iNGroup,indxGroup,ID,NElem,indxElem, &
                                             iDest
    REAL(8)                               :: DummyArray(10+Stratigraphy%NLayers)
    INTEGER,ALLOCATABLE                   :: TempArray(:),iColIrigFrac(:),iColAdjust(:)
    CHARACTER                             :: ALine*2000
    TYPE(GenericFileType)                 :: ElemPumpDataFile
    TYPE(FlowDestinationType),ALLOCATABLE :: ElemPumpDest(:)
    TYPE(ElemGroupType),ALLOCATABLE       :: ElemGroups(:)
  
    !Initialize
    iStat   = 0
    NLayers = Stratigraphy%NLayers
    indx    = 4+NLayers
    
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
        CALL SetLastMessage('Error in allocating memory for element pumping specifications!',iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    !Process data
    DO indxSink=1,NSink
      !Read data
      CALL ElemPumpDataFile%ReadData(DummyArray,iStat)  ;  IF (iStat .EQ. -1) RETURN
      
      !Element
      iElem = INT(DummyArray(1))
      
      !Vertex information for element
      NVertex = AppGrid%Element(iElem)%NVertex
      Vertex  = AppGrid%Element(iElem)%Vertex
      
      !Allocate memory for vertical and to-individual-node distribution fractions
      ALLOCATE (ElemPump(indxSink)%rLayerFactor(NLayers) , ElemPump(indxSink)%rNodePumpFactor(NVertex,NLayers) , STAT=ErrorCode)
      IF (ErrorCode .NE. 0) THEN
          CALL SetLastMessage('Error in allocating memory for the vertical distribution fractions for pumping at element ' // TRIM(IntToText(iElem)) // '!',iFatal,ThisProcedure)
          iStat = -1
          RETURN
      END IF
      ElemPump(indxSink)%rLayerFactor    = 0.0
      ElemPump(indxSink)%rNodePumpFactor = 0.0

      !Store information
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
      IF (ElemPumpDest(indxSink)%iDestType .NE. FlowDest_Outside) THEN
          IF (iColIrigFrac(indxSink) .LE. 0) THEN
              MessageArray(1) = 'Irrigation fraction column number for element pumping at element '//TRIM(IntTotext(iElem))
              MessageArray(2) = 'must be larger than zero when pumping is delivered within the model domain!'
              MessageArray(3) = 'Alternatively, pumping can be delivered outside the model domain.'
              CALL SetLastMessage(MessageArray(1:3),iFatal,ThisProcedure)
              iStat = -1
              RETURN
          END IF
      END IF
      
      !Make sure that iDistMethod is an acceptable value
      IF (.NOT. ANY(ElemPump(indxSink)%iDistMethod .EQ. iDistTypeArray)) THEN
          CALL SetLastMessage('Pumping distribution option (IOPTSK) for element ' // TRIM(IntToText(iElem)) // ' is not recognized!',iFatal,ThisProcedure)
          iStat = -1
          RETURN
      END IF
      
      !Make sure that destination type is recognized
      IF (.NOT. ANY(ElemPumpDest(indxSink)%iDestType .EQ. DestTypeArray)) THEN
          CALL SetLastMessage('Destination type for element '//TRIM(IntToText(iElem))//' is not recognized!',iFatal,ThisProcedure)
          iStat = -1
          RETURN
      END IF

      !Delivery region
      IF (ElemPumpDest(indxSink)%iDestType .LT. 0) THEN
        !Delivery to its own element
        ElemPumpDest(indxSink)%iDestType   = FlowDest_Element
        ElemPumpDest(indxSink)%iDest       = iElem
        ElemPumpDest(indxSink)%iDestRegion = AppGrid%AppElement(iElem)%Subregion
      
      !Otherwise
      ELSE
        SELECT CASE (ElemPumpDest(indxSink)%iDestType)
          CASE (FlowDest_Outside)
            !Do nothing
          CASE (FlowDest_Element)
            iDestElem = ElemPumpDest(indxSink)%iDest
            IF (iDestElem .GT. 0) ElemPumpDest(indxSink)%iDestRegion = AppGrid%AppElement(iDestElem)%Subregion
          CASE (FlowDest_Subregion)
            iRegion = ElemPumpDest(indxSink)%iDest
            IF (iRegion .GT. 0) ElemPumpDest(indxSink)%iDestRegion = iRegion
          CASE (FlowDest_ElementSet)
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
        
        !Make sure groups are entered sequentially
        IF (ID .NE. indxGroup) THEN
            MessageArray(1) = 'Element group IDs for element pumping delivery locations must be entered sequentially!'
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
            CALL ElemPumpDataFile%ReadData(iElem,iStat)  ;  IF (iStat .EQ. -1) RETURN
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

    !Assign element groups to element pumps
    DO indxSink=1,NSink
       IF (ElemPumpDest(indxSink)%iDestType .EQ. FlowDest_ElementSet) THEN
           iDest = ElemPumpDest(indxSink)%iDest
           
           !Make sure element group is defined
           IF (iDest.LT.1  .OR.  iDest.GT.iNGroup) THEN
               CALL SetLastMessage('Element group number '//TRIM(IntToText(iDest))//' to which element pumping '//TRIM(IntToText(indxSink))//' is delivered is not defined!',iFatal,ThisProcedure)  
               iStat = -1
               RETURN
           END IF
           
           !Make sure there is at least one element in the group
           IF (ElemGroups(iDest)%NElems .EQ. 0) THEN
               CALL SetLastMessage('Element group '//TRIM(IntToText(iDest))//' as destination for element pumping '//TRIM(IntToText(indxSink))//' has no elements listed!',iFatal,ThisProcedure)
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
    DEALLOCATE (ElemPumpDest , ElemGroups , TempArray , iColIrigFrac , iColAdjust , STAT=ErrorCode)
    
    !Close file
    CALL ElemPumpDataFile%Kill()
    
  END SUBROUTINE ElemPump_New
  
  
END MODULE