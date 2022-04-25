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
MODULE Class_ZoneList
  USE MessageLogger          , ONLY: SetLastMessage           , &
                                     MessageArray             , &
                                     f_iFatal
  USE Class_BinaryTree       , ONLY: BinaryTreeType 
  USE GeneralUtilities       , ONLY: GetUniqueArrayComponents , &
                                     IntToText                , &
                                     CleanSpecialCharacters   , &
                                     StripTextUntilCharacter  , &
                                     LocateInList             , &
                                     ShellSort                , &
                                     FirstLocation            , &
                                     ConvertID_To_Index
  USE IOInterface            , ONLY: GenericFileType
  USE Class_SystemData       , ONLY: SystemDataType
  USE ZBudget_Parameters     , ONLY: f_iZoneHorizontal        , &
                                     f_iZoneVertical          , &
                                     f_iVerticalFlowType      , &
                                     f_iUndefinedZone 
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
  PUBLIC :: ZoneType         , &
            AdjacentZoneType , &
            ZoneListType    
  
  
  ! -------------------------------------------------------------
  ! --- DATA TYPE THAT STORES ADJACENT ZONE INFORMATION
  ! -------------------------------------------------------------
  TYPE AdjacentZoneType
      INTEGER                      :: ZoneNumber      =  f_iUndefinedZone
      REAL(8)                      :: FLOW_IN         =  0.0
      REAL(8)                      :: FLOW_OUT        =  0.0
      TYPE(BinaryTreeType),POINTER :: iFaceNumberList => NULL()          !List of faces between the zone and the adjacent zone for horizontal flow exchnage calculations
  CONTAINS
      PROCEDURE,PASS :: Kill => AdjacentZone_Kill
  END TYPE AdjacentZoneType

  
  ! -------------------------------------------------------------
  ! --- DATA TYPE THAT STORES ELEMENT NUMBERS IN A LAYER FOR A ZONE
  ! -------------------------------------------------------------
  TYPE LayerZoneElemType
      INTEGER,ALLOCATABLE :: Elements(:)
  END TYPE LayerZoneElemType
  
  
  ! -------------------------------------------------------------
  ! --- DATA TYPE THAT STORES ZONE INFORMATION
  ! -------------------------------------------------------------
  TYPE ZoneType
      INTEGER                             :: ZoneNumber             =  f_iUndefinedZone
      CHARACTER(LEN=50)                   :: cName                  = ''
      REAL(8)                             :: Area                   =  0.0            !Zone area    
      TYPE(LayerZoneElemType),ALLOCATABLE :: LayerZoneElements(:)                     !Elements in the zone at each layer; if zones are defined only in horizontal this data is defined only for top layer
      INTEGER                             :: NAdjacentZones         =  0                 
      INTEGER,ALLOCATABLE                 :: AdjacentZoneNumbers(:)                   !List of adjacent zone numbers
      TYPE(BinaryTreeType),POINTER        :: AdjacentZoneList       => NULL()            
      REAL(8),ALLOCATABLE                 :: AccumulatedData(:)                       !Data for each (data) column accumulated for the zone (vertical and horizontal flow exchange with adjacent zones are not part of this)
      REAL(8)                             :: Storage                =  0.0
  CONTAINS
      PROCEDURE,PASS :: Kill      => Zone_Kill
      PROCEDURE,PASS :: ZeroOutFlows
      PROCEDURE,PASS :: AddAdjacentZoneFlow
      PROCEDURE,PASS :: GetPackedZoneData
  END TYPE ZoneType


  ! -------------------------------------------------------------
  ! --- DATA TYPE THAT STORES LIST OF ZONES IN A BINARY TREE
  ! -------------------------------------------------------------
  TYPE,EXTENDS(BinaryTreeType) :: ZoneListType
      INTEGER             :: iZoneExtent         = f_iZoneHorizontal  !Type of zonation
      INTEGER,ALLOCATABLE :: ElemZones(:,:)                           !Zone numbers that each element belong to; listed for each (element,layer) combination
      INTEGER,ALLOCATABLE :: iOrderedZoneList(:)                      !Ordered list of zone numbers
  CONTAINS
      PROCEDURE,PASS :: GenerateZoneList_DataFromASCIIFile
      PROCEDURE,PASS :: GenerateZoneList
      PROCEDURE,PASS :: Kill                               => ZoneList_Kill
      PROCEDURE,PASS :: GetNZones
      PROCEDURE,PASS :: GetNames
      PROCEDURE,PASS :: GetNMaxAdjacentZones
      PROCEDURE,PASS :: GetNAdjacentZones
      GENERIC        :: New                                => GenerateZoneList_DataFromASCIIFile , &
                                                              GenerateZoneList
  END TYPE ZoneListType
  
  
  ! -------------------------------------------------------------
  ! --- MISCELLENEOUS VARIABLES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                   :: ModNameLen = 16
  CHARACTER(LEN=ModNameLen),PARAMETER :: ModName    = 'Class_ZoneList::'


  
  
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
  ! --- NEW ZONE LIST
  ! -------------------------------------------------------------
  SUBROUTINE ZoneList_New(ZoneList,iNDataCols,lElemToElemFlow_Defined,ElemZones,iZoneListWithNames,cZoneNames,SystemData)
    CLASS(ZoneListType)             :: ZoneList
    LOGICAL,INTENT(IN)              :: lElemToElemFlow_Defined
    INTEGER,INTENT(IN)              :: iNDataCols,ElemZones(:,:),iZoneListWithNames(:)
    CHARACTER(LEN=*),INTENT(IN)     :: cZoneNames(:)
    TYPE(SystemDataType),INTENT(IN) :: SystemData

    !Local variables
    INTEGER                :: indxLayer,indxFace,iElem1,iElem2,ZoneNum1,ZoneNum2,NLayers,NNodes,NVertex1,NVertex2,Vertex1(4),Vertex2(4), &
                              indxZone,iZone,iNZoneElems,NZones,indxElem,iCount,NTotalElements,ErrorCode,iLoc
    LOGICAL                :: lZonePreviouslyAdded,lFacePreviouslyAdded
    TYPE(ZoneType)         :: ZoneData,VertZoneData
    TYPE(AdjacentZoneType) :: AdjZoneData
    CLASS(*),POINTER       :: pCurrentZone,pCurrentAdjZone,pDummy
    INTEGER,ALLOCATABLE    :: OrderedAdjZoneList(:),ElemListAll(:),ElemListUnique(:)
    
    !Initialize
    NNodes = SystemData%NNodes
    IF (ZoneList%iZoneExtent .EQ. f_iZoneVertical) THEN
        NLayers = SystemData%NLayers
    ELSE
        NLayers = 1
    END IF
    
    !Allocate memory for the zone data
    ALLOCATE (ZoneData%LayerZoneElements(SystemData%NLayers)     , ZoneData%AccumulatedData(iNDataCols)     , &
              VertZoneData%LayerZoneElements(SystemData%NLayers) , VertZoneData%AccumulatedData(iNDataCols) )
    
    DO indxFace=1,SystemData%NFaces
        iElem1   = SystemData%iFaceElems(1,indxFace)
        iElem2   = SystemData%iFaceElems(2,indxFace)
        IF (iElem1 .GT. 0) THEN
            NVertex1 = SystemData%iElementNNodes(iElem1)
            Vertex1  = SystemData%iElementNodes(:,iElem1)
        END IF
        IF (iElem2 .GT. 0) THEN
            NVertex2 = SystemData%iElementNNodes(iElem2)
            Vertex2  = SystemData%iElementNodes(:,iElem2)
        END IF
          
        DO indxLayer=1,NLayers
        
            !Boundary face
            IF (SystemData%lBoundaryFace(indxFace)) THEN
                iElem1              = MAX(iElem1,iElem2)
                NVertex1            = SystemData%iElementNNodes(iElem1)
                Vertex1             = SystemData%iElementNodes(:,iElem1)
                ZoneData%ZoneNumber = ElemZones(iElem1,indxLayer)
                !Zonation in horizontal
                CALL ZoneList%AddNode(ZoneData,ZoneData%ZoneNumber,pCurrentZone,lZonePreviouslyAdded)
                !Zonation in vertical
                IF (ZoneList%iZoneExtent .EQ. f_iZoneVertical) CALL ProcessVerticalZones(ZoneList,ZoneData%ZoneNumber,indxLayer,iElem1,Vertex1(1:NVertex1))

            !Internal face 
            ELSE
                ZoneNum1 = ElemZones(iElem1,indxLayer)
                ZoneNum2 = ElemZones(iElem2,indxLayer)
                ! ... that is not a zone boundary
                IF (ZoneNum1 .EQ. ZoneNum2) THEN
                    ZoneData%ZoneNumber = ZoneNum1
                    CALL ZoneList%AddNode(ZoneData,ZoneData%ZoneNumber,pCurrentZone,lZonePreviouslyAdded)
                  
                ! ... that is a zone boundary
                ELSE
                    !Add first element as zone and second element as adjacent zone to it
                    ZoneData%ZoneNumber    = ZoneNum1
                    AdjZoneData%ZoneNumber = ZoneNum2
                    CALL ZoneList%AddNode(ZoneData,ZoneData%ZoneNumber,pCurrentZone,lZonePreviouslyAdded)
                    IF (lElemToElemFlow_Defined) THEN
                        SELECT TYPE (pCurrentZone)
                            TYPE IS (ZoneType)
                                IF (.NOT. ASSOCIATED(pCurrentZone%AdjacentZoneList)) ALLOCATE(pCurrentZone%AdjacentZoneList)
                                CALL pCurrentZone%AdjacentZoneList%AddNode(AdjZoneData,AdjZoneData%ZoneNumber,pCurrentAdjZone,lZonePreviouslyAdded)
                                !Add the face number to list of faces between the zone and adjacent zone
                                SELECT TYPE (pCurrentAdjZone)
                                    TYPE IS (AdjacentZoneType)
                                        IF (.NOT. ASSOCIATED(pCurrentAdjZone%iFaceNumberList)) ALLOCATE(pCurrentAdjZone%iFaceNumberList)
                                        CALL pCurrentAdjZone%iFaceNumberList%AddNode(indxFace,indxFace,pDummy,lFacePreviouslyAdded)
                                END SELECT
                        END SELECT
                    END IF
                    
                    !Add second element as zone and first element as adjacent zone to it
                    ZoneData%ZoneNumber    = ZoneNum2
                    AdjZoneData%ZoneNumber = ZoneNum1
                    CALL ZoneList%AddNode(ZoneData,ZoneData%ZoneNumber,pCurrentZone,lZonePreviouslyAdded)
                    IF (lElemToElemFlow_Defined) THEN
                        SELECT TYPE (pCurrentZone)
                            TYPE IS (ZoneType)
                                IF (.NOT. ASSOCIATED(pCurrentZone%AdjacentZoneList)) ALLOCATE(pCurrentZone%AdjacentZoneList)
                                CALL pCurrentZone%AdjacentZoneList%AddNode(AdjZoneData,AdjZoneData%ZoneNumber,pCurrentAdjZone,lZonePreviouslyAdded)
                                !Add the face number to list of faces between the zone and adjacent zone
                                SELECT TYPE (pCurrentAdjZone)
                                    TYPE IS (AdjacentZoneType)
                                        IF (.NOT. ASSOCIATED(pCurrentAdjZone%iFaceNumberList)) ALLOCATE(pCurrentAdjZone%iFaceNumberList)
                                        CALL pCurrentAdjZone%iFaceNumberList%AddNode(indxFace,indxFace,pDummy,lFacePreviouslyAdded)
                                END SELECT
                        END SELECT
                    END IF
                END IF
                
                !Zonation in vertical
                IF (ZoneList%iZoneExtent .EQ. f_iZoneVertical) THEN
                    CALL ProcessVerticalZones(ZoneList,ZoneNum1,indxLayer,iElem1,Vertex1(1:NVertex1))   
                    CALL ProcessVerticalZones(ZoneList,ZoneNum2,indxLayer,iElem2,Vertex2(1:NVertex2)) 
                END IF
            END IF
            
        END DO
    END DO
    
    !Get list of zones
    CALL ZoneList%GetOrderedKeyList(ZoneList%iOrderedZoneList)
    NZones = SIZE(ZoneList%iOrderedZoneList)
    
    !Store element numbers within each zone, calculate zone areas,assign zone names and find the number of adjacent zones to each zone
    !... when zonation is defined for horizontal
    IF (ZoneList%iZoneExtent .EQ. f_iZoneHorizontal) THEN
        DO indxZone=1,NZones
            iZone =  ZoneList%iOrderedZoneList(indxZone)
            iLoc  = LocateInList(iZone,iZoneListWithNames)
            NULLIFY (pCurrentZone)
            pCurrentZone => ZoneList%GetPointerToNode(iZone)
            SELECT TYPE (pCurrentZone)
                TYPE IS (ZoneType)
                    !Elements in the zone
                    iNZoneElems  = COUNT(ElemZones(:,1) .EQ. iZone)
                    DO indxLayer=1,SystemData%NLayers
                        ALLOCATE (pCurrentZone%LayerZoneElements(indxLayer)%Elements(iNZoneElems))
                    END DO
                    IF (iNZoneElems .EQ. 0) CYCLE
                    iCount = 1
                    DO indxElem=1,SIZE(ElemZones,DIM=1)
                       IF (ElemZones(indxElem,1) .EQ. iZone) THEN
                           pCurrentZone%LayerZoneElements%Elements(iCount) = indxElem
                           iCount = iCount + 1
                           IF (iCount .GT. iNZoneElems) EXIT
                        END IF
                    END DO
                    
                    !Number of adjacent zones and list of adjacent zone numbers
                    IF (ASSOCIATED(pCurrentZone%AdjacentZoneList)) THEN
                        CALL pCurrentZone%AdjacentZoneList%GetOrderedKeyList(OrderedAdjZoneList)
                        pCurrentZone%NAdjacentZones = SIZE(OrderedAdjZoneList)
                        ALLOCATE (pCurrentZone%AdjacentZoneNumbers(pCurrentZone%NAdjacentZones))
                        pCurrentZone%AdjacentZoneNumbers = OrderedAdjZoneList
                    END IF
                    
                    !Zone area
                    IF (iZone .NE. f_iUndefinedZone) &
                        pCurrentZone%Area = SUM(SystemData%rElementAreas(pCurrentZone%LayerZoneElements(1)%Elements))
                    
                    !Zone name
                    IF (iLoc .GT. 0)  &
                        pCurrentZone%cName = cZoneNames(iLoc)
                    
            END SELECT
        END DO
        
    !... when zonation is defined for vertical and horizontal
    ELSE
        DO indxZone=1,NZones
            iZone        =  ZoneList%iOrderedZoneList(indxZone)
            iLoc         =  LocateInList(iZone,iZoneListWithNames)
            pCurrentZone => ZoneList%GetPointerToNode(iZone)
            SELECT TYPE (pCurrentZone)
                TYPE IS (ZoneType)
                    !Elements in the zone
                    DO indxLayer=1,SystemData%NLayers
                        iNZoneElems  = COUNT(ElemZones(:,indxLayer) .EQ. iZone)
                        ALLOCATE (pCurrentZone%LayerZoneElements(indxLayer)%Elements(iNZoneElems))
                        IF (iNZoneElems .EQ. 0) CYCLE
                        iCount = 1
                        DO indxElem=1,SIZE(ElemZones,DIM=1)
                           IF (ElemZones(indxElem,indxLayer) .EQ. iZone) THEN
                               pCurrentZone%LayerZoneElements(indxLayer)%Elements(iCount) = indxElem
                               iCount = iCount + 1
                               IF (iCount .GT. iNZoneElems) EXIT
                            END IF
                        END DO
                    END DO
                    
                    !Number of adjacent zones and list of adjacent zone numbers
                    IF (ASSOCIATED(pCurrentZone%AdjacentZoneList)) THEN
                        CALL pCurrentZone%AdjacentZoneList%GetOrderedKeyList(OrderedAdjZoneList)
                        pCurrentZone%NAdjacentZones = SIZE(OrderedAdjZoneList)
                        ALLOCATE (pCurrentZone%AdjacentZoneNumbers(pCurrentZone%NAdjacentZones))
                        pCurrentZone%AdjacentZoneNumbers = OrderedAdjZoneList
                    END IF
                    
                    !Zone area (plan-view area for 3-D zones)
                    IF (iZone .NE. f_iUndefinedZone) THEN
                        NTotalElements = 0
                        DO indxLayer=1,SystemData%NLayers
                            NTotalElements = NTotalElements + SIZE(pCurrentZone%LayerZoneElements(indxLayer)%Elements)
                        END DO
                        DEALLOCATE (ElemListAll , STAT=ErrorCode)
                        ALLOCATE (ElemListAll(NTotalElements))
                        iCount = 0
                        DO indxLayer=1,SystemData%NLayers
                            ElemListAll(iCount+1:iCount+SIZE(pCurrentZone%LayerZoneElements(indxLayer)%Elements)) = pCurrentZone%LayerZoneElements(indxLayer)%Elements
                            iCount                                                                                = iCount + SIZE(pCurrentZone%LayerZoneElements(indxLayer)%Elements)
                        END DO
                        CALL GetUniqueArrayComponents(ElemListAll,ElemListUnique)
                        pCurrentZone%Area = SUM(SystemData%rElementAreas(ElemListUnique))
                    END IF
                    
                    !Zone name
                    IF (iLoc .GT. 0) &
                        pCurrentZone%cName = cZoneNames(iLoc)
                    
            END SELECT
        END DO
    END IF
        
    !Store element zones 
    ALLOCATE (ZoneList%ElemZones(SystemData%NElements,SystemData%NLayers))
    ZoneList%ElemZones = ElemZones
    
    !Clear memory
    DEALLOCATE (ElemListAll , ElemListUnique , OrderedAdjZoneList , STAT=ErrorCode)

    
  CONTAINS
    

    ! ##########################################
    ! --- ADD ZONES IN VERTICAL
    ! ##########################################
    SUBROUTINE ProcessVerticalZones(ZoneList,ZoneNumUp,iLayerUp,iElemUp,Vertex)
      TYPE(ZoneListType) :: ZoneList
      INTEGER,INTENT(IN) :: ZoneNumUp,iLayerUp,iElemUp,Vertex(:)
      
      !Local variables
      INTEGER                :: iElemDown,ZoneNumDown,iLayerDown,indxNode,iNode
      LOGICAL                :: lZonePreviouslyAdded
      TYPE(AdjacentZoneType) :: VertAdjZoneData
      CLASS(*),POINTER       :: pThisZone,pThisAdjZone
      
      
      !Return if zonation is not defined in vertical, if it is the last layer, or all the nodes of the element are inactive nodes
      IF (ZoneList%iZoneExtent .EQ. f_iZoneHorizontal) RETURN
      IF (iLayerUp .EQ. NLayers) RETURN
      IF (.NOT. ANY(SystemData%lActiveNode(Vertex,iLayerUp))) RETURN
      
      
      !Zone number of element below the element being analyzed (regardless of active or inactive layer)
      ZoneNumDown = ElemZones(iElemUp,iLayerUp+1)
      IF (ZoneNumUp .NE. ZoneNumDown) THEN
          VertZoneData%ZoneNumber    = ZoneNumUp
          VertAdjZoneData%ZoneNumber = ZoneNumDown
          CALL ZoneList%AddNode(VertZoneData,VertZoneData%ZoneNumber,pThisZone,lZonePreviouslyAdded)
          SELECT TYPE (pThisZone)
              TYPE IS (ZoneType)
                  IF (.NOT. ASSOCIATED(pThisZone%AdjacentZoneList)) ALLOCATE(pThisZone%AdjacentZoneList)
                  CALL pThisZone%AdjacentZoneList%AddNode(VertAdjZoneData,VertAdjZoneData%ZoneNumber,pThisAdjZone,lZonePreviouslyAdded)
          END SELECT

          VertZoneData%ZoneNumber    = ZoneNumDown
          VertAdjZoneData%ZoneNumber = ZoneNumUp
          CALL ZoneList%AddNode(VertZoneData,VertZoneData%ZoneNumber,pThisZone,lZonePreviouslyAdded)
          SELECT TYPE (pThisZone)
              TYPE IS (ZoneType)
                  IF (.NOT. ASSOCIATED(pThisZone%AdjacentZoneList)) ALLOCATE(pThisZone%AdjacentZoneList)
                  CALL pThisZone%AdjacentZoneList%AddNode(VertAdjZoneData,VertAdjZoneData%ZoneNumber,pThisAdjZone,lZonePreviouslyAdded)
          END SELECT
      END IF
      
      
      !Zone number below element based on if nodes are active or not
      DO indxNode=1,SIZE(Vertex)
          iNode = Vertex(indxNode)
          
          !Skip if node is inactive
          IF (.NOT. SystemData%lActiveNode(iNode,iLayerUp)) CYCLE
          
          !Find the active layer below and the zone number
          iLayerDown = SystemData%iActiveLayerBelow(iNode,iLayerUp)
          IF (iLayerDown .LT. 1) CYCLE
          ZoneNumDown = ElemZones(iElemUp,iLayerDown)
      
          !If the zones are different, add
          IF (ZoneNumUp .NE. ZoneNumDown) THEN
              VertZoneData%ZoneNumber    = ZoneNumUp
              VertAdjZoneData%ZoneNumber = ZoneNumDown
              CALL ZoneList%AddNode(VertZoneData,VertZoneData%ZoneNumber,pThisZone,lZonePreviouslyAdded)
              SELECT TYPE (pThisZone)
                  TYPE IS (ZoneType)
                  IF (.NOT. ASSOCIATED(pThisZone%AdjacentZoneList)) ALLOCATE(pThisZone%AdjacentZoneList)
                      CALL pThisZone%AdjacentZoneList%AddNode(VertAdjZoneData,VertAdjZoneData%ZoneNumber,pThisAdjZone,lZonePreviouslyAdded)
              END SELECT
              
              VertZoneData%ZoneNumber    = ZoneNumDown
              VertAdjZoneData%ZoneNumber = ZoneNumUp
              CALL ZoneList%AddNode(VertZoneData,VertZoneData%ZoneNumber,pThisZone,lZonePreviouslyAdded)
              SELECT TYPE (pThisZone)
                  TYPE IS (ZoneType)
                  IF (.NOT. ASSOCIATED(pThisZone%AdjacentZoneList)) ALLOCATE(pThisZone%AdjacentZoneList)
                      CALL pThisZone%AdjacentZoneList%AddNode(VertAdjZoneData,VertAdjZoneData%ZoneNumber,pThisAdjZone,lZonePreviouslyAdded)
              END SELECT
          END IF
      END DO
    
    END SUBROUTINE ProcessVerticalZones
    
  END SUBROUTINE ZoneList_New
  
  
  ! -------------------------------------------------------------
  ! --- GENERATE ZONE LIST WITH DATA READ FROM TEXT FILE
  ! -------------------------------------------------------------
  SUBROUTINE GenerateZoneList_DataFromASCIIFile(ZoneList,iNDataCols,lFaceFlows_Defined,SystemData,cZoneDefFileName,iStat)
    CLASS(ZoneListType)             :: ZoneList
    INTEGER,INTENT(IN)              :: iNDataCols
    LOGICAL,INTENT(IN)              :: lFaceFlows_Defined
    TYPE(SystemDataType),INTENT(IN) :: SystemData
    CHARACTER(LEN=*),INTENT(IN)     :: cZoneDefFileName
    INTEGER,INTENT(OUT)             :: iStat
  
    !Local variables
    CHARACTER(LEN=ModNameLen+34),PARAMETER :: ThisProcedure = ModName // 'GenerateZoneList_DataFromASCIIFile'
    INTEGER                                :: ErrorCode,indx,nZonesWithNames,indx1,iZone,iZone1,iLoc
    INTEGER,ALLOCATABLE                    :: ElemZones(:,:),iZonesWithNames(:)
    CHARACTER(LEN=300),ALLOCATABLE         :: cArray(:)
    CHARACTER(LEN=50),ALLOCATABLE          :: cZoneNames(:)
    TYPE(GenericFileType)                  :: ZoneDefFile
    
    !Initialize
    iStat = 0
    
    !Open zone definition file
    CALL ZoneDefFile%New(FileName=cZoneDefFileName,InputFile=.TRUE.,Descriptor='Zone definition file',FileType='TXT',iStat=iStat)
    IF (iStat .EQ. -1) THEN
        CALL ZoneDefFile%Kill()
        RETURN
    END IF
    
    !Zone definition (in horizontal or vertical)
    CALL ZoneDefFile%ReadData(ZoneList%iZoneExtent,iStat)  ;  IF (iStat .EQ. -1) GOTO 100
    IF (ZoneList%iZoneExtent .NE. f_iZoneHorizontal  .AND.  ZoneList%iZoneExtent .NE. f_iZoneVertical) THEN
        CALL SetLastMessage('Value entered for ZExtent variable is not recognized in file '//TRIM(cZoneDefFileName)//'!',f_iFatal,ThisProcedure)
        iStat = -1
        GOTO 100
    END IF
    
    !Zone names
    CALL ZoneDefFile%ReadData(cArray,iStat)  ;  IF (iStat .EQ. -1) GOTO 100
    nZonesWithNames = SIZE(cArray)
    CALL CleanSpecialCharacters(cArray)
    ALLOCATE (iZonesWithNames(nZonesWithNames) , cZoneNames(nZonesWithNames))
    DO indx=1,nZonesWithNames
        cArray(indx) = ADJUSTL(StripTextUntilCharacter(cArray(indx),'/'))
        READ (cArray(indx),*) iZonesWithNames(indx)
        iLoc = FirstLocation(' ',cArray(indx))  
        cZoneNames(indx) = ADJUSTL(cArray(indx)(iLoc:300))
    END DO
    
    !Make sure same zone is not given different names
    DO indx=1,nZonesWithNames
        iZone = iZonesWithNames(indx)
        DO indx1=indx+1,nZonesWithNames
            iZone1 = iZonesWithNames(indx1)
            IF (iZone1 .EQ. iZone) THEN
                CALL SetLastMessage('Zone number '//TRIM(IntToText(iZone1))//' is given two seperate names!',f_iFatal,ThisProcedure)
                iStat = -1
                GOTO 100
            END IF
        END DO
    END DO
    
    !Order zones with names
    CALL ShellSort(iZonesWithNames,cZoneNames)
    
    !Allocate ElemZones array
    DEALLOCATE (ElemZones , STAT=ErrorCode)
    ALLOCATE (ElemZones(SystemData%NElements,SystemData%NLayers))
    
    !Read element zonal data
    CALL ReadElemZoneInformation(ZoneDefFile,SystemData%NElements,SystemData%NLayers,ZoneList%iZoneExtent,SystemData%iElementIDs,ElemZones,iStat)
    IF (iStat .EQ. -1) GOTO 100
    
    !Compile zone list in a binary tree
    CALL ZoneList_New(ZoneList,iNDataCols,lFaceFlows_Defined,ElemZones,iZonesWithNames,cZoneNames,SystemData)
    
    !Close zone definition file
100 CALL ZoneDefFile%Kill()
    
  END SUBROUTINE GenerateZoneList_DataFromASCIIFile
  
  
  ! -------------------------------------------------------------
  ! --- GENERATE ZONE LIST WITH ELEMENTS, LAYERS AND ZONES DATA PROVIDED
  ! -------------------------------------------------------------
  SUBROUTINE GenerateZoneList(ZoneList,iNDataCols,lFaceFlows_Defined,SystemData,iZExtent,iElems,iLayers,iZones,iZonesWithNames,cZoneNames,iStat)
    CLASS(ZoneListType)             :: ZoneList
    INTEGER,INTENT(IN)              :: iNDataCols,iZExtent,iElems(:),iLayers(:),iZones(:),iZonesWithNames(:)
    LOGICAL,INTENT(IN)              :: lFaceFlows_Defined
    TYPE(SystemDataType),INTENT(IN) :: SystemData
    CHARACTER(LEN=*),INTENT(IN)     :: cZoneNames(:)
    INTEGER,INTENT(OUT)             :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+16),PARAMETER :: ThisProcedure = ModName // 'GenerateZoneList'
    INTEGER                                :: indxElem,indxLayer,iZonesWithNames_Local(SIZE(iZonesWithNames)),nZonesWithNames,  &
                                              indx,indx1,iZone,iZone1,iElemIndices(SIZE(iElems))
    INTEGER,ALLOCATABLE                    :: iElemZones(:,:)
    CHARACTER(LEN=50)                      :: cZoneNames_Local(SIZE(iZonesWithNames))
    
    !Initialize
    iStat                 = 0
    nZonesWithNames       = SIZE(iZonesWithNames)
    iZonesWithNames_Local = iZonesWithNames
    cZoneNames_Local      = cZoneNames
    ZoneList%iZoneExtent  = iZExtent
    
    !Make sure one zone is not given multiple names
    DO indx=1,nZonesWithNames
        iZone = iZonesWithNames_Local(indx)
        DO indx1=indx+1,nZonesWithNames
            iZone1 = iZonesWithNames_Local(indx1)
            IF (iZone1 .EQ. iZone) THEN
                CALL SetLastMessage('Zone number '//TRIM(IntToText(iZone1))//' is given two seperate names!',f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
        END DO
    END DO
    
    !Order zones with names
    CALL ShellSort(iZonesWithNames_Local,cZoneNames_Local)
    
    !Convert element IDs to element indices
    CALL ConvertID_To_Index(iElems,SystemData%iElementIDs,iElemIndices)
    IF (ANY(iElemIndices.EQ.0)) THEN
        CALL SetLastMessage('One or more element numbers defined for zonation are not in the model!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
        
    !Compile iElemZones array
    ALLOCATE (iElemZones(SystemData%NElements,SystemData%NLayers))
    iElemZones = f_iUndefinedZone
    IF (iZExtent .EQ. f_iZoneHorizontal) THEN
        DO indxLayer=1,SystemData%NLayers
            iElemZones(iElemIndices,indxLayer) = iZones
        END DO
    ELSE
        DO indxElem=1,SIZE(iElems)
            !Make sure specified layer for zonation is not greater than the available layers
            IF (iLayers(indxElem) .GT. SystemData%NLayers) THEN
                CALL SetLastMessage('Zonation layer at element '//TRIM(IntToText(iElems(indxElem)))//' is larger than the simulated number of layers!',f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
            !Assign zone
            iElemZones(iElemIndices(indxElem),iLayers(indxElem)) = iZones(indxElem)
        END DO
    END IF
    
    !Compile zone list in a binary tree
    CALL ZoneList_New(ZoneList,iNDataCols,lFaceFlows_Defined,iElemZones,iZonesWithNames_Local,cZoneNames_Local,SystemData)
    
    !Store iZExtent data
    ZoneList%iZoneExtent = iZExtent
    
  END SUBROUTINE GenerateZoneList
  
  
  
  
! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** DESTRUCTORS
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- KILL ZONE LIST
  ! -------------------------------------------------------------
  SUBROUTINE ZoneList_Kill(This)
    CLASS(ZoneListType),TARGET :: This
    
    !Local variables
    INTEGER            :: ErrorCode,iZone,indx
    CLASS(*),POINTER   :: pZone
    TYPE(ZoneListType) :: Dummy
    
    !Traverse through the zone list and clear memory associated with each zone data
    DO indx=1,SIZE(This%iOrderedZoneList)
        iZone =  This%iOrderedZoneList(indx)
        pZone => This%GetPointerToNode(iZone)
        SELECT TYPE (pZone)
            TYPE IS (ZoneType)
                CALL pZone%Kill()
        END SELECT
    END DO
    
    !Clear the nodes of the binary tree
    CALL This%BinaryTreeType%Kill()
       
    !Deallocate arrays
    DEALLOCATE (This%ElemZones ,This%iOrderedZoneList , STAT=ErrorCode)
    
    !Restore to defaults
    SELECT TYPE (This)
        TYPE IS (ZoneListType)
            This = Dummy
    END SELECT
    
  END SUBROUTINE ZoneList_Kill
  
  
  ! -------------------------------------------------------------
  ! --- KILL A ZONE DATA
  ! -------------------------------------------------------------
  SUBROUTINE Zone_Kill(Zone)
    CLASS(ZoneType) :: Zone
    
    !Local variables
    INTEGER          :: indxLayer,ErrorCode,indxAdjZone,iAdjZone
    TYPE(ZoneType)   :: Dummy
    CLASS(*),POINTER :: pAdjZone
    
    !Clear memory from adjacent zone list
    DO indxAdjZone=1,Zone%NAdjacentZones
        iAdjZone =  Zone%AdjacentZoneNumbers(indxAdjZone)
        pAdjZone => Zone%AdjacentZoneList%GetPointerToNode(iAdjZone)
        SELECT TYPE (pAdjZone)
            TYPE IS (AdjacentZoneType)
                CALL pAdjZone%Kill()
        END SELECT
    END DO
    CALL Zone%AdjacentZoneList%Kill()
    DEALLOCATE (Zone%AdjacentZoneList , STAT=ErrorCode)
    
    !Deallocate arrays
    IF (ALLOCATED(Zone%LayerZoneElements)) THEN
        DO indxLayer=1,SIZE(Zone%LayerZoneElements)
            DEALLOCATE (Zone%LayerZoneElements(indxLayer)%Elements , STAT=ErrorCode)
        END DO
    END IF
    DEALLOCATE (Zone%LayerZoneElements   , &
                Zone%AdjacentZoneNumbers , &
                Zone%AccumulatedData     , &
                STAT=ErrorCode           )
    
    !Retrieve to default values
    SELECT TYPE (Zone)
        TYPE IS (ZoneType)
            Zone = Dummy
    END SELECT
    
  END SUBROUTINE Zone_Kill
  
  
  ! -------------------------------------------------------------
  ! --- KILL AN  ADJACENT ZONE DATA
  ! -------------------------------------------------------------
  SUBROUTINE AdjacentZone_Kill(AdjZone)
    CLASS(AdjacentZoneType) :: AdjZone
    
    !Local variables
    INTEGER                :: ErrorCode
    TYPE(AdjacentZoneType) :: Dummy
    
    !Clear memory from face number list
    CALL AdjZone%iFaceNumberList%Kill()
    DEALLOCATE (AdjZone%iFAceNumberList , STAT=ErrorCode)
    
    !Revert to defaults
    AdjZone%ZoneNumber = Dummy%ZoneNumber
    AdjZone%FLOW_IN    = Dummy%FLOW_IN
    AdjZone%FLOW_OUT   = Dummy%FLOW_OUT
    
  END SUBROUTINE AdjacentZone_Kill
    
  
  
  
! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** GETTERS
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- GET ZONE NAMES
  ! -------------------------------------------------------------
  SUBROUTINE GetNames(ZoneList,cNames)
    CLASS(ZoneListType),INTENT(IN) :: ZoneList
    CHARACTER(LEN=*),ALLOCATABLE   :: cNames(:)
    
    !Local variables
    INTEGER          :: indxZone,ErrorCode,NZones,iZone
    CLASS(*),POINTER :: pZone
    
    !Number of zones
    NZones = SIZE(ZoneList%iOrderedZoneList)
    
    !Allocate NAmes array
    DEALLOCATE (cNames , STAT=ErrorCode)
    ALLOCATE (cNames(NZones))
    cNames = ''
    
    !Get names
    DO indxZone=1,SIZE(ZoneList%iOrderedZoneList)
        iZone =  ZoneList%iOrderedZoneList(indxZone)
        pZone => ZoneList%GetPointerToNode(iZone)
        SELECT TYPE (pZone)
            TYPE IS (ZoneType)
                cNames(indxZone) = pZone%cName
        END SELECT
    END DO
    
  END SUBROUTINE GetNames
  
  
  ! -------------------------------------------------------------
  ! --- GET NUMBER OF ZONES
  ! -------------------------------------------------------------
  FUNCTION GetNZones(ZoneList) RESULT(NZones)
    CLASS(ZoneListType),INTENT(IN) :: ZoneList
    INTEGER                        :: NZones
    
    NZones = ZoneList%GetNNodes()
    
  END FUNCTION GetNZones
  

  ! -------------------------------------------------------------
  ! --- PACK ALL ZONE DATA INCLUDING COMPUTED MASS BALANCE ERROR AND STORAGE INTO A SINGLE ARRAY 
  ! -------------------------------------------------------------
  SUBROUTINE GetPackedZoneData(Zone,iNDataColumns,lStorage_Defined,lComputeError,iErrorInCols,iErrorOutCols,rPackedFlows)
    CLASS(ZoneType),INTENT(IN) :: Zone
    INTEGER,INTENT(IN)         :: iNDataColumns,iErrorInCols(:),iErrorOutCols(:)
    LOGICAL,INTENT(IN)         :: lStorage_Defined,lComputeError
    REAL(8),INTENT(OUT)        :: rPackedFlows(:)
    
    !Local variables
    INTEGER          :: indxAdjZone,iAdjZone,iCount
    REAL(8)          :: Error
    CLASS(*),POINTER :: pAdjZone
    
    !Accumulated flows for the zone except flow exchnage between zones, error and storage
    rPackedFlows(1:iNDataColumns) = Zone%AccumulatedData
    
    !Initialize mass balance error
    IF (lComputeError) Error = SUM(Zone%AccumulatedData(iErrorInCols)) - SUM(Zone%AccumulatedData(iErrorOutCols)) 
    
    !Inflows and outflows from adjacent zones
    iCount = iNDataColumns + 1
    DO indxAdjZone=1,Zone%NAdjacentZones
        iAdjZone =  Zone%AdjacentZoneNumbers(indxAdjZone)
        pAdjZone => Zone%AdjacentZoneList%GetPointerToNode(iAdjZone)
        SELECT TYPE (pAdjZone)
            TYPE IS (AdjacentZoneType)
                rPackedFlows(iCount)   = pAdjZone%FLOW_IN
                rPackedFlows(iCount+1) = pAdjZone%FLOW_OUT
                IF (lComputeError) Error = Error + pAdjZone%FLOW_IN - pAdjZone%FLOW_OUT
        END SELECT
        iCount = iCount + 2    
    END DO
    
    !Mass balance error
    IF (lComputeError) rPackedFlows(iCount) = Error
    
    !Storage if defined
    IF (lStorage_Defined) rPackedFlows(iCount+1) = Zone%Storage
    
  END SUBROUTINE GetPackedZoneData
  
  
  ! -------------------------------------------------------------
  ! --- GET THE MAXIMUM NUMBER OF ADJACENT ZONES
  ! -------------------------------------------------------------
  FUNCTION GetNMaxAdjacentZones(ZoneList) RESULT(NMax)
    CLASS(ZoneListType),INTENT(IN) :: ZoneList
    INTEGER                        :: NMax
    
    !Local variables
    INTEGER          :: indx,iZone
    CLASS(*),POINTER :: pZone
    
    !Initialize
    NMax = 0
    
    DO indx=1,SIZE(ZoneList%iOrderedZoneList)
        iZone = ZoneList%iOrderedZoneList(indx)
        IF (iZone .EQ. f_iUndefinedZone) CYCLE
        pZone => ZoneList%GetPointerToNode(iZone)
        SELECT TYPE (pZone)
            TYPE IS (ZoneType)
                NMax = MAX(NMax , pZone%NAdjacentZones)
        END SELECT
    END DO
    
  END FUNCTION GetNMaxAdjacentZones


  ! -------------------------------------------------------------
  ! --- GET THE NUMBER OF ADJACENT ZONES FOR A ZONE
  ! -------------------------------------------------------------
  FUNCTION GetNAdjacentZones(ZoneList,iZone) RESULT(iNAdjZones)
    CLASS(ZoneListType),INTENT(IN) :: ZoneList
    INTEGER,INTENT(IN)             :: iZone
    INTEGER                        :: iNAdjZones
    
    !Local variables
    CLASS(*),POINTER :: pZone
    
    !Get a pointer to the zone and find the number of adjacanet zones
    pZone => ZoneList%GetPointerToNode(iZone)
    SELECT TYPE (pZone)
        TYPE IS (ZoneType)
            iNAdjZones = pZone%NAdjacentZones
    END SELECT
        
  END FUNCTION GetNAdjacentZones
  
  
    
  
! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** MISC. METHODS
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- ZERO OUT FLOWS FOR A ZONE AND ITS ADJACENT ZONES
  ! -------------------------------------------------------------
  SUBROUTINE ZeroOutFlows(Zone)
    CLASS(ZoneType) :: Zone
    
    !Local variables
    INTEGER          :: indxAdjZone,iAdjZone
    CLASS(*),POINTER :: pAdjZone
    
    Zone%AccumulatedData = 0.0
    Zone%Storage         = 0.0
    
    DO indxAdjZone=1,Zone%NAdjacentZones
        iAdjZone =  Zone%AdjacentZoneNumbers(indxAdjZone)
        pAdjZone => Zone%AdjacentZoneList%GetPointerToNode(iAdjZone)
        SELECT TYPE (pAdjZone)
            TYPE IS (AdjacentZoneType)
                pAdjZone%FLOW_IN  = 0.0
                pAdjZone%FLOW_OUT = 0.0
        END SELECT
    END DO
    
  END SUBROUTINE ZeroOutFlows
  
  
  ! -------------------------------------------------------------
  ! --- ADD EXCHANGE FLOW BETWEEN A ZONE AND AN ADJACENT ZONE
  ! -------------------------------------------------------------
  SUBROUTINE AddAdjacentZoneFlow(Zone,iAdjZone,rFlow)  
    CLASS(ZoneType)    :: Zone
    INTEGER,INTENT(IN) :: iAdjZone
    REAL(8),INTENT(IN) :: rFlow
    
    !Local variables
    CLASS(*),POINTER :: pAdjZone
    
    pAdjZone => Zone%AdjacentZoneList%GetPointerToNode(iAdjZone)
    SELECT TYPE(pAdjZone)
        TYPE IS (AdjacentZoneType)
            IF (rFlow .GT. 0) THEN
                pAdjZone%FLOW_IN  = pAdjZone%FLOW_IN + rFlow
            ELSE
                pAdjZone%FLOW_OUT = pAdjZone%FLOW_OUT - rFlow
            END IF                    
    END SELECT
    
  END SUBROUTINE AddAdjacentZoneFlow
  
  
  ! -------------------------------------------------------------
  ! --- READ IN THE ZONE NUMBERING FROM MAIN CONTROL FILE
  ! -------------------------------------------------------------
  SUBROUTINE ReadElemZoneInformation(InputFile,NElements,NLayers,ZExtent,ElementIDs,ElemZones,iStat)
    TYPE(GenericFileType) :: InputFile
    INTEGER,INTENT(IN)    :: NElements,NLayers,ZExtent,ElementIDs(NElements)
    INTEGER,INTENT(OUT)   :: ElemZones(NElements,NLayers),iStat

    !Local variables
    CHARACTER(LEN=ModNameLen+23)   :: ThisProcedure = ModName // 'ReadElemZoneInformation'
    INTEGER                        :: ElementID,LayerNo,indx,ZoneNo,ElementIndex
    CHARACTER(LEN=300),ALLOCATABLE :: DummyCharArray(:)
    CHARACTER(:),ALLOCATABLE       :: cFileName

    !Initialize
    iStat     = 0
    ElemZones = f_iUndefinedZone      !Initially zone numbers have not been assigned

    !Read element zonation information
    CALL InputFile%ReadData(DummyCharArray,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL CleanSpecialCharacters(DummyCharArray)

    !If none of the elements are assigned zone numbers, stop
    IF (SIZE(DummyCharArray) .EQ. 0) THEN
        CALL InpuTFile%GetName(cFileName)
        CALL SetLastMessage('Element zone information is not found in file '//TRIM(CFileName)//'!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    !Assign zone numbers to elements
    DO indx=1,SIZE(DummyCharArray)
        !If DummyChar is empty, exit
        IF (DummyCharArray(indx) .EQ. '') EXIT
        
        !Zone numbering applies to all layers
        IF (ZExtent .EQ. f_iZoneHorizontal) THEN
            READ (DummyCharArray(indx),*) ElementID,ZoneNo
          
        !Zone numbering is different for each layer
        ELSE
            READ (DummyCharArray(indx),*) ElementID,LayerNo,ZoneNo
            IF (LayerNo .GT. NLayers) THEN
                MessageArray(1) = 'Zone layer number at element '//TRIM(IntToText(ElementID))//' is specified as '//TRIM(IntToText(LayerNo))
                MessageArray(2) = 'when the number of layers simulated is '//TRIM(IntToText(NLayers))//'!' 
                CALL SetLastMessage(MessageArray(1:2),f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
        END IF
        
        !Convert element ID to element index
        CALL ConvertID_To_Index(ElementID,ElementIDs,ElementIndex)
        
        !Make sure element id is in the model
        IF (ElementIndex .EQ. 0) THEN
            CALL SetLastMessage('Element number '//TRIM(IntToText(ElementID))//' is not in the model!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        
        !Make sure that zone number is not specified as -999
        IF (ZoneNo .EQ. -999) THEN
            CALL SetLastMessage('-999 cannot be used as a zone number!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        
        !Assign zone number to the elements of all layers when zone numbering applies to all layers
        IF (ZExtent .EQ. f_iZoneHorizontal) THEN
            !Check if zone number for elements have already been specified
            IF (ANY(ElemZones(ElementIndex,:) .NE. f_iUndefinedZone)) THEN
                CALL SetLastMessage('Multiple zone numbers are specified for element '//TRIM(IntToText(ElementID))//'!',f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
            ElemZones(ElementIndex,:) = ZoneNo
          
        !Assign zone number to the individual element of the given layer
        ELSE
            !Check if zone number for elements have already been specified
            IF (ElemZones(ElementIndex,LayerNo) .NE. f_iUndefinedZone) THEN
                CALL SetLastMessage('Multiple zone numbers are specified for element '//TRIM(IntToText(ElementID))//' in layer '//TRIM(IntToText(LayerNo))//'!',f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
            ElemZones(ElementIndex,LayerNo) = ZoneNo
        END IF
    END DO

  END SUBROUTINE ReadElemZoneInformation

  
END MODULE