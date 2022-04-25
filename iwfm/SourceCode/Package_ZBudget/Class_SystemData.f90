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
MODULE Class_SystemData
  USE MessageLogger      , ONLY: SetLastMessage   , &
                                 f_iFatal
  USE IOInterface        , ONLY: GenericFileType  , &
                                 f_iGroup
  USE ZBudget_Parameters , ONLY: f_cAttributesDir
  USE ZBudget_Util       , ONLY: IsZBudgetFile
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
  PUBLIC :: SystemDataType    
  
  
  ! -------------------------------------------------------------
  ! --- PHYSICAL SYSTEM DATA TYPE 
  ! -------------------------------------------------------------
  TYPE SystemDataType
      INTEGER             :: NNodes                   = 0
      INTEGER             :: NElements                = 0
      INTEGER             :: NLayers                  = 0    !Number of layers considred for the hydrologic system (not necessarily equal to the aquifer layers)
      INTEGER             :: NFaces                   = 0    !Number of element faces
      REAL(8),ALLOCATABLE :: rNodeAreas(:)                   !Nodal area at each (node)
      INTEGER,ALLOCATABLE :: iElementIDs(:)                  !Element ID numbers
      REAL(8),ALLOCATABLE :: rElementAreas(:)                !Element area at each (element)
      INTEGER,ALLOCATABLE :: iElementNNodes(:)               !Number of nodes for each (element)
      INTEGER,ALLOCATABLE :: iElementNodes(:,:)              !Nodes for each element in counter-clockwise direction for (4,element) combination
      REAL(8),ALLOCATABLE :: rElementNodeAreas(:,:)          !Area associated with the vertex of each element given for (4,element) combination
      REAL(8),ALLOCATABLE :: rElementNodeAreaFractions(:,:)  !Ratio of area associated with the vertex of an element to the element area given for (4,element) combination
      INTEGER,ALLOCATABLE :: iFaceElems(:,:)                 !Element numbers on both sides of the face for (2,face) combination
      LOGICAL,ALLOCATABLE :: lBoundaryFace(:)                !Flag to check if a face is boundary face for each (face)
      LOGICAL,ALLOCATABLE :: lActiveNode(:,:)                !Flag that shows if a node is active or not for (node,layer) combination
      INTEGER,ALLOCATABLE :: iActiveLayerBelow(:,:)          !Active layer below each node for each (node,layer) combination; computed from lActiveNode
  CONTAINS
      PROCEDURE,PASS :: ReadFromFile
      PROCEDURE,PASS :: Kill
      PROCEDURE,PASS :: WriteToFile
  END TYPE SystemDataType

 
  ! -------------------------------------------------------------
  ! --- MISC. ENTITIES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                   :: ModNameLen = 18
  CHARACTER(LEN=ModNameLen),PARAMETER :: ModName    = 'Class_SystemData::'
  
  
CONTAINS
    

    
    
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
  ! --- READ SYSTEM DATA FROM HDF FILE
  ! -------------------------------------------------------------
  SUBROUTINE ReadFromFile(SystemData,HDFFile,iStat)
    CLASS(SystemDataType),INTENT(OUT) :: SystemData
    TYPE(GenericFileType)             :: HDFFile
    INTEGER,INTENT(OUT)               :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+12),PARAMETER :: ThisProcedure = ModName // 'ReadFromFile'
    INTEGER                                :: indxLayer,indxNode,indxLayerBelow,indx
    CHARACTER(:),ALLOCATABLE               :: cFileName
    
    !Check that this is indeed Z-Budget data file by checking if an object that Budget file doesn't have exist
    IF (.NOT. IsZBudgetFile(HDFFile)) THEN
        CALL HDFFile%GetName(cFileName)
        CALL SetLastMessage('File '//TRIM(cFileName)//' is not a Z-Budget file type!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    CALL HDFFile%ReadData(f_cAttributesDir,'SystemData%NNodes',ScalarAttrData=SystemData%NNodes,iStat=iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL HDFFile%ReadData(f_cAttributesDir,'SystemData%NElements',ScalarAttrData=SystemData%NElements,iStat=iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL HDFFile%ReadData(f_cAttributesDir,'SystemData%NLayers',ScalarAttrData=SystemData%NLayers,iStat=iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL HDFFile%ReadData(f_cAttributesDir,'SystemData%NFaces',ScalarAttrData=SystemData%NFaces,iStat=iStat)  ;  IF (iStat .EQ. -1) RETURN
    ALLOCATE (SystemData%rNodeAreas(SystemData%NNodes)                           , &
              SystemData%iElementIDs(SystemData%NElements)                       , &
              SystemData%rElementAreas(SystemData%NElements)                     , &
              SystemData%iElementNNodes(SystemData%NElements)                    , &
              SystemData%iElementNodes(4,SystemData%NElements)                   , &
              SystemData%iFaceElems(2,SystemData%NFaces)                         , &
              SystemData%lBoundaryFace(SystemData%NFaces)                        , &
              SystemData%lActiveNode(SystemData%NNodes,SystemData%NLayers), &
              SystemData%rElementNodeAreas(4,SystemData%NElements)               , &
              SystemData%rElementNodeAreaFractions(4,SystemData%NElements)       )
    CALL HDFFile%ReadData(f_cAttributesDir//'/SystemData%NodeAreas',SystemData%rNodeAreas,iStat=iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL HDFFile%ReadData(f_cAttributesDir//'/SystemData%ElementAreas',SystemData%rElementAreas,iStat=iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL HDFFile%ReadData(f_cAttributesDir//'/SystemData%ElementNNodes',SystemData%iElementNNodes,iStat=iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL HDFFile%ReadData(f_cAttributesDir//'/SystemData%ElementNodes',SystemData%iElementNodes,iStat=iStat)  ;  IF (iStat .EQ. -1) RETURN
    IF (SystemData%NFaces .GT. 0) THEN
        CALL HDFFile%ReadData(f_cAttributesDir//'/SystemData%FaceElements',SystemData%iFaceElems,iStat=iStat)  ;  IF (iStat .EQ. -1) RETURN
        CALL HDFFile%ReadData(f_cAttributesDir//'/SystemData%BoundaryFace',SystemData%lBoundaryFace,iStat=iStat)  ;  IF (iStat .EQ. -1) RETURN
    END IF
    CALL HDFFile%ReadData(f_cAttributesDir//'/SystemData%ActiveNode',SystemData%lActiveNode,iStat=iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL HDFFile%ReadData(f_cAttributesDir//'/SystemData%ElementNodeAreas',SystemData%rElementNodeAreas,iStat=iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL HDFFile%ReadData(f_cAttributesDir//'/SystemData%ElementNodeAreaFractions',SystemData%rElementNodeAreaFractions,iStat=iStat)  ;  IF (iStat .EQ. -1) RETURN
    
    !Backward compatibility: Check if SystemData%ElementIDs object exists. Read if it does; otherwise create the data in memory as being equal to the element index
    IF (HDFFile%DoesHDFObjectExist(f_cAttributesDir//'/SystemData%ElementIDs')) THEN
        CALL HDFFile%ReadData(f_cAttributesDir//'/SystemData%ElementIDs',SystemData%iElementIDs,iStat=iStat)  ;  IF (iStat .EQ. -1) RETURN
    ELSE
        SystemData%iElementIDs = [(indx,indx=1,SystemData%NElements)]
    END IF
    
    !Compile the array that holds active layer below each node
    ALLOCATE (SystemData%iActiveLayerBelow(SystemData%NNodes,SystemData%NLayers-1))
    SystemData%iActiveLayerBelow = 0
    DO indxLayer=1,SystemData%NLayers-1
        DO indxNode=1,SystemData%NNodes
            DO indxLayerBelow=indxLayer+1,SystemData%NLayers
                IF (SystemData%lActiveNode(indxNode,indxLayerBelow)) THEN
                    SystemData%iActiveLayerBelow(indxNode,indxLayer) = indxLayerBelow
                    EXIT
                END IF
            END DO
        END DO
    END DO

  END SUBROUTINE ReadFromFile
  
  
  ! -------------------------------------------------------------
  ! --- WRITE SYSTEM DATA TO HDF FILE
  ! -------------------------------------------------------------
  SUBROUTINE WriteToFile(SystemData,HDFFile)
    CLASS(SystemDataType),INTENT(IN) :: SystemData
    TYPE(GenericFileType)            :: HDFFile
    
    CALL HDFFile%WriteData(f_iGroup,f_cAttributesDir,'SystemData%NNodes',ScalarAttrData=SystemData%NNodes)
    CALL HDFFile%WriteData(f_iGroup,f_cAttributesDir,'SystemData%NElements',ScalarAttrData=SystemData%NElements)
    CALL HDFFile%WriteData(f_iGroup,f_cAttributesDir,'SystemData%NLayers',ScalarAttrData=SystemData%NLayers)
    CALL HDFFile%WriteData(f_iGroup,f_cAttributesDir,'SystemData%NFaces',ScalarAttrData=SystemData%NFaces)
    CALL HDFFile%WriteData(SystemData%rNodeAreas,cHDFPath=f_cAttributesDir//'/SystemData%NodeAreas')
    CALL HDFFile%WriteData(SystemData%iElementIDs,cHDFPath=f_cAttributesDir//'/SystemData%ElementIDs')
    CALL HDFFile%WriteData(SystemData%rElementAreas,cHDFPath=f_cAttributesDir//'/SystemData%ElementAreas')
    CALL HDFFile%WriteData(SystemData%iElementNNodes,cHDFPath=f_cAttributesDir//'/SystemData%ElementNNodes')
    CALL HDFFile%WriteData(SystemData%iElementNodes,cHDFPath=f_cAttributesDir//'/SystemData%ElementNodes')
    IF (SystemData%NFaces .GT. 0) THEN
        CALL HDFFile%WriteData(SystemData%iFaceElems,cHDFPath=f_cAttributesDir//'/SystemData%FaceElements')
        CALL HDFFile%WriteData(SystemData%lBoundaryFace,cHDFPath=f_cAttributesDir//'/SystemData%BoundaryFace')
    END IF
    CALL HDFFile%WriteData(SystemData%lActiveNode,cHDFPath=f_cAttributesDir//'/SystemData%ActiveNode')
    CALL HDFFile%WriteData(SystemData%rElementNodeAreas,cHDFPath=f_cAttributesDir//'/SystemData%ElementNodeAreas')
    CALL HDFFile%WriteData(SystemData%rElementNodeAreas,cHDFPath=f_cAttributesDir//'/SystemData%ElementNodeAreaFractions')
    !CALL HDFFile%WriteData(SystemData%iActiveLayerBelow,cAttributesDir//'/SystemData%iActiveLayerBelow')  !This is computed based on SystemData%lActiveNode array
    
  END SUBROUTINE WriteToFile
  
  
  ! -------------------------------------------------------------
  ! --- KILL SYSTEM DATA
  ! -------------------------------------------------------------
  SUBROUTINE Kill(SystemData)
    CLASS(SystemDataType) :: SystemData
    
    !Local variables
    INTEGER              :: ErrorCode
    TYPE(SystemDataType) :: Dummy
    
    DEALLOCATE(SystemData%rNodeAreas                 , &
               SystemData%iElementIDs                , &
               SystemData%rElementAreas              , &
               SystemData%iElementNNodes             , &
               SystemData%iElementNodes              , &
               SystemData%iFaceElems                 , &
               SystemData%lBoundaryFace              , &
               SystemData%lActiveNode                , &
               SystemData%iActiveLayerBelow          , &
               SystemData%rElementNodeAreas          , &
               SystemData%rElementNodeAreaFractions  , &
               STAT = ErrorCode                      )
    
    !Back to default values
    SELECT TYPE (SystemData)
        TYPE IS (SystemDataType)
            SystemData = Dummy
    END SELECT
        
  END SUBROUTINE Kill

  
  
END MODULE
