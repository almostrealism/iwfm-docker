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
MODULE Class_Lake
  USE MessageLogger          , ONLY: SetLastMessage           , &
                                     MessageArray             , &
                                     iFatal
  USE GeneralUtilities       , ONLY: AllocArray               , &
                                     GetUniqueArrayComponents , &
                                     ShellSort                , &
                                     LocateInList             , &
                                     IntToText
  USE IOInterface            , ONLY: GenericFileType
  USE Package_Misc           , ONLY: PairedDataType  
  USE Package_Discretization , ONLY: AppGridType
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
  PUBLIC :: LakeType              , &
            ReadInitialLakeElevs


  ! -------------------------------------------------------------
  ! --- LAKE DATA TYPE
  ! -------------------------------------------------------------
  TYPE LakeType
      CHARACTER(LEN=200)   :: cName           = ''       !Name of the lake
      INTEGER              :: NElements       = 0        !Number of lake elements
      INTEGER              :: NNodes          = 0        !Number of groundwater nodes under lake
      REAL(8)              :: Area            = 0.0      !Lake area
      REAL(8)              :: Storage         = 0.0      !Lake storage at current time
      REAL(8)              :: Storage_P       = 0.0      !Lake storage at previous time
      REAL(8)              :: Elev            = 0.0      !Lake elevation at current time
      REAL(8)              :: Elev_P          = 0.0      !Lake elevation at the previous time
      REAL(8)              :: MaxElev         = 0.0      !Maximum lake elevation
      INTEGER              :: iColPrecip      = 0        !Pointer to a data column in precipitation data file
      INTEGER              :: iColET          = 0        !Pointer to data column in the potential ET data file
      REAL(8)              :: ETp_Rate        = 0.0      !Potential ET rate at lake
      REAL(8)              :: ETa             = 0.0      !Actual ET at lake
      REAL(8)              :: InflowUpLake    = 0.0      !Inflow into lake from upstream lakes
      REAL(8)              :: Outflow         = 0.0      !Lake outflow
      INTEGER              :: OutflowDestType = 0        !Destination type for outflow
      INTEGER              :: OutflowDest     = 0        !Outflow destination
      REAL(8)              :: PrecipRate      = 0.0      !Precipitation rate over lake
      INTEGER,ALLOCATABLE  :: Elements(:)                !List of elements under lake
      INTEGER,ALLOCATABLE  :: Nodes(:)                   !List of nodes under lake
      REAL(8),ALLOCATABLE  :: NodeAreas(:)               !Lake area associated with a lake node
      TYPE(PairedDataType) :: RatingTable                !Elevation vs. storage rating table
  CONTAINS
      PROCEDURE,PASS :: CompileLakeNodes
      PROCEDURE,PASS :: ComputeLakeNodeAreas
  END TYPE LakeType
  
  
  ! -------------------------------------------------------------
  ! --- MISC. DATA 
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                   :: ModNameLen = 12
  CHARACTER(LEN=ModNameLen),PARAMETER :: ModName    = 'Class_Lake::'




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
  ! --- COMPILE LIST OF GROUNDWATER NODES FOR ALL LAKE
  ! -------------------------------------------------------------
  SUBROUTINE CompileLakeNodes(Lake,AppGrid,iStat)
    CLASS(LakeType)              :: Lake
    TYPE(AppGridType),INTENT(IN) :: AppGrid
    INTEGER,INTENT(OUT)          :: iStat

    !Local variables
    CHARACTER(LEN=ModNameLen+16) :: ThisProcedure = ModName // 'CompileLakeNodes'
    INTEGER                      :: indxElem,iCounter,ErrorCode,ElemNo,NVertex,NNodes
    INTEGER,ALLOCATABLE          :: Nodes(:)

    !Initialize
    iStat    = 0
    iCounter = 0
      
    !Total number of nodes
    NNodes = SUM(AppGrid%Element(Lake%Elements)%NVertex)
    
    !Allocate memory
    CALL AllocArray(Nodes,NNodes,ThisProcedure,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Compile all nodes
    DO indxElem=1,Lake%NElements
      ElemNo                             = Lake%Elements(indxElem)
      NVertex                            = AppGrid%Element(ElemNo)%NVertex
      Nodes(iCounter+1:iCounter+NVertex) = AppGrid%Element(ElemNo)%Vertex(1:NVertex)
      iCounter                           = iCounter + NVertex
    END DO
    
    !Store unique nodes in persistent array
    CALL GetUniqueArrayComponents(Nodes,Lake%Nodes)
    
    !Sort the nodes
    CALL ShellSort(Lake%Nodes)
    
    !Number of nodes
    Lake%NNodes = SIZE(Lake%Nodes)
    
    !Clear memory
    DEALLOCATE (Nodes , STAT=ErrorCode)
    
  END SUBROUTINE CompileLakeNodes


  ! -------------------------------------------------------------
  ! --- COMPUTE LAKE NODE AREAS
  ! -------------------------------------------------------------
  SUBROUTINE ComputeLakeNodeAreas(Lake,AppGrid)
    CLASS(LakeType)              :: Lake
    TYPE(AppGridType),INTENT(IN) :: AppGrid

    !Local variables
    INTEGER :: indxElem,indxNode,iNode,ElemNo,NVertex,iVertex(4),iLoc
    REAL(8) :: VertexArea(4)

    !Initialize
    ALLOCATE (Lake%NodeAreas(Lake%NNodes))
    Lake%NodeAreas = 0.0

    !Compute nodal areas
    DO indxElem=1,Lake%NElements
        ElemNo                = Lake%Elements(indxElem)
        NVertex               = AppGrid%Element(ElemNo)%NVertex
        iVertex               = AppGrid%Element(ElemNo)%Vertex
        VertexArea(1:NVertex) = AppGrid%AppElement(ElemNo)%VertexArea
        DO indxNode=1,NVertex
            iNode = iVertex(indxNode)
            iLoc  = LocateInList(iNode,Lake%Nodes)
            Lake%NodeAreas(iLoc) = Lake%NodeAreas(iLoc) + VertexArea(indxNode)
        END DO
    END DO
        
    !Total lake area
    Lake%Area = SUM(Lake%NodeAreas)

  END SUBROUTINE ComputeLakeNodeAreas


  ! -------------------------------------------------------------
  ! --- READ INITIAL LAKE ELEVATIONS
  ! -------------------------------------------------------------
  SUBROUTINE ReadInitialLakeElevs(LakeDataFile,Lakes,iStat)
    TYPE(GenericFileType) :: LakeDataFile
    TYPE(LakeType)        :: Lakes(:)
    INTEGER,INTENT(OUT)   :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+20) :: ThisProcedure = ModName // 'ReadInitialLakeElevs'
    INTEGER                      :: NLakes,indxLake,ID
    REAL(8)                      :: Fact,DummyArray(2)
    
    !Initialize
    iStat  = 0
    NLakes = SIZE(Lakes)
    
    !Read conversion factor
    CALL LakeDataFile%ReadData(Fact,iStat)  
    IF(iStat .EQ. -1) RETURN
    
    !Read initial lake elavtions and process
    DO indxLake=1,NLakes
      CALL LakeDataFile%ReadData(DummyArray,iStat)  
      IF(iStat .EQ. -1) RETURN
      
      !Make sure that lakes are entered sequentially
      ID = INT(DummyArray(1))
      IF (ID .NE. indxLake) THEN 
          MessageArray(1) = 'Initial lake elevations should be entered sequentialy.'
          MessageArray(2) = 'Expected lake = '//TRIM(IntToText(indxLake))
          MessageArray(3) = 'Entered lake  = '//TRIM(IntToText(ID))
          CALL SetLAstMessage(MessageArray(1:3),iFatal,ThisProcedure)
          iStat = -1
          RETURN
      END IF
      
      !Assign initial elevations
      Lakes(indxLake)%Elev   = DummyArray(2) * Fact
      Lakes(indxLake)%Elev_P = Lakes(indxLake)%Elev
      
      !Make sure that initial elevations are not less than the lowest ground surface elevation
      IF (Lakes(indxLake)%Elev .LT. Lakes(indxLake)%RatingTable%XPoint(1)) THEN
          MessageArray(1) = 'Initial lake elevation for lake '//TRIM(IntToText(indxLake))//' is lower than the lowest ground surface elevation!'
          WRITE(MessageArray(2),'(A,F8.4)') 'Lowest ground surface elevation = ',Lakes(indxLake)%RatingTable%XPoint(1)
          WRITE(MessageArray(3),'(A,F8.4)') 'Initial lake elevation          = ',Lakes(indxLake)%Elev
          CALL SetLastMessage(MessageArray(1:3),iFatal,ThisProcedure)
          iStat = -1
          RETURN
      END IF
      
      !Compute initial storages
      Lakes(indxLake)%Storage   = Lakes(indxLake)%RatingTable%Evaluate(Lakes(indxLake)%Elev)
      Lakes(indxLake)%Storage_P = Lakes(indxLake)%Storage
      
    END DO

  END SUBROUTINE ReadInitialLakeElevs
  
  
   
END MODULE