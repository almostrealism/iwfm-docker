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
MODULE ParametricGrid
  USE Class_Grid        , ONLY: GridType               , &
                                CheckElementConvexity
  USE MessageLogger     , ONLY: SetLastMessage         , &
                                MessageArray           , &
                                f_iFatal
  USE IOInterface       , ONLY: GenericFileType
  USE GeneralUtilities  , ONLY: LocateInList           , &
                                ConvertID_To_Index     , &
                                IntToText              , &
                                TextToInt              , &
                                AllocArray             , &
                                CleanSpecialCharacters
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
  PUBLIC :: GetValuesFromParametricGrid


  ! -------------------------------------------------------------
  ! --- PARAMETRIC GRID RELATED DATA TYPES
  ! -------------------------------------------------------------

  !Parameter value data type
  TYPE ParamValueType
    REAL(8),ALLOCATABLE :: ParamValue(:) 
  END TYPE ParamValueType

  !Parametric grid node data type
  TYPE ParamGridNodeType
    TYPE(ParamValueType),ALLOCATABLE :: Layer(:)   
  END TYPE ParamGridNodeType

  !Parameteric grid data type
  TYPE ParamGridType
    TYPE(GridType)                      :: GridData
    TYPE(ParamGridNodeType),ALLOCATABLE :: ParamNode(:)   
    INTEGER                             :: NLayer     =  0
    INTEGER                             :: NParam     =  0
  END TYPE ParamGridType


  ! -------------------------------------------------------------
  ! --- PARAMETRIC GRID RELATED DATA
  ! -------------------------------------------------------------
  TYPE(ParamGridType),TARGET :: ParamGrid


  ! -------------------------------------------------------------
  ! --- MISC. DATA
  ! -------------------------------------------------------------
  CHARACTER(LEN=16),PARAMETER :: ModName='ParametricGrid::'
  INTEGER,PARAMETER           :: ModNameLen = 16




CONTAINS




! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** PARAMETRIC GRID CONSTRUCTOR
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************
  SUBROUTINE NewParametricGrid(X,Y,NVertex,Vertex,NLayer,NParam,ParamGrid,iStat)
    REAL(8),INTENT(IN)  :: X(:),Y(:)
    INTEGER,INTENT(IN)  :: NVertex(:),Vertex(:,:),NLayer,NParam
    TYPE(ParamGridType) :: ParamGrid
    INTEGER,INTENT(OUT) :: iStat

    !Local variables
    INTEGER :: indxNode,indxLayer,NNodes
        
    !Initialize
    iStat  = 0
    NNodes = SIZE(X)

    !Create the storage for the nodes of parametric grid
    CALL ParamGrid%GridData%Init(X,Y,NVertex,Vertex,iStat)  ;  IF (iStat .EQ. -1) RETURN
    ALLOCATE (ParamGrid%ParamNode(NNodes))
    DO indxNode=1,NNodes
        ALLOCATE (ParamGrid%ParamNode(indxNode)%Layer(NLayer))
        DO indxLayer=1,NLayer
            ALLOCATE (ParamGrid%ParamNode(indxNode)%Layer(indxLayer)%ParamValue(NParam))
        END DO
    END DO

    !Layer and parameter numbers
    ParamGrid%NLayer = NLayer
    ParamGrid%NParam = NParam

  END SUBROUTINE NewParametricGrid





! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** PARAMETRIC GRID DESTRUCTOR
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************
  SUBROUTINE KillParamGrid(ParamGrid)
    TYPE(ParamGridType) :: ParamGrid

        !Local variables
        INTEGER :: ErrorCode

    CALL ParamGrid%GridData%KillGrid()
    DEALLOCATE (ParamGrid%ParamNode , STAT=ErrorCode)

    ParamGrid%NLayer = 0
    ParamGrid%NParam = 0
    
  END SUBROUTINE KillParamGrid





! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** DATA READERS
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- READ PARAMETRIC GRID INFO
  ! -------------------------------------------------------------
  SUBROUTINE GetValuesFromParametricGrid(DataFile,Grid,FeatureIDs,NParamGrids,ConversionFactor,CellCentered,cDescription,ParamValues,iStat)
    TYPE(GenericFileType)       :: DataFile
    TYPE(GridType),INTENT(IN)   :: Grid
    INTEGER,INTENT(IN)          :: FeatureIDs(:),NParamGrids
    REAL(8),INTENT(IN)          :: ConversionFactor(:)
    LOGICAL,INTENT(IN)          :: CellCentered
    CHARACTER(LEN=*),INTENT(IN) :: cDescription
    REAL(8),INTENT(OUT)         :: ParamValues(:,:,:)
    INTEGER,INTENT(OUT)         :: iStat

    !Local variables
    CHARACTER(LEN=ModNameLen+33)  :: ThisProcedure = ModName // 'GetAquiferParamFromParametricGrid'
    INTEGER                       :: indxGrid,indxFENode,indxLayer,indxParamElem,indxParamNode,indxNodeStart,indxNodeEnd,indxNorm, &
                                     NDP,NEP,DummyIntArray(5),NVertex,ConvexNode,NParam,NLayers,ErrorCode,Vertex(4)
    INTEGER,ALLOCATABLE           :: NodesInterp(:),Param_NVertex(:),Param_Vertex(:,:)
    REAL(8)                       :: DummyRealArray(3+SIZE(ParamValues,2)*SIZE(ParamValues,3)),LocalParamValues(SIZE(ParamValues,2),SIZE(ParamValues,3)),XP,YP,X(4),Y(4)
    REAL(8),ALLOCATABLE           :: ParamNode_X(:),ParamNode_Y(:)
    LOGICAL                       :: Stat
    TYPE(GridType),POINTER        :: pGridData

    !Initialize
    iStat         = 0
    indxNodeStart = 1
    NLayers       = SIZE(ParamValues,2)
    NParam        = SIZE(ParamValues,3)

    !Process parametric grids
    ParamGridLoop: &
    DO indxGrid=1,NParamGrids

      !Initialize variables
      CALL KillParamGrid(ParamGrid)
      DEALLOCATE (ParamNode_X , ParamNode_Y , Param_NVertex , Param_Vertex , STAT=ErrorCode)
      
      !Read grid node/element numbers for which parameter values will be computed
      IF (CellCentered) THEN  !Element numbers are entered
          CALL READCH(DataFile,indxGrid,SIZE(Grid%NVertex),FeatureIDs,'Element',cDescription,NodesInterp,iStat)
      ELSE    !Node numbers are entered
          CALL READCH(DataFile,indxGrid,SIZE(Grid%X),FeatureIDs,'Node',cDescription,NodesInterp,iStat)
      END IF
      IF (iStat .NE. 0) RETURN

      !Read parametric grid dimensions and allocate arrays
      CALL DataFile%ReadData(NDP,iStat)  ;  IF (iStat .EQ. -1) RETURN      !Number of parametric nodes in the group
      CALL DataFile%ReadData(NEP,iStat)  ;  IF (iStat .EQ. -1) RETURN      !Number of parametric elements in the group
      IF (NDP.GT.1 .AND. NEP.EQ.0) THEN 
          CALL SetLastMessage('NDP in parameteric group '//TRIM(IntToText(indxGrid))//' should be zero!',f_iFatal,ThisProcedure)
          iStat = -1
          RETURN
      END IF
      indxNodeEnd = indxNodeStart+NDP-1
      ALLOCATE (ParamNode_X(indxNodeStart:indxNodeEnd) , ParamNode_Y(indxNodeStart:indxNodeEnd) , Param_NVertex(NEP) , Param_Vertex(4,NEP) , STAT=ErrorCode)
      IF (ErrorCode .NE. 0) THEN
          CALL SetLastMessage('Error in allocating memory for the nodes and/or elements of parametric grid '//TRIM(IntToText(indxGrid))//' for aquifer parameters!',f_iFatal,ThisProcedure)
          iStat = -1
          RETURN
      END IF

      !Create storage for parametric grid
      CALL NewParametricGrid(ParamNode_X,ParamNode_Y,Param_NVertex,Param_Vertex,NLayers,NParam,ParamGrid,iStat)
      IF (iStat .EQ. -1) RETURN

      !Read parametric elements and surrounding nodes
      DO indxParamElem=1,NEP
        CALL DataFile%ReadData(DummyIntArray,iStat)  ;  IF (iStat .EQ. -1) RETURN 
        IF (DummyIntArray(1) .NE. indxParamElem)  THEN
          MessageArray(1) = 'Parametric elements should be entered sequentially!'
          MessageArray(2) = 'Expected element number = '//TRIM(IntToText(indxParamElem))
          MessageArray(3) = 'Entered element number  = '//TRIM(IntToText(DummyIntArray(1)))
          CALL SetLastMessage(MessageArray(1:3),f_iFatal,ThisProcedure)
          iStat = -1
          RETURN
        END IF
        NVertex = 4 ; IF (DummyIntArray(5) .EQ. 0) NVertex = 3
        ParamGrid%GridData%NVertex(indxParamElem)  = NVertex
        ParamGrid%GridData%Vertex(:,indxParamElem) = DummyIntArray(2:)-indxNodeStart+1
      END DO 

      !Read coordinates and parameter values at parametric nodes
      DO indxParamNode=indxNodeStart,indxNodeEnd
        indxNorm = indxParamNode-indxNodeStart+1
        CALL DataFile%ReadData(DummyRealArray,iStat)  ;  IF (iStat .EQ. -1) RETURN 
        IF (INT(DummyRealArray(1)) .NE. indxParamNode) THEN
            MessageArray(1) = 'Parametric node data should be entered sequentially!'
            MessageArray(2) = 'Parametric grid      = '//TRIM(IntToText(indxGrid))
            MessageArray(3) = 'Expected node number = '//TRIM(IntToText(indxParamNode))
            MessageArray(4) = 'Entered node number  = '//TRIM(IntToText(INT(DummyRealArray(1))))
            CALL SetLastMessage(MessageArray(1:4),f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF

        !Store coordinates of parametric nodes
        ParamGrid%GridData%X(indxNorm) = DummyRealArray(2) * ConversionFactor(1) 
        ParamGrid%GridData%Y(indxNorm) = DummyRealArray(3) * ConversionFactor(1)

        !Reshape the 1-D DummyRealArray to 2-D LocalParamValues
        LocalParamValues = RESHAPE(DummyRealArray(4:),[NLayers,NParam],ORDER=[2,1])

        !Store parameter values at parametric nodes
        DO indxLayer=1,NLayers
          ParamGrid%ParamNode(indxNorm)%Layer(indxLayer)%ParamValue = LocalParamValues(indxLayer,:)*ConversionFactor(2:)
        END DO

      END DO

      !Check for parametric element convexity
      pGridData => ParamGrid%GridData
      DO indxParamElem=1,NEP
        NVertex  =  pGridData%NVertex(indxParamElem)
        IF (NVertex .EQ. 3) CYCLE
        Vertex   = pGridData%Vertex(:,indxParamElem)
        X = pGridData%X(Vertex)
        Y = pGridData%Y(Vertex)
        CALL CheckElementConvexity(NVertex,Vertex,X,Y,ConvexNode)
        IF (ConvexNode .NE. 0) THEN
          MessageArray(1) = 'Parametric element '//TRIM(IntToText(indxParamElem))//' in parametric grid '//TRIM(IntToText(indxGrid))
          MessageArray(2) = 'is not convex (has an angle greater than 180 degrees) at node '//TRIM(IntToText(ConvexNode+indxNodeStart-1))//'!'
          CALL SetLastMessage(MessageArray(1:2),f_iFatal,ThisProcedure)
          iStat = -1
          RETURN
        END IF
      END DO

      !Interpolate parametric values onto desired nodes
      DO indxFENode=1,SIZE(NodesInterp)
        IF (CellCentered) THEN
          CALL Grid%Centroid(NodesInterp(indxFENode),XP,YP)
        ELSE
          XP = Grid%X(NodesInterp(indxFENode))
          YP = Grid%Y(NodesInterp(indxFENode))
        END IF
        CALL InterpolateParametricGrid( XP , YP , ParamValues(NodesInterp(indxFENode),:,:) , Stat)
        IF (Stat .EQ. .FALSE.) THEN
            CALL SetLastMessage('FE node/element ' // TRIM(IntTotext(NodesInterp(indxFENode))) // ' cannot be located in parametric grid number ' // TRIM(IntToText(indxGrid)) // ' for ' // TRIM(cDescription) // '!',f_iFatal,Thisprocedure)
            iStat = -1
            RETURN
        END IF
      END DO

      !Increment indx_s
      indxNodeStart = indxNodeEnd+1

    END DO ParamGridLoop

    !Free memory 
    CALL KillParamGrid(ParamGrid)
    DEALLOCATE (ParamNode_X , ParamNode_Y , Param_NVertex, Param_Vertex , NodesInterp , STAT=ErrorCode)  
    
  END SUBROUTINE GetValuesFromParametricGrid


  ! -------------------------------------------------------------
  ! --- GET THE NUMBER OF PARAMETRIC GRIDS FOR AQUIFER
  ! -------------------------------------------------------------
  SUBROUTINE GetNAquiferParamGrid(ParameterFileName,LenParameterFileName,NParamGrid,iStat)
    INTEGER,INTENT(IN)                             :: LenParameterFileName
    CHARACTER(LEN=LenParameterFileName),INTENT(IN) :: ParameterFileName
    INTEGER,INTENT(OUT)                            :: NParamGrid,iStat

    !Local variables
    TYPE(GenericFileType) :: ParamFile

    !Open file
    CALL ParamFile%New(FileName=ParameterFileName,InputFile=.TRUE.,IsTSFile=.FALSE.,Descriptor='parameter data file',FileType='TXT',iStat=iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Read number of aquifer parametric grids
    CALL ParamFile%ReadData(NParamGrid,iStat)  
    IF (iStat .EQ. -1) RETURN 

    !Close file
    CALL ParamFile%Kill()

  END SUBROUTINE GetNAquiferParamGrid




! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** PARAMETRIC GRID INTERPOLATER
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- INTERPOLATE SPECIFIED NODAL VALUES ONTO AN (XP,YP) LOCATION
  ! -------------------------------------------------------------
  SUBROUTINE InterpolateParametricGrid(XP,YP,InterpolatedParamValue,Stat)
    REAL(8),INTENT(IN)  :: XP , YP
    REAL(8)             :: InterpolatedParamValue(:,:)
    LOGICAl,INTENT(OUT) :: Stat

    !Local variables
    INTEGER             :: indxLayer,indxNode,indxParam,NNodes,ElemID
    REAL(8)             :: InterpValue
    INTEGER,ALLOCATABLE :: Nodes(:)
    REAL(8),ALLOCATABLE :: Coeff(:)

    !Initialize
    Stat = .TRUE.

    !If assigning values to FE nodes without interpolation, do so and return
    IF (SIZE(ParamGrid%GridData%NVertex) .EQ. 0) THEN
      DO indxParam=1,ParamGrid%NParam
        DO indxLayer=1,ParamGrid%NLayer
           InterpValue = ParamGrid%ParamNode(1)%Layer(indxLayer)%ParamValue(indxParam)
           IF (InterpValue .GT. 0.0) InterpolatedParamValue(indxLayer,indxParam) = InterpValue
        END DO 
      END DO 
      RETURN
    END IF

    !Find the nodes and interpolation coefficients 
    CALL ParamGrid%GridData%FEInterpolate(XP,YP,ElemID,Nodes,Coeff)

    !If the coefficients are less than zero, point (XP,YP) wasn't in the grid; return
    IF (.NOT. ALLOCATED(Nodes)) THEN
      Stat = .FALSE.
      RETURN
    END IF

    !If the point was located in the grid, compute interpolated parameter values
    NNodes = SIZE(Nodes)  
    ParamLoop : DO indxParam=1,ParamGrid%NParam
    LayerLoop :   DO indxLayer=1,ParamGrid%NLayer
                    InterpValue = 0.0
    NodeLoop  :     DO indxNode=1,NNodes
                      InterpValue = InterpValue + Coeff(indxNode) * ParamGrid%ParamNode(Nodes(indxNode))%Layer(indxLayer)%ParamValue(indxParam)                          
                    END DO NodeLoop
                    IF (InterpValue .GE. 0.0) InterpolatedParamValue(indxLayer,indxParam) = InterpValue
                  END DO LayerLoop
                END DO ParamLoop

  END SUBROUTINE InterpolateParametricGrid




! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** MISCELLENOUS METHODS
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! ---  SUBROUTINE TO READ SPECIFIC CHARACTER STRING FROM FILE
  ! -------------------------------------------------------------
  SUBROUTINE READCH(ParameterFile,iParamGridNo,NLocations,iFeatureIDs,cNodeOrElem,cDescriptor,IW,iStat)
    TYPE(GenericFileType)           :: ParameterFile
    INTEGER,INTENT(IN)              :: iParamGridNo,NLocations,iFeatureIDs(:)
    CHARACTER(LEN=*),INTENT(IN)     :: cNodeOrElem,cDescriptor
    INTEGER,ALLOCATABLE,INTENT(OUT) :: IW(:)
    INTEGER,INTENT(OUT)             :: iStat

    !Local variables
    CHARACTER(LEN=ModNameLen+6)           :: ThisProcedure = ModName // 'READCH'
    INTEGER                               :: indx,INDX_S_ID,INDX_L_ID,LTH,IPOS,IPOS_DASH,N,ErrorCode,iLoc, &
                                             LocationsForInterp(NLocations),LocationList(NLocations), &
                                             ID
    INTEGER,PARAMETER                     :: LineLength=3000
    CHARACTER(LEN=LineLength),ALLOCATABLE :: DataLines(:)
    CHARACTER(LEN=LineLength)             :: WorkLine

    !Initialize
    iStat              = 0
    LocationsForInterp = -1
    LocationList       = [(indx,indx=1,NLocations)]

    !Read in the FE nodes for which data will be interpolated
    CALL ParameterFile%ReadData(DataLines,iStat)  ;  IF (iStat .EQ. -1) RETURN 
    CALL CleanSpecialCharacters(DataLines)
    DataLines = ADJUSTL(DataLines)
    
    DO indx=1,SIZE(DataLines)

        !Replace commas that seperate element numbers with spaces
        IPOS=SCAN(DataLines(indx),',')
        DO
            IF (IPOS.EQ.0) EXIT
            DataLines(indx)(IPOS:IPOS) = ' '
            IPOS=SCAN(DataLines(indx),',')
        END DO
        
        !Extract location numbers from DataLines(indx)
        LTH  = LEN_TRIM(DataLines(indx)) !Length of DataLines without the trailing blanks
        IPOS = SCAN(DataLines(indx),' ')
        DO
            WorkLine  = DataLines(indx)(1:IPOS-1)
            IPOS_DASH = SCAN(WorkLine,'-')
            !No dash is found; means one single location is given
            IF (IPOS_DASH .EQ. 0) THEN
                ID = TextToInt(WorkLine) 
                IF (ID .NE. 0) THEN
                    iLoc = LocateInList(ID,iFeatureIDs)
                    IF (iLoc .EQ. 0) THEN
                        CALL SetLastMessage(cNodeOrElem // ' ID number ' // TRIM(IntToText(ID)) // ', listed in parametric grid number '// TRIM(IntToText(iParamGridNo)) // ' for ' // TRIM(cDescriptor) // ', is not in the model!',f_iFatal,ThisProcedure)
                        iStat = -1
                        RETURN
                    END IF
                    LocationsForInterp(iLoc) = 0
                END IF
            !Dash is found; means a range of locations is given
            ELSE
                INDX_S_ID = TextToInt(WorkLine(1:IPOS_DASH-1))
                INDX_L_ID = TextToInt(WorkLine(IPOS_DASH+1:LEN_TRIM(WorkLine)))
                DO ID=INDX_S_ID,INDX_L_ID
                    iLoc = LocateInList(ID,iFeatureIDs)
                    IF (iLoc .EQ. 0) THEN
                        CALL SetLastMessage(cNodeOrElem // ' ID number ' // TRIM(IntToText(ID)) // ', listed in parametric grid number '// TRIM(IntToText(iParamGridNo)) // ' for ' // TRIM(cDescriptor) // ', is not in the model!',f_iFatal,ThisProcedure)
                        iStat = -1
                        RETURN
                    END IF
                    LocationsForInterp(iLoc) = 0
                END DO
            END IF
            !Delete the number(s) from DataLine
            DataLines(indx) = ADJUSTL(DataLines(indx)(IPOS:LTH))
            !Exit if there are no more locations
            IF (DataLines(indx) .EQ. '') EXIT
            LTH             = LEN_TRIM(DataLines(indx))
            IPOS            = SCAN(DataLines(indx),' ')
        END DO

    END DO 

    !Prepare IW for return value
    N = COUNT(LocationsForInterp .EQ. 0)
    CALL AllocArray(IW,N,ThisProcedure,iStat)  ;  IF (iStat .EQ. -1) RETURN
    IW = PACK(LocationList , MASK=LocationsForInterp.EQ.0)
    
    !Free memory
    DEALLOCATE (DataLines , STAT=ErrorCode)
           
  END SUBROUTINE READCH

END MODULE ParametricGrid