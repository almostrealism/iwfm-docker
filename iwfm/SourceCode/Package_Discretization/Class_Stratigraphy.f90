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
MODULE Class_Stratigraphy
  USE MessageLogger    , ONLY: SetLastMessage  , &
                               EchoProgress    , &
                               MessageArray    , &
                               iFatal
  USE IOInterface      
  USE GeneralUtilities , ONLY: IntToText
  USE Class_AppGrid    , ONLY: AppGridType     
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
  PUBLIC :: StratigraphyType                       


  ! -------------------------------------------------------------
  ! --- STRATIGRAPHY DATA TYPE
  ! -------------------------------------------------------------
  TYPE StratigraphyType
    INTEGER             :: NLayers = 0         !Number of stratigraphy layers
    INTEGER,ALLOCATABLE :: TopActiveLayer(:)   !Topmost active layer at node (node)
    LOGICAL,ALLOCATABLE :: ActiveNode(:,:)     !Active node indicator (node,layer)
    REAL(8),ALLOCATABLE :: GSElev(:)           !Ground surface elevation (node)
    REAL(8),ALLOCATABLE :: TopElev(:,:)        !Elevation of aquifer top (node,layer)
    REAL(8),ALLOCATABLE :: BottomElev(:,:)     !Elevation of aquifer bottom (node,layer)
  CONTAINS
    PROCEDURE,PASS :: ReadStratigraphyData 
    PROCEDURE,PASS :: ReadProcessedStratigraphyData
    PROCEDURE,PASS :: NewStratigraphy   
    PROCEDURE,PASS :: Kill
    PROCEDURE,PASS :: GetActiveLayerAbove        
    PROCEDURE,PASS :: GetActiveLayerBelow        
    PROCEDURE,PASS :: GetAllActiveLayerAbove     
    PROCEDURE,PASS :: GetAllActiveLayerBelow     
    PROCEDURE,PASS :: GetNActiveLayers           
    PROCEDURE,PASS :: GetTopActiveLayer          
    PROCEDURE,PASS :: GetLayerNumberForElevation 
    PROCEDURE,PASS :: GetNLayers
    PROCEDURE,PASS :: WritePreProcessedData      
    GENERIC        :: New        => ReadStratigraphyData           , &
                                    ReadProcessedStratigraphyData  , &
                                    NewStratigraphy
  END TYPE StratigraphyType
  
   
  ! -------------------------------------------------------------
  ! --- MISC. DATA
  ! -------------------------------------------------------------
  CHARACTER(LEN=20),PARAMETER :: ModName    = 'Class_Stratigraphy::'
  INTEGER,PARAMETER           :: ModNameLen = 20




CONTAINS




! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** CONSTRUCTOR
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- NEW STRATIGRAPHY
  ! -------------------------------------------------------------
  SUBROUTINE NewStratigraphy(Stratigraphy,NNodes,NLayers,TopActiveLayer,ActiveNode,GSElev,TopElev,BottomElev,iStat)
    CLASS(StratigraphyType),INTENT(OUT) :: Stratigraphy
    INTEGER,INTENT(IN)                  :: NLayers , NNodes , TopActiveLayer(NNodes)
    LOGICAL,INTENT(IN)                  :: ActiveNode(NNodes,NLayers)
    REAL(8),INTENT(IN)                  :: GSElev(NNodes),TopElev(NNodes,NLayers),BottomElev(NNodes,NLayers)
    INTEGER,INTENT(OUT)                 :: iStat

    !Local variables
    CHARACTER(LEN=ModNameLen+15) :: ThisProcedure = ModName // 'NewStratigraphy'
    INTEGER                      :: ErrorCode
    
    !Initialize
    iStat = 0

    !Allocate memory
    ALLOCATE (Stratigraphy%TopActiveLayer(NNodes)     , &
              Stratigraphy%ActiveNode(NNodes,NLayers) , &
              Stratigraphy%GSElev(NNodes)             , &
              Stratigraphy%TopElev(NNodes,NLayers)    , &
              Stratigraphy%BottomElev(NNodes,NLayers) , &
              STAT = ErrorCode                        )
    IF (ErrorCode .NE. 0) THEN
        CALL SetLastMessage('Error in allocating memory for stratigraphy data!',iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF

    !Store values
    Stratigraphy%NLayers        = NLayers
    Stratigraphy%TopActiveLayer = TopActiveLayer
    Stratigraphy%ActiveNode     = ActiveNode
    Stratigraphy%GSElev         = GSElev
    Stratigraphy%TopElev        = TopElev
    Stratigraphy%BottomElev     = BottomElev
    
  END SUBROUTINE NewStratigraphy




! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** DESTRUCTOR
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- KILL STRATIGRAPHY
  ! -------------------------------------------------------------
  SUBROUTINE Kill(Stratigraphy)
    CLASS(StratigraphyType) :: Stratigraphy
    
    !Local variables
    INTEGER :: ErrorCode
    
    Stratigraphy%NLayers = 0
    DEALLOCATE (Stratigraphy%TopActiveLayer  , &
                Stratigraphy%ActiveNode      , &
                Stratigraphy%GSElev          , &
                Stratigraphy%TopElev         , &
                Stratigraphy%BottomElev      , &
                STAT = ErrorCode             )
  
  END SUBROUTINE Kill
  
  

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
  ! --- READ DATA FROM PRE-PROCESSOR FILE
  ! -------------------------------------------------------------
  SUBROUTINE ReadStratigraphyData(Stratigraphy,NNodes,FileName,iStat) 
    CLASS(StratigraphyType),INTENT(OUT) :: Stratigraphy
    INTEGER,INTENT(IN)                  :: NNodes
    CHARACTER(LEN=*),INTENT(IN)         :: FileName
    INTEGER,INTENT(OUT)                 :: iStat

    !Local variables
    CHARACTER(LEN=ModNameLen+20) :: ThisProcedure=ModName//'ReadStratigraphyData'
    TYPE(GenericFileType)        :: StratigraphyFile
    INTEGER                      :: indxNode,ID,indxLayer,indxNL,NLayers,ErrorCode,TopActiveLayer(NNodes)
    REAL(8)                      :: Factor,GSElev(NNodes),Elevation
    REAL(8),POINTER              :: pTopElev,pBottomElev
    REAL(8),TARGET,ALLOCATABLE   :: Dummy2DRealArray(:,:),W(:),TopElev(:,:),BottomElev(:,:)
    LOGICAL,ALLOCATABLE          :: ActiveNode(:,:)
    
    !Inituialize
    iStat = 0

    !Make sure number of nodes is non-zero
    IF (NNodes .LE. 0) THEN
        CALL SetLastMessage('Application grid needs to be defined before reading stratigraphy data!',iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF

    !Print progress
    CALL EchoProgress('Instantiating stratigraphy')

    !Open file
    CALL StratigraphyFile%New(FileName=FileName,InputFile=.TRUE.,IsTSFile=.FALSE.,Descriptor='stratigraphy data file',iStat=iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Read number of layers and conversion factor
    CALL StratigraphyFile%ReadData(NLayers,iStat)  ;  IF (iStat .EQ. -1) RETURN        
    CALL StratigraphyFile%ReadData(Factor,iStat)   ;  IF (iStat .EQ. -1) RETURN 

    !Allocate memory
    ALLOCATE (TopElev(NNodes,NLayers)              , &
              BottomElev(NNodes,NLayers)           , &
              ActiveNode(NNodes,NLayers)           , &
              Dummy2DRealArray(NNodes,2+2*NLayers) , &
              W(2*NLayers)                         , &
              STAT=ErrorCode)
    IF (ErrorCode .NE. 0) THEN
        CALL SetLastMessage('Error in allocating memory to read stratigraphy data!',iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
   
    !Read layer thickness, etc
    CALL StratigraphyFile%ReadData(Dummy2DRealArray,iStat)  ;  IF (iStat .EQ. -1) RETURN 
    
    !Construct stratigraphy
    TopActiveLayer = -1
    DO indxNode=1,NNodes
      ID               =INT(Dummy2DRealArray(indxNode,1))
      GSElev(indxNode) =    Dummy2DRealArray(indxNode,2)*Factor
      W                =    Dummy2DRealArray(indxNode,3:)*Factor
      IF (ID .NE. indxNode) THEN
        MessageArray(1)='The stratigraphy data should be entered sequentially!'
        MessageArray(2)='Node number expected = '//TRIM(IntToText(indxNode))
        MessageArray(3)='Node number entered  = '//TRIM(IntToText(ID))
        CALL SetLastMessage(MessageArray(1:3),iFatal,ThisProcedure)
        iStat = -1
        RETURN
      END IF
      Elevation        = GSElev(indxNode)
      DO indxLayer = 1,NLayers
        pTopElev     => TopElev(indxNode,indxLayer)
        pBottomElev  => BottomElev(indxNode,indxLayer)
        indxNL       =  (indxLayer-1)*NNodes+indxNode
        Elevation    =  Elevation - W((indxLayer-1)*2+1)
        pTopElev     =  Elevation
        Elevation    =  Elevation - W((indxLayer-1)*2+2)
        pBottomElev  =  Elevation
        !Error if a negative aquifer thickness is supplied
        IF (pTopElev .LT. pBottomElev) THEN 
          MessageArray(1)='Aquifer thickness cannot be less than zero!'
          MessageArray(2)=                    'Node               = '//TRIM(IntToText(indxNode))
          WRITE (MessageArray(3),'(A,F10.2)') 'Specified thickness= ',W((indxLayer-1)*2+2)
          CALL SetLastMessage(MessageArray(1:3),iFatal,ThisProcedure)
          iStat = -1
          RETURN
        END IF
        !Top active layer
        IF (pTopElev.GT.pBottomElev .AND. TopActiveLayer(indxNode).EQ.-1) TopActiveLayer(indxNode) = indxLayer
        !Active node indicator
        ActiveNode(indxNode,indxLayer) = .TRUE.  
        IF (pTopElev .EQ. pBottomElev) ActiveNode(indxNode,indxLayer) = .FALSE.
      END DO
    END DO
    CALL Stratigraphy%New(NNodes,NLayers,TopActiveLayer,ActiveNode,GSElev,TopElev,BottomElev,iStat)
    IF (iStat .EQ. -1) RETURN

    !Close stratigraphy file
    CALL StratigraphyFile%Kill()
    
    !Free memory
    DEALLOCATE (ActiveNode , TopElev , BottomElev , Dummy2DRealArray , W , STAT=ErrorCode)

  END SUBROUTINE ReadStratigraphyData


  ! -------------------------------------------------------------
  ! --- READ DATA FROM BINARY FILE
  ! -------------------------------------------------------------
  SUBROUTINE ReadProcessedStratigraphyData(Stratigraphy,NNodes,InFile,iStat) 
    CLASS(StratigraphyType),INTENT(OUT) :: Stratigraphy
    INTEGER,INTENT(IN)                  :: NNodes
    TYPE(GenericFileType)               :: InFile
    INTEGER,INTENT(OUT)                 :: iStat

    !Local variables
    CHARACTER(LEN=ModNameLen+29),PARAMETER :: ThisProcedure = ModName//'ReadProcessedStratigraphyData'
    INTEGER                                :: NLayers,TopActiveLayer(NNodes),ErrorCode
    REAL(8)                                :: GSElev(NNodes)
    LOGICAL,ALLOCATABLE                    :: ActiveNode1D(:),ActiveNode(:,:)
    REAL(8),ALLOCATABLE                    :: TopElev(:,:),BottomElev(:,:)
    
    !Initialize
    iStat = 0

    !Check if number of nodes in the application grid is specified
    IF (NNodes .LE. 0) THEN
        CALL SetLastMessage('Application grid needs to be set before reading stratigraphy data!',iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF

    !Read number of layers
    CALL InFile%ReadData(NLayers,iStat)  ;  IF (iStat .EQ. -1) RETURN 
    
    !Allocate memory
    ALLOCATE (ActiveNode1D(NNodes*NLayers)  , &
              ActiveNode(NNodes,NLayers)    , &
              TopElev(NNodes,NLayers)       , &
              BottomElev(NNodes,NLayers)    , &
              STAT=ErrorCode                )
    IF (ErrorCode .NE. 0) THEN
        CALL SetLastMessage('Error in allocating memory for stratigraphy data when reading from pre-processor binary file!',iFatal,ThisProcedure)  
        iStat = -1
        RETURN
    END IF
                
    !Read the rest of the data
    CALL InFile%ReadData(TopActiveLayer,iStat)  ;  IF (iStat .EQ. -1) RETURN         
    CALL InFile%ReadData(ActiveNode1D,iStat)    ;  IF (iStat .EQ. -1) RETURN   ;  ActiveNode = RESHAPE(ActiveNode1D,(/NNodes,NLayers/))         
    CALL InFile%ReadData(GSElev,iStat)          ;  IF (iStat .EQ. -1) RETURN         
    CALL InFile%ReadData(TopElev,iStat)         ;  IF (iStat .EQ. -1) RETURN         
    CALL InFile%ReadData(BottomElev,iStat)      ;  IF (iStat .EQ. -1) RETURN 

    !Construct stratigraphy
    CALL Stratigraphy%New(NNodes,NLayers,TopActiveLayer,ActiveNode,GSElev,TopElev,BottomElev,iStat)
           
    !Free memory
    DEALLOCATE (ActiveNode1D , ActiveNode , TopElev , BottomElev , STAT=ErrorCode)
    
  END SUBROUTINE ReadProcessedStratigraphyData




! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** DATA WRITERS
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- WRITE DATA TO PRE-PROCESSOR BINARY FILE
  ! -------------------------------------------------------------
  SUBROUTINE WritePreProcessedData(Stratigraphy,OutFile)
    CLASS(StratigraphyType),INTENT(IN) :: Stratigraphy
    TYPE(GenericFileType)              :: OutFile

    !Local variables
    INTEGER             :: NNodes , NLayers , ErrorCode
    LOGICAL,ALLOCATABLE :: ActiveNode1D(:)
    
    !Initialize
    NLayers = Stratigraphy%NLayers
    NNodes  = SIZE(Stratigraphy%GSElev)
    ALLOCATE (ActiveNode1D(NNodes*NLayers))
    ActiveNode1D = RESHAPE(Stratigraphy%ActiveNode , [NNodes*NLayers])
    
    !Write data
    CALL OutFile%WriteData(Stratigraphy%NLayers)         
    CALL OutFile%WriteData(Stratigraphy%TopActiveLayer)          
    CALL OutFile%WriteData(ActiveNode1D)                         
    CALL OutFile%WriteData(Stratigraphy%GSElev)                  
    CALL OutFile%WriteData(Stratigraphy%TopElev)               
    CALL OutFile%WriteData(Stratigraphy%BottomElev) 
    
    !Free memory
    DEALLOCATE (ActiveNode1D , STAT=ErrorCode)        

  END SUBROUTINE WritePreProcessedData


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
  ! --- GET NUMBER OF MODELED LAYERS
  ! -------------------------------------------------------------
  PURE FUNCTION GetNLayers(Stratigraphy) RESULT(NLayers)
    CLASS(StratigraphyType),INTENT(IN) :: Stratigraphy
    INTEGER                            :: NLayers
    
    NLayers = Stratigraphy%NLayers
    
  END FUNCTION GetNLayers
  
  
  ! -------------------------------------------------------------
  ! --- GET TOP MOST ACTIVE LAYER NUMBER AT A NODE
  ! -------------------------------------------------------------
  FUNCTION GetTopActiveLayer(Stratigraphy,Node) RESULT(Layer)
    CLASS(StratigraphyType),INTENT(IN) :: Stratigraphy
    INTEGER,INTENT(IN)                 :: Node
    INTEGER                            :: Layer
    
    DO Layer=1,Stratigraphy%NLayers
      IF (Stratigraphy%ActiveNode(Node,Layer)) EXIT
    END DO
    
    IF (Layer .GT. Stratigraphy%NLayers) Layer = 0
    
  END FUNCTION GetTopActiveLayer
  

  ! -------------------------------------------------------------
  ! --- GET THE NUMBER OF ACTIVE LAYERS AT A NODE
  ! -------------------------------------------------------------
  FUNCTION GetNActiveLayers(Stratigraphy,Node) RESULT(NL)
    CLASS(StratigraphyType),INTENT(IN) :: Stratigraphy
    INTEGER,INTENT(IN)                 :: Node
    INTEGER                            :: NL

    NL = COUNT(Stratigraphy%ActiveNode(Node,:) .EQ. .TRUE.)

  END FUNCTION GetNActiveLayers
  
  
  ! -------------------------------------------------------------
  ! --- GET THE ACTIVE LAYER ABOVE ALL NODES AT A LAYER
  ! -------------------------------------------------------------
  FUNCTION GetAllActiveLayerAbove(Stratigraphy,Layer) RESULT(LayerAbove)
    CLASS(StratigraphyType),INTENT(IN) :: Stratigraphy
    INTEGER,INTENT(IN)                 :: Layer
    INTEGER                            :: LayerAbove(SIZE(Stratigraphy%TopActiveLayer))
    
    !Local variables
    INTEGER :: indxNode,AllNodes(SIZE(Stratigraphy%TopActiveLayer))
    
    !Initialize
    AllNodes   =[(indxNode , indxNode=1,SIZE(AllNodes))]
       
    LayerAbove = Stratigraphy%GetActiveLayerAbove(AllNodes,Layer)
    
  END FUNCTION GetAllActiveLayerAbove
  
  
  ! -------------------------------------------------------------
  ! --- GET THE ACTIVE LAYER ABOVE A NODE
  ! -------------------------------------------------------------
  ELEMENTAL FUNCTION GetActiveLayerAbove(Stratigraphy,Node,Layer) RESULT(LayerAbove)
    CLASS(StratigraphyType),INTENT(IN) :: Stratigraphy
    INTEGER,INTENT(IN)                 :: Node , Layer
    INTEGER                            :: LayerAbove
    
    !Local variables
    INTEGER :: indxLayer
    
    !Initialize
    LayerAbove = -1
       
    DO indxLayer=Layer-1,1,-1
      IF (Stratigraphy%ActiveNode(Node,indxLayer)) THEN
        LayerAbove = indxLayer
        EXIT
      END IF
    END DO
    
  END FUNCTION GetActiveLayerAbove
  
  
  ! -------------------------------------------------------------
  ! --- GET THE ACTIVE LAYER BELOW ALL NODES AT A LAYER
  ! -------------------------------------------------------------
  PURE FUNCTION GetAllActiveLayerBelow(Stratigraphy,Layer) RESULT(LayerBelow)
    CLASS(StratigraphyType),INTENT(IN) :: Stratigraphy
    INTEGER,INTENT(IN)                 :: Layer
    INTEGER                            :: LayerBelow(SIZE(Stratigraphy%TopActiveLayer))
    
    !Local variables
    INTEGER :: indxNode,AllNodes(SIZE(Stratigraphy%TopActiveLayer))
    
    !Initialize
    AllNodes   =[(indxNode , indxNode=1,SIZE(AllNodes))]
       
    LayerBelow = Stratigraphy%GetActiveLayerBelow(AllNodes,Layer)
    
  END FUNCTION GetAllActiveLayerBelow
  
  
  ! -------------------------------------------------------------
  ! --- GET THE ACTIVE LAYER BELOW A NODE
  ! -------------------------------------------------------------
  ELEMENTAL FUNCTION GetActiveLayerBelow(Stratigraphy,Node,Layer) RESULT(LayerBelow)
    CLASS(StratigraphyType),INTENT(IN) :: Stratigraphy
    INTEGER,INTENT(IN)                 :: Node , Layer
    INTEGER                            :: LayerBelow
   
    !Local variables
    INTEGER :: indxLayer
    
    !Initialize
    LayerBelow = -1
       
    DO indxLayer=Layer+1,Stratigraphy%NLayers
      IF (Stratigraphy%ActiveNode(Node,indxLayer)) THEN
        LayerBelow = indxLayer
        EXIT
      END IF
    END DO
    
  END FUNCTION GetActiveLayerBelow
  
  
  ! -------------------------------------------------------------
  ! --- GET THE LAYER NUMBER GIVEN ELEVATION
  ! -------------------------------------------------------------
  FUNCTION GetLayerNumberForElevation(Stratigraphy,Elevation,X,Y,AppGrid) RESULT(Layer)
    CLASS(StratigraphyTYpe),INTENT(IN) :: Stratigraphy
    REAL(8),INTENT(IN)                 :: Elevation,X,Y
    TYPE(AppGridType),INTENT(IN)       :: AppGrid
    INTEGER                            :: Layer
    
    !Local variables
    INTEGER             :: NLayers,indxLayer,ElemNo
    REAL(8)             :: TopElev(Stratigraphy%NLayers+1)
    INTEGER,ALLOCATABLE :: Nodes(:)
    REAL(8),ALLOCATABLE :: Coeff(:)
    
    !Initialize
    NLayers = Stratigraphy%NLayers 
    Layer   = 0
    
    !Find interpoltaion coefficients
    CALL AppGrid%FeInterpolate(X,Y,ElemNo,Nodes,Coeff)
    IF (.NOT. ALLOCATED(Nodes)) RETURN
    
    !Compute aquifer layer top elevations at x,y location
    TopElev(1) = SUM(Stratigraphy%GSElev(Nodes) * Coeff)
    DO indxLayer=2,NLayers+1
      TopElev(indxLayer) = SUM(Stratigraphy%BottomElev(Nodes,indxLayer-1) * Coeff)
    END DO
    
    !Find the layer number for Elevation
    DO indxLayer=1,NLayers
      IF (Elevation.LE.TopElev(indxLayer) .AND. Elevation.GE.TopElev(indxLayer+1)) THEN
        Layer = indxLayer
        RETURN
      END IF
    END DO
    
  END FUNCTION GetLayerNumberForElevation


END MODULE