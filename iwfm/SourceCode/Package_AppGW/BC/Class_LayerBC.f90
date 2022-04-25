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
MODULE Class_LayerBC
  USE GenericLinkedList      , ONLY: GenericLinkedListType
  USE GeneralUtilities       , ONLY: StripTextUntilCharacter , &
                                     IntToText               , &
                                     CleanSpecialCharacters  , &
                                     AllocArray              , &
                                     ShellSort               , &
                                     LocateInList            , &
                                     GetUniqueArrayComponents, &
                                     ConvertID_To_Index      , &
                                     FEXP
  USE TimeSeriesUtilities    , ONLY: IsTimeIntervalValid     , &
                                     TimeIntervalConversion
  USE IOInterface            , ONLY: GenericFileType         
  USE MessageLogger          , ONLY: SetLastMessage          , &
                                     LogMessage              , &
                                     MessageArray            , &
                                     f_iFatal                , &
                                     f_iWarn
  USE Package_Misc           , ONLY: RealTSDataInFileType    , &
                                     f_iGWComp               , &
                                     f_rSmoothMaxP           , &
                                     f_rSmoothStepP 
  USE Package_Discretization , ONLY: AppGridType             , &
                                     StratigraphyType        
  USE Package_Matrix         , ONLY: MatrixType
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
  PUBLIC :: LayerBCType                            , &
            LayerBC_InitSpecifiedFlowBC            , &
            LayerBC_InitSpecifiedHeadBC            , &
            LayerBC_InitGeneralHeadBC              , &
            LayerBC_InitConstrainedGeneralHeadBC   , &
            LayerBC_Kill                           , &
            LayerBC_GetNNodesWithBC                , &
            LayerBC_GetNodesWithBC                 , &
            LayerBC_GetTSFlowBCColumns             , &
            LayerBC_GetTSHeadBCColumns             , &
            LayerBC_GetNetBCFlow                   , &
            LayerBC_GetNetBCFlowWithBCType         , &
            LayerBC_SetTSBoundaryConditions        , &
            LayerBC_IsBCNode                       , &
            LayerBC_IsBCNodeWithBCTypes            , &
            LayerBC_CheckConsistency               , &
            LayerBC_ConvertSpecifiedFlowTimeUnit   , &
            LayerBC_ConvertGHBCTimeUnit            , &
            LayerBC_ConvertConstrainedGHBCTimeUnit , &
            LayerBC_Simulate                       , &
            f_iSpFlowBCID                          , &
            f_iSpHeadBCID                          , &
            f_iGHBCID                              , &
            f_iConstrainedGHBCID          
  
  
  ! -------------------------------------------------------------
  ! --- BASE B.C. DATA TYPE
  ! -------------------------------------------------------------
  TYPE BaseBCType
      INTEGER :: iNode     = 0    !Node number where the b.c. is defined
      INTEGER :: iTSColumn = 0    !Pointer to the data column in the time series b.c. data file (0 if the b.c. is not defined as time series
      REAL(8) :: rFlow     = 0.0  !Flow at the b.c. node
  END TYPE BaseBCType
  
  
  ! -------------------------------------------------------------
  ! --- SPECIFIED FLOW B.C. DATA TYPE 
  ! -------------------------------------------------------------
  TYPE,EXTENDS(BaseBCType) :: SpecifiedFlowBCType
      REAL(8) :: rFlowRead = 0.0  !Flow that is originally read from file
      REAL(8) :: rGradient = 0.0  !Gradient of the b.c. flow w.r.t. head, in case it gets scaled down for drying node
  END TYPE SpecifiedFlowBCType
  

  ! -------------------------------------------------------------
  ! --- SPECIFIED HEAD B.C. DATA TYPE AND LINKED LIST
  ! -------------------------------------------------------------
  TYPE,EXTENDS(BaseBCType) :: SpecifiedHeadBCType
      REAL(8) :: rHead  = 0.0    !Specified head b.c.
  END TYPE SpecifiedHeadBCType
  
 
  ! -------------------------------------------------------------
  ! --- GENERAL HEAD B.C. DATA TYPE
  ! -------------------------------------------------------------
  TYPE,EXTENDS(BaseBCType) :: GHBCType
      REAL(8) :: rHead        = 0.0    !Specified head b.c.
      REAL(8) :: rConductance = 0.0    !Conductance at the GHBC node
      REAL(8) :: rGradient    = 0.0    !Gradient of the flow w.r.t. head 
  END TYPE GHBCType
 

  ! -------------------------------------------------------------
  ! --- CONSTRAINED GENERAL HEAD B.C. DATA TYPE
  ! -------------------------------------------------------------
  TYPE,EXTENDS(GHBCType) :: ConstrainedGHBCType
      REAL(8) :: rConstrainingBCHead = 0.0    !Head that will be used instead of groundwater head if groundwater head is below this level 
      REAL(8) :: rMaxBCFlow          = 0.0    !Maximum flow amount that the constrained head b.c. can achieve
      INTEGER :: iMaxFlowTSColumn    = 0      !Column number if max bc flow will be read in as time series data
  END TYPE ConstrainedGHBCType
 

  ! -------------------------------------------------------------
  ! --- BOUNDARY CONDITIONS LINKED LIST TYPE
  ! -------------------------------------------------------------
  TYPE,EXTENDS(GenericLinkedListType) :: BCListType
  END TYPE BCListType
 

  ! -------------------------------------------------------------
  ! --- LAYER B.C. DATA TYPE
  ! -------------------------------------------------------------
  TYPE LayerBCType
      INTEGER                               :: NSpecFlowBC            = 0      !Number of specified flow b.c. at an aquifer layer
      INTEGER                               :: NSpecHeadBC            = 0      !Number of specified head b.c. at an aquifer layer
      INTEGER                               :: NGHBC                  = 0      !Number of general head b.c. at an aquifer layer
      INTEGER                               :: NConstrainedGHBC       = 0      !Number of constrained head b.c. at an aquifer layer
      TYPE(SpecifiedFlowBCType),ALLOCATABLE :: SpecFlowBC(:)                   !List of specified flow b.c. data
      TYPE(SpecifiedHeadBCType),ALLOCATABLE :: SpecHeadBC(:)                   !List of specified head b.c. data
      TYPE(GHBCType),ALLOCATABLE            :: GHBC(:)                         !List of general head b.c. data
      TYPE(ConstrainedGHBCType),ALLOCATABLE :: ConstrainedGHBC(:)              !List of constrained general head b.c. data
  CONTAINS
      PROCEDURE,PASS :: SetBCNode
      PROCEDURE,PASS :: SetBC
      PROCEDURE,PASS :: RemoveBC
  END TYPE LayerBCType

  
  ! -------------------------------------------------------------
  ! --- BOUNDARY CONDITION TYPE ID NUMBERS AND DESCRIPTIONS
  ! -------------------------------------------------------------
  INTEGER,PARAMETER           :: f_iSpFlowBCID         = 1 , &  !Specified flow
                                 f_iSpHeadBCID         = 2 , &  !Specified head
                                 f_iGHBCID             = 3 , &  !General head boundary
                                 f_iConstrainedGHBCID  = 4      !Constrained general head boundary
  CHARACTER(LEN=29),PARAMETER :: f_cBCDescriptor(4) = ['specified flow b.c.'          , &
                                                       'specified head b.c.'          , &
                                                       'general head b.c.'            , &
                                                       'constrained general head b.c.']
    
  
  ! -------------------------------------------------------------
  ! --- MISC. ENTITIES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                   :: ModNameLen = 15
  CHARACTER(LEN=ModNameLen),PARAMETER :: ModName    = 'Class_LayerBC::'

  

  
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
  ! --- INSTANTIATE SPECIFIED FLOW B.C. FOR ALL LAYERS
  ! -------------------------------------------------------------
  SUBROUTINE LayerBC_InitSpecifiedFlowBC(cFileName,NNodes,NodeIDs,Stratigraphy,cTimeUnit,LayerBC,iStat)
    CHARACTER(LEN=*),INTENT(IN)       :: cFileName
    INTEGER,INTENT(IN)                :: NNodes,NodeIDs(NNodes)
    TYPE(StratigraphyType),INTENT(IN) :: Stratigraphy
    CHARACTER(LEN=6),INTENT(OUT)      :: cTimeUnit
    TYPE(LayerBCType)                 :: LayerBC(:)
    INTEGER,INTENT(OUT)               :: iStat
       
    !Local variables
    CHARACTER(LEN=ModNameLen+25)          :: ThisProcedure = ModName // 'LayerBC_InitSpecifiedFlowBC'
    INTEGER                               :: iNode,indx,NQB,iTSCol,iLayer,indxLayer,NBCNode,ErrorCode,ID
    REAL(8)                               :: FACT,BQ
    CHARACTER                             :: ALine*1000
    REAL(8),ALLOCATABLE                   :: DummyArray(:,:)
    INTEGER,ALLOCATABLE                   :: iNodes(:),iRanks(:),Indices(:)
    TYPE(GenericFileType)                 :: InFile
    TYPE(SpecifiedFlowBCType)             :: aFlowBC
    TYPE(SpecifiedFlowBCType),ALLOCATABLE :: TempFlowBCArray(:)
    TYPE(BCListType)                      :: BCList(Stratigraphy%NLayers)
    CLASS(*),POINTER                      :: pCurrentData
    
    !Initialize
    iStat = 0
    
    !Open file
    CALL InFile%New(FileName=TRIM(cFileName),InputFile=.TRUE.,IsTSFile=.FALSE.,Descriptor='specified flow boundary conditions data',iStat=iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Read the specification data
    CALL InFile%ReadData(NQB,iStat)  ;  IF (iStat .EQ. -1) RETURN 
    IF (NQB .EQ. 0) THEN       !Return if no b.c. is listed
        CALL InFile%Kill()
        RETURN
    END IF
    CALL InFile%ReadData(FACT,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN  ;  CALL CleanSpecialCharacters(ALine)
    cTimeUnit = ADJUSTL(StripTextUntilCharacter(ALine,'/'))
    
    !Make sure time unit is recognized
    IF (IsTimeIntervalValid(cTimeUnit) .EQ. 0) THEN
        CALL SetLastMessage(TRIM(cTimeUnit) // ' in Specified Flow Boundary Conditions File is not a recognized time unit!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    !Allocate memory
    CALL AllocArray(DummyArray,NQB,4,ThisProcedure,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Read and process specified flow b.c.
    CALL InFile%ReadData(DummyArray,iStat)  ;  IF (iStat .EQ. -1) RETURN
    ALLOCATE (Indices(NQB))
    CALL ConvertID_To_Index(INT(DummyArray(:,1)),NodeIDs,Indices)
    DO indx=1,NQB
        ID     = INT(DummyArray(indx,1))
        iNode  = Indices(indx)
        iLayer = INT(DummyArray(indx,2))
        iTSCol = INT(DummyArray(indx,3))
        BQ     =     DummyArray(indx,4)
      
        !Make sure iNode is in model range
        IF (iNode .EQ. 0) THEN
            CALL SetLastMessage('Node number '//TRIM(IntToText(ID))//' listed in groundwater specified flow boundary condition file is not in the model.',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        
        !Make sure iLayer is in model range
        IF (iLayer .GT. Stratigraphy%Nlayers  .OR.  iLayer .LT. 1) THEN
            CALL SetLastMessage('Layer number '//TRIM(IntToText(iLayer))//' listed in groundwater specified flow boundary condition file is out of range.',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        
        !Make sure boundary node is an active node
        IF (.NOT. Stratigraphy%ActiveNode(iNode,iLayer)) THEN
            CALL SetLastMessage('Specified flow boundary node '//TRIM(IntToText(ID))//' in layer '//TRIM(IntToText(iLayer))//' is an inactive node!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        
        !Store data in temporary variable
        aFlowBC%iNode = iNode
        IF (iTSCol .GT. 0) THEN
            aFlowBC%iTSColumn = iTSCol
        ELSE
            aFlowBC%iTSColumn = 0
            aFlowBC%rFlow     = BQ * FACT
            aFlowBC%rFlowRead = BQ * FACT
        END IF
        
        !Add flow b.c. to designated linked list
        CALL BCList(iLayer)%AddNode(aFlowBC,iStat)
        IF (iStat .EQ. -1) RETURN

    END DO
    
    !Store b.c. data in persistant variables and order the b.c. nodes
    DO indxLayer=1,Stratigraphy%NLayers
        !Store in persistant array
        NBCNode = BCList(indxLayer)%GetNNodes()
        IF (NBCNode .EQ. 0) CYCLE
        LayerBC(indxLayer)%NSpecFlowBC = NBCNode
        ALLOCATE (LayerBC(indxLayer)%SpecFlowBC(NBCNode))
        CALL BCList(indxLayer)%Reset()
        DO indx=1,NBCNode
            pCurrentData => BCList(indxLayer)%GetCurrentValue()
            SELECT TYPE (pCurrentData)
               TYPE IS (SpecifiedFlowBCType)
                  LayerBC(indxLayer)%SpecFlowBC(indx) = pCurrentData
            END SELECT
            CALL BCList(indxLayer)%Next()
        END DO
        
        !Order nodes
        DEALLOCATE (iNodes , iRanks , TempFlowBCArray , STAT=ErrorCode)
        ALLOCATE (iNodes(NBCNode) , iRanks(NBCNode) , TempFlowBCArray(NBCNode))
        TempFlowBCArray               = LayerBC(indxLayer)%SpecFlowBC
        iNodes                        = LayerBC(indxLayer)%SpecFlowBC%iNode
        iRanks                        = OrderBCNodes(iNodes)
        LayerBC(indxLayer)%SpecFlowBC = TempFlowBCArray(iRanks)
        
        !Delete linked list
        CALL BCList(indxLayer)%Delete()
    END DO
    
    !Clear memory
    DEALLOCATE (Indices , iNodes , iRanks , TempFlowBCArray , DummyArray , STAT=ErrorCode)
    
    !Close file
    CALL InFile%Kill()
            
  END SUBROUTINE LayerBC_InitSpecifiedFlowBC


  ! -------------------------------------------------------------
  ! --- INSTANTIATE SPECIFIED HEAD B.C. FOR ALL LAYERS
  ! -------------------------------------------------------------
  SUBROUTINE LayerBC_InitSpecifiedHeadBC(cFileName,NNodes,NodeIDs,Stratigraphy,GWHeads,LayerBC,iStat)
    CHARACTER(LEN=*),INTENT(IN)       :: cFileName
    INTEGER,INTENT(IN)                :: NNodes,NodeIDs(NNodes)
    TYPE(StratigraphyType),INTENT(IN) :: Stratigraphy
    REAL(8)                           :: GWHeads(:,:)
    TYPE(LayerBCType)                 :: LayerBC(:)
    INTEGER,INTENT(OUT)               :: iStat
       
    !Local variables
    CHARACTER(LEN=ModNameLen+27)          :: ThisProcedure = ModName // 'LayerBC_InitSpecifiedHeadBC'
    INTEGER                               :: iNode,indx,NHB,iTSCol,iLayer,indxLayer,NBCNode,ErrorCode,ID
    REAL(8)                               :: FACT,BH,BottomElev
    REAL(8),ALLOCATABLE                   :: DummyArray(:,:)
    INTEGER,ALLOCATABLE                   :: iNodes(:),iRanks(:),Indices(:)
    TYPE(GenericFileType)                 :: InFile
    TYPE(SpecifiedHeadBCType)             :: aHeadBC
    TYPE(SpecifiedHeadBCType),ALLOCATABLE :: TempHeadBCArray(:)
    TYPE(BCListType)                      :: BCList(Stratigraphy%NLayers)
    CLASS(*),POINTER                      :: pCurrentData
    
    !Initialize
    iStat = 0
    
    !Open file
    CALL InFile%New(FileName=TRIM(cFileName),InputFile=.TRUE.,IsTSFile=.FALSE.,Descriptor='specified head boundary conditions data',iStat=iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Read the specification data
    CALL InFile%ReadData(NHB,iStat)  ;  IF (iStat .EQ. -1) RETURN 
    IF (NHB .EQ. 0) THEN      !Return if no b.c. is listed
        CALL InFile%Kill()
        RETURN
    END IF
    CALL InFile%ReadData(FACT,iStat)  ;  IF (iStat .EQ. -1) RETURN

    !Allocate memory
    CALL AllocArray(DummyArray,NHB,4,ThisProcedure,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Read and process specified flow b.c.
    CALL InFile%ReadData(DummyArray,iStat)  ;  IF (iStat .EQ. -1) RETURN
    ALLOCATE (Indices(NHB))
    CALL ConvertID_To_Index(INT(DummyArray(:,1)),NodeIDs,Indices)
    DO indx=1,NHB
        ID     = INT(DummyArray(indx,1))
        iNode  = Indices(indx)
        iLayer = INT(DummyArray(indx,2))
        iTSCol = INT(DummyArray(indx,3))
        BH     =     DummyArray(indx,4) * FACT
        
        !Make sure iNode is in model range
        IF (iNode .EQ. 0) THEN
            CALL SetLastMessage('Node number '//TRIM(IntToText(ID))//' listed in groundwater specified head boundary condition file is not in the model.',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        
        !Make sure iLayer is in model range
        IF (iLayer .GT. Stratigraphy%Nlayers  .OR.  iLayer .LT. 1) THEN
            CALL SetLastMessage('Layer number '//TRIM(IntToText(iLayer))//' listed in groundwater specified head boundary condition file is out of range.',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        
        !Make sure boundary node is an active node
        IF (.NOT. Stratigraphy%ActiveNode(iNode,iLayer)) THEN
            CALL SetLastMessage('Specified head boundary node '//TRIM(IntToText(ID))//' in layer '//TRIM(IntToText(iLayer))//' is an inactive node!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        
        !For non-time-series b.c. make sure specified head is above aquifer bottom and apply the b.c. to the groundwater heads
        IF (iTSCol .LE. 0) THEN
            !Check that b.c. is above aquifer bottom
            BottomElev = Stratigraphy%BottomElev(iNode,iLayer)
            IF (BH .LT. BottomElev) THEN
                MessageArray(1) = 'Specified head at boundary node '  // TRIM(IntToText(ID)) // ' in layer '//TRIM(IntToText(iLayer))
                MessageArray(2) = 'is below the bottom of the aquifer.'
                WRITE (MessageArray(3),'(A,F9.3)') 'Specified head   =',BH
                WRITE (MessageArray(4),'(A,F9.3)') 'Bottom of aquifer=',BottomElev
                MessageArray(5) = 'Assigning the elevation of aquifer bottom as specified head b.c.'
                CALL LogMessage(MessageArray(1:5),f_iWarn,ThisProcedure)
                BH = BottomElev
            END IF
            
            !Apply b.c.
            GWHeads(iNode,iLayer) = BH
        END IF
        
        !Store data in temporary variable
        aHeadBC%iNode = iNode
        IF (iTSCol .GT. 0) THEN
            aHeadBC%iTSColumn = iTSCol
        ELSE
            aHeadBC%iTSColumn = 0
            aHeadBC%rHead     = BH 
        END IF
        
        !Add flow b.c. to designated linked list
        CALL BCList(iLayer)%AddNode(aHeadBC,iStat)
        IF (iStat .EQ. -1) RETURN

    END DO
    
    !Store b.c. data in persistant variables and order the b.c. nodes
    DO indxLayer=1,Stratigraphy%NLayers
        !Store in persistant array
        NBCNode = BCList(indxLayer)%GetNNodes()
        IF (NBCNode .EQ. 0) CYCLE
        LayerBC(indxLayer)%NSpecHeadBC = NBCNode
        ALLOCATE (LayerBC(indxLayer)%SpecHeadBC(NBCNode))
        CALL BCList(indxLayer)%Reset()
        DO indx=1,NBCNode
            pCurrentData => BCList(indxLayer)%GetCurrentValue()
            SELECT TYPE (pCurrentData)
               TYPE IS (SpecifiedHeadBCType)
                  LayerBC(indxLayer)%SpecHeadBC(indx) = pCurrentData
            END SELECT
            CALL BCList(indxLayer)%Next()
        END DO
        
        !Order nodes
        DEALLOCATE (iNodes , iRanks , TempHeadBCArray , STAT=ErrorCode)
        ALLOCATE (iNodes(NBCNode) , iRanks(NBCNode) , TempHeadBCArray(NBCNode))
        TempHeadBCArray               = LayerBC(indxLayer)%SpecHeadBC
        iNodes                        = LayerBC(indxLayer)%SpecHeadBC%iNode
        iRanks                        = OrderBCNodes(iNodes)
        LayerBC(indxLayer)%SpecHeadBC = TempHeadBCArray(iRanks)
        
        !Delete linked list
        CALL BCList(indxLayer)%Delete()
        
    END DO
    
    !Clear memory
    DEALLOCATE (Indices , iNodes , iRanks , TempHeadBCArray , DummyArray , STAT=ErrorCode)
    
    !Close file
    CALL InFile%Kill()
            
  END SUBROUTINE LayerBC_InitSpecifiedHeadBC
  
  
  ! -------------------------------------------------------------
  ! --- INSTANTIATE GENERAL HEAD B.C. FOR ALL LAYERS
  ! -------------------------------------------------------------
  SUBROUTINE LayerBC_InitGeneralHeadBC(cFileName,NNodes,NodeIDs,Stratigraphy,cTimeUnit,LayerBC,iStat)
    CHARACTER(LEN=*),INTENT(IN)       :: cFileName
    INTEGER,INTENT(IN)                :: NNodes,NodeIDs(NNodes)
    TYPE(StratigraphyType),INTENT(IN) :: Stratigraphy
    CHARACTER(LEN=6),INTENT(OUT)      :: cTimeUnit
    TYPE(LayerBCType)                 :: LayerBC(:)
    INTEGER,INTENT(OUT)               :: iStat
       
    !Local variables
    CHARACTER(LEN=ModNameLen+25) :: ThisProcedure = ModName // 'LayerBC_InitGeneralHeadBC'
    CHARACTER                    :: ALine*1000
    INTEGER                      :: iNode,indx,NGB,iTSCol,iLayer,indxLayer,NBCNode,ErrorCode,ID
    REAL(8)                      :: FACTH,FACTC,BH,BC
    REAL(8),ALLOCATABLE          :: DummyArray(:,:)
    INTEGER,ALLOCATABLE          :: iNodes(:),iRanks(:),Indices(:)
    TYPE(GenericFileType)        :: InFile
    TYPE(GHBCType)               :: aGHBC
    TYPE(GHBCType),ALLOCATABLE   :: TempGHBCArray(:)
    TYPE(BCListType)             :: BCList(Stratigraphy%NLayers)
    CLASS(*),POINTER             :: pCurrentData
    
    !Initialize
    iStat = 0
    
    !Open file
    CALL InFile%New(FileName=TRIM(cFileName),InputFile=.TRUE.,IsTSFile=.FALSE.,Descriptor='general head boundary conditions data',iStat=iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Read the specification data
    CALL InFile%ReadData(NGB,iStat)  ;  IF (iStat .EQ. -1) RETURN 
    IF (NGB .EQ. 0) THEN        !Return if no b.c. is defined
        CALL InFile%Kill()
        RETURN
    END IF
    CALL InFile%ReadData(FACTH,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(FACTC,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN  ;  CALL CleanSpecialCharacters(ALine)
    cTimeUnit = ADJUSTL(StripTextUntilCharacter(ALine,'/'))
    
    !Make sure time unit is recognized
    IF (IsTimeIntervalValid(cTimeUnit) .EQ. 0) THEN
        CALL SetLastMessage(TRIM(cTimeUnit) // ' in General Head Boundary Conditions File is not a recognized time unit!',f_iFatal,ThisProcedure)
          iStat = -1
          RETURN
      END IF
    
    !Allocate memory
    CALL AllocArray(DummyArray,NGB,5,ThisProcedure,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Read and process specified flow b.c.
    CALL InFile%ReadData(DummyArray,iStat)  ;  IF (iStat .EQ. -1) RETURN
    ALLOCATE (Indices(NGB))
    CALL ConvertID_To_Index(INT(DummyArray(:,1)),NodeIDs,Indices)
    DO indx=1,NGB
        ID     = INT(DummyArray(indx,1))
        iNode  = Indices(indx)
        iLayer = INT(DummyArray(indx,2))
        iTSCol = INT(DummyArray(indx,3))
        BH     =     DummyArray(indx,4)
        BC     =     DummyArray(indx,5)
      
        !Make sure iNode is in model range
        IF (iNode .EQ. 0) THEN
            CALL SetLastMessage('Node number '//TRIM(IntToText(ID))//' listed in groundwater general head boundary condition file is not in the model.',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        
        !Make sure iLayer is in model range
        IF (iLayer .GT. Stratigraphy%Nlayers  .OR.  iLayer .LT. 1) THEN
            CALL SetLastMessage('Layer number '//TRIM(IntToText(iLayer))//' listed in groundwater general head boundary condition file is out of range.',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        
        !Make sure boundary node is an active node
        IF (.NOT. Stratigraphy%ActiveNode(iNode,iLayer)) THEN
            CALL SetLastMessage('General head boundary node '//TRIM(IntToText(ID))//' in layer '//TRIM(IntToText(iLayer))//' is an inactive node!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        
        !Store data in temporary variable
        aGHBC%iNode = iNode
        IF (iTSCol .GT. 0) THEN
            aGHBC%iTSColumn = iTSCol
        ELSE
            aGHBC%iTSColumn = 0
            aGHBC%rHead     = BH * FACTH
        END IF
        aGHBC%rConductance  = BC * FACTC
        
        !Add flow b.c. to designated linked list
        CALL BCList(iLayer)%AddNode(aGHBC,iStat)
        IF (iStat .EQ. -1) RETURN
        
    END DO
    
    !Store b.c. data in persistant variables and order the b.c. nodes
    DO indxLayer=1,Stratigraphy%NLayers
        !Store in persistant array
        NBCNode = BCList(indxLayer)%GetNNodes()
        IF (NBCNode .EQ. 0) CYCLE
        LayerBC(indxLayer)%NGHBC = NBCNode
        ALLOCATE (LayerBC(indxLayer)%GHBC(NBCNode))
        CALL BCList(indxLayer)%Reset()
        DO indx=1,NBCNode
            pCurrentData => BCList(indxLayer)%GetCurrentValue()
            SELECT TYPE (pCurrentData)
               TYPE IS (GHBCType)
                  LayerBC(indxLayer)%GHBC(indx) = pCurrentData
            END SELECT
            CALL BCList(indxLayer)%Next()
        END DO
        
        !Order nodes
        DEALLOCATE (iNodes , iRanks , TempGHBCArray , STAT=ErrorCode)
        ALLOCATE (iNodes(NBCNode) , iRanks(NBCNode) , TempGHBCArray(NBCNode))
        TempGHBCArray           = LayerBC(indxLayer)%GHBC
        iNodes                  = LayerBC(indxLayer)%GHBC%iNode
        iRanks                  = OrderBCNodes(iNodes)
        LayerBC(indxLayer)%GHBC = TempGHBCArray(iRanks)
        
        !Delete linked list
        CALL BCList(indxLayer)%Delete()
    END DO
    
    !Clear memory
    DEALLOCATE (Indices , iNodes , iRanks , TempGHBCArray , DummyArray , STAT=ErrorCode)
    
    !Close file
    CALL InFile%Kill()
            
  END SUBROUTINE LayerBC_InitGeneralHeadBC
  

  ! -------------------------------------------------------------
  ! --- INSTANTIATE CONSTRAINED GENERAL HEAD B.C. FOR ALL LAYERS
  ! -------------------------------------------------------------
  SUBROUTINE LayerBC_InitConstrainedGeneralHeadBC(cFileName,NNodes,NodeIDs,Stratigraphy,cTimeUnit,LayerBC,iStat)
    CHARACTER(LEN=*),INTENT(IN)       :: cFileName
    INTEGER,INTENT(IN)                :: NNodes,NodeIDs(NNodes)
    TYPE(StratigraphyType),INTENT(IN) :: Stratigraphy
    CHARACTER(LEN=6),INTENT(OUT)      :: cTimeUnit
    TYPE(LayerBCType)                 :: LayerBC(:)
    INTEGER,INTENT(OUT)               :: iStat
       
    !Local variables
    CHARACTER(LEN=ModNameLen+36)          :: ThisProcedure = ModName // 'LayerBC_InitConstrainedGeneralHeadBC'
    CHARACTER                             :: ALine*1000,cTimeUnitConductance*6
    INTEGER                               :: iNode,indx,NCGB,iTSCol,iLayer,indxLayer,NBCNode,ErrorCode,iTSColF,ID
    REAL(8)                               :: FACTH,FACTC,FACTVL,BH,BC,LBH,CFlow
    REAL(8),ALLOCATABLE                   :: DummyArray(:,:)
    INTEGER,ALLOCATABLE                   :: iNodes(:),iRanks(:),Indices(:)
    TYPE(GenericFileType)                 :: InFile
    TYPE(ConstrainedGHBCType)             :: aCGHBC
    TYPE(ConstrainedGHBCType),ALLOCATABLE :: TempCGHBCArray(:)
    TYPE(BCListType)                      :: BCList(Stratigraphy%NLayers)
    CLASS(*),POINTER                      :: pCurrentData
    
    !Initialize
    iStat = 0
    
    !Open file
    CALL InFile%New(FileName=TRIM(cFileName),InputFile=.TRUE.,IsTSFile=.FALSE.,Descriptor='constrained general head boundary conditions data',iStat=iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Read the specification data
    CALL InFile%ReadData(NCGB,iStat)  ;  IF (iStat .EQ. -1) RETURN  
    IF (NCGB .EQ. 0) THEN          !Return if no  b.c. is listed
        CALL InFile%Kill() 
        RETURN
    END IF
    CALL InFile%ReadData(FACTH,iStat)   ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(FACTVL,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN  ;  CALL CleanSpecialCharacters(ALine)
    cTimeUnit = ADJUSTL(StripTextUntilCharacter(ALine,'/'))
    CALL InFile%ReadData(FACTC,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN  ;  CALL CleanSpecialCharacters(ALine)
    cTimeUnitConductance = ADJUSTL(StripTextUntilCharacter(ALine,'/'))
        
    !Make sure that time units are recognized
    IF (IsTimeIntervalValid(cTimeUnit) .EQ. 0) THEN
        CALL SetLastMessage(TRIM(cTimeUnit) // ' in Constrained General Head Boundary Conditions File is not a recognized time unit!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    IF (IsTimeIntervalValid(cTimeUnitConductance) .EQ. 0) THEN
        CALL SetLastMessage(TRIM(cTimeUnitConductance) // ' in Constrained General Head Boundary Conditions File is not a recognized time unit!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    !Equalize the time units for maximum boundary flow and conductance
    FACTC = FACTC * TimeIntervalConversion(cTimeUnit,cTimeUnitConductance)

    !Allocate memory
    CALL AllocArray(DummyArray,NCGB,8,ThisProcedure,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Read and process specified flow b.c.
    CALL InFile%ReadData(DummyArray,iStat)  ;  IF (iStat .EQ. -1) RETURN
    ALLOCATE (Indices(NCGB))
    CALL ConvertID_To_Index(INT(DummyArray(:,1)),NodeIDs,Indices)
    DO indx=1,NCGB
        ID      = INT(DummyArray(indx,1))
        iNode   = Indices(indx)
        iLayer  = INT(DummyArray(indx,2))
        iTSCol  = INT(DummyArray(indx,3))
        BH      =     DummyArray(indx,4)
        BC      =     DummyArray(indx,5)
        LBH     =     DummyArray(indx,6)
        iTSColF = INT(DummyArray(indx,7))
        CFlow   =     DummyArray(indx,8)
      
        !Make sure iNode is in model range
        IF (iNode .EQ. 0) THEN
            CALL SetLastMessage('Node number '//TRIM(IntToText(ID))//' listed in groundwater constrained general head boundary condition file is not in the model.',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
      
        !Make sure iLayer is in model range
        IF (iLayer .GT. Stratigraphy%Nlayers  .OR.  iLayer .LT. 1) THEN
            CALL SetLastMessage('Layer number '//TRIM(IntToText(iLayer))//' listed in groundwater constrained general head boundary condition file is out of range.',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        
        !Make sure boundary node is an active node
        IF (.NOT. Stratigraphy%ActiveNode(iNode,iLayer)) THEN
            CALL SetLastMessage('Constrained general head boundary node '//TRIM(IntToText(ID))//' in layer '//TRIM(IntToText(iLayer))//' is an inactive node!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        
        !Store data in temporary variable
        aCGHBC%iNode = iNode
        IF (iTSCol .GT. 0) THEN
            aCGHBC%iTSColumn       = iTSCol
        ELSE
            aCGHBC%iTSColumn       = 0
            aCGHBC%rHead           = BH * FACTH
        END IF
        aCGHBC%rConductance        = BC * FACTC
        aCGHBC%rConstrainingBCHead = LBH * FACTH
        IF (iTSColF .GT. 0) THEN
            aCGHBC%iMaxFlowTSColumn = iTSColF
        ELSE
            aCGHBC%iMaxFlowTSColumn = 0
            aCGHBC%rMaxBCFlow       = cFlow * FACTVL
        END IF
        
        !Add flow b.c. to designated linked list
        CALL BCList(iLayer)%AddNode(aCGHBC,iStat)
        IF (iStat .EQ. -1) RETURN
        
    END DO
    
    !Store b.c. data in persistant variables and order the b.c. nodes
    DO indxLayer=1,Stratigraphy%NLayers
        !Store in persistant array
        NBCNode = BCList(indxLayer)%GetNNodes()
        IF (NBCNode .EQ. 0) CYCLE
        LayerBC(indxLayer)%NConstrainedGHBC = NBCNode
        ALLOCATE (LayerBC(indxLayer)%ConstrainedGHBC(NBCNode))
        CALL BCList(indxLayer)%Reset()
        DO indx=1,NBCNode
            pCurrentData => BCList(indxLayer)%GetCurrentValue()
            SELECT TYPE (pCurrentData)
               TYPE IS (ConstrainedGHBCType)
                  LayerBC(indxLayer)%ConstrainedGHBC(indx) = pCurrentData
            END SELECT
            CALL BCList(indxLayer)%Next()
        END DO
        
        !Order nodes
        DEALLOCATE (iNodes , iRanks , TempCGHBCArray , STAT=ErrorCode)
        ALLOCATE (iNodes(NBCNode) , iRanks(NBCNode) , TempCGHBCArray(NBCNode))
        TempCGHBCArray                     = LayerBC(indxLayer)%ConstrainedGHBC
        iNodes                             = LayerBC(indxLayer)%ConstrainedGHBC%iNode
        iRanks                             = OrderBCNodes(iNodes)
        LayerBC(indxLayer)%ConstrainedGHBC = TempCGHBCArray(iRanks)
        
        !Delete linked list
        CALL BCList(indxLayer)%Delete()
    END DO
    
    !Clear memory
    DEALLOCATE (Indices , iNodes , iRanks , TempCGHBCArray , DummyArray , STAT=ErrorCode)
    
    !Close file
    CALL InFile%Kill()
            
  END SUBROUTINE LayerBC_InitConstrainedGeneralHeadBC

  

  
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
  ! --- KILL LAYER B.C. DATA
  ! -------------------------------------------------------------
  SUBROUTINE LayerBC_Kill(LayerBC)
    TYPE(LayerBCType) :: LayerBC(:)
    
    !Local variables
    INTEGER           :: ErrorCode,indxLayer
    TYPE(LayerBCType) :: Dummy
    
    DO indxLayer=1,SIZE(LayerBC)
        !Deallocate arrays
        DEALLOCATE (LayerBC(indxLayer)%SpecFlowBC      , &
                    LayerBC(indxLayer)%SpecHeadBC      , &
                    LayerBC(indxLayer)%GHBC            , &
                    LayerBC(indxLayer)%ConstrainedGHBC , &
                    STAT=ErrorCode                     )
    
        !Set layer b.c. object to defaults
        LayerBC(indxLayer) = Dummy
    END DO
    
  END SUBROUTINE LayerBC_Kill
  
  
  
  
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
  ! --- GET A LIST OF COLUMN NUMBERS IN TIME SERIES B.C. DATA FILE FOR FLOW B.C.
  ! -------------------------------------------------------------
  SUBROUTINE LayerBC_GetTSFlowBCColumns(LayerBC,iTSColumns,iStat)
    TYPE(LayerBCType),INTENT(IN)    :: LayerBC(:)
    INTEGER,ALLOCATABLE,INTENT(OUT) :: iTSColumns(:)
    INTEGER,INTENT(OUT)             :: iStat
    
    !Local data type
    TYPE,EXTENDS(GenericLinkedListType) :: TSColumnListType
    END TYPE TSColumnListType
    
    !Local variables
    INTEGER                :: indxLayer,indx,NCol,iCol,ErrorCode
    INTEGER,ALLOCATABLE    :: iWorkArray(:)
    TYPE(TSColumnListType) :: TSColumnList
    CLASS(*),POINTER       :: pCurrentData
    
    !Initialize
    iStat = 0
    
    !Create a linked list that stores time series column numbers for flow b.c.
    DO indxLayer=1,SIZE(LayerBC)
        !Specified flow b.c.
        DO indx=1,LayerBC(indxLayer)%NSpecFlowBC
            iCol = LayerBC(indxLayer)%SpecFlowBC(indx)%iTSColumn
            IF (iCol .GT. 0) THEN
                CALL TSColumnList%AddNode(iCol,iStat)
                IF (iStat .EQ. -1) RETURN
            END IF
        END DO
        
        !Constrained general head b.c. (for maximum flow rate)
        DO indx=1,LayerBC(indxLayer)%NConstrainedGHBC
            iCol = LayerBC(indxLayer)%ConstrainedGHBC(indx)%iMaxFlowTSColumn
            IF (iCol .GT. 0) THEN
                CALL TSColumnList%AddNode(iCol,iStat)
                IF (iStat .EQ. -1) RETURN
            END IF
        END DO
    END DO
    
    !Number of column numbers in the list
    NCol = TSColumnList%GetNNodes()
    
    !Return if there are no time series flow boundary conditions
    IF (NCol .EQ. 0) RETURN
    
    !Extract column numbers into return array
    ALLOCATE (iWorkArray(NCol))
    CALL TSColumnList%Reset()
    DO indx=1,NCol
        pCurrentData => TSColumnList%GetCurrentValue()
        SELECT TYPE (pCurrentData)
           TYPE IS (INTEGER)
              iWorkArray(indx) = pCurrentData
        END SELECT
        CALL TSColumnList%Next()
    END DO
    
    !Store unique column numbers in return array
    CALL GetUniqueArrayComponents(iWorkArray,iTSColumns)
    CALL ShellSort(iTSColumns)
    
    !Clear memory
    CALL TSColumnList%Delete()
    DEALLOCATE (iWorkArray , STAT=ErrorCode)
    
  END SUBROUTINE LayerBC_GetTSFlowBCColumns
  
  
  ! -------------------------------------------------------------
  ! --- GET A LIST OF COLUMN NUMBERS IN TIME SERIES B.C. DATA FILE FOR HEAD B.C.
  ! -------------------------------------------------------------
  SUBROUTINE LayerBC_GetTSHeadBCColumns(LayerBC,iTSColumns,iStat)
    TYPE(LayerBCType),INTENT(IN)    :: LayerBC(:)
    INTEGER,ALLOCATABLE,INTENT(OUT) :: iTSColumns(:)
    INTEGER,INTENT(OUT)             :: iStat
    
    !Local data type
    TYPE,EXTENDS(GenericLinkedListType) :: TSColumnListType
    END TYPE TSColumnListType
    
    !Local variables
    INTEGER                :: indxLayer,indx,NCol,iCol,ErrorCode
    INTEGER,ALLOCATABLE    :: iWorkArray(:)
    TYPE(TSColumnListType) :: TSColumnList
    CLASS(*),POINTER       :: pCurrentData
    
    !Initialize
    iStat = 0
    
    !Create a linked list that stores time series column numbers for flow b.c.
    DO indxLayer=1,SIZE(LayerBC)
        !Specified head b.c.
        DO indx=1,LayerBC(indxLayer)%NSpecHeadBC
            iCol = LayerBC(indxLayer)%SpecheadBC(indx)%iTSColumn
            IF (iCol .GT. 0) THEN
                CALL TSColumnList%AddNode(iCol,iStat)
                IF (iStat .EQ. -1) RETURN
            END IF
        END DO
        
        !General head b.c. 
        DO indx=1,LayerBC(indxLayer)%NGHBC
            iCol = LayerBC(indxLayer)%GHBC(indx)%iTSColumn
            IF (iCol .GT. 0) THEN
                CALL TSColumnList%AddNode(iCol,iStat)
                IF (iStat .EQ. -1) RETURN
            END IF
        END DO

        !Constrained general head b.c. (for boundary head)
        DO indx=1,LayerBC(indxLayer)%NConstrainedGHBC
            iCol = LayerBC(indxLayer)%ConstrainedGHBC(indx)%iTSColumn
            IF (iCol .GT. 0) THEN
                CALL TSColumnList%AddNode(iCol,iStat)
                IF (iStat .EQ. -1) RETURN
            END IF
        END DO
    END DO
    
    !Number of column numbers in the list
    NCol = TSColumnList%GetNNodes()
    
    !Return if there are no time series flow boundary conditions
    IF (NCol .EQ. 0) RETURN
    
    !Extract column numbers into return array
    ALLOCATE (iWorkArray(NCol))
    CALL TSColumnList%Reset()
    DO indx=1,NCol
        pCurrentData => TSColumnList%GetCurrentValue()
        SELECT TYPE (pCurrentData)
           TYPE IS (INTEGER)
              iWorkArray(indx) = pCurrentData
        END SELECT
        CALL TSColumnList%Next()
    END DO
    
    !Store unique column numbers in return array
    CALL GetUniqueArrayComponents(iWorkArray,iTSColumns)
    CALL ShellSort(iTSColumns)
    
    !Clear memory
    CALL TSColumnList%Delete()
    DEALLOCATE (iWorkArray , STAT=ErrorCode)
    
  END SUBROUTINE LayerBC_GetTSHeadBCColumns
  
  
  ! -------------------------------------------------------------
  ! --- GET TOTAL NUMBER OF NODES WITH B.C.
  ! -------------------------------------------------------------
  PURE FUNCTION LayerBC_GetNNodesWithBC(LayerBC) RESULT(NNodes)
    TYPE(LayerBCType),INTENT(IN) :: LayerBC
    INTEGER                      :: NNodes
    
    NNodes = LayerBC%NSpecFlowBC + LayerBC%NSpecHeadBC + LayerBC%NGHBC + LayerBC%NConstrainedGHBC
    
  END FUNCTION LayerBC_GetNNodesWithBC
  
  
  ! -------------------------------------------------------------
  ! --- GET A LIST OF NODES WITH B.C.
  ! -------------------------------------------------------------
  PURE FUNCTION LayerBC_GetNodesWithBC(NNodes,LayerBC) RESULT(iNodes)
    INTEGER,INTENT(IN)           :: NNodes
    TYPE(LayerBCType),INTENT(IN) :: LayerBC
    INTEGER                      :: iNodes(NNodes)
    
    !Local variables
    INTEGER :: iCount_S,iCount_L
    
    !Nodes with specified flow b.c.
    iCount_L           = LayerBC%NSpecFlowBC 
    iNodes(1:iCount_L) = LayerBC%SpecFlowBC%iNode
    
    !Nodes with specified head b.c.
    iCount_S                  = iCount_L + 1
    iCount_L                  = iCount_L + LayerBC%NSpecHeadBC
    iNodes(iCount_S:iCount_L) = LayerBC%SpecHeadBC%iNode
    
    !Nodes with general head b.c.
    iCount_S                  = iCount_L + 1
    iCount_L                  = iCount_L + LayerBC%NGHBC
    iNodes(iCount_S:iCount_L) = LayerBC%GHBC%iNode
    
    !Nodes with constrained general head b.c.
    iCount_S                  = iCount_L + 1
    iCount_L                  = iCount_L + LayerBC%NConstrainedGHBC
    iNodes(iCount_S:iCount_L) = LayerBC%ConstrainedGHBC%iNode
    
  END FUNCTION LayerBC_GetNodesWithBC
  
  
  ! -------------------------------------------------------------
  ! --- GET A LIST OF B.C. TYPES
  ! -------------------------------------------------------------
  PURE FUNCTION GetBCTypes(NNodes,LayerBC) RESULT(iBCTypes)
    INTEGER,INTENT(IN)           :: NNodes
    TYPE(LayerBCType),INTENT(IN) :: LayerBC
    INTEGER                      :: iBCTypes(NNodes)
    
    !Local variables
    INTEGER :: iCount_S,iCount_L
    
    !Nodes with specified flow b.c.
    iCount_L             = LayerBC%NSpecFlowBC 
    iBCTypes(1:iCount_L) = f_iSpFlowBCID
    
    !Nodes with specified head b.c.
    iCount_S                    = iCount_L + 1
    iCount_L                    = iCount_L + LayerBC%NSpecHeadBC
    iBCTypes(iCount_S:iCount_L) = f_iSpHeadBCID
    
    !Nodes with general head b.c.
    iCount_S                    = iCount_L + 1
    iCount_L                    = iCount_L + LayerBC%NGHBC
    iBCTypes(iCount_S:iCount_L) = f_iGHBCID
    
    !Nodes with constrained general head b.c.
    iCount_S                    = iCount_L + 1
    iCount_L                    = iCount_L + LayerBC%NConstrainedGHBC
    iBCTypes(iCount_S:iCount_L) = f_iConstrainedGHBCID
    
  END FUNCTION GetBCTypes
  
  
  ! -------------------------------------------------------------
  ! --- GET THE NET FLOW AT A BOUNDARY NODE
  ! -------------------------------------------------------------
  PURE FUNCTION LayerBC_GetNetBCFlow(iNode,LayerBC) RESULT(Flow)
    INTEGER,INTENT(IN)           :: iNode
    TYPE(LayerBCType),INTENT(IN) :: LayerBC
    REAL(8)                      :: Flow

    !Specified flow
    Flow = LayerBC_GetNetBCFlowWithBCType(iNode,f_iSpFlowBCID,LayerBC)

    !Specified head
    Flow = Flow + LayerBC_GetNetBCFlowWithBCType(iNode,f_iSpHeadBCID,LayerBC)

    !General head
    Flow = Flow + LayerBC_GetNetBCFlowWithBCType(iNode,f_iGHBCID,LayerBC)

    !Constrained general head
    Flow = Flow + LayerBC_GetNetBCFlowWithBCType(iNode,f_iConstrainedGHBCID,LayerBC)

  END FUNCTION LayerBC_GetNetBCFlow
  
  
  ! -------------------------------------------------------------
  ! --- GET THE NET FLOW AT A BOUNDARY NODE WITH A SPECIFIC B.C. TYPE AT LAYER
  ! -------------------------------------------------------------
  PURE FUNCTION LayerBC_GetNetBCFlowWithBCType(iNode,iBCType,LayerBC) RESULT(Flow)
    INTEGER,INTENT(IN)           :: iNode,iBCType
    TYPE(LayerBCType),INTENT(IN) :: LayerBC
    REAL(8)                      :: Flow

    SELECT CASE (iBCType)
        CASE (f_iSpFlowBCID)
            Flow = SUM(LayerBC%SpecFlowBC%rFlow , MASK=LayerBC%SpecFlowBC%iNode .EQ. iNode)
        
        CASE (f_iSpHeadBCID)
            Flow = SUM(LayerBC%SpecHeadBC%rFlow , MASK=LayerBC%SpecHeadBC%iNode .EQ. iNode)
            
        CASE (f_iGHBCID)
            Flow = SUM(LayerBC%GHBC%rFlow , MASK=LayerBC%GHBC%iNode .EQ. iNode)
            
        CASE (f_iConstrainedGHBCID)
            Flow = SUM(LayerBC%ConstrainedGHBC%rFlow , MASK=LayerBC%ConstrainedGHBC%iNode .EQ. iNode)
            
        CASE DEFAULT
            Flow = 0.0
    END SELECT

  END FUNCTION LayerBC_GetNetBCFlowWithBCType

    
  
  

! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** SETTERS
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- SET A NODE WITH A B.C. TYPE
  ! -------------------------------------------------------------
  SUBROUTINE SetBCNode(LayerBC,iNode,iBCType,iStat,iTSCol,iTSColMaxBCFlow,rConductance,rConstrainingBCHead)
    CLASS(LayerBCType)          :: LayerBC
    INTEGER,INTENT(IN)          :: iNode,iBCType
    INTEGER,INTENT(OUT)         :: iStat
    INTEGER,OPTIONAL,INTENT(IN) :: iTSCol,iTSColMaxBCFlow
    REAL(8),OPTIONAL,INTENT(IN) :: rConductance,rConstrainingBCHead
    
    !Local variables
    INTEGER                               :: iLoc,iNBC,iTSColLocal,iTSColMaxBCFlowLocal
    TYPE(SpecifiedFlowBCType),ALLOCATABLE :: TempSpecFlowBC(:)
    TYPE(SpecifiedHeadBCType),ALLOCATABLE :: TempSpecHeadBC(:)
    TYPE(GHBCType),ALLOCATABLE            :: TempGHBC(:)
    TYPE(ConstrainedGHBCType),ALLOCATABLE :: TempConstrainedGHBC(:)
    INTEGER,ALLOCATABLE                   :: iNodes(:),iRanks(:)
    
    !Initialize
    iStat = 0
    
    !Optional arguments to local arguments
    IF (PRESENT(iTSCol)) THEN
        iTSColLocal = iTSCol
    ELSE
        iTSColLocal = 0
    END IF
    IF (PRESENT(iTSColMaxBCFlow)) THEN
        iTSColMaxBCFlowLocal = iTSColMaxBCFlow
    ELSE
        iTSColMaxBCFlowLocal = 0
    END IF
        
    
    !Add bc node
    SELECT CASE (iBCType)
        CASE (f_iSpFlowBCID)
            !No specified flow b.c. listed yet
            IF (LayerBC%NSpecFlowBC .EQ. 0) THEN
                ALLOCATE (LayerBC%SpecFlowBC(1))
                LayerBC%SpecFlowBC(1)%iNode     = iNode
                LayerBC%SpecFlowBC(1)%iTSColumn = iTSColLocal
                LayerBC%NSpecFlowBC             = 1
            !There are specified flow b.c.s
            ELSE
                iLoc = LocateInList(iNode,LayerBC%SpecFlowBC%iNode)
                !Add the b.c. as new b.c.
                IF (iLoc .EQ. 0) THEN
                    iNBC                = LayerBC%NSpecFlowBC
                    LayerBC%NSpecFlowBC = iNBC + 1
                    ALLOCATE (TempSpecFlowBC(iNBC+1))
                    TempSpecFlowBC(1:iNBC)           = LayerBC%SpecFlowBC
                    TempSpecFlowBC(iNBC+1)%iNode     = iNode
                    TempSpecFlowBC(iNBC+1)%iTSColumn = iTSColLocal
                    CALL MOVE_ALLOC(TempSpecFlowBC , LayerBC%SpecFlowBC)
                    !Order nodes
                    iNBC = iNBC + 1
                    ALLOCATE (iNodes(iNBC) , iRanks(iNBC) , TempSpecFlowBC(iNBC))
                    TempSpecFlowBC     = LayerBC%SpecFlowBC
                    iNodes             = LayerBC%SpecFlowBC%iNode
                    iRanks             = OrderBCNodes(iNodes)
                    LayerBC%SpecFlowBC = TempSpecFlowBC(iRanks)
                !Replace previous b.c.
                ELSE
                    LayerBC%SpecFlowBC(iLoc)%iTSColumn = iTSColLocal
                END IF
            END IF    

            
        CASE (f_iSpHeadBCID)
            !No specified head b.c. listed yet
            IF (LayerBC%NSpecHeadBC .EQ. 0) THEN
                ALLOCATE (LayerBC%SpecHeadBC(1))
                LayerBC%SpecHeadBC(1)%iNode     = iNode
                LayerBC%SpecHeadBC(1)%iTSColumn = iTSColLocal
                LayerBC%NSpecHeadBC             = 1
            !There are specified head b.c.s
            ELSE
                iLoc = LocateInList(iNode,LayerBC%SpecHeadBC%iNode)
                !Add the b.c. as new b.c.
                IF (iLoc .EQ. 0) THEN
                    iNBC                = LayerBC%NSpecHeadBC
                    LayerBC%NSpecHeadBC = iNBC + 1
                    ALLOCATE (TempSpecHeadBC(iNBC+1))
                    TempSpecHeadBC(1:iNBC)           = LayerBC%SpecHeadBC
                    TempSpecHeadBC(iNBC+1)%iNode     = iNode
                    TempSpecHeadBC(iNBC+1)%iTSColumn = iTSColLocal
                    CALL MOVE_ALLOC(TempSpecHeadBC , LayerBC%SpecHeadBC)
                    !Order nodes
                    iNBC = iNBC + 1
                    ALLOCATE (iNodes(iNBC) , iRanks(iNBC) , TempSpecHeadBC(iNBC))
                    TempSpecHeadBC     = LayerBC%SpecHeadBC
                    iNodes             = LayerBC%SpecHeadBC%iNode
                    iRanks             = OrderBCNodes(iNodes)
                    LayerBC%SpecHeadBC = TempSpecHeadBC(iRanks)
                !Replace previous b.c.
                ELSE
                    LayerBC%SpecHeadBC(iLoc)%iTSColumn = iTSColLocal
                END IF
            END IF 
            
            
        CASE (f_iGHBCID)
            !No general head b.c. listed yet
            IF (LayerBC%NGHBC .EQ. 0) THEN
                ALLOCATE (LayerBC%GHBC(1))
                LayerBC%GHBC(1)%iNode        = iNode
                LayerBC%GHBC(1)%iTSColumn    = iTSColLocal
                LayerBC%GHBC(1)%rConductance = rConductance
                LayerBC%NGHBC                = 1
            !There are general head b.c.s
            ELSE
                iLoc = LocateInList(iNode,LayerBC%GHBC%iNode)
                !Add the b.c. as new b.c.
                IF (iLoc .EQ. 0) THEN
                    iNBC          = LayerBC%NGHBC
                    LayerBC%NGHBC = iNBC + 1
                    ALLOCATE (TempGHBC(iNBC+1))
                    TempGHBC(1:iNBC)              = LayerBC%GHBC
                    TempGHBC(iNBC+1)%iNode        = iNode
                    TempGHBC(iNBC+1)%iTSColumn    = iTSColLocal
                    TempGHBC(iNBC+1)%rConductance = rConductance
                    CALL MOVE_ALLOC(TempGHBC , LayerBC%GHBC)
                    !Order nodes
                    iNBC = iNBC + 1
                    ALLOCATE (iNodes(iNBC) , iRanks(iNBC) , TempGHBC(iNBC))
                    TempGHBC     = LayerBC%GHBC
                    iNodes       = LayerBC%GHBC%iNode
                    iRanks       = OrderBCNodes(iNodes)
                    LayerBC%GHBC = TempGHBC(iRanks)
                !Replace previous b.c.
                ELSE
                    LayerBC%GHBC(iLoc)%rConductance = rConductance
                    LayerBC%GHBC(iLoc)%iTSColumn    = iTSColLocal
                END IF
            END IF      
            
            
        CASE (f_iConstrainedGHBCID)
            !No constrained general head b.c. listed yet
            IF (LayerBC%NConstrainedGHBC .EQ. 0) THEN
                ALLOCATE (LayerBC%ConstrainedGHBC(1))
                LayerBC%ConstrainedGHBC(1)%iNode               = iNode
                LayerBC%ConstrainedGHBC(1)%iTSColumn           = iTSColLocal
                LayerBC%ConstrainedGHBC(1)%rConstrainingBCHead = rConstrainingBCHead
                LayerBC%ConstrainedGHBC(1)%rConductance        = rConductance
                LayerBC%ConstrainedGHBC(1)%iMaxFlowTSColumn    = iTSColMaxBCFlowLocal
                LayerBC%NConstrainedGHBC                       = 1
            !There are general head b.c.s
            ELSE
                iLoc = LocateInList(iNode,LayerBC%ConstrainedGHBC%iNode)
                !Add the b.c. as new b.c.
                IF (iLoc .EQ. 0) THEN
                    iNBC                     = LayerBC%NConstrainedGHBC
                    LayerBC%NConstrainedGHBC = iNBC + 1
                    ALLOCATE (TempConstrainedGHBC(iNBC+1))
                    TempConstrainedGHBC(1:iNBC)                     = LayerBC%ConstrainedGHBC
                    TempConstrainedGHBC(iNBC+1)%iNode               = iNode
                    TempConstrainedGHBC(iNBC+1)%iTSColumn           = iTSColLocal
                    TempConstrainedGHBC(iNBC+1)%rConstrainingBCHead = rConstrainingBCHead
                    TempConstrainedGHBC(iNBC+1)%rConductance        = rConductance
                    TempConstrainedGHBC(iNBC+1)%iMaxFlowTSColumn    = iTSColMaxBCFlowLocal
                    CALL MOVE_ALLOC(TempConstrainedGHBC , LayerBC%ConstrainedGHBC)
                    !Order nodes
                    iNBC = iNBC + 1
                    ALLOCATE (iNodes(iNBC) , iRanks(iNBC) , TempConstrainedGHBC(iNBC))
                    TempGHBC                = LayerBC%GHBC
                    iNodes                  = LayerBC%ConstrainedGHBC%iNode
                    iRanks                  = OrderBCNodes(iNodes)
                    LayerBC%ConstrainedGHBC = TempConstrainedGHBC(iRanks)
                !Replace previous b.c.
                ELSE
                    LayerBC%ConstrainedGHBC(iLoc)%rConstrainingBCHead = rConstrainingBCHead
                    LayerBC%ConstrainedGHBC(iLoc)%rConductance        = rConductance
                    LayerBC%ConstrainedGHBC(iLoc)%iTSColumn           = iTSColLocal
                    LayerBC%ConstrainedGHBC(iLoc)%iMaxFlowTSColumn    = iTSColMaxBCFlowLocal
                END IF
            END IF      

    END SELECT
    
  END SUBROUTINE SetBCNode
    
  
  ! -------------------------------------------------------------
  ! --- SET BOUNDARY CONDITION AT A NODE 
  ! ---- (Assumes node is already listed as a b.c. node)
  ! -------------------------------------------------------------
  SUBROUTINE SetBC(LayerBC,iNode,iBCType,iStat,rFlow,rHead,rMaxBCFlow)
    CLASS(LayerBCType)          :: LayerBC
    INTEGER,INTENT(IN)          :: iNode,iBCType
    INTEGER,INTENT(OUT)         :: iStat
    REAL(8),OPTIONAL,INTENT(IN) :: rFlow,rHead,rMaxBCFlow
    
    !Local variables
    INTEGER :: iLoc
    
    !Initialize
    iStat = 0
    
    !Set b.c.
    SELECT CASE (iBCType)
        CASE (f_iSpFlowBCID)
            iLoc                           = LocateInList(iNode,LayerBC%SpecFlowBC%iNode)
            LayerBC%SpecFlowBC(iLoc)%rFlow = rFlow

            
        CASE (f_iSpHeadBCID)
            iLoc                           = LocateInList(iNode,LayerBC%SpecHeadBC%iNode)
            LayerBC%SpecHeadBC(iLoc)%rHead = rHead
            
            
        CASE (f_iGHBCID)
            iLoc                     = LocateInList(iNode,LayerBC%GHBC%iNode)
            LayerBC%GHBC(iLoc)%rHead = rHead
            
            
        CASE (f_iConstrainedGHBCID)
            iLoc                                     = LocateInList(iNode,LayerBC%ConstrainedGHBC%iNode)
            LayerBC%ConstrainedGHBC(iLoc)%rHead      = rHead
            LayerBC%ConstrainedGHBC(iLoc)%rMaxBCFlow = rMaxBCFlow

    END SELECT
    
  END SUBROUTINE SetBC
    
  
  ! -------------------------------------------------------------
  ! --- SET THE TIME SERIES BOUNDARY CONDITIONS 
  ! -------------------------------------------------------------
  SUBROUTINE LayerBC_SetTSBoundaryConditions(NodeIDs,BottomElev,rBCValues,Heads,LayerBC)
    INTEGER,INTENT(IN) :: NodeIDs(:)
    REAL(8),INTENT(IN) :: BottomElev(:,:),rBCValues(:)
    REAL(8)            :: Heads(:,:)
    TYPE(LayerBCType)  :: LayerBC(:)
    
    !Local variables
    CHARACTER(LEN=ModNameLen+31) :: ThisProcedure = ModName // 'LayerBC_SetTSBoundaryConditions'
    INTEGER                      :: indxLayer,iTSColumn,indx,iNode,iMaxFlowTSColumn
    REAL(8)                      :: rHead
    
    DO indxLayer=1,SIZE(LayerBC)
        ASSOCIATE (pLayerBC => LayerBC(indxLayer))           
            !Specified flow b.c.
            IF (pLayerBC%NSpecFlowBC .GT. 0) THEN
                DO indx=1,pLayerBC%NSpecFlowBC
                    iTSColumn = pLayerBC%SpecFlowBC(indx)%iTSColumn
                    IF (iTSColumn .GT. 0) THEN
                        pLayerBC%SpecFlowBC(indx)%rFlowRead = rBCValues(iTSColumn)
                        pLayerBC%SpecFlowBC(indx)%rFlow     = rBCValues(iTSColumn)
                    END IF
                END DO
            END IF
            
            !Specified head b.c.
            IF (pLayerBC%NSpecHeadBC .GT. 0) THEN
                DO indx=1,pLayerBC%NSpecHeadBC
                    iTSColumn = pLayerBC%SpecHeadBC(indx)%iTSColumn
                    IF (iTSColumn .GT. 0) THEN
                        !Make sure specified head is not below the bottom of aquifer
                        iNode = pLayerBC%SpecHeadBC(indx)%iNode
                        IF (rBCValues(iTSColumn) .LT. BottomElev(iNode,indxLayer)) THEN
                            MessageArray(1) = 'Specified head at boundary node '  // TRIM(IntToText(NodeIDs(iNode))) // ' in aquifer layer '//TRIM(IntToText(indxLayer))
                            MessageArray(2) = 'is below the bottom elevation of the layer.'
                            WRITE (MessageArray(3),'(A,F9.3)') 'Specified head            =',rBCValues(iTSColumn)
                            WRITE (MessageArray(4),'(A,F9.3)') 'Bottom elevation of layer =',BottomElev(iNode,indxLayer)
                            MessageArray(5) = 'Assigning the bottom elevation of the layer as specified head b.c.'
                            CALL LogMessage(MessageArray(1:5),f_iWarn,ThisProcedure)
                            rHead = BottomElev(iNode,indxLayer)
                        ELSE
                            rHead = rBCValues(iTSColumn)
                        END IF
                        pLayerBC%SpecHeadBC(indx)%rHead = rHead
                        Heads(iNode,indxLayer)          = rHead
                    END IF
                END DO
            END IF

            !General head b.c.
            IF (pLayerBC%NGHBC .GT. 0) THEN
                DO indx=1,pLayerBC%NGHBC
                    iTSColumn = pLayerBC%GHBC(indx)%iTSColumn
                    IF (iTSColumn .GT. 0) pLayerBC%GHBC(indx)%rHead = rBCValues(iTSColumn)
                END DO
            END IF
            
            !Constrained general head b.c.
            IF (pLayerBC%NConstrainedGHBC .GT. 0) THEN
                DO indx=1,pLayerBC%NConstrainedGHBC
                    iTSColumn = pLayerBC%ConstrainedGHBC(indx)%iTSColumn
                    IF (iTSColumn .GT. 0) pLayerBC%ConstrainedGHBC(indx)%rHead = rBCValues(iTSColumn)
                    iMaxFlowTSColumn = pLayerBC%ConstrainedGHBC(indx)%iMaxFlowTSColumn
                    IF (iMaxFlowTSColumn .GT. 0) pLayerBC%ConstrainedGHBC(indx)%rMaxBCFlow = rBCValues(iMaxFlowTSColumn)
                END DO
            END IF

        END ASSOCIATE
    END DO
       
  END SUBROUTINE LayerBC_SetTSBoundaryConditions
  

    
  
! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** PREDICATES
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- CHECK IF A NODE AT A LAYER IS BOUNDARY CONDITION NODE WITH SPECIFIED B.C. TYPE(S)
  ! -------------------------------------------------------------
  PURE FUNCTION LayerBC_IsBCNodeWithBCTypes(iNode,iBCTypes,LayerBC) RESULT(IsBCNode)
    INTEGER,INTENT(IN)           :: iNode,iBCTypes(:)
    TYPE(LayerBCType),INTENT(IN) :: LayerBC
    LOGICAL                      :: IsBCNode
    
    !Local variables
    INTEGER :: indx,iBCType
    
    !Initialize
    IsBCNode = .FALSE.
    
    !Iterate over b.c. types
    DO indx=1,SIZE(iBCTypes)
        iBCType = iBCTypes(indx)
        SELECT CASE (iBCType)
            CASE (f_iSpFlowBCID)
                IF (LayerBC%NSpecFlowBC .GT. 0) THEN
                    IF (LocateInList(iNode,LayerBC%SpecFlowBC%iNode) .GT. 0) THEN
                        IsBCNode = .TRUE.
                        RETURN
                    END IF
                END IF
                
            CASE (f_iSpHeadBCID) 
                IF (LayerBC%NSpecHeadBC .GT. 0) THEN
                    IF (LocateInList(iNode,LayerBC%SpecHeadBC%iNode) .GT. 0) THEN
                        IsBCNode = .TRUE.
                        RETURN
                    END IF
                END IF
        
            CASE (f_iGHBCID) 
                IF (LayerBC%NGHBC .GT. 0) THEN
                    IF (LocateInList(iNode,LayerBC%GHBC%iNode) .GT. 0) THEN
                        IsBCNode = .TRUE.
                        RETURN
                    END IF
                END IF
            
            CASE (f_iConstrainedGHBCID)
                IF (LayerBC%NConstrainedGHBC .GT. 0) THEN
                    IF (LocateInList(iNode,LayerBC%ConstrainedGHBC%iNode) .GT. 0) THEN
                        IsBCNode = .TRUE.
                        RETURN
                    END IF
                END IF
                    
        END SELECT
    END DO
    
  END FUNCTION LayerBC_IsBCNodeWithBCTypes
  
    
  ! -------------------------------------------------------------
  ! --- CHECK IF A NODE AT A LAYER IS BOUNDARY NODE 
  ! -------------------------------------------------------------
  FUNCTION LayerBC_IsBCNode(iNode,LayerBC) RESULT(IsBCNode)
    INTEGER,INTENT(IN)           :: iNode
    TYPE(LayerBCType),INTENT(IN) :: LayerBC
    LOGICAL                      :: IsBCNode
    
    !Local variables
    INTEGER             :: NNodes
    INTEGER,ALLOCATABLE :: iBCNodes(:)
    
    !Initialize
    NNodes = LayerBC_GetNNodesWithBC(LayerBC)
    
    !Return if there are no nodes with b.c.
    IF (NNodes .EQ. 0) THEN
        IsBCNode = .FALSE.
        RETURN
    END IF
    
    !Get a list of b.c. nodes 
    ALLOCATE (iBCNodes(NNodes))
    iBCNodes = LayerBC_GetNodesWithBC(NNodes,LayerBC)
    
    !Check if iNode is listed in iBCNodes
    IF (LocateInList(iNode,iBCNodes) .EQ. 0) THEN
        IsBCNode = .FALSE.
    ELSE
        IsBCNode = .TRUE.
    END IF
  
  END FUNCTION LayerBC_IsBCNode
  
  
  
  
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
  ! --- CONVERT TIME UNIT OF SPECIFIED FLOW B.C. 
  ! -------------------------------------------------------------
  SUBROUTINE LayerBC_ConvertSpecifiedFlowTimeUnit(Factor,LayerBC)
    REAL(8),INTENT(IN) :: Factor
    TYPE(LayerBCType)  :: LayerBC(:)
    
    !Local variables
    INTEGER :: indxLayer
    
    !Convert specified flow b.c. time unit
    DO indxLayer=1,SIZE(LayerBC)
        LayerBC(indxLayer)%SpecFlowBC%rFlow = LayerBC(indxLayer)%SpecFlowBC%rFlow * Factor 
    END DO
    
  END SUBROUTINE LayerBC_ConvertSpecifiedFlowTimeUnit
  
  
  ! -------------------------------------------------------------
  ! --- CONVERT TIME UNIT OF GENERAL HEAD B.C. 
  ! -------------------------------------------------------------
  SUBROUTINE LayerBC_ConvertGHBCTimeUnit(Factor,LayerBC)
    REAL(8),INTENT(IN) :: Factor
    TYPE(LayerBCType)  :: LayerBC(:)
    
    !Local variables
    INTEGER :: indxLayer
    
    !Convert specified flow b.c. time unit
    DO indxLayer=1,SIZE(LayerBC)
        LayerBC(indxLayer)%GHBC%rConductance = LayerBC(indxLayer)%GHBC%rConductance * Factor 
    END DO
    
  END SUBROUTINE LayerBC_ConvertGHBCTimeUnit
  
  
  ! -------------------------------------------------------------
  ! --- CONVERT TIME UNIT OF CONSTRAINED GENERAL HEAD B.C. 
  ! -------------------------------------------------------------
  SUBROUTINE LayerBC_ConvertConstrainedGHBCTimeUnit(Factor,LayerBC)
    REAL(8),INTENT(IN) :: Factor
    TYPE(LayerBCType)  :: LayerBC(:)
    
    !Local variables
    INTEGER :: indxLayer
    
    !Convert specified flow b.c. time unit
    DO indxLayer=1,SIZE(LayerBC)
        LayerBC(indxLayer)%ConstrainedGHBC%rConductance = LayerBC(indxLayer)%ConstrainedGHBC%rConductance * Factor 
        LayerBC(indxLayer)%ConstrainedGHBC%rMaxBCFlow   = LayerBC(indxLayer)%ConstrainedGHBC%rMaxBCFlow * Factor 
    END DO
    
  END SUBROUTINE LayerBC_ConvertConstrainedGHBCTimeUnit
  
  
  ! -------------------------------------------------------------
  ! --- ORDER B.C. NODES
  ! -------------------------------------------------------------
  FUNCTION OrderBCNodes(iNodes) RESULT(iRanks)
    INTEGER :: iNodes(:)
    INTEGER :: iRanks(SIZE(iNodes))
    
    !Local variables
    INTEGER :: i
    
    !Initialize
    iRanks = [(i,i=1,SIZE(iNodes))]
    
    !Order nodes by keeping track of their ranks
    CALL ShellSort(iNodes,iRanks)
    
  END FUNCTION OrderBCNodes
  
  
  ! -------------------------------------------------------------
  ! --- MAKE SURE ONLY ONE B.C. IS SPECIFIED FOR A NODE
  ! -------------------------------------------------------------
  SUBROUTINE LayerBC_CheckConsistency(LayerBC,NodeIDs,iStat)
    TYPE(LayerBCType),INTENT(IN) :: LayerBC(:)
    INTEGER,INTENT(IN)           :: NodeIDs(:)
    INTEGER,INTENT(OUT)          :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+24) :: ThisProcedure = ModName // 'LayerBC_CheckConsistency'
    CHARACTER                    :: cMessage*200
    INTEGER                      :: indxLayer,NBCNode,ErrorCode,iNode,iNode1,indxNode,indxNode1,indx,indx1,iColFlow
    INTEGER,ALLOCATABLE          :: iNodes(:),iBCTypes(:),iTSFlowBCColumns(:),iTSHeadBCColumns(:)
    
    !Initialize
    iStat = 0
    
    DO indxLayer=1,SIZE(LayerBC)
        !Number of nodes with b.c. for the layer
        NBCNode = LayerBC_GetNNodesWithBC(LayerBC(indxLayer))
        
        !List of nodes with b.c.
        DEALLOCATE (iNodes , iBCTypes , STAT=ErrorCode)
        ALLOCATE (iNodes(NBCNode) , iBCTypes(NBCNode))
        iNodes   = LayerBC_GetNodesWithBC(NBCNode,LayerBC(indxLayer))
        iBCTypes = GetBCTypes(NBCNode,LayerBC(indxLayer))
        
        !Check if more than one b.c. is specified for a given node
        DO indxNode=1,NBCNode
           iNode = iNodes(indxNode)
           DO indxNode1=indxNode+1,NBCNode
               iNode1 = iNodes(indxNode1)
               IF (iNode .EQ. iNode1) THEN
                   cMessage = 'Multiple boundary conditions are specified for node '//TRIM(IntToText(NodeIDs(iNode)))//' at layer '//TRIM(IntToText(indxLayer))//' ('
                   cMessage = TRIM(cMessage) // TRIM(f_cBCDescriptor(iBCTypes(indxNode))) // ' ,'
                   cMessage = TRIM(cMessage) //' ' // TRIM(f_cBCDescriptor(iBCTypes(indxNode1)))// ').'
                   CALL SetLastMessage(TRIM(cMessage),f_iFatal,ThisProcedure)
                   iStat = -1
                   RETURN
                END IF
           END DO
        END DO
    END DO
    
    !Make sure same time series b.c. data column is not being used as flow and head b.c. at the same time
    CALL LayerBC_GetTSFlowBCColumns(LayerBC,iTSFlowBCColumns,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL LayerBC_GetTSHeadBCColumns(LayerBC,iTSHeadBCColumns,iStat)  ;  IF (iStat .EQ. -1) RETURN
    DO indx=1,SIZE(iTSFlowBCColumns)
        iColFlow = iTSFlowBCColumns(indx)
        indx1    = LocateInList(iColFlow,iTSHeadBCColumns)
        IF (indx1 .NE. 0) THEN
            cMessage = 'Time series boundary condition column ' // TRIM(IntToText(iColFlow)) // ' is being used as both flow and head boundary conditions!'
            CALL SetLastMessage(TRIM(cMessage),f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
    END DO
    
    !Clear memory
    DEALLOCATE(iNodes , iBCTypes , iTSFlowBCColumns , iTSHeadBCColumns , STAT=ErrorCode)
    
  END SUBROUTINE LayerBC_CheckConsistency


  ! -------------------------------------------------------------
  ! --- SIMULATE BOUNDARY CONDITIONS
  ! -------------------------------------------------------------
  SUBROUTINE LayerBC_Simulate(LayerBC,NNodes,rHeads,rBottomElevs,rStorage,rdStorage,Matrix)
    TYPE(LayerBCType)  :: LayerBC(:)
    INTEGER,INTENT(IN) :: NNodes
    REAL(8),INTENT(IN) :: rHeads(:,:),rBottomElevs(:,:),rStorage(:,:),rdStorage(:,:)
    TYPE(MatrixType)   :: Matrix
    
    !Local variables
    INTEGER           :: indxLayer,indxBC,iBase,iGWNode,iNodeIDs(1)
    REAL(8)           :: rUpdateValues(1)
    INTEGER,PARAMETER :: iCompIDs(1) = [f_iGWComp]
    
    !Evaluate specified flows first
    CALL SpecifiedFlowBC_Simulate(rStorage,rdStorage,LayerBC)
    
    !Evaluate general head b.c. second
    CALL GHBC_Simulate(rHeads,rBottomElevs,LayerBC)
    
    !Evaluate constrained general head b.c. third
    CALL ConstrainedGHBC_Simulate(rHeads,rBottomElevs,LayerBC)
    
    !Calculate the effects of b.c.s on RHS and COEFF arrays last
    DO indxLayer=1,SIZE(LayerBC)
        iBase = (indxLayer-1) * NNodes
        ASSOCIATE (pLayerBC => LayerBC(indxLayer))
            !Specified flow b.c.
            DO indxBC=1,pLayerBC%NSpecFlowBC
                iNodeIDs(1)      = iBase + pLayerBC%SpecFlowBC(indxBC)%iNode
                rUpdateValues(1) = -pLayerBC%SpecFlowBC(indxBC)%rFlow
                CALL Matrix%UpdateRHS(f_iGWComp,iNodeIDs(1),rUpdateValues)
                IF (pLayerBC%SpecFlowBC(indxBC)%rGradient .NE. 0.0) THEN
                    rUpdateValues(1) = -pLayerBC%SpecFlowBC(indxBC)%rGradient
                    CALL Matrix%UpdateCOEFF(f_iGWComp,iNodeIDs(1),1,iCompIDs,iNodeIDs,rUpdateValues)
                END IF
            END DO
        
            !General head b.c.
            DO indxBC=1,pLayerBC%NGHBC
                iNodeIDs(1)      = iBase + pLayerBC%GHBC(indxBC)%iNode
                rUpdateValues(1) = -pLayerBC%GHBC(indxBC)%rFlow
                CALL Matrix%UpdateRHS(f_iGWComp,iNodeIDs(1),rUpdateValues)
                rUpdateValues(1) = -pLayerBC%GHBC(indxBC)%rGradient
                CALL Matrix%UpdateCOEFF(f_iGWComp,iNodeIDs(1),1,iCompIDs,iNodeIDs,rUpdateValues)
            END DO
        
            !Constrained general head b.c.
            DO indxBC=1,pLayerBC%NConstrainedGHBC
                iNodeIDs(1)      = iBase + pLayerBC%ConstrainedGHBC(indxBC)%iNode
                rUpdateValues(1) = -pLayerBC%ConstrainedGHBC(indxBC)%rFlow
                CALL Matrix%UpdateRHS(f_iGWComp,iNodeIDs(1),rUpdateValues)
                rUpdateValues(1) = -pLayerBC%ConstrainedGHBC(indxBC)%rGradient
                CALL Matrix%UpdateCOEFF(f_iGWComp,iNodeIDs(1),1,iCompIDs,iNodeIDs,rUpdateValues)
            END DO
        
            !Specified head b.c. (must be the last to be considered in the entire model simulation)
            DO indxBC=1,pLayerBC%NSpecHeadBC
                iGWNode                           = iBase + pLayerBC%SpecHeadBC(indxBC)%iNode
                pLayerBC%SpecHeadBC(indxBC)%rFlow = Matrix%GetRHS(f_iGWComp,iGWNode)
                CALL Matrix%SetRHS(f_iGWComp,iGWNode,0d0)
                CALL Matrix%SetCOEFF(f_iGWComp,iGWNode,0d0)
                CALL Matrix%SetCOEFF(f_iGWComp,iGWNode,f_iGWComp,iGWNode,1d0)
            END DO
        END ASSOCIATE 
    END DO
    
  END SUBROUTINE LayerBC_Simulate
  
  
  ! -------------------------------------------------------------
  ! --- SIMULATE SPECIFIED FLOW B.C.
  ! -------------------------------------------------------------
  SUBROUTINE SpecifiedFlowBC_Simulate(rStorage,rdStorage,LayerBC)
    REAL(8),INTENT(IN) :: rStorage(:,:),rdStorage(:,:)
    TYPE(LayerBCType)  :: LayerBC(:)

    !Local variables
    INTEGER :: indxLayer,iNode,indxBC
    REAL(8) :: rFlow,rStor,rStoragePos,rDiffQ,rDiffQSQRT,rStorSQRT

    DO indxLayer=1,SIZE(LayerBC)
        DO indxBC=1,LayerBC(indxLayer)%NSpecFlowBC
            rFlow = LayerBC(indxLayer)%SpecFlowBC(indxBC)%rFlowRead
            !Outflow - scale down outflow for drying nodes
            IF (rFlow .LT. 0.0) THEN
                iNode                                           = LayerBC(indxLayer)%SpecFlowBC(indxBC)%iNode
                rStor                                           = rStorage(iNode,indxLayer)
                rStoragePos                                     = MAX(rStor, 0.0)
                rDiffQ                                          = -rFlow - rStoragePos
                rDiffQSQRT                                      = SQRT(rDiffQ*rDiffQ + f_rSmoothMaxP)
                rStorSQRT                                       = SQRT(rStor*rStor + f_rSmoothMaxP)  
                LayerBC(indxLayer)%SpecFlowBC(indxBC)%rFlow     = rFlow + MAX(rDiffQ , 0.0) 
                LayerBC(indxLayer)%SpecFlowBC(indxBC)%rGradient = -0.25d0 * (1d0+rDiffQ/rDiffQSQRT) * (1d0+rStor/rStorSQRT) * rdStorage(iNode,indxLayer) 
                
            !Inflow - no need to scale down flow
            ELSE
                LayerBC(indxLayer)%SpecFlowBC(indxBC)%rFlow     = rFlow
                LayerBC(indxLayer)%SpecFlowBC(indxBC)%rGradient = 0.0
            END IF
        END DO
    END DO

  END SUBROUTINE SpecifiedFlowBC_Simulate
  

  ! -------------------------------------------------------------
  ! --- SIMULATE GENERAL HEAD B.C.
  ! -------------------------------------------------------------
  SUBROUTINE GHBC_Simulate(rHeads,rBottomElevs,LayerBC)
    REAL(8),INTENT(IN) :: rHeads(:,:),rBottomElevs(:,:)
    TYPE(LayerBCType)  :: LayerBC(:)

    !Local variables
    INTEGER :: indxLayer,iNode,indxBC
    REAL(8) :: rFlow,rGWHead,rSaturatedThick,rHeadDiff,rExp,rExpPlusOne,rFactor,rdFactor,rConductance

    DO indxLayer=1,SIZE(LayerBC)
        DO indxBC=1,LayerBC(indxLayer)%NGHBC
            rConductance = LayerBC(indxLayer)%GHBC(indxBC)%rConductance
            iNode        = LayerBC(indxLayer)%GHBC(indxBC)%iNode
            rGWHead      = rHeads(iNode,indxLayer)
            rHeadDiff    = LayerBC(indxLayer)%GHBC(indxBC)%rHead - rGWHead
            rFlow        = rConductance * rHeadDiff
            !Outflow - scale down outflow for drying nodes
            IF (rFlow .LT. 0.0) THEN
                rSaturatedThick = rGWHead - rBottomElevs(iNode,indxLayer)
                rExp            = FEXP(-f_rSmoothStepP * rSaturatedThick)
                IF (rExp .LT. 1d100) THEN
                    rExpPlusOne                               = 1d0 + rExp
                    rFactor                                   = 1d0 / rExpPlusOne
                    rdFactor                                  = f_rSmoothStepP * rExp / (rExpPlusOne*rExpPlusOne)
                    LayerBC(indxLayer)%GHBC(indxBC)%rFlow     = rFactor * rFlow
                    LayerBC(indxLayer)%GHBC(indxBC)%rGradient = rdFactor * rConductance * rHeadDiff - rFactor * rConductance
                ELSE
                    LayerBC(indxLayer)%GHBC(indxBC)%rFlow     = 0.0
                    LayerBC(indxLayer)%GHBC(indxBC)%rGradient = 0.0
                END IF
                
            !Inflow - no need to scale down flow
            ELSE
                LayerBC(indxLayer)%GHBC(indxBC)%rFlow     = rFlow
                LayerBC(indxLayer)%GHBC(indxBC)%rGradient = -rConductance
            END IF
        END DO
    END DO

  END SUBROUTINE GHBC_Simulate
  
  
  ! -------------------------------------------------------------
  ! --- SIMULATE COSNTRAINED GENERAL HEAD B.C.
  ! -------------------------------------------------------------
  SUBROUTINE ConstrainedGHBC_Simulate(rHeads,rBottomElevs,LayerBC)
    REAL(8),INTENT(IN) :: rHeads(:,:),rBottomElevs(:,:)
    TYPE(LayerBCType)  :: LayerBC(:)

    !Local variables
    INTEGER  :: indxLayer,indxBC,iNode
    REAL(8)  :: rGWHead,rBCHead,rLimitHead,rMaxBCFlow,rHeadDiff,rConductance,rSaturatedThick,  &
                rExp,rExpPlusOne,rFactor,rFlow,rdFactor,rFlowDiff,rHDiff,rdFlow  
 
    DO indxLayer=1,SIZE(LayerBC)
        IF (LayerBC(indxLayer)%NConstrainedGHBC .EQ. 0) CYCLE
        ASSOCIATE (pLayerBC => LayerBC(indxLayer))
            DO indxBC=1,pLayerBC%NConstrainedGHBC
                iNode        = pLayerBC%ConstrainedGHBC(indxBC)%iNode
                rGWHead      = rHeads(iNode,indxLayer)
                rBCHead      = pLayerBC%ConstrainedGHBC(indxBC)%rHead
                rLimitHead   = pLayerBC%ConstrainedGHBC(indxBC)%rConstrainingBChead
                rMaxBCFlow   = pLayerBC%ConstrainedGHBC(indxBC)%rMaxBCFlow
                rConductance = pLayerBC%ConstrainedGHBC(indxBC)%rConductance
                
                rHeadDiff = MAX(rBCHead , rLimitHead) - MAX(rGWHead , rLimitHead)
                rFlow     = rConductance * rHeadDiff
                !Outflow - limit the b.c. flow if drying occurs
                IF (rFlow .LT. 0.0) THEN
                    rSaturatedThick = rGWHead - rBottomElevs(iNode,indxLayer) 
                    rExp            = FEXP(-f_rSmoothStepP * rSaturatedThick)
                    IF (rExp .LT. 1d100) THEN
                        rExpPlusOne                                = 1d0 + rExp
                        rFactor                                    = 1d0 / rExpPlusOne
                        rdFactor                                   = f_rSmoothStepP * rExp / (rExpPlusOne*rExpPlusOne) 
                        rFlow                                      = rFactor * rFlow
                        rFlowDiff                                  = rMaxBCFlow - rFlow
                        rHDiff                                     = rGWHead - rLimitHead
                        rdFlow                                     = rdFactor * rConductance * rHeadDiff    &
                                                                   - 0.5d0 * rFactor * rConductance * (1d0 + (rHDiff/SQRT(rHDiff*rHDiff+f_rSmoothMaxP))) 
                        pLayerBC%ConstrainedGHBC(indxBC)%rFlow     = MIN(rMaxBCFlow , rFlow)
                        pLayerBC%ConstrainedGHBC(indxBC)%rGradient = 0.5d0 * (1d0 + rFlowDiff/SQRT(rFlowDiff*rFlowDiff+f_rSmoothMaxP)) * rdFlow
                    ELSE
                        pLayerBC%ConstrainedGHBC(indxBC)%rFlow     = 0.0
                        pLayerBC%ConstrainedGHBC(indxBC)%rGradient = 0.0
                    END IF
                !Inflow - no need to limit the b.c. flow for drying node
                ELSE
                    rFlowDiff                                  = rMaxBCFlow - rFlow
                    rHDiff                                     = rGWHead - rLimitHead
                    rdFlow                                     = -0.5d0 * rConductance * (1d0 + (rHDiff/SQRT(rHDiff*rHDiff+f_rSmoothMaxP))) 
                    pLayerBC%ConstrainedGHBC(indxBC)%rFlow     = MIN(rMaxBCFlow , rFlow)
                    pLayerBC%ConstrainedGHBC(indxBC)%rGradient = 0.5d0 * (1d0 + rFlowDiff/SQRT(rFlowDiff*rFlowDiff+f_rSmoothMaxP)) * rdFlow
                END IF
            END DO
        END ASSOCIATE
    END DO

  END SUBROUTINE ConstrainedGHBC_Simulate


  ! -------------------------------------------------------------
  ! --- REMOVE BOUNDARY CONDITION AT A NODE 
  ! -------------------------------------------------------------
  SUBROUTINE RemoveBC(LayerBC,iNode,iStat)
    CLASS(LayerBCType)          :: LayerBC
    INTEGER,INTENT(IN)          :: iNode
    INTEGER,INTENT(OUT)         :: iStat
    
    !Local variables
    INTEGER                               :: iLoc,iNBC 
    TYPE(SpecifiedFlowBCType),ALLOCATABLE :: TempSpecFlowBC(:)
    TYPE(SpecifiedHeadBCType),ALLOCATABLE :: TempSpecHeadBC(:)
    TYPE(GHBCType),ALLOCATABLE            :: TempGHBC(:)
    TYPE(ConstrainedGHBCType),ALLOCATABLE :: TempConstrainedGHBC(:)
    
    !Initialize
    iStat = 0        
    
    !Specified flow b.c.
    iNBC = LayerBC%NSpecFlowBC
    IF (iNBC .GT. 0) THEN
        iLoc = LocateInList(iNode,LayerBC%SpecFlowBC%iNode)
        IF (iLoc .GT. 0) THEN
            IF (iNBC .EQ. 1) THEN
                DEALLOCATE (LayerBC%SpecFlowBC)
            ELSE
                ALLOCATE (TempSpecFlowBC(iNBC-1))
                IF (iLoc .EQ. 1) THEN
                    TempSpecFlowBC = LayerBC%SpecFlowBC(2:)
                ELSEIF (iLoc .EQ. iNBC) THEN
                    TempSpecFlowBC = LayerBC%SpecFlowBC(:iNBC-1)
                ELSE
                    TempSpecFlowBC(1:iLoc-1) = LayerBC%SpecFlowBC(1:iLoc-1)
                    TempSpecFlowBC(iLoc:)    = LayerBC%SpecFlowBC(iLoc+1:)
                END IF
                CALL MOVE_ALLOC(TempSpecFlowBC,LayerBC%SpecFlowBC)
            END IF
            LayerBC%NSpecFlowBC = iNBC - 1
        END IF
    END IF
    
    !Specified head b.c.
    iNBC = LayerBC%NSpecHeadBC
    IF (iNBC .GT. 0) THEN
        iLoc = LocateInList(iNode,LayerBC%SpecHeadBC%iNode)
        IF (iLoc .GT. 0) THEN
            IF (iNBC .EQ. 1) THEN
                DEALLOCATE (LayerBC%SpecHeadBC)
            ELSE
                ALLOCATE (TempSpecHeadBC(iNBC-1))
                IF (iLoc .EQ. 1) THEN
                    TempSpecHeadBC = LayerBC%SpecHeadBC(2:)
                ELSEIF (iLoc .EQ. iNBC) THEN
                    TempSpecHeadBC = LayerBC%SpecHeadBC(:iNBC-1)
                ELSE
                    TempSpecHeadBC(1:iLoc-1) = LayerBC%SpecHeadBC(1:iLoc-1)
                    TempSpecHeadBC(iLoc:)    = LayerBC%SpecHeadBC(iLoc+1:)
                END IF
                CALL MOVE_ALLOC(TempSpecHeadBC,LayerBC%SpecHeadBC)
            END IF
            LayerBC%NSpecHeadBC = iNBC - 1
        END IF
    END IF 
    
    !General head b.c.
    iNBC = LayerBC%NGHBC
    IF (iNBC .GT. 0) THEN
        iLoc = LocateInList(iNode,LayerBC%GHBC%iNode)
        IF (iLoc .GT. 0) THEN
            IF (iNBC .EQ. 1) THEN
                DEALLOCATE (LayerBC%GHBC)
            ELSE
                ALLOCATE (TempGHBC(iNBC-1))
                IF (iLoc .EQ. 1) THEN
                    TempGHBC = LayerBC%GHBC(2:)
                ELSEIF (iLoc .EQ. iNBC) THEN
                    TempGHBC = LayerBC%GHBC(:iNBC-1)
                ELSE
                    TempGHBC(1:iLoc-1) = LayerBC%GHBC(1:iLoc-1)
                    TempGHBC(iLoc:)    = LayerBC%GHBC(iLoc+1:)
                END IF
                CALL MOVE_ALLOC(TempGHBC,LayerBC%GHBC)
            END IF
            LayerBC%NGHBC = iNBC - 1
        END IF
    END IF 
    
    !Constrained general head b.c.
    iNBC = LayerBC%NConstrainedGHBC
    IF (iNBC .GT. 0) THEN
        iLoc = LocateInList(iNode,LayerBC%ConstrainedGHBC%iNode)
        IF (iLoc .GT. 0) THEN
            IF (iNBC .EQ. 1) THEN
                DEALLOCATE (LayerBC%ConstrainedGHBC)
            ELSE
                ALLOCATE (TempConstrainedGHBC(iNBC-1))
                IF (iLoc .EQ. 1) THEN
                    TempConstrainedGHBC = LayerBC%ConstrainedGHBC(2:)
                ELSEIF (iLoc .EQ. iNBC) THEN
                    TempConstrainedGHBC = LayerBC%ConstrainedGHBC(:iNBC-1)
                ELSE
                    TempConstrainedGHBC(1:iLoc-1) = LayerBC%ConstrainedGHBC(1:iLoc-1)
                    TempConstrainedGHBC(iLoc:)    = LayerBC%ConstrainedGHBC(iLoc+1:)
                END IF
                CALL MOVE_ALLOC(TempConstrainedGHBC,LayerBC%ConstrainedGHBC)
            END IF
            LayerBC%NConstrainedGHBC = iNBC - 1
        END IF
    END IF 
    
  END SUBROUTINE RemoveBC
  
END MODULE
