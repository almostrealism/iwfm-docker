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
MODULE Class_StrmNode_v50
  USE IOInterface    , ONLY: GenericFileType  
  USE Package_Misc   , ONLY: AbstractFunctionType
  USE Package_Matrix , ONLY: ConnectivityListType
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
  PUBLIC :: StrmNode_v50_Type                  , &
            StrmNode_v50_ReadPreprocessedData  , &
            StrmNode_v50_WritePreprocessedData
  
  
  ! -------------------------------------------------------------
  ! --- STREAM CROSS SECTION DATA TYPE
  ! -------------------------------------------------------------
  TYPE CrossSectionType
      REAL(8) :: B0 = 0.0   !Bottom width of channel cross section; 0.0 indicates triangular channel
      REAL(8) :: s  = 0.0   !Inverse of the gradient of channel side; 0.0 indicates rectangular channel
      REAL(8) :: n  = 0.04  !Manning's roughness coefficient given in units of meter and seconds 
  END TYPE CrossSectionType
  
  
  ! -------------------------------------------------------------
  ! --- STREAM NODE DATA TYPE
  ! -------------------------------------------------------------
  TYPE,EXTENDS(AbstractFunctionType) :: StrmNode_v50_Type
      INTEGER                    :: ID                 = 0          !Node ID
      REAL(8)                    :: BottomElev         = 0.0        !Elevation of stream bottom
      REAL(8)                    :: MaxElev            = 0.0        !Maximum elevation for the stream surface (used only to limit the range of Newton-Raphson iterations to speed up convergence)
      REAL(8)                    :: Slope              = 0.0        !Slope of the stream bed
      REAL(8)                    :: Length             = 0.0        !Length of the stream reach associated with the node
      REAL(8)                    :: Area_P             = 0.0        !Flow area at previous time step
      TYPE(ConnectivityListType) :: Connectivity                    !Stream node connectivity (list of upstream nodes)
      TYPE(CrossSectionType)     :: CrossSection                    !Cross section related data
  CONTAINS
      PROCEDURE,PASS :: Flow
      PROCEDURE,PASS :: Area
      PROCEDURE,PASS :: Head
      PROCEDURE,PASS :: dArea
      PROCEDURE,PASS :: dFlow
      PROCEDURE,PASS :: Evaluate              => WetP
      PROCEDURE,PASS :: Derivative            => dWetP
      PROCEDURE,PASS :: InverseEvaluate       => WetP_InverseEvaluate
  END TYPE StrmNode_v50_Type 
  
  
  ! -------------------------------------------------------------
  ! --- MISC. ENTITIES
  ! -------------------------------------------------------------
  REAL(8),PARAMETER :: MinFlowDepth = 1d-6
  
  
  
  
CONTAINS




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
  ! --- READ STREAM NODE PREPROCESSED DATA
  ! -------------------------------------------------------------
  SUBROUTINE StrmNode_v50_ReadPreprocessedData(Nodes,InFile,iStat)
    TYPE(StrmNode_v50_Type),INTENT(OUT) :: Nodes(:)
    TYPE(GenericFileType)               :: InFile
    INTEGER,INTENT(OUT)                 :: iStat
    
    !Local variables
    INTEGER :: indxNode,nConnectedNodes
    
    !Read data
    DO indxNode=1,SIZE(Nodes)
        CALL InFile%ReadData(Nodes(indxNode)%ID,iStat)  ;  IF (iStat .EQ. -1) RETURN
        CALL InFile%ReadData(nConnectedNodes,iStat)     ;  IF (iStat .EQ. -1) RETURN  ;  Nodes(indxNode)%Connectivity%nConnectedNodes = nConnectedNodes
        ALLOCATE (Nodes(indxNode)%Connectivity%ConnectedNodes(nConnectedNodes))
        CALL InFile%ReadData(Nodes(indxNode)%Connectivity%ConnectedNodes,iStat)  ;  IF (iStat .EQ. -1) RETURN
    END DO
    
  END SUBROUTINE StrmNode_v50_ReadPreprocessedData




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
  ! --- WRITE STREAM NODE PREPROCESSED DATA
  ! -------------------------------------------------------------
  SUBROUTINE StrmNode_v50_WritePreprocessedData(Nodes,OutFile)
    TYPE(StrmNode_v50_Type),INTENT(IN) :: Nodes(:)
    TYPE(GenericFileType)              :: OutFile
    
    !Local variables
    INTEGER :: indxNode
    
    !Write data
    DO indxNode=1,SIZE(Nodes)
        CALL OutFile%WriteData(Nodes(indxNode)%ID)
        CALL OutFile%WriteData(Nodes(indxNode)%Connectivity%nConnectedNodes)
        CALL OutFile%WriteData(Nodes(indxNode)%Connectivity%ConnectedNodes)
    END DO
        
  END SUBROUTINE StrmNode_v50_WritePreprocessedData
  
  
  
  
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
  ! --- FLOW AREA
  ! -------------------------------------------------------------
  PURE FUNCTION Area(Node,H) RESULT(rArea)
    CLASS(StrmNode_v50_Type),INTENT(IN) :: Node
    REAL(8),INTENT(IN)                  :: H
    REAL(8)                             :: rArea
    
    !Local variables
    REAL(8) :: Depth
    
    Depth = H - Node%BottomElev
    rArea = Depth * (Node%CrossSection%B0 + Node%CrossSection%s * Depth)
       
  END FUNCTION Area
  
  
  ! -------------------------------------------------------------
  ! --- WETTED PERIMETER 
  ! -------------------------------------------------------------
  FUNCTION WetP(Func,XP) RESULT(YP)
    CLASS(StrmNode_v50_Type),INTENT(IN) :: Func
    REAL(8),INTENT(IN)                  :: XP
    REAL(8)                             :: YP
    
    !Local variables
    REAL(8) :: Depth,B0,s
    
    Depth = XP - Func%BottomElev
    IF (Depth .EQ. 0.0) THEN
        YP = 0.0
        RETURN
    END IF
    B0    = Func%CrossSection%B0
    s     = Func%CrossSection%s
    YP    = B0 + 2D0 * Depth * SQRT(1D0+s*s)
       
  END FUNCTION WetP


  ! -------------------------------------------------------------
  ! --- HYDRAULIC RADIUS
  ! -------------------------------------------------------------
  FUNCTION HydRadius(Node,H) RESULT(R)
    CLASS(StrmNode_v50_Type),INTENT(IN) :: Node
    REAL(8),INTENT(IN)                  :: H
    REAL(8)                             :: R
    
    !Local variables
    REAL(8) :: A,W
    
    A = Area(Node,H)
    IF (A .EQ. 0.0) THEN
        R = 0.0
        RETURN
    END IF
    W = WetP(Node,H)
    R = A / W
       
  END FUNCTION HydRadius

  
  ! -------------------------------------------------------------
  ! --- FLOW
  ! -------------------------------------------------------------
  FUNCTION Flow(Node,H) RESULT(Q)
    CLASS(StrmNode_v50_Type),INTENT(IN) :: Node
    REAL(8),INTENT(IN)                  :: H
    REAL(8)                             :: Q
    
    !Local variables
    REAL(8) :: A,R
    
    A = Area(Node,H)
    R = HydRadius(Node,H)
    Q = A * R**(2D0/3D0) * SQRT(Node%Slope) / Node%CrossSection%n
       
  END FUNCTION Flow


  ! -------------------------------------------------------------
  ! --- COMPUTE HEAD GIVEN FLOW
  ! -------------------------------------------------------------
  FUNCTION Head(Node,Flow) RESULT(H)
    CLASS(StrmNode_v50_Type),INTENT(IN) :: Node
    REAL(8),INTENT(IN)                  :: Flow
    REAL(8)                             :: H
    
    !Local variables
    REAL(8)           :: B0,n,s,SQRSlope,BottomElev,C,Func,dFunc,FlowAtH,Delta
    INTEGER           :: Iter
    REAL(8),PARAMETER :: Toler = 1D-8
    INTEGER,PARAMETER :: MaxIter = 500
    
    !If flow is zero, set Head and return
    IF (Flow .EQ. 0.0) THEN
        H = Node%BottomElev
        RETURN
    END IF
    
    !Initialize
    SQRSlope   = SQRT(Node%Slope)
    BottomElev = NOde%BottomElev
    B0         = Node%CrossSection%B0
    s          = Node%CrossSection%s
    n          = Node%CrossSection%n
    C          = 2D0 / 3D0
    H          = BottomElev + 1.0
    
    !Start iteration
    DO Iter=1,MaxIter
        FlowAtH = Node%Flow(H)
        Func    = Flow - FlowAtH
        dFunc   = -Node%dFlow(H)
        Delta   = Func/dFunc
        IF (ABS(Delta) .LT. Toler) THEN
            H = H - Delta
            RETURN
        END IF
        H = H - Delta
    END DO
    
    !If reached this point, there was no convergence; send back a strange number
    H = -9999.9999d0
    
  END FUNCTION Head


  ! -------------------------------------------------------------
  ! --- DERIVATIVE OF AREA W.R.T. HEAD
  ! -------------------------------------------------------------
  PURE FUNCTION dArea(Node,H) RESULT(d)
    CLASS(StrmNode_v50_Type),INTENT(IN) :: Node
    REAL(8),INTENT(IN)                  :: H
    REAL(8)                             :: d
    
    !Local variables
    REAL(8) :: Depth,B0,s
    
    Depth = H - Node%BottomElev
    B0    = Node%CrossSection%B0
    s     = Node%CrossSection%s
    d     = B0 + 2D0 * s * Depth
       
  END FUNCTION dArea


  ! -------------------------------------------------------------
  ! --- DERIVATIVE OF WETTED PERIMETER W.R.T. HEAD
  ! -------------------------------------------------------------
  FUNCTION dWetP(Func,XP) RESULT(YP)
    CLASS(StrmNode_v50_Type),INTENT(IN) :: Func
    REAL(8),INTENT(IN)                  :: XP    !Not used; only here to comply with the abstract interface
    REAL(8)                             :: YP
    
    !Local variables
    REAL(8) :: s
    
    s  = Func%CrossSection%s
    YP = 2D0 * SQRT(1D0 + s*s)
       
  END FUNCTION dWetP


  ! -------------------------------------------------------------
  ! --- DERIVATIVE OF HYDRAULIC RADIUS W.R.T. HEAD
  ! -------------------------------------------------------------
  FUNCTION dHydRadius(Node,H) RESULT(d)
    CLASS(StrmNode_v50_Type),INTENT(IN) :: Node
    REAL(8),INTENT(IN)                  :: H
    REAL(8)                             :: d
    
    !Local variables
    REAL(8) :: A,W,dA,dW
    
    A  = Area(Node,H)
    W  = WetP(Node,H)
    dA = dArea(Node,H)
    dW = dWetP(Node,H)
    d  = (dA * W - A * dW) / (W*W)
       
  END FUNCTION dHydRadius
  
  
  ! -------------------------------------------------------------
  ! --- DERIVATIVE OF FLOW W.R.T. HEAD
  ! -------------------------------------------------------------
  FUNCTION dFlow(Node,H) RESULT(d)
    CLASS(StrmNode_v50_Type),INTENT(IN) :: Node
    REAL(8),INTENT(IN)                  :: H
    REAL(8)                             :: d
    
    !Local variables
    REAL(8) :: Slope,n,R,dA,W,dR,C,HLocal
    
    IF (H .EQ. Node%BottomElev) THEN
        HLocal = Node%BottomElev + MinFlowDepth
    ELSE
        HLocal = H
    END IF
    C     = 2D0/3D0
    Slope = Node%Slope
    n     = Node%CrossSection%n
    R     = HydRadius(Node,HLocal)
    W     = WetP(Node,HLocal)
    dA    = dArea(Node,HLocal)
    dR    = dHydRadius(Node,HLocal)
    d     = SQRT(Slope) * R**C * (dA + C*W*dR) / n
       
  END FUNCTION dFlow


  ! -------------------------------------------------------------
  ! --- CALCULATE HEAD GIVEN WETTED PERIMETER
  ! -------------------------------------------------------------
  FUNCTION WetP_InverseEvaluate(Func,YP) RESULT(XP)
      CLASS(StrmNode_v50_Type),INTENT(IN) :: Func
      REAL(8),INTENT(IN)                  :: YP
      REAL(8)                             :: XP
      
      !Local variables
      REAL(8) :: B0,s
      
      !Initailize
      B0 = Func%CrossSection%B0
      s  = Func%CrossSection%s
      
      !Calculate
      XP = (YP - B0) / (2D0 * SQRT(s*s))
      
  END FUNCTION WetP_InverseEvaluate
    

END MODULE