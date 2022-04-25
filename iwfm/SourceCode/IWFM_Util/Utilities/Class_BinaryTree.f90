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
MODULE Class_BinaryTree
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
  PUBLIC :: BinaryTreeType 
  
  
  ! -------------------------------------------------------------
  ! --- BINARY TREE DATA TYPE
  ! -------------------------------------------------------------
  TYPE BinaryTreeType
      PRIVATE
      CLASS(*),ALLOCATABLE         :: Key                 !Key to compare against each other in traversing the tree
      CLASS(*),ALLOCATABLE         :: Data                !Data to store in the binary tree
      TYPE(BinaryTreeType),POINTER :: pLeft   => NULL()
      TYPE(BinaryTreeType),POINTER :: pRight  => NULL()
  CONTAINS
      PROCEDURE,PASS :: AddNode
      PROCEDURE,PASS :: Kill
      PROCEDURE,PASS :: GetNNodes
      PROCEDURE,PASS :: GetPointerToNode
      PROCEDURE,PASS :: GetOrderedIntKeyList
      PROCEDURE,PASS :: GetOrderedReal8KeyList
      PROCEDURE,PASS :: GetOrderedCharKeyList
      GENERIC        :: GetOrderedKeyList         => GetOrderedIntKeyList      , &
                                                     GetOrderedReal8KeyList    , &
                                                     GetOrderedCharKeyList
  END TYPE BinaryTreeType


  ! -------------------------------------------------------------
  ! --- MISC. ENTITIES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                   :: f_iCharKeyLen = 30
  INTEGER,PARAMETER                   :: ModNameLen    = 18
  CHARACTER(LEN=ModNameLen),PARAMETER :: ModName       = 'Class_BinaryTree::'
  
  
  
  
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
  ! --- GATEWAY METHOD TO ADD A NODE TO THE TREE
  ! -------------------------------------------------------------
  SUBROUTINE AddNode(BTree,DataToStore,Key,pCurrent,lNodePreviouslyAdded)
    CLASS(BinaryTreeType),TARGET :: BTree
    CLASS(*),INTENT(IN)          :: DataToStore,Key
    CLASS(*),POINTER,INTENT(OUT) :: pCurrent
    LOGICAL,INTENT(OUT)          :: lNodePreviouslyAdded
    
    !Local variables
    TYPE(BinaryTreeType),POINTER :: pBTree
    
    pBTree => BTree
    SELECT TYPE (pKey => Key)
        TYPE IS (INTEGER)
            CALL AddNode_Recursive(pBTree,DataToStore,pCurrent,lNodePreviouslyAdded,IntKey=pKey)
        TYPE IS (REAL(8))
            CALL AddNode_Recursive(pBTree,DataToStore,pCurrent,lNodePreviouslyAdded,Real8Key=pKey)
        TYPE IS (CHARACTER(LEN=*))
            CALL AddNode_Recursive(pBTree,DataToStore,pCurrent,lNodePreviouslyAdded,CharKey=pKey)
    END SELECT
    
  END SUBROUTINE AddNode

  
  ! -------------------------------------------------------------
  ! --- GATEWAY METHOD TO KILL THE BINARY TREE
  ! -------------------------------------------------------------
  SUBROUTINE Kill(This)
    CLASS(BinaryTreeType),TARGET :: This
    
    !Local variables
    TYPE(BinaryTreeType),POINTER :: pBTree
    
    pBTree => This
    IF (.NOT. ASSOCIATED(pBTree)) RETURN
    
    !First clear memory to the left of the root
    IF (ASSOCIATED(pBTree%pLeft)) CALL Kill_Recursive(pBTree%pLeft)
    
    !Then clear memory to the right of the root
    IF (ASSOCIATED(pBTree%pRight)) CALL Kill_Recursive(pBTree%pRight)
    
    !The root will be killed by the caller
    !DEALLOCATE (This)
    
  END SUBROUTINE Kill

  
  ! -------------------------------------------------------------
  ! --- GATEWAY METHOD TO COUNT THE NUMBER OF NODES
  ! -------------------------------------------------------------
  FUNCTION GetNNodes(BTree) RESULT(NNodes)
    CLASS(BinaryTreeType),TARGET,INTENT(IN) :: BTree
    INTEGER                                 :: NNodes
    
    !Local variables
    TYPE(BinaryTreeType),POINTER :: pBTree
    
    !Initialize
    NNodes = 0
    
    pBTree => BTree
    CALL GetNNodes_Recursive(pBTree,NNodes)
    
  END FUNCTION GetNNodes
  
  
  ! -------------------------------------------------------------
  ! --- GATEWAY METHOD TO GET POINTER TO A NODE BASED ON KEY
  ! -------------------------------------------------------------
  FUNCTION GetPointerToNode(BTree,Key) RESULT(pNode)
    CLASS(BinaryTreeType),TARGET,INTENT(IN) :: BTree
    CLASS(*),INTENT(IN)                     :: Key
    CLASS(*),POINTER                        :: pNode
    
    !Local variables
    TYPE(BinaryTreeType),POINTER :: pBTree
    
    pBTree => BTree
    
    SELECT TYPE (Key)
        TYPE IS (INTEGER)
            pNode => GetPointerToNode_Recursive(pBTree,IntKey=Key)
        TYPE IS (REAL(8))
            pNode => GetPointerToNode_Recursive(pBTree,Real8Key=Key)
        TYPE IS (CHARACTER(LEN=*))
            pNode => GetPointerToNode_Recursive(pBTree,CharKey=Key)
    END SELECT

  END FUNCTION GetPointerToNode
  
  
  ! -------------------------------------------------------------
  ! --- GET ORDERED INTEGER KEY LIST
  ! -------------------------------------------------------------
  SUBROUTINE GetOrderedIntKeyList(BTree,OrderedKeyList)
    CLASS(BinaryTreeType),TARGET,INTENT(IN) :: BTree
    INTEGER,ALLOCATABLE                     :: OrderedKeyList(:)
    
    !Local variables
    TYPE(BinaryTreeType),POINTER :: pBTree
    INTEGER                      :: ErrorCode,iCount,NNodes
    
    !Initialize
    DEALLOCATE (OrderedKeyList , STAT=ErrorCode)
    
    !Return if the binary tree is not instantiated
    IF (.NOT. ALLOCATED(BTree%Key)) RETURN
    
    !Pointer to binary tree
    pBTree => BTree
    
    !Number of nodes
    NNodes = GetNNodes(pBTree)
    
    !Compile the list
    iCount = 0
    ALLOCATE (OrderedKeyList(NNodes))
    CALL GetOrderedKeyList_Recursive(pBTree,iCount,IntOrderedKeyList=OrderedKeyList)    
    
  END SUBROUTINE GetOrderedIntKeyList
  
  
  ! -------------------------------------------------------------
  ! --- GET ORDERED REAL(8) KEY LIST
  ! -------------------------------------------------------------
  SUBROUTINE GetOrderedReal8KeyList(BTree,OrderedKeyList)
    CLASS(BinaryTreeType),TARGET,INTENT(IN) :: BTree
    REAL(8),ALLOCATABLE                     :: OrderedKeyList(:)
    
    !Local variables
    TYPE(BinaryTreeType),POINTER :: pBTree
    INTEGER                      :: ErrorCode,iCount,NNodes
    
    !Initialize
    DEALLOCATE (OrderedKeyList , STAT=ErrorCode)
    
    !Return if the binary tree is not instantiated
    IF (.NOT. ALLOCATED(BTree%Key)) RETURN
    
    
    !Pointer to binary tree
    pBTree => BTree
    
    !Number of nodes
    NNodes = GetNNodes(pBTree)
    
    !Compile the list
    iCount = 0
    ALLOCATE (OrderedKeyList(NNodes))
    CALL GetOrderedKeyList_Recursive(pBTree,iCount,Real8OrderedKeyList=OrderedKeyList)    
    
  END SUBROUTINE GetOrderedReal8KeyList
  

  ! -------------------------------------------------------------
  ! --- GET ORDERED CHARACTER KEY LIST
  ! -------------------------------------------------------------
  SUBROUTINE GetOrderedCharKeyList(BTree,OrderedKeyList)
    CLASS(BinaryTreeType),TARGET,INTENT(IN) :: BTree
    CHARACTER(LEN=*),ALLOCATABLE            :: OrderedKeyList(:)
    
    !Local variables
    TYPE(BinaryTreeType),POINTER :: pBTree
    INTEGER                      :: ErrorCode,iCount,NNodes
    
    !Initialize
    DEALLOCATE (OrderedKeyList , STAT=ErrorCode)
    
    !Return if the binary tree is not instantiated
    IF (.NOT. ALLOCATED(BTree%Key)) RETURN
    
    
    !Pointer to binary tree
    pBTree => BTree
    
    !Number of nodes
    NNodes = GetNNodes(pBTree)
    
    !Compile the list
    iCount = 0
    ALLOCATE (OrderedKeyList(NNodes))
    CALL GetOrderedKeyList_Recursive(pBTree,iCount,CharOrderedKeyList=OrderedKeyList)    
    
  END SUBROUTINE GetOrderedCharKeyList

  
  
  
! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** RECURSIVE METHODS
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- ADD A NODE TO THE TREE
  ! -------------------------------------------------------------
  RECURSIVE SUBROUTINE AddNode_Recursive(pBTree,DataToStore,pCurrent,lNodePreviouslyAdded,IntKey,Real8Key,CharKey)
    TYPE(BinaryTreeType),POINTER         :: pBTree
    CLASS(*),INTENT(IN)                  :: DataToStore
    CLASS(*),POINTER,INTENT(OUT)         :: pCurrent
    LOGICAL,INTENT(OUT)                  :: lNodePreviouslyAdded
    INTEGER,OPTIONAL,INTENT(IN)          :: IntKey
    REAL(8),OPTIONAL,INTENT(IN)          :: Real8Key
    CHARACTER(LEN=*),OPTIONAL,INTENT(IN) :: CharKey
    
    !Local variables
    CHARACTER(:),ALLOCATABLE :: TempChar 
    
    !Initialize
    lNodePreviouslyAdded = .FALSE.
    
    !Add node as leaf
    IF (.NOT. ASSOCIATED(pBTree)) THEN
        ALLOCATE (pBTree)
        ALLOCATE (pBTree%Data , SOURCE=DataToStore)
        IF (PRESENT(IntKey)) THEN
            ALLOCATE (pBTree%Key , SOURCE=IntKey)
        ELSEIF (PRESENT(Real8Key)) THEN
            ALLOCATE (pBTree%Key , SOURCE=Real8Key)
        ELSE
            ALLOCATE(CHARACTER(LEN=f_iCharKeyLen) :: TempChar)
            TempChar = CharKey
            ALLOCATE (pBTree%Key , SOURCE=TempChar)
        END IF
        pCurrent  => pBTree%Data
        
    !If this is first data entry to the tree
    ELSEIF (.NOT. ALLOCATED(pBTree%Key)) THEN
        IF (PRESENT(IntKey)) THEN
            ALLOCATE (pBTree%Key , SOURCE=IntKey)
        ELSEIF (PRESENT(Real8Key)) THEN
            ALLOCATE (pBTree%Key , SOURCE=Real8Key)
        ELSE
            TempChar = CharKey
            ALLOCATE (pBTree%Key , SOURCE=TempChar)
        END IF
        ALLOCATE (pBTree%Data , SOURCE=DataToStore)
        pCurrent => pBTree%Data
        
    !Traverse the tree
    ELSE
        SELECT TYPE (pKey => pBTree%Key)
            TYPE IS (INTEGER)               
                IF (IntKey .GT. pKey) THEN
                    CALL AddNode_Recursive(pBTree%pRight,DataToStore,pCurrent,lNodePreviouslyAdded,IntKey=IntKey)
                ELSEIF (IntKey .LT. pKey) THEN
                    CALL AddNode_Recursive(pBTree%pLeft,DataToStore,pCurrent,lNodePreviouslyAdded,IntKey=IntKey)
                ELSE
                    pCurrent             => pBTree%Data
                    lNodePreviouslyAdded = .TRUE.
                END IF
                
            TYPE IS (REAL(8))               
                IF (Real8Key .GT. pKey) THEN
                    CALL AddNode_Recursive(pBTree%pRight,DataToStore,pCurrent,lNodePreviouslyAdded,Real8Key=Real8Key)
                ELSEIF (Real8Key .LT. pKey) THEN
                    CALL AddNode_Recursive(pBTree%pLeft,DataToStore,pCurrent,lNodePreviouslyAdded,Real8Key=Real8Key)
                ELSE
                    pCurrent             => pBTree%Data
                    lNodePreviouslyAdded = .TRUE.
                END IF
                
            TYPE IS (CHARACTER(LEN=*))               
                IF (CharKey .GT. pKey) THEN
                    CALL AddNode_Recursive(pBTree%pRight,DataToStore,pCurrent,lNodePreviouslyAdded,CharKey=CharKey)
                ELSEIF (CharKey .LT. pKey) THEN
                    CALL AddNode_Recursive(pBTree%pLeft,DataToStore,pCurrent,lNodePreviouslyAdded,CharKey=CharKey)
                ELSE
                    pCurrent             => pBTree%Data
                    lNodePreviouslyAdded = .TRUE.
                END IF
        END SELECT
    END IF
    
  END SUBROUTINE AddNode_Recursive
  
  
  ! -------------------------------------------------------------
  ! --- KILL THE TREE
  ! -------------------------------------------------------------
  RECURSIVE SUBROUTINE Kill_Recursive(pBTree)
    TYPE(BinaryTreeType),POINTER :: pBTree
    
    !Local variables
    INTEGER :: ErrorCode
    
    !First traverse to the left
    IF (ASSOCIATED(pBTree%pLeft)) CALL Kill_Recursive(pBTree%pLeft)
    
    !Then, traverse to the right
    IF (ASSOCIATED(pBTree%pRight)) CALL Kill_Recursive(pBTree%pRight)
    
    !If reached the end, deallocate memory
    DEALLOCATE (pBTree , STAT=ErrorCode)
    
  END SUBROUTINE Kill_Recursive
  
  
  ! -------------------------------------------------------------
  ! --- COUNT THE NUMBER OF NODES
  ! -------------------------------------------------------------
  RECURSIVE SUBROUTINE GetNNodes_Recursive(pBTree,NNodes)
    TYPE(BinaryTreeType),POINTER,INTENT(IN) :: pBTree
    INTEGER                                 :: NNodes
    
    !If the node is not set-up, return
    IF (.NOT. ALLOCATED(pBTree%Key)) RETURN
    
    !Otherwise, count it and move to first right then to left branches
    NNodes = NNodes + 1
    IF (ASSOCIATED(pBTree%pRight)) CALL GetNNodes_Recursive(pBTree%PRight,NNodes)
    IF (ASSOCIATED(pBTree%pLeft)) CALL GetNNodes_Recursive(pBTree%PLeft,NNodes)

  END SUBROUTINE GetNNodes_Recursive
  
  
  ! -------------------------------------------------------------
  ! --- GET POINTER TO A NODE BASED ON KEY
  ! -------------------------------------------------------------
  RECURSIVE FUNCTION GetPointerToNode_Recursive(pBTree,IntKey,Real8Key,CharKey) RESULT(pNode)
    TYPE(BinaryTreeType),POINTER,INTENT(IN) :: pBTree
    INTEGER,OPTIONAL,INTENT(IN)             :: IntKey
    REAL(8),OPTIONAL,INTENT(IN)             :: Real8Key
    CHARACTER(LEN=*),OPTIONAL,INTENT(IN)    :: CharKey
    CLASS(*),POINTER                        :: pNode
    
    IF (.NOT. ASSOCIATED(pBTree)) RETURN
    
    IF (PRESENT(IntKey)) THEN
        SELECT TYPE (pKey => pBTree%Key)
            TYPE IS (INTEGER)
                IF (IntKey .EQ. pKey) THEN
                    pNode => pBTree%Data
                    RETURN
                ELSEIF (IntKey .GT. pKey) THEN
                    pNode => GetPointerToNode_Recursive(pBTree%pRight,IntKey=IntKey)
                ELSE
                    pNode => GetPointerToNode_Recursive(pBTree%pLeft,IntKey=IntKey)
                END IF
        END SELECT
            
    ELSEIF (PRESENT(Real8Key)) THEN       
        SELECT TYPE (pKey => pBTree%Key)
            TYPE IS (REAL(8))
                IF (Real8Key .EQ. pKey) THEN
                    pNode => pBTree%Data
                    RETURN
                ELSEIF (Real8Key .GT. pKey) THEN
                    pNode => GetPointerToNode_Recursive(pBTree%pRight,Real8Key=Real8Key)
                ELSE
                    pNode => GetPointerToNode_Recursive(pBTree%pLeft,Real8Key=Real8Key)
                END IF
        END SELECT
        
    ELSE       
        SELECT TYPE (pKey => pBTree%Key)
            TYPE IS (CHARACTER(LEN=*))
                IF (CharKey .EQ. pKey) THEN
                    pNode => pBTree%Data
                    RETURN
                ELSEIF (CharKey .GT. pKey) THEN
                    pNode => GetPointerToNode_Recursive(pBTree%pRight,CharKey=CharKey)
                ELSE
                    pNode => GetPointerToNode_Recursive(pBTree%pLeft,CharKey=CharKey)
                END IF
        END SELECT
            
    END IF
    
  END FUNCTION GetPointerToNode_Recursive
  
  
  ! -------------------------------------------------------------
  ! --- GET ORDERED KEY LIST
  ! -------------------------------------------------------------
  RECURSIVE SUBROUTINE GetOrderedKeyList_Recursive(pBTree,iCount,IntOrderedKeyList,Real8OrderedKeyList,CharOrderedKeyList)
    TYPE(BinaryTreeType),POINTER,INTENT(IN) :: pBTree
    INTEGER                                 :: iCount
    INTEGER,OPTIONAL                        :: IntOrderedKeyList(:)
    REAL(8),OPTIONAL                        :: Real8OrderedKeyList(:)
    CHARACTER(LEN=*),OPTIONAL               :: CharOrderedKeyList(:)
    
    IF (.NOT. ASSOCIATED(pBTree)) RETURN
    
    IF (PRESENT(IntOrderedKeyList)) THEN
        !First traverse to left
        CALL GetOrderedKeyList_Recursive(pBTree%pLeft,iCount,IntOrderedKeyList=IntOrderedKeyList)
        SELECT TYPE (pKey => pBTree%Key)
            TYPE IS (INTEGER)
                iCount                    = iCount + 1
                IntOrderedKeyList(iCount) = pKey
        END SELECT
        !Then traverse to right
        CALL GetOrderedKeyList_Recursive(pBTree%pRight,iCount,IntOrderedKeyList=IntOrderedKeyList)
        
    ELSEIF (PRESENT(Real8OrderedKeyList)) THEN
        !First traverse to left
        CALL GetOrderedKeyList_Recursive(pBTree%pLeft,iCount,Real8OrderedKeyList=Real8OrderedKeyList)
        SELECT TYPE (pKey => pBTree%Key)
            TYPE IS (REAL(8))
                iCount                      = iCount + 1
                Real8OrderedKeyList(iCount) = pKey
        END SELECT
        !Then traverse to right
        CALL GetOrderedKeyList_Recursive(pBTree%pRight,iCount,Real8OrderedKeyList=Real8OrderedKeyList)
        
    ELSE
        !First traverse to left
        CALL GetOrderedKeyList_Recursive(pBTree%pLeft,iCount,CharOrderedKeyList=CharOrderedKeyList)
        SELECT TYPE (pKey => pBTree%Key)
            TYPE IS (CHARACTER(LEN=*))
                iCount                     = iCount + 1
                CharOrderedKeyList(iCount) = pKey
        END SELECT
        !Then traverse to right
        CALL GetOrderedKeyList_Recursive(pBTree%pRight,iCount,CharOrderedKeyList=CharOrderedKeyList)
            
    END IF
    
  END SUBROUTINE GetOrderedKeyList_Recursive
  
END MODULE
