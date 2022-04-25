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
MODULE GenericLinkedList
  USE MessageLogger  , ONLY: SetLastMessage  , &
                             f_iFatal
  USE Class_LLNode
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
  PUBLIC :: GenericLinkedListType  
  
  
  ! -------------------------------------------------------------
  ! --- GENERIC LIST TYPE
  ! -------------------------------------------------------------
  TYPE,ABSTRACT :: GenericLinkedListType
      PRIVATE
      INTEGER                  :: NNodes   =  0        !Number of nodes in the list
      TYPE(LLNodeType),POINTER :: pHead    => NULL()   !Head of the list
      TYPE(LLNodeType),POINTER :: pTail    => NULL()   !Tail of the list
      TYPE(LLNodeType),POINTER :: pCurrent => NULL()   !Current node in the list
  CONTAINS
      PROCEDURE,NON_OVERRIDABLE,PASS :: AddNode         => GenericLinkedList_AddNode
      PROCEDURE,NON_OVERRIDABLE,PASS :: GetNNodes       => GenericLinkedList_GetNNodes
      PROCEDURE,NON_OVERRIDABLE,PASS :: GetCurrentValue => GenericLinkedList_GetCurrentValue
      PROCEDURE,NON_OVERRIDABLE,PASS :: Reset           => GenericLinkedList_Reset
      PROCEDURE,NON_OVERRIDABLE,PASS :: Next            => GenericLinkedList_Next
      PROCEDURE,NON_OVERRIDABLE,PASS :: Delete          => GenericLinkedList_Delete
      PROCEDURE,NON_OVERRIDABLE,PASS :: GetArray        => GenericLinkedList_ConvertToIntegerArray
  END TYPE GenericLinkedListType
  
  
  ! -------------------------------------------------------------
  ! --- MISC. ENTITIES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                   :: ModNameLen = 19
  CHARACTER(LEN=ModNameLen),PARAMETER :: ModName    = 'GenericLinkedList::'
  
    
  
  
CONTAINS
    
    
    
    
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
  ! --- DELETE LIST 
  ! -------------------------------------------------------------
  SUBROUTINE GenericLinkedList_Delete(List)
    CLASS(GenericLinkedListType) :: List
    
    !Local variables
    INTEGER :: indx
    TYPE(LLNodeType),POINTER :: pCurrent
    
    !Get a pointer to the head of the list
    CALL List%Reset()
    
    !Delete nodes one by one
    DO indx=1,List%NNodes
        pCurrent => List%pCurrent
        CALL List%Next()
        CALL pCurrent%Delete()
    END DO
    
    !Set the number of data nodes to zero
    List%NNodes = 0
    
  END SUBROUTINE GenericLinkedList_Delete



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
  ! --- GET NUMBER OF NODES IN LIST
  ! -------------------------------------------------------------
  PURE FUNCTION GenericLinkedList_GetNNodes(List) RESULT(NNodes)
    CLASS(GenericLinkedListType),INTENT(IN) :: List
    INTEGER                                 :: NNodes
    
    NNodes = List%NNodes
    
  END FUNCTION GenericLinkedList_GetNNodes
  
  
  ! -------------------------------------------------------------
  ! --- GET DATA STORED IN THE NODE POINTED BY pCurrent
  ! -------------------------------------------------------------
  FUNCTION GenericLinkedList_GetCurrentValue(List) RESULT(pValue)
    CLASS(GenericLinkedListType),INTENT(IN) :: List
    CLASS(*),POINTER                        :: pValue
    
    IF (ASSOCIATED(List%pCurrent)) THEN
        pValue => List%pCurrent%GetValue()
    ELSE
        NULLIFY(pValue)
    ENDIF

  END FUNCTION GenericLinkedList_GetCurrentValue 
  
  
  

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
  ! --- CONVERT AN INTEGER LINKED-LIST TO ARRAY
  ! -------------------------------------------------------------
  SUBROUTINE GenericLinkedList_ConvertToIntegerArray(List,iArray,iStat)
    CLASS(GenericLinkedListType),INTENT(IN) :: List
    INTEGER,ALLOCATABLE,INTENT(OUT)         :: iArray(:)
    INTEGER,INTENT(OUT)                     :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+39) :: ThisProcedure = ModName // 'GenericLinkedList_ConvertToIntegerArray'
    INTEGER                      :: indx,ErrorCode
    CHARACTER                    :: cErrorMsg*200
    CLASS(*),POINTER             :: pCurrent
    
    !Initialize
    iStat = 0
    
    !Make sure that the list is not empty
    IF (List%NNodes .EQ. 0) RETURN
    
    !Allocate return array
    ALLOCATE (iArray(List%NNodes) , STAT=ErrorCode , ERRMSG=cErrorMsg)
    IF (ErrorCode .NE. 0) THEN
        CALL SetLastMessage('Error in allocating memory to convert a linked list to an integer array.'//NEW_LINE('x')//TRIM(cErrorMsg),f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    !Store integer linked list in the return array
    CALL List%Reset()
    DO indx=1,List%NNodes
        pCurrent => List%GetCurrentValue()
        SELECT TYPE (pCurrent)
           TYPE IS(INTEGER)
              iArray(indx) = pCurrent 
        END SELECT
        CALL List%Next()
    END DO
    
  END SUBROUTINE GenericLinkedList_ConvertToIntegerArray
  
  
  ! -------------------------------------------------------------
  ! --- ADD A NODE TO THE LIST
  ! -------------------------------------------------------------
  SUBROUTINE GenericLinkedList_AddNode(List,ValueToStore,iStat)
    CLASS(GenericLinkedListType) :: List
    CLASS(*),INTENT(IN)          :: ValueToStore
    INTEGER,INTENT(OUT)          :: iStat
    
    !Local variables
    TYPE(LlNodeType),POINTER :: pNewNode
    
    IF (List%NNodes .EQ. 0) THEN
        List%pHead   => LLNode_New(ValueToStore)
        List%pTail   => List%pHead
    ELSE
        pNewNode   => LLNode_New(ValueToStore)
        CALL List%pTail%SetNextNode(pNewNode,iStat)  ;  IF (iStat .EQ. -1) RETURN
        List%pTail => pNewNode
    END IF
    
    List%pCurrent => List%pTail
    List%NNodes   =  List%NNodes + 1
    
  END SUBROUTINE GenericLinkedList_AddNode
    

  ! -------------------------------------------------------------
  ! --- RESET THE LIST 
  ! -------------------------------------------------------------
  SUBROUTINE GenericLinkedList_Reset(List)
    CLASS(GenericLinkedListType) :: List
    
    List%pCurrent => List%pHead
    
  END SUBROUTINE GenericLinkedList_Reset
  
  
  ! -------------------------------------------------------------
  ! --- MOVE CURENT POINTER TO NEXT NODE IN LIST 
  ! --- Pre-condition: Initial pCurrent must not point to pTail
  ! -------------------------------------------------------------
  SUBROUTINE GenericLinkedList_Next(List)
    CLASS(GenericLinkedListType) :: List
    
    List%pCurrent => List%pCurrent%GetNext()
    
  END SUBROUTINE GenericLinkedList_Next
  
  
END MODULE