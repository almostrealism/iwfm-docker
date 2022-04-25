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
MODULE Class_LLNode
  USE MessageLogger  , ONLY: SetLastMessage , &
                             f_iFatal
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
  PUBLIC :: LLNodeType   , &
            LLNode_New
  
  
  ! -------------------------------------------------------------
  ! --- GENERIC NODE TYPE
  ! -------------------------------------------------------------
  TYPE LLNodeType
      PRIVATE
      CLASS(*),ALLOCATABLE     :: Value
      TYPE(LLNodeType),POINTER :: Next  => NULL()
  CONTAINS
      PROCEDURE,PASS :: Delete      => LLNode_Delete       !Delete a node
      PROCEDURE,PASS :: GetNext     => LLNode_GetNext      !Get a pointer to the next node
      PROCEDURE,PASS :: GetValue    => LLNode_GetValue     !Get a pointer to the data stored in the node
      PROCEDURE,PASS :: SetNextNode => LLNode_SetNextNode  !Set the next data node for a given node
  END TYPE LLNodeType
    
  
  ! -------------------------------------------------------------
  ! --- MISC. ENTITIES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                   :: ModNameLen = 14
  CHARACTER(LEN=ModNameLen),PARAMETER :: ModName    = 'Class_LLNode::'
  
  

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
  ! --- INSTANTIATE A NEW LINKED LIST NODE
  ! -------------------------------------------------------------
  FUNCTION LLNode_New(ValueToStore) RESULT(pNode)
    CLASS(*),INTENT(IN)      :: ValueToStore
    TYPE(LLNodeType),POINTER :: pNode
    
    !Allocate memory for node
    ALLOCATE (pNode)
    
    !Then allocate memory for the node data
    ALLOCATE (pNode%Value , SOURCE=ValueToStore)
    
  END FUNCTION LLNode_New
  
  
  
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
  ! --- DELETE A NODE
  ! -------------------------------------------------------------
  SUBROUTINE LLNode_Delete(pNode)
    CLASS(LLNodeType) :: pNode
    
    !Local variables
    INTEGER :: ErrorCode
    
    DEALLOCATE (pNode%Value , STAT=ErrorCode)
    NULLIFY(pNode%Next)
    
  END SUBROUTINE LLNode_Delete
  
  
  
  
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
  ! --- GET A POINTER TO THE NEXT NODE
  ! -------------------------------------------------------------
  FUNCTION LLNode_GetNext(Node) RESULT(pNext)
    CLASS(LLNodeType),TARGET,INTENT(IN) :: Node
    TYPE(LLNodeType),POINTER            :: pNext
    
    pNext => Node%Next
    
  END FUNCTION LLNode_GetNext
  
  
  ! -------------------------------------------------------------
  ! --- GET NODE DATA
  ! --- Pre-condition: Node must be defined
  ! -------------------------------------------------------------
  FUNCTION LLNode_GetValue(Node) RESULT(pValue)
    CLASS(LLNodeType),TARGET,INTENT(IN) :: Node
    CLASS(*),POINTER                    :: pValue
    
    ALLOCATE (pValue , SOURCE=Node%Value)
    pValue => Node%Value
    
  END FUNCTION LLNode_GetValue
  
  
 
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
  ! --- SET NEXT NODE (ONLY IF IT IS NOT ASSOCIATED ALREADY)
  ! -------------------------------------------------------------
  SUBROUTINE LLNode_SetNextNode(aNode,pNext,iStat)
    CLASS(LLNodeType)                   :: aNode
    TYPE(LLNodeType),POINTER,INTENT(IN) :: pNext
    INTEGER,INTENT(OUT)                 :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+18) :: ThisProcedure = ModName //'LLNode_SetNextNode'
    
    !Initailize
    iStat = 0
    
    !Make sure next node is not defined
    IF (ASSOCIATED(aNode%Next))  THEN
        CALL SetLastMessage('Can only add a node to the end of a linked-list.',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    aNode%Next => pNext
    
  END SUBROUTINE LLNode_SetNextNode

  
END MODULE