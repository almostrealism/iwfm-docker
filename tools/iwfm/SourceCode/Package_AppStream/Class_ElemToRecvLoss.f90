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
MODULE Class_ElemToRecvLoss
  USE GeneralUtilities  , ONLY: GetUniqueArrayComponents , &
                                ShellSort
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
  PUBLIC :: ElemToRecvLossType      , &
            ElemToRecvLoss_GetElems
  
  
  ! -------------------------------------------------------------
  ! --- ELEMENT TO RECOVERABLE LOSS DATA TYPE
  ! -------------------------------------------------------------
  TYPE ElemToRecvLossType
    INTEGER             :: nElemToRecvLoss       = 0
    INTEGER,ALLOCATABLE :: iElemToRecvLoss(:)
  END TYPE ElemToRecvLossType
  
  
 
 
CONTAINS




  ! -------------------------------------------------------------
  ! --- GET ELEMENT NUMBERS WITH RECOVERABLE LOSS 
  ! -------------------------------------------------------------
  SUBROUTINE ElemToRecvLoss_GetElems(ElemToRecvLoss,Elems)
    TYPE(ElemToRecvLossType),INTENT(IN) :: ElemToRecvLoss(:)
    INTEGER,ALLOCATABLE,INTENT(OUT)     :: Elems(:)
    
    !Local variables
    INTEGER,ALLOCATABLE :: TempElems(:)
    INTEGER             :: iCount,indx,indxElem,ErrorCode
    
    !If there are no elements with recovrable loss, return
    IF (SIZE(ElemToRecvLoss) .EQ. 0) THEN
      ALLOCATE (Elems(0))
      RETURN
    END IF
    
    !Total number of elements
    iCount = SUM(ElemToRecvLoss%nElemToRecvLoss)
    IF (iCount .EQ. 0) THEN
      ALLOCATE (Elems(0))
      RETURN
    END IF
    
    !Allocate memory
    ALLOCATE (TempElems(iCount))
    
    !Store element numbers in temporary array
    iCount = 0
    DO indxElem=1,SIZE(ElemToRecvLoss)
      DO indx=1,ElemToRecvLoss(indxElem)%nElemToRecvLoss
        iCount            = iCount + 1
        TempElems(iCount) = indxElem
      END DO
    END DO
    
    !Store the unique element numbers in return variable
    CALL GetUniqueArrayComponents(TempElems , Elems)
    
    !Order the elements
    CALL ShellSort(Elems)

    !Clear memery
    DEALLOCATE (TempElems , STAT=ErrorCode)
    
  END SUBROUTINE ElemToRecvLoss_GetElems
  
 
END MODULE







  
  
