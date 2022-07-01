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
MODULE Class_PumpsAtElem
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
  PUBLIC
  
  
  ! -------------------------------------------------------------
  ! --- DATA TYPE FOR PUMPS AT ELEMENTS
  ! -------------------------------------------------------------
  TYPE PumpsAtElemType
    INTEGER             :: nPumps = 0  !Number of pumps at an element
    INTEGER,ALLOCATABLE :: iPumpIDs(:) !List of pump IDs at element
  END TYPE PumpsAtElemType
  
  
  
  
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
  ! --- LIST OF PUMPS AT A SET OF ELEMENTS
  ! -------------------------------------------------------------
  SUBROUTINE PumpsAtElem_New(NElements,Elements,PumpsAtElems)
    INTEGER,INTENT(IN)    :: NElements,Elements(:)
    TYPE(PumpsAtElemType) :: PumpsAtElems(NElements)
    
    !Local variables
    INTEGER             :: indxPump,iElem,nPumps
    INTEGER,ALLOCATABLE :: TempPumpIDs(:)
    
    !Compile
    DO indxPump=1,SIZE(Elements)
      iElem  = Elements(indxPump)
      nPumps = PumpsAtElems(iElem)%nPumps
      ALLOCATE (TempPumpIDs(nPumps+1))
      TempPumpIDs(1:nPumps) = PumpsAtElems(iElem)%iPumpIDs
      nPumps                = nPumps + 1
      TempPumpIDs(nPumps)   = indxPump
      CALL MOVE_ALLOC(TempPumpIDs , PumpsAtElems(iElem)%iPumpIDs)
      PumpsAtElems(iElem)%nPumps = nPumps
    END DO 
    
  END SUBROUTINE PumpsAtElem_New


END MODULE