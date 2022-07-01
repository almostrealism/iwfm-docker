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
MODULE Package_Budget
  USE Class_Version          , ONLY: VersionType
  USE Budget_Parameters
  USE Class_BudgetInputFile  , ONLY: BudgetHeaderType    
  USE Class_Budget           , ONLY: BudgetType          , &
                                     PrintIntervalType   , &
                                     ModifiedAgSupplyReq
  IMPLICIT NONE
  
  
  ! -------------------------------------------------------------
  ! --- VERSION RELEATED DATA
  ! -------------------------------------------------------------
  INTEGER,PRIVATE,PARAMETER                    :: iLenVersion = 8
  CHARACTER(LEN=iLenVersion),PRIVATE,PARAMETER :: cVersion ='4.0.0000'
  INCLUDE 'Package_Budget_Revision.fi'
  
  
  
CONTAINS

    

  ! -------------------------------------------------------------
  ! --- GET VERSION NUMBER
  ! -------------------------------------------------------------
  FUNCTION Package_Budget_GetVersion() RESULT(cVrs)
    CHARACTER(:),ALLOCATABLE :: cVrs
    
    !Local variables
    TYPE(VersionType) :: MyVersion
    
    MyVersion = MyVersion%New(iLenVersion,cVersion,cRevision)
    cVrs      = TRIM(MyVersion%GetVersion()) 
    
  END FUNCTION Package_Budget_GetVersion




END MODULE