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
MODULE IWFM_Core_Version
  USE Class_Version
  IMPLICIT NONE
  
  PRIVATE
  PUBLIC :: IWFM_Core

  ! -------------------------------------------------------------
  ! --- DATA TYPE TO INHERIT THE VERSION DATA TYPE
  ! -------------------------------------------------------------
  TYPE,EXTENDS(VersionType) :: IWFM_Core_Type
    PRIVATE
  CONTAINS
    PROCEDURE,PASS,PUBLIC :: GetVersion => IWFM_Core_GetVersion 
  END TYPE IWFM_Core_Type
  
  
  ! -------------------------------------------------------------
  ! --- MISC. ENTTIES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                    :: iLenVersion = 11
  CHARACTER(LEN=iLenVersion),PARAMETER :: cVersion    = '2015.0.0000'  !Last 4 digits are for revision number and are dynamically replaced
  TYPE(IWFM_Core_Type)                 :: IWFM_Core

  

CONTAINS



  ! -------------------------------------------------------------
  ! --- GET DYNAMICALLY CREATED VERSION NUMBER
  ! -------------------------------------------------------------
  FUNCTION IWFM_Core_GetVersion(Version) RESULT(cVrs)
    CLASS(IWFM_Core_Type)    :: Version
    CHARACTER(:),ALLOCATABLE :: cVrs
    
    INCLUDE 'Revision.fi'
        
    !First generate the version number
    IF (.NOT. Version%IsDefined())   &
      Version%VersionType   = Version%VersionType%New(iLenVersion,cVersion,cRevision)
    
    !Then, retrieve the version number
    cVrs = Version%VersionType%GetVersion()
    
  END FUNCTION IWFM_Core_GetVersion
  

END MODULE IWFM_Core_Version