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
MODULE IWFM_Util_VersionF
  USE Class_Version
  IMPLICIT NONE
  

  ! -------------------------------------------------------------
  ! --- PUBLIC ENTITIES
  ! -------------------------------------------------------------
  PRIVATE
  PUBLIC :: IWFM_Util  
            
  
  ! -------------------------------------------------------------
  ! --- DATA TYPE TO INHERET THE VERSION DATA TYPE
  ! -------------------------------------------------------------
  TYPE,EXTENDS(VersionType) :: IWFM_Util_Type
    PRIVATE
  CONTAINS
    PROCEDURE,PASS,PUBLIC :: GetVersion => IWFM_Util_GetVersion 
  END TYPE IWFM_Util_Type
  
  
  ! -------------------------------------------------------------
  ! --- MISC. ENTTIES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                              :: IWFM_Util_iLenVersion = 8
  CHARACTER(LEN=IWFM_Util_iLenVersion),PARAMETER :: cVersion              = '1.0.0000'  !Last 4 digits are for revision number and are dynamically replaced
  TYPE(IWFM_Util_Type)                           :: IWFM_Util


  
CONTAINS



  ! -------------------------------------------------------------
  ! --- GET DYNAMICALLY CREATED VERSION NUMBER
  ! -------------------------------------------------------------
  FUNCTION IWFM_Util_GetVersion(Version) RESULT(cVrs)
    CLASS(IWFM_Util_Type)    :: Version
    CHARACTER(:),ALLOCATABLE :: cVrs
    
    INCLUDE 'IWFM_Util_Revision.fi'
        
    !First generate the version number
    Version%VersionType = Version%New(IWFM_Util_iLenVersion,cVersion,cRevision)
    
    !Then, retrieve the version number
    cVrs = Version%VersionType%GetVersion()
    
  END FUNCTION IWFM_Util_GetVersion

          
END MODULE 