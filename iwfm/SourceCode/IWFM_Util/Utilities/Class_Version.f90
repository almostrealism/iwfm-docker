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
MODULE Class_Version
  USE GeneralUtilities  ,  ONLY : FirstLocation           , &
                                  CleanSpecialCharacters
  USE MessageLogger     ,  ONLY : SetLastMessage          , &
                                  MessageArray            , &
                                  f_iFatal
  USE IOInterface       ,  ONLY : GenericFileType
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
    PUBLIC :: VersionType  , &
              ReadVersion


  ! -------------------------------------------------------------
  ! --- VERSION DATA TYPE
  ! -------------------------------------------------------------
  TYPE VersionType
    PRIVATE
    INTEGER                  :: iLenVersion = 0
    CHARACTER(:),ALLOCATABLE :: cVersion
  CONTAINS
    GENERIC,PUBLIC          :: New  => Version_New_FromComponents  , &
                                       Version_New_FromFullString
    PROCEDURE,NOPASS,PUBLIC :: Version_New_FromComponents
    PROCEDURE,NOPASS,PUBLIC :: Version_New_FromFullString
    PROCEDURE,PASS,PUBLIC   :: Kill
    PROCEDURE,PASS,PUBLIC   :: GetVersion
    PROCEDURE,PASS,PUBLIC   :: IsDefined
  END TYPE VersionType
  
  
  ! -------------------------------------------------------------
  ! --- MISC. METHODS
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                   :: ModNameLen = 15
  CHARACTER(LEN=ModNameLen),PARAMETER :: ModName    = 'Class_Version::'


  
CONTAINS



! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** METHODS
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- CONSTRUCTOR 1
  ! -------------------------------------------------------------
  FUNCTION Version_New_FromComponents(iLenVersion,cVersion,cRevision) RESULT(Version)
    INTEGER,INTENT(IN)                    :: iLenVersion
    CHARACTER(LEN=iLenVersion),INTENT(IN) :: cVersion
    CHARACTER(LEN=*),INTENT(IN)           :: cRevision
    TYPE(VersionType)                     :: Version
    
    !Local variables
    INTEGER :: iLenRev
    
    !Initialize the version
    Version%iLenVersion = iLenVersion   
    ALLOCATE(CHARACTER(iLenVersion) :: Version%cVersion)
    Version%cVersion = cVersion
    
    !Include the revision number in the version
    iLenRev                                             = LEN_TRIM(ADJUSTL(cRevision))
    Version%cVersion(iLenVersion-iLenRev+1:iLenVersion) = TRIM(ADJUSTL(cRevision))
    
  END FUNCTION Version_New_FromComponents
  
  
  ! -------------------------------------------------------------
  ! --- CONSTRUCTOR 2
  ! -------------------------------------------------------------
  FUNCTION Version_New_FromFullString(cVrs) RESULT(Version)
    CHARACTER(LEN=*),INTENT(IN) :: cVrs
    TYPE(VersionType)           :: Version
    
    !Local variables
    INTEGER :: iLenVersion
    
    iLenVersion         = LEN_TRIM(cVrs)
    Version%iLenVersion = iLenVersion
    ALLOCATE (CHARACTER(iLenVersion) :: Version%cVersion)
    Version%cVersion = cvrs
    
  END FUNCTION Version_New_FromFullString
  
  
  ! -------------------------------------------------------------
  ! --- DESTRUCTOR
  ! -------------------------------------------------------------
  SUBROUTINE Kill(Version)
    CLASS(VersionType) :: Version
    
    !Local variables
    INTEGER           :: ErrorCode
    
    DEALLOCATE (Version%cVersion , STAT=ErrorCode)
    Version%iLenVersion = 0
    
  END SUBROUTINE Kill
  
  
  ! -------------------------------------------------------------
  ! --- GETTER
  ! -------------------------------------------------------------
  FUNCTION GetVersion(Version) RESULT(cVrs)
    CLASS(VersionType)       :: Version
    CHARACTER(:),ALLOCATABLE :: cVrs
    
    ALLOCATE( CHARACTER(LEN=Version%iLenVersion) :: cVrs )
    cVrs = Version%cVersion
    
  END FUNCTION GetVersion
  
  
  ! -------------------------------------------------------------
  ! --- PREDICATE
  ! -------------------------------------------------------------
  FUNCTION IsDefined(Version) RESULT(lDefined)
    CLASS(VersionType),INTENT(IN) :: Version
    LOGICAL                       :: lDefined
    
    IF (Version%iLenVersion .EQ. 0) THEN
        lDefined = .FALSE.
    ELSE
        lDefined = .TRUE.
    END IF
    
  END FUNCTION IsDefined
  
  
  ! -------------------------------------------------------------
  ! --- READ VERSION FROM A FILE
  ! -------------------------------------------------------------
  SUBROUTINE ReadVersion(InFile,cComponent,cVersion,iStat)
    TYPE(GenericFileType)       :: InFile
    CHARACTER(LEN=*),INTENT(IN) :: cComponent
    CHARACTER(:),ALLOCATABLE    :: cVersion
    INTEGER,INTENT(OUT)         :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+11) :: ThisProcedure = ModName // 'ReadVersion'
    CHARACTER                    :: ALine*2000
    INTEGER                      :: iStart
    
    !Initialize
    iStat = 0
    
    CALL InFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN 
    CALL CleanSpecialCharacters(ALine)  ;  ALine = ADJUSTL(ALine)
    iStart   = FirstLocation('#',ALine,Back=.FALSE.)
    IF (iStart .LE. 0) THEN
        MessageArray(1) = 'Error in identifying the version number of the '//TRIM(cComponent)//' component!'
        MessageArray(2) = 'Make sure that the version number is listed at the first line of the'
        MessageArray(3) = TRIM(cComponent)//' input file (see the input file template for format).'
        CALL SetLastMessage(MessageArray(1:3),f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    cVersion = ALine(iStart+1:LEN_TRIM(ALine))
    
  END SUBROUTINE ReadVersion
 

END MODULE