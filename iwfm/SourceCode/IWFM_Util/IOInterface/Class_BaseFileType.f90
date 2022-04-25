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
MODULE Class_BaseFileType
  USE MessageLogger     , ONLY: SetLastMessage  , &
                                f_iFatal
  USE GeneralUtilities
  USE,INTRINSIC :: ISO_FORTRAN_ENV
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
  PUBLIC  :: BaseFileType    , &
             IsFileOpen      , &
             GetAUnitNumber

  
  ! -------------------------------------------------------------
  ! --- PARAMETERS
  ! -------------------------------------------------------------
  INTEGER,PARAMETER :: f_iDefaultUnitNumber = -200   ,&
                       f_iMinUnitNumber     = 8      ,&
                       f_iMaxUnitNumber     = 50008     
  INTEGER,SAVE      :: LastUnitConnected    = f_iMinUnitNumber - 1
  
  
  ! -------------------------------------------------------------
  ! --- BASE FILE DATA TYPE
  ! -------------------------------------------------------------
  TYPE,ABSTRACT :: BaseFileType
      INTEGER                  :: UnitN = f_iDefaultUnitNumber   !File unit number
      CHARACTER(:),ALLOCATABLE :: Name                           !Name of file
  CONTAINS
      PROCEDURE(Abstract_New),PASS,DEFERRED             :: New
      PROCEDURE(Abstract_Kill),PASS,DEFERRED            :: Kill
      PROCEDURE,PASS                                    :: NewBaseFile
      PROCEDURE,PASS                                    :: KillBaseFile
      PROCEDURE,PASS                                    :: IOStatHandler
  END TYPE BaseFileType


  ! -------------------------------------------------------------
  ! --- MISC. ENTITIES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                   :: ModNameLen = 20
  CHARACTER(LEN=ModNameLen),PARAMETER :: ModName    ='Class_BaseFileType::'


  ! -------------------------------------------------------------
  ! --- ABSTRACT PROCEDURE INTERFACES
  ! -------------------------------------------------------------
  ABSTRACT INTERFACE
    
    SUBROUTINE Abstract_New(ThisFile,FileName,lInputFile,AccessType,FileOpenCode,iStat)
      IMPORT                               :: BaseFileType
      CLASS(BaseFileType)                  :: ThisFile
      CHARACTER(LEN=*),INTENT(IN)          :: FileName
      LOGICAL,INTENT(IN)                   :: lInputFile
      CHARACTER(LEN=*),OPTIONAL,INTENT(IN) :: AccessType  !Used only for ASCII files
      INTEGER,OPTIONAL,INTENT(OUT)         :: FileOpenCode
      INTEGER,INTENT(OUT)                  :: iStat
    END SUBROUTINE Abstract_New

    
    SUBROUTINE Abstract_Kill(ThisFile,Status)
      IMPORT                               :: BaseFileType
      CLASS(BaseFileType)                  :: ThisFile
      CHARACTER(LEN=*),OPTIONAL,INTENT(IN) :: Status
    END SUBROUTINE Abstract_Kill
 
  END INTERFACE
  
  
  
  
CONTAINS

    
    

! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***** CONSTRUCTOR
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- NEW BASE FILE
  ! -------------------------------------------------------------
  SUBROUTINE NewBaseFile(ThisFile,FileName) 
    CLASS(BaseFileType)         :: ThisFile
    CHARACTER(LEN=*),INTENT(IN) :: FileName
    
    !Local variables
    INTEGER :: iLen

    !Initial data settings
    iLen = LEN_TRIM(ADJUSTL(FileName))
    ALLOCATE (CHARACTER(LEN=iLen) :: ThisFile%Name)
    ThisFile%Name  = TRIM(ADJUSTL(FileName))
    ThisFile%UnitN = GetAUnitNumber()

  END SUBROUTINE NewBaseFile
  
  
  
  
! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***** DESTRUCTOR
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- KILL BASE FILE
  ! -------------------------------------------------------------
  SUBROUTINE KillBaseFile(ThisFile)
    CLASS(BaseFileType) :: ThisFile
    
    !Kill basefile
    ThisFile%UnitN = f_iDefaultUnitNumber
    DEALLOCATE(ThisFile%Name)
    
  END SUBROUTINE KillBaseFile

  
  
  
! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***** MISC. METHODS
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- HANDLE IOSTAT
  ! -------------------------------------------------------------
  SUBROUTINE IOStatHandler(ThisFile,Error,Status,iStat)
    CLASS(BaseFileType),INTENT(IN) :: ThisFile
    INTEGER,INTENT(IN)             :: Error
    INTEGER,OPTIONAL,INTENT(OUT)   :: Status
    INTEGER,INTENT(OUT)            :: iStat
    
    !Local varaibles
    CHARACTER(LEN=ModNameLen+13),PARAMETER :: ThisProcedure = ModName // 'IOStatHandler'
    
    !Initialize
    iStat = 0
    
    IF (PRESENT(Status)) THEN
        Status = Error
        RETURN
    END IF

    IF (Error .EQ. 0) THEN
      !Do nothing; read/write action was successful
    ELSEIF (IS_IOSTAT_END(Error)) THEN
      CALL SetLastMessage('Error in reading data! End-of-file reached in file '//TRIM(ThisFile%Name),f_iFatal,ThisProcedure)
      iStat = -1
    ELSEIF (Error .EQ. 47) THEN
      CALL SetLastMessage('Error in writing out to file! File '//TRIM(ThisFile%Name)//' is read-only',f_iFatal,ThisProcedure)
      iStat = -1
    ELSE
      CALL SetLastMessage('Error in reading data from file '//TRIM(ThisFile%Name),f_iFatal,ThisProcedure)
      iStat = -1
    END IF 

  END SUBROUTINE IOStatHandler
  
  
  ! -------------------------------------------------------------
  ! --- FIND AN UNCONNECTED UNIT NUMBER
  ! -------------------------------------------------------------
  FUNCTION GetAUnitNumber() RESULT(UnitNumber)
    INTEGER :: UnitNumber

    DO UnitNumber=LastUnitConnected+1,f_iMaxUnitNumber
      IF (UnitNumber .EQ. ERROR_UNIT) CYCLE
      IF (UnitNumber .EQ. INPUT_UNIT) CYCLE
      IF (UnitNumber .EQ. OUTPUT_UNIT) CYCLE
      IF (.NOT.IsUnitNumberConnected(UnitNumber)) EXIT
    END DO

    LastUnitConnected = UnitNumber

  END FUNCTION GetAUnitNumber


  ! -------------------------------------------------------------
  ! --- CHECK IF A SPECIFIC UNIT NUMBER IS CONNECTED
  ! -------------------------------------------------------------
  FUNCTION IsUnitNumberConnected(UnitNumber) RESULT(IsConnected)
    INTEGER,INTENT(IN) :: UnitNumber
    LOGICAL            :: IsConnected

    INQUIRE (UNIT=UnitNumber,OPENED=IsConnected)

  END FUNCTION IsUnitNumberConnected


  ! -------------------------------------------------------------
  ! --- CHECK IF A SPECIFIC FILE IS CONNECTED
  ! -------------------------------------------------------------
  FUNCTION IsFileOpen(FileName) RESULT(IsConnected)
    CHARACTER(LEN=*),INTENT(IN) :: FileName
    LOGICAL                     :: IsConnected

    INQUIRE (FILE=FileName,OPENED=IsConnected)

  END FUNCTION IsFileOpen


END MODULE