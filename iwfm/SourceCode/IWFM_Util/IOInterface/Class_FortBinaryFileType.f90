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
MODULE Class_FortBinaryFileType
  USE MessageLogger      , ONLY: SetLastMessage          , &
                                 LogMessage              , &
                                 f_iWarn                 , &
                                 f_iFatal
  USE GeneralUtilities   , ONLY: GenericString           , &
                                 GenericString_To_String , &
                                 f_cLineFeed
  USE Class_BaseFileType , ONLY: BaseFileType 
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
  PUBLIC :: FortBinFileType               


  ! -------------------------------------------------------------
  ! --- FORTRAN BINARY FILE DATA TYPE
  ! -------------------------------------------------------------
  TYPE,EXTENDS(BaseFileType) :: FortBinFileType
  CONTAINS
     PROCEDURE,PASS :: New             => New_FortBinFile
     PROCEDURE,PASS :: Kill            => Kill_FortBinFile
     PROCEDURE,PASS :: ReadSingleData
     PROCEDURE,PASS :: ReadArrayData
     PROCEDURE,PASS :: ReadMatrixData
     PROCEDURE,PASS :: WriteSingleData
     PROCEDURE,PASS :: WriteArrayData
     PROCEDURE,PASS :: WriteMatrixData
     PROCEDURE,PASS :: Rewind
     PROCEDURE,PASS :: GetPosition
     GENERIC        :: ReadData     => ReadSingleData  , &
                                       ReadArrayData   , &
                                       ReadMatrixData  
     GENERIC        :: WriteData    => WriteSingleData , &
                                       WriteArrayData  , &
                                       WriteMatrixData     
  END TYPE FortBinFileType

  
  ! -------------------------------------------------------------
  ! --- MISC. ENTITIES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                   :: ModNameLen = 24
  CHARACTER(LEN=ModNameLen),PARAMETER :: ModName    = 'Class_FortBinaryFileType'

          
        

CONTAINS

    
    

! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***** CONSTRUCTOR
! ******************************************************************
! ******************************************************************
! ******************************************************************
    
  ! -------------------------------------------------------------
  ! --- NEW FORTRAN BINARY FILE
  ! -------------------------------------------------------------
  SUBROUTINE New_FortBinFile(ThisFile,FileName,lInputFile,AccessType,FileOpenCode,iStat) 
    CLASS(FortBinFileType)               :: ThisFile
    CHARACTER(LEN=*),INTENT(IN)          :: FileName
    LOGICAL,INTENT(IN)                   :: lInputFile
    CHARACTER(LEN=*),OPTIONAL,INTENT(IN) :: AccessType  !Never used for Fortran binary files
    INTEGER,OPTIONAL,INTENT(OUT)         :: FileOpenCode
    INTEGER,INTENT(OUT)                  :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+15),PARAMETER :: ThisProcedure = ModName // 'New_FortBinFile'
    CHARACTER                              :: StatusJargon*7,ActionJargon*9,cErrMessage*500
    INTEGER                                :: ErrorCode
    
    !Initialize
    iStat = 0
    IF (PRESENT(FileOpenCode)) FileOpenCode = 0
    
    !Instantiate the base file
    CALL ThisFile%NewBaseFile(FileName)

    !Specify the OPEN command arguments
    SELECT CASE (lInputFile)
      CASE (.TRUE.)
        StatusJargon = 'OLD'
        ActionJargon = 'READ'
      CASE (.FALSE.)
        StatusJargon = 'UNKNOWN'
        ActionJargon = 'READWRITE'
    END SELECT
      
    !Open file
    OPEN (UNIT=ThisFile%UnitN,FILE=TRIM(ThisFile%Name),ACCESS='STREAM',FORM='UNFORMATTED',STATUS=StatusJargon,ACTION=ActionJargon,IOSTAT=ErrorCode,IOMSG=cErrMessage)

    !Implement actions based on error code
    IF (ErrorCode .NE. 0) THEN
        IF (PRESENT(FileOpenCode)) THEN
            FileOpenCode = ErrorCode
            RETURN
        ELSE
            CALL SetLastMessage('Error in opening file '//TRIM(ThisFile%Name)//'!'//f_cLineFeed//TRIM(cErrMessage),f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
    END IF

  END SUBROUTINE New_FortBinFile

  
  

! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***** DESTRUCTOR
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- CLOSE FORTRAN BINARY FILE
  ! -------------------------------------------------------------
  SUBROUTINE Kill_FortBinFile(ThisFile,Status)
    CLASS(FortBinFileType)               :: ThisFile
    CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: Status

    !Local variables
    CHARACTER(LEN=ModNAmeLen+16),PARAMETER :: ThisProcedure = ModName // 'Kill_FortBinFile'
    CHARACTER                              :: LocalStatus*6,cErrMessage*500
    INTEGER                                :: iIteration,ErrorCode
    
    IF (PRESENT(Status)) THEN
        LocalStatus = Status
    ELSE
        LocalStatus = 'KEEP'
    END IF

    !Try to close file 3 times (in certain cases the file is not closed with one try)
    DO iIteration=1,3
        !Close file
        CLOSE(ThisFile%UnitN,IOSTAT=ErrorCode,STATUS=LocalStatus,IOMSG=cErrMessage)
        
        !Exit if succesful
        IF (ErrorCode .EQ. 0) EXIT

    END DO
    
    !Kill the base file
    CALL ThisFile%KillBaseFile()

  END SUBROUTINE Kill_FortBinFile
  
  
  
  
! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***** DATA READERS
! ******************************************************************
! ******************************************************************
! ******************************************************************
  
  ! -------------------------------------------------------------
  ! --- READ SINGLE DATA
  ! -------------------------------------------------------------
  SUBROUTINE ReadSingleData(ThisFile,Data,iPos,iStat)
    CLASS(FortBinFileType)              :: ThisFile
    CLASS(*),INTENT(OUT)                :: Data
    INTEGER(KIND=8),OPTIONAL,INTENT(IN) :: iPos
    INTEGER,INTENT(OUT)                 :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNAmeLen+14),PARAMETER :: ThisProcedure = ModName // 'ReadSingleData'
    INTEGER                                :: ErrorCode
    
    !Initialize
    iStat = 0
    
    SELECT TYPE(Data)
        TYPE IS (CHARACTER(LEN=*))
            IF (PRESENT(iPos)) THEN
                READ (ThisFile%UnitN,IOSTAT=ErrorCode,POS=iPos) Data
            ELSE
                READ (ThisFile%UnitN,IOSTAT=ErrorCode) Data
            END IF
            
            
        TYPE IS (REAL(8))
            IF (PRESENT(iPos)) THEN
                READ (ThisFile%UnitN,IOSTAT=ErrorCode,POS=iPos) Data
            ELSE
                READ (ThisFile%UnitN,IOSTAT=ErrorCode) Data
            END IF

            
        TYPE IS (REAL)
            IF (PRESENT(iPos)) THEN
                READ (ThisFile%UnitN,IOSTAT=ErrorCode,POS=iPos) Data
            ELSE
                READ (ThisFile%UnitN,IOSTAT=ErrorCode) Data
            END IF

            
        TYPE IS (INTEGER)
            IF (PRESENT(iPos)) THEN
                READ (ThisFile%UnitN,IOSTAT=ErrorCode,POS=iPos) Data
            ELSE
                READ (ThisFile%UnitN,IOSTAT=ErrorCode) Data
            END IF
            
            
        TYPE IS (LOGICAL)
            IF (PRESENT(iPos)) THEN
                READ (ThisFile%UnitN,IOSTAT=ErrorCode,POS=iPos) Data
            ELSE
                READ (ThisFile%UnitN,IOSTAT=ErrorCode) Data
            END IF

            
        CLASS DEFAULT
            CALL SetLastMessage('Trying to read unrecognized data type from file '//ThisFile%Name//'!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
            
    END SELECT 
    
    CALL ThisFile%IOStatHandler(ErrorCode,iStat=iStat)

  END SUBROUTINE ReadSingleData
  
  
  ! -------------------------------------------------------------
  ! --- READ ARRAY DATA
  ! -------------------------------------------------------------
  SUBROUTINE ReadArrayData(ThisFile,Data,iPos,iStat)
    CLASS(FortBinFileType)              :: ThisFile
    CLASS(*),INTENT(OUT)                :: Data(:)
    INTEGER(KIND=8),OPTIONAL,INTENT(IN) :: iPos
    INTEGER,INTENT(OUT)                 :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+13),PARAMETER :: ThisProcedure = ModName // 'ReadArrayData'
    INTEGER                                :: ErrorCode,iSize
    
    !Initialize
    iStat = 0
    iSize = SIZE(Data)
    IF (iSize .EQ. 0) RETURN
    
    SELECT TYPE(Data)
        TYPE IS (CHARACTER(LEN=*))
            IF (PRESENT(iPos)) THEN
                READ (ThisFile%UnitN,IOSTAT=ErrorCode,POS=iPos) Data
            ELSE
                READ (ThisFile%UnitN,IOSTAT=ErrorCode) Data
            END IF
            
        TYPE IS (REAL(8))
            IF (PRESENT(iPos)) THEN
                READ (ThisFile%UnitN,IOSTAT=ErrorCode,POS=iPos) Data 
            ELSE
                READ (ThisFile%UnitN,IOSTAT=ErrorCode) Data 
            END IF
            
            
        TYPE IS (REAL)
            IF (PRESENT(iPos)) THEN
                READ (ThisFile%UnitN,IOSTAT=ErrorCode,POS=iPos) Data
            ELSE
                READ (ThisFile%UnitN,IOSTAT=ErrorCode) Data
            END IF
            
            
        TYPE IS (INTEGER)
            IF (PRESENT(iPos)) THEN
                READ (ThisFile%UnitN,IOSTAT=ErrorCode,POS=iPos) Data
            ELSE
                READ (ThisFile%UnitN,IOSTAT=ErrorCode) Data
            END IF
            
            
        TYPE IS (LOGICAL)
            IF (PRESENT(iPos)) THEN
                READ (ThisFile%UnitN,IOSTAT=ErrorCode,POS=iPos) Data
            ELSE
                READ (ThisFile%UnitN,IOSTAT=ErrorCode) Data
            END IF
        
            
        CLASS DEFAULT
            CALL SetLastMessage('Trying to read unrecognized data type from file '//ThisFile%Name//'!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
            
    END SELECT 
        
    IF (ErrorCode .NE. 0) CALL ThisFile%IOStatHandler(ErrorCode,iStat=iStat)
        
  END SUBROUTINE ReadArrayData
  
  
  ! -------------------------------------------------------------
  ! --- READ MATRIX DATA
  ! -------------------------------------------------------------
  SUBROUTINE ReadMatrixData(ThisFile,Data,iPos,iStat)
    CLASS(FortBinFileType)              :: ThisFile
    CLASS(*),INTENT(OUT)                :: Data(:,:)
    INTEGER(KIND=8),OPTIONAL,INTENT(IN) :: iPos
    INTEGER,INTENT(OUT)                 :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+14),PARAMETER :: ThisProcedure = ModName // 'ReadMatrixData'
    INTEGER                                :: ErrorCode
    
    !Initialize
    iStat = 0

    SELECT TYPE(Data)
        TYPE IS (REAL(8))
            IF (PRESENT(iPos)) THEN
                READ (ThisFile%UnitN,IOSTAT=ErrorCode,POS=iPos) Data(:,1)
                CALL ThisFile%IOStatHandler(ErrorCode,iStat=iStat)  ;  IF (iStat .EQ. -1) RETURN
                READ (ThisFile%UnitN,IOSTAT=ErrorCode) Data(:,2:)
                CALL ThisFile%IOStatHandler(ErrorCode,iStat=iStat)  ;  IF (iStat .EQ. -1) RETURN
            ELSE
                READ (ThisFile%UnitN,IOSTAT=ErrorCode) Data
                CALL ThisFile%IOStatHandler(ErrorCode,iStat=iStat)  
                IF (iStat .EQ. -1) RETURN
            END IF
       
            
        TYPE IS (REAL)
            IF (PRESENT(iPos)) THEN
                READ (ThisFile%UnitN,IOSTAT=ErrorCode,POS=iPos) Data(:,1)
                CALL ThisFile%IOStatHandler(ErrorCode,iStat=iStat)  ;  IF (iStat .EQ. -1) RETURN
                READ (ThisFile%UnitN,IOSTAT=ErrorCode) Data(:,2:)
                CALL ThisFile%IOStatHandler(ErrorCode,iStat=iStat)  ;  IF (iStat .EQ. -1) RETURN
            ELSE
                READ (ThisFile%UnitN,IOSTAT=ErrorCode) Data
                CALL ThisFile%IOStatHandler(ErrorCode,iStat=iStat)  
                IF (iStat .EQ. -1) RETURN
            END IF
       
            
        TYPE IS (INTEGER)
            IF (PRESENT(iPos)) THEN
                READ (ThisFile%UnitN,IOSTAT=ErrorCode,POS=iPos) Data(:,1)
                CALL ThisFile%IOStatHandler(ErrorCode,iStat=iStat)  ;  IF (iStat .EQ. -1) RETURN
                READ (ThisFile%UnitN,IOSTAT=ErrorCode) Data(:,2:)
                CALL ThisFile%IOStatHandler(ErrorCode,iStat=iStat)  ;  IF (iStat .EQ. -1) RETURN
            ELSE
                READ (ThisFile%UnitN,IOSTAT=ErrorCode) Data
                CALL ThisFile%IOStatHandler(ErrorCode,iStat=iStat)  
                IF (iStat .EQ. -1) RETURN
            END IF

            
        TYPE IS (LOGICAL)
            IF (PRESENT(iPos)) THEN
                READ (ThisFile%UnitN,IOSTAT=ErrorCode,POS=iPos) Data(:,1)
                CALL ThisFile%IOStatHandler(ErrorCode,iStat=iStat)  ;  IF (iStat .EQ. -1) RETURN
                READ (ThisFile%UnitN,IOSTAT=ErrorCode) Data(:,2:)
                CALL ThisFile%IOStatHandler(ErrorCode,iStat=iStat)  ;  IF (iStat .EQ. -1) RETURN
            ELSE
                READ (ThisFile%UnitN,IOSTAT=ErrorCode) Data
                CALL ThisFile%IOStatHandler(ErrorCode,iStat=iStat)  
                IF (iStat .EQ. -1) RETURN
            END IF

            
        CLASS DEFAULT
            CALL SetLastMessage('Trying to read unrecognized data type from file '//ThisFile%Name//'!',f_iFatal,ThisProcedure)
            iStat = -1
            
    END SELECT 
    
  END SUBROUTINE ReadMatrixData
  
  

  
! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***** DATA WRITERS
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- WRITE SINGLE DATA
  ! -------------------------------------------------------------
  SUBROUTINE WriteSingleData(ThisFile,Data)
    CLASS(FortBinFileType) :: ThisFile
    CLASS(*),INTENT(IN)    :: Data

    !Local variables
    CHARACTER(LEN=ModNameLen+15),PARAMETER :: ThisProcedure = ModName // 'WriteSingleData'
    
    SELECT TYPE (Data)
        TYPE IS (CHARACTER(LEN=*))
            WRITE (ThisFile%UnitN) Data
        
        TYPE IS (REAL(8))
            WRITE (ThisFile%UnitN) Data
            
        TYPE IS (REAL)
            WRITE (ThisFile%UnitN) Data
            
        TYPE IS (INTEGER)
            WRITE (ThisFile%UnitN) Data
            
        TYPE IS (LOGICAL)
            WRITE (ThisFile%UnitN) Data
            
        TYPE IS (GenericString)
            WRITE (ThisFile%UnitN) GenericString_To_String(Data)

        CLASS DEFAULT
            CALL LogMessage('Trying to write unrecognized data type to file '//ThisFile%Name//'!',f_iWarn,ThisProcedure) 
            RETURN

    END SELECT
         
  END SUBROUTINE WriteSingleData

  
  ! -------------------------------------------------------------
  ! --- WRITE ARRAY DATA
  ! -------------------------------------------------------------
  SUBROUTINE WriteArrayData(ThisFile,Data)
    CLASS(FortBinFileType) :: ThisFile
    CLASS(*),INTENT(IN)    :: Data(:)

    !Local variables
    CHARACTER(LEN=ModNameLen+14),PARAMETER :: ThisProcedure = ModName // 'WriteArrayData'
    INTEGER                                :: indx
    
    SELECT TYPE (Data)
        TYPE IS (CHARACTER(LEN=*))
            WRITE (ThisFile%UnitN) Data
            
        TYPE IS (REAL(8))
            WRITE (ThisFile%UnitN) Data
            
        TYPE IS (REAL)
            WRITE (ThisFile%UnitN) Data
            
        TYPE IS (INTEGER)
            WRITE (ThisFile%UnitN) Data
            
        TYPE IS (LOGICAL)
            WRITE (ThisFile%UnitN) Data
            
        TYPE IS (GenericString)
            DO indx=1,SIZE(Data)
                WRITE (ThisFile%UnitN) GenericString_To_String(Data(indx))
            END DO
            
        CLASS DEFAULT
            CALL LogMessage('Trying to write unrecognized data type to file '//ThisFile%Name//'!',f_iWarn,ThisProcedure)

        END SELECT


  END SUBROUTINE WriteArrayData
  

  ! -------------------------------------------------------------
  ! --- WRITE MATRIX DATA
  ! -------------------------------------------------------------
  SUBROUTINE WriteMatrixData(ThisFile,Data)
    CLASS(FortBinFileType) :: ThisFile
    CLASS(*),INTENT(IN)    :: Data(:,:)

    !Local variables
    CHARACTER(LEN=ModNameLen+15),PARAMETER :: ThisProcedure = 'WriteMatrixData'
    
    SELECT TYPE (Data)        
        TYPE IS (REAL(8))
            WRITE (ThisFile%UnitN) Data
            
            
        TYPE IS (REAL)
            WRITE (ThisFile%UnitN) Data
            
            
        TYPE IS (INTEGER)
            WRITE (ThisFile%UnitN) Data
            
            
        TYPE IS (LOGICAL)
            WRITE (ThisFile%UnitN) Data
            
            
        CLASS DEFAULT
            CALL LogMessage('Trying to write unrecognized data type to file '//ThisFile%Name//'!',f_iWarn,ThisProcedure) 
            RETURN
            
    END SELECT
         
  END SUBROUTINE WriteMatrixData
  
  
  
  
! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***** MISC. METHODS
! ******************************************************************
! ******************************************************************
! ******************************************************************
 
  ! -------------------------------------------------------------
  ! --- REWIND BINARY FILE
  ! -------------------------------------------------------------
  SUBROUTINE Rewind(ThisFile)
    CLASS(FortBinFileType) :: ThisFile
    
    !Rewind the file
    REWIND(ThisFile%UnitN)

  END SUBROUTINE Rewind
  
  
  ! -------------------------------------------------------------
  ! --- GET POSITION IN A BINARY FILE
  ! -------------------------------------------------------------
  FUNCTION GetPosition(ThisFile) RESULT(iPos)
    CLASS(FortBinFileType),INTENT(IN) :: ThisFile
    INTEGER(KIND=8)                   :: iPos
    
    IF (.NOT. ALLOCATED(ThisFile%Name)) THEN
      iPos = -1
    ELSE
      INQUIRE (UNIT=ThisFile%UnitN,POS=iPos)
    END IF
    
  END FUNCTION GetPosition
   

END MODULE