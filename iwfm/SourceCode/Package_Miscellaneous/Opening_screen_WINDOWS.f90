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
MODULE Opening_screen
  USE MessageLogger     , ONLY: LogMessage             , &
                                f_iSCREEN              , &
                                f_iMessage               
  USE Class_Version     , ONLY: VersionType            
  USE GeneralUtilities  , ONLY: ArrangeText            , &
                                LowerCase              , &
                                CleanSpecialCharacters , &
                                GetDate
  IMPLICIT NONE

  
  PRIVATE
  PUBLIC :: PRINT_SCREEN  ,&
            GET_MAIN_FILE


! **********************************************************************
! ***** DATA FOR OPENING SCREEN
! **********************************************************************
  INTEGER,PARAMETER :: f_OPEN_SCREEN_LINE_LENGTH=56          !Length of each line of openning screen
  INTEGER,PARAMETER :: f_ProgramNameLineNumber=4             !The line number of openning screen which will be replaced by the program name
  INTEGER,PARAMETER :: f_VersionLineNumber=6                 !The line number where the IWFM version number is displayed
  INTEGER,PARAMETER :: f_CopyrightLineNumber=7               !Line number where copyright date is displayed
         


CONTAINS


        
  ! -------------------------------------------------------------
  ! --- SUBROUTINE THAT PRINTS OUT THE OPENING SCREEN IN WINDOWS
  ! -------------------------------------------------------------
  SUBROUTINE PRINT_SCREEN(PNAME_IN,Version)
    CHARACTER(LEN=*),INTENT(IN)   :: PNAME_IN
    CLASS(VersionType),INTENT(IN) :: Version

    !Local variables
    INTEGER,PARAMETER                    :: f_RepeatCount_2 = f_OPEN_SCREEN_LINE_LENGTH-2 
    CHARACTER(LEN=10)                    :: TodaysDate
    CHARACTER(f_OPEN_SCREEN_LINE_LENGTH) ::                                                          &   !The text (line by line) of opening screen
       f_L(44) =[CHAR(201) //            REPEAT(CHAR(205),f_RepeatCount_2)             // CHAR(187) ,&
                 CHAR(186) // '             Integrated Water Flow Model              ' // CHAR(186) ,&
                 CHAR(186) // '                       IWFM                           ' // CHAR(186) ,&
                 CHAR(186) // 'THIS PART OF ARRAY IS OVERWRITEN BY INDIVIDUAL PROGRAM' // CHAR(186) ,&
                 CHAR(199) //            REPEAT(CHAR(196),f_RepeatCount_2)             // CHAR(182) ,&
                 CHAR(186) // '  THIS PART OF ARRAY IS OVERWRITEN BY VERSION NUMBER  ' // CHAR(186) ,&
                 CHAR(186) // '  THIS PART OF ARRAY IS OVERWRITEN BY COPYRIGHT DATE  ' // CHAR(186) ,&
                 CHAR(186) // '  State of California, Department of Water Resources  ' // CHAR(186) ,&
                 CHAR(199) //            REPEAT(CHAR(196),f_RepeatCount_2)             // CHAR(182) ,&
                 CHAR(186) // ' This program is free software; you can redistribute  ' // CHAR(186) ,& 
                 CHAR(186) // ' it and/or modify it under the terms of the GNU       ' // CHAR(186) ,&
                 CHAR(186) // ' General Public License as published by the Free      ' // CHAR(186) ,&
                 CHAR(186) // ' Software Foundation; either version 2 of the         ' // CHAR(186) ,&
                 CHAR(186) // ' License or (at your option) any later version.       ' // CHAR(186) ,&
                 CHAR(186) // '                                                      ' // CHAR(186) ,&
                 CHAR(186) // ' This program is distributed in the hope that it      ' // CHAR(186) ,&
                 CHAR(186) // ' will be useful, but WITHOUT ANY WARRANTY; without    ' // CHAR(186) ,&
                 CHAR(186) // ' even the implied warranty of MERCHANTABILITY or      ' // CHAR(186) ,&
                 CHAR(186) // ' FITNESS FOR A PARTICULAR PURPOSE.                    ' // CHAR(186) ,&
                 CHAR(186) // ' See the GNU General Public License for more details. ' // CHAR(186) ,&
                 CHAR(186) // ' (http://www.gnu.org/copyleft/gpl.html)               ' // CHAR(186) ,&
                 CHAR(186) // '                                                      ' // CHAR(186) ,&
                 CHAR(186) // ' You should have received a copy of the GNU           ' // CHAR(186) ,&
                 CHAR(186) // ' General Public License along with this program;      ' // CHAR(186) ,&
                 CHAR(186) // ' if not, write to the                                 ' // CHAR(186) ,&
                 CHAR(186) // ' Free Software Foundation, Inc.,                      ' // CHAR(186) ,&
                 CHAR(186) // ' 59 Temple Place - Suite 330, Boston, MA              ' // CHAR(186) ,&
                 CHAR(186) // '                      02111-1307, USA.                ' // CHAR(186) ,&
                 CHAR(186) // '                                                      ' // CHAR(186) ,&
                 CHAR(186) // ' For technical support, e-mail:                       ' // CHAR(186) ,&
                 CHAR(186) // ' IWFMtechsupport@water.ca.gov                         ' // CHAR(186) ,&
                 CHAR(186) // '                                                      ' // CHAR(186) ,&
                 CHAR(186) // '   Principal Contact:                                 ' // CHAR(186) ,&
                 CHAR(186) // '       Dr. Tariq N. Kadir ... Senior Engineer, DWR    ' // CHAR(186) ,&
                 CHAR(186) // '                  (916) 653 3513                      ' // CHAR(186) ,&
                 CHAR(186) // '                  kadir@water.ca.gov                  ' // CHAR(186) ,&
                 CHAR(186) // '                                                      ' // CHAR(186) ,&
                 CHAR(186) // '   Principal Programmer and Technical Support:        ' // CHAR(186) ,&
                 CHAR(186) // '       Dr. Emin Can Dogrul ... Senior Engineer, DWR   ' // CHAR(186) ,&
                 CHAR(186) // '                  (916) 654 7018                      ' // CHAR(186) ,&
                 CHAR(186) // '                  dogrul@water.ca.gov                 ' // CHAR(186) ,&
                 CHAR(186) // '                                                      ' // CHAR(186) ,&
                 CHAR(186) // '                                                      ' // CHAR(186) ,&
                 CHAR(200) //            REPEAT(CHAR(205),f_RepeatCount_2)             // CHAR(188) ]  
    
    !Prepare the program name line for print out
    f_L(f_ProgramNameLineNumber)                                                      = ArrangeText(PNAME_IN,f_OPEN_SCREEN_LINE_LENGTH)
    f_L(f_ProgramNameLineNumber)(1:1)                                                 = CHAR(186)
    f_L(f_ProgramNameLineNumber)(f_OPEN_SCREEN_LINE_LENGTH:f_OPEN_SCREEN_LINE_LENGTH) = CHAR(186)
     
    !Prepare the version number line for print out
    f_L(f_VersionLineNumber)                                                      = ArrangeText('Version: '//TRIM(ADJUSTL(Version%GetVersion())),f_OPEN_SCREEN_LINE_LENGTH)
    f_L(f_VersionLineNumber)(1:1)                                                 = CHAR(186)
    f_L(f_VersionLineNumber)(f_OPEN_SCREEN_LINE_LENGTH:f_OPEN_SCREEN_LINE_LENGTH) = CHAR(186)
    
    !Prepare copyright line
    TodaysDate                                                                      = GetDate()
    f_L(f_CopyrightLineNumber)                                                      = ArrangeText('Copyright (C) 2005-'//TodaysDate(7:10),f_OPEN_SCREEN_LINE_LENGTH)
    f_L(f_CopyrightLineNumber)(1:1)                                                 = CHAR(186)
    f_L(f_CopyrightLineNumber)(f_OPEN_SCREEN_LINE_LENGTH:f_OPEN_SCREEN_LINE_LENGTH) = CHAR(186)
    
    !Display opening screen
    CALL LogMessage(f_L,f_iMessage,'',Destination=f_iSCREEN,Fmt='(8X,A)')

  END SUBROUTINE PRINT_SCREEN


  ! -------------------------------------------------------------
  ! --- SUBROUTINE THAT READS AND OPENS MAIN CONTROL FILE NAME
  ! -------------------------------------------------------------
  SUBROUTINE GET_MAIN_FILE(cPrompt,MFILE,MFile_Optional)
    CHARACTER(LEN=*),INTENT(IN)           :: cPrompt
    CHARACTER(LEN=*),INTENT(OUT)          :: MFILE
    CHARACTER(LEN=*),OPTIONAL,INTENT(OUT) :: MFile_Optional

    !Local variables
    INTEGER   :: NArguments
    CHARACTER :: cSecondArgument*500

    NArguments = COMMAND_ARGUMENT_COUNT()
    SELECT CASE (NArguments)
      !No extra arguments are specified; ask for file name
      CASE (0)
        CALL LogMessage(' ',f_iMessage,'',Destination=f_iSCREEN)
        CALL LogMessage(cPrompt,f_iMessage,'',Destination=f_iSCREEN,Advance='NO')
        READ (*,*) MFILE
        CALL CleanSpecialCharacters(MFILE)
        MFILE = ADJUSTL(MFILE)
        IF (PRESENT(MFile_Optional)) MFile_Optional = ''

      !1 extra argument is specified
      CASE (1)
        CALL GET_COMMAND_ARGUMENT(1,MFILE)
        MFILE = ADJUSTL(MFILE)
        !If only the informational page is required 
        IF (LowerCase(TRIM(MFILE)) .EQ. '-about') MFILE='-about'
        IF (PRESENT(MFile_Optional)) MFile_Optional = ''
        
      !2 extra arguments are specified
      CASE (2)
        CALL GET_COMMAND_ARGUMENT(1,MFILE)
        MFILE = ADJUSTL(MFILE)
        !If only the informational page is required 
        IF (LowerCase(TRIM(MFILE)) .EQ. '-about') MFILE='-about'
        CALL GET_COMMAND_ARGUMENT(2,cSecondArgument)
        IF (PRESENT(MFile_Optional)) MFile_Optional = ADJUSTL(cSecondArgument)
    END SELECT

  END SUBROUTINE GET_MAIN_FILE
  
  
END MODULE 