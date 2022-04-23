!***********************************************************************
!  Integrated Water Flow Model (IWFM)
!  Copyright (C) 2005-2018
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
  USE MessageLogger     , ONLY: LogMessage  , &
                                SCREEN      , &
                                iMessage
  USE Class_Version
  USE GeneralUtilities
  IMPLICIT NONE

  
  PRIVATE
  PUBLIC::PRINT_SCREEN  ,&
          GET_MAIN_FILE


! **********************************************************************
! ***** DATA FOR OPENING SCREEN
! **********************************************************************
  INTEGER,PARAMETER::OPEN_SCREEN_LINE_LENGTH=56          !Length of each line of openning screen
  INTEGER,PARAMETER::ProgramNameLineNumber=4             !The line number of openning screen which will be replaced by the program name
  INTEGER,PARAMETER::VersionLineNumber=6                 !The line number where the IWFM version number is displayed
  INTEGER,PARAMETER::CopyrightLineNumber=7               !Line number where copyright date is displayed
  CHARACTER(OPEN_SCREEN_LINE_LENGTH),DIMENSION(44):: &   !The text (line by line) of openning screen
     L=(/'ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»' ,&
         'º             Integrated Water Flow Model              º' ,&
         'º                       IWFM                           º' ,&
       !------------------------------------------------------------------
         'ºTHIS PART OF ARRAY IS OVERWRITEN BY INDIVIDUAL PROGRAMº' ,&
       !------------------------------------------------------------------                                                
         'ÇÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¶' ,&
       !------------------------------------------------------------------
         'º  THIS PART OF ARRAY IS OVERWRITEN BY VERSION NUMBER  º' ,&
       !------------------------------------------------------------------
         'º  THIS PART OF ARRAY IS OVERWRITEN BY COPYRIGHT DATE  º' ,&
       !------------------------------------------------------------------
         'º  State of California, Department of Water Resources  º' ,&
         'ºÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¶' ,&
         'º This program is free software; you can redistribute  º' ,& 
         'º it and/or modify it under the terms of the GNU       º' ,&
         'º General Public License as published by the Free      º' ,&
         'º Software Foundation; either version 2 of the         º' ,&
         'º License or (at your option) any later version.       º' ,&
         'º                                                      º' ,&
         'º This program is distributed in the hope that it      º' ,&
         'º will be useful, but WITHOUT ANY WARRANTY; without    º' ,&
         'º even the implied warranty of MERCHANTABILITY or      º' ,&
         'º FITNESS FOR A PARTICULAR PURPOSE.                    º' ,&
         'º See the GNU General Public License for more details. º' ,&
         'º (http://www.gnu.org/copyleft/gpl.html)               º' ,&
         'º                                                      º' ,&
         'º You should have received a copy of the GNU           º' ,&
         'º General Public License along with this program;      º' ,&
         'º if not, write to the                                 º' ,&
         'º Free Software Foundation, Inc.,                      º' ,&
         'º 59 Temple Place - Suite 330, Boston, MA              º' ,&
         'º                      02111-1307, USA.                º' ,&
         'º                                                      º' ,&
         'º For technical support, e-mail:                       º' ,&
         'º IWFMtechsupport@water.ca.gov                         º' ,&
         'º                                                      º' ,&
         'º   Principal Contact:                                 º' ,&
         'º       Tariq N. Kadir, PE .... Senior Engineer, DWR   º' ,&
         'º                  (916) 653 3513                      º' ,&
         'º                  kadir@water.ca.gov                  º' ,&
         'º                                                      º' ,&
         'º   Principal Programmer and Technical Support:        º' ,&
         'º       Dr. Emin Can Dogrul ... Engineer, DWR          º' ,&
         'º                  (916) 654 7018                      º' ,&
         'º                  dogrul@water.ca.gov                 º' ,&
         'º                                                      º' ,&
         'º                                                      º' ,&
         'ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼'/)
         


CONTAINS


  ! -------------------------------------------------------------
  ! --- SUBROUTINE THAT PRINTS OUT THE OPENING SCREEN
  ! -------------------------------------------------------------
  SUBROUTINE PRINT_SCREEN(PNAME_IN,Version)
    CHARACTER(LEN=*),INTENT(IN)   :: PNAME_IN
    CLASS(VersionType),INTENT(IN) :: Version

    !Local variables
    CHARACTER(LEN=10) :: TodaysDate
  
    !Prepare the program name line for print out
    L(ProgramNameLineNumber)=ArrangeText(PNAME_IN,OPEN_SCREEN_LINE_LENGTH)
    L(ProgramNameLineNumber)(1:1)='º'
    L(ProgramNameLineNumber)(OPEN_SCREEN_LINE_LENGTH:OPEN_SCREEN_LINE_LENGTH)='º'
     
    !Prepare the version number line for print out
    L(VersionLineNumber)=ArrangeText('Version: '//TRIM(ADJUSTL(Version%GetVersion())),OPEN_SCREEN_LINE_LENGTH)
    L(VersionLineNumber)(1:1)='º'
    L(VersionLineNumber)(OPEN_SCREEN_LINE_LENGTH:OPEN_SCREEN_LINE_LENGTH)='º'
    
    !Prepare copyright line
    TodaysDate             = GetDate()
    L(CopyrightLineNumber) = ArrangeText('Copyright (C) 2005-'//TodaysDate(7:10),OPEN_SCREEN_LINE_LENGTH)
    L(CopyrightLineNumber)(1:1) = 'º'
    L(CopyrightLineNumber)(OPEN_SCREEN_LINE_LENGTH:OPEN_SCREEN_LINE_LENGTH) = 'º'
    

    !Display opening screen
    CALL LogMessage(L,iMessage,'',Destination=SCREEN,Fmt='(8X,A)')

  END SUBROUTINE PRINT_SCREEN


  ! -------------------------------------------------------------
  ! --- SUBROUTINE THAT READS AND OPENS MAIN CONTROL FILE NAME
  ! -------------------------------------------------------------
  SUBROUTINE GET_MAIN_FILE(MFILE)
    CHARACTER(LEN=*),INTENT(OUT) :: MFILE

    !Local variables
    INTEGER :: NArguments

    NArguments = COMMAND_ARGUMENT_COUNT()
    SELECT CASE (NArguments)
      !No extra arguments are specified; ask for file name
      CASE (0)
        CALL LogMessage(' ',iMessage,'',Destination=SCREEN)
        CALL LogMessage(' Enter the Name of the Main Input File >  ',iMessage,'',Destination=SCREEN,Advance='NO')
        READ (*,*) MFILE
        CALL CleanSpecialCharacters(MFILE)
        MFILE = ADJUSTL(MFILE)

      !Extra argument is specified
      CASE (1)
        CALL GET_COMMAND_ARGUMENT(1,MFILE)
        MFILE = ADJUSTL(MFILE)
        !If only the informational page is required 
        IF (LowerCase(TRIM(MFILE)) .EQ. '-about') MFILE='-about'
        
    END SELECT

  END SUBROUTINE GET_MAIN_FILE
  
  
END MODULE 