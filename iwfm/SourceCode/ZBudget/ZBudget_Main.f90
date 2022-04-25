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
PROGRAM ZBudget
  USE GeneralUtilities  , ONLY: GetDate               , &
                                GetTime
  USE IOInterface       , ONLY: GenericFileType
  USE MessageLogger     , ONLY: SetLogFileName        , &
                                SetFlagToEchoProgress , &
                                LogMessage            , &
                                LogLastMessage        , &
                                MessageArray          , &
                                f_iMessage            , &
                                f_iYesEchoProgress    , &
                                f_iFILE                  
  USE ProgramTimer      , ONLY: StartTimer               
  USE IWFM_Core_Version , ONLY: IWFM_Core
  USE ZBudgetControls   , ONLY: ProcessZBudgets       , &
                                EndExecution
  IMPLICIT NONE

  !Local variables
  TYPE(GenericFileType) :: StandardOutputFile
  INTEGER               :: iStat

  !Start timer
  CALL StartTimer()
  
  !Set flag to echo progress
  CALL SetFlagToEchoProgress(f_iYesEchoProgress,iStat)
  IF (iStat .EQ. -1) CALL EndExecution(StandardOutputFile,iStat)

  !Refresh the standard output file
  CALL StandardOutputFile%New(FileName='ZBudgetMessages.out',InputFile=.FALSE.,iStat=iStat)  ;  IF (iStat .EQ. -1) CALL EndExecution(StandardOutputFile,iStat)
  CALL StandardOutputFile%Kill(Status='DELETE')
  CALL SetLogFileName('ZBudgetMessages.out',iStat)  ;  IF (iStat .EQ. -1) CALL EndExecution(StandardOutputFile,iStat)

  !Print-out date and time of the execution
  MessageArray(1) = 'PROGRAM: IWFM Z-Budget Post-Processor ' // TRIM(IWFM_Core%GetVersion())
  MessageArray(2) = 'This run is made on '//TRIM(GetDate())//' at '//TRIM(GetTime())
  MessageArray(3) = ''
  CALL LogMessage(MessageArray(1:3),f_iMessage,'',Destination=f_iFILE)

  !Read in the main control data
  CALL ProcessZBudgets('',0,iStat)  

  !Stop timer, print run-time and close files
  CALL EndExecution(StandardOutputFile,iStat)

END 