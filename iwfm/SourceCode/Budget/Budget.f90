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
PROGRAM BUDGET
  USE MessageLogger    , ONLY: SetLogFileName , &
                               LogMessage     , &
                               LogLastMessage , &
                               PrintRunTime   , &
                               MessageArray   , &
                               f_iMessage     , &
                               f_iFILE        
  USE IWFM_Core_Version
  USE BudgetControls
  USE GeneralUtilities
  USE ProgramTimer     , ONLY: StartTimer
  USE IOInterface
  IMPLICIT NONE

  !Local variables
  TYPE(GenericFileType) :: DummyFile !Dummy file to re-set the standard output file
  INTEGER               :: iStat
  
  !Start CPU timer
  CALL StartTimer()

  !Refresh the standard output file
  CALL DummyFile%New(FileName='BudgetMessages.out',InputFile=.FALSE.,iStat=iStat)
  IF (iStat .EQ. -1) CALL EndExecution(iStat)
  CALL DummyFile%Kill(Status='DELETE')

  !Set the file for messages
  CALL SetLogFileName('BudgetMessages.out',iStat)
  IF (iStat .EQ. -1) CALL EndExecution(iStat)
  
  !Print-out date and time of the execution
  MessageArray(1) = 'PROGRAM: IWFM Budget Post-Processor ' // TRIM(IWFM_Core%GetVersion())
  MessageArray(2) = 'This run is made on '//TRIM(GetDate())//' at '//TRIM(GetTime())
  MessageArray(3) = ''
  CALL LogMessage(MessageArray(1:3),f_iMessage,'',Destination=f_iFILE)


  !Print budget tables
  CALL PrintBudgetTables(iStat=iStat)
  
  !End execution
  CALL EndExecution(iStat)

END
