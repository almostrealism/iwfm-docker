!***********************************************************************
!  Integrated Water Flow Model (IWFM)
!  Copyright (C) 2005-2022  
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
PROGRAM IWFM_F1
  !$ USE OMP_LIB
  USE ProgramTimer    , ONLY: StartTimer       , &
                              StopTimer
  USE MessageLogger   , ONLY: PrintRunTime     , &
                              SetLogFileName   , &
                              KillLogFile      , &
                              LogLastMessage
  USE Package_Model   , ONLY: ModelType
  IMPLICIT NONE

  !Local variables
  TYPE(ModelType) :: Model
  INTEGER         :: iStat
  
  
  !Set environment for parallel processing
  !$ CALL KMP_SET_BLOCKTIME(0)
  !$ CALL OMP_SET_NUM_THREADS(OMP_GET_NUM_PROCS()-1)
  
  
  !Start timer
  CALL StartTimer()
  
  !Set message log file
  CALL SetLogFileName('PreprocessorMessages.out',iStat)
  IF (iStat .EQ. -1) THEN
      CALL LogLastMessage()
      
  ELSE
      !Instantiate the static component of the model
      CALL Model%New('',lRoutedStreams=.TRUE.,lPrintBinFile=.TRUE.,iStat=iStat)
      IF (iStat .EQ. -1) CALL LogLastMessage()
  END IF
  
  !Print run-time 
  CALL StopTimer()
  CALL PrintRunTime()
  
  !Close message log file
  CALL KillLogFile()

END
