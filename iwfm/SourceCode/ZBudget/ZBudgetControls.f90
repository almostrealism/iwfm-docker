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
MODULE ZBudgetControls
  USE IWFM_Core_Version      , ONLY: IWFM_Core
  USE IWFM_Util_VersionF     , ONLY: IWFM_Util
  USE MessageLogger          , ONLY: LogMessage                         , &
                                     LogLastMessage                     , &
                                     PrintRunTime                       , &
                                     MessageArray                       , &
                                     f_iMessage                         , &
                                     f_iWarn                            , &  
                                     f_iFatal                           , &
                                     f_iSCREEN_FILE                     , &
                                     f_iSCREEN
  USE ProgramTimer           , ONLY: StopTimer           
  USE GeneralUtilities       , ONLY: StripTextUntilCharacter            , &
                                     CleanSpecialCharacters             , &
                                     IntToText                          , &
                                     ShellSort                          , &
                                     LowerCase                          , &
                                     LocateInList                       , &
                                     f_cLineFeed
  USE TimeSeriesUtilities    , ONLY: TimeStepType                       , &
                                     StripTimeStamp                     , &
                                     IsTimeStampValid                   , &
                                     SetCacheLimit                      , &
                                     OPERATOR(.TSGT.)                   , &
                                     f_iTimeStampLength                    
  USE IOInterface            , ONLY: GenericFileType                    , &
                                     iGetFileType_FromName              , &
                                     f_iHDF                                
  USE Package_Discretization , ONLY: Package_Discretization_GetVersion
  USE Package_Misc           , ONLY: Get_Main_File                      , &
                                     Print_Screen                       , &
                                     Package_Misc_GetVersion
  USE Package_Budget         , ONLY: Package_Budget_GetVersion
  USE Package_ZBudget        , ONLY: ZBudgetType                        , &
                                     SystemDataType                     , &
                                     ZoneListType                       , &
                                     Package_ZBudget_GetVersion         , &
                                     f_iZoneHorizontal                  , &
                                     f_iZoneVertical                    , &
                                     f_iUndefinedZone
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
  PUBLIC :: ProcessZBudgets   , &
            EndExecution
  
         
  ! -------------------------------------------------------------
  ! --- MODEL SYSTEM DATA
  ! -------------------------------------------------------------
  TYPE(SystemDataType),SAVE :: SystemData
  
  
  ! -------------------------------------------------------------
  ! --- OUTPUT RELATED DATA
  ! -------------------------------------------------------------  
  REAL(8),SAVE                           :: Fact_AR                , &
                                            Fact_VL               
  CHARACTER(LEN=8),SAVE                  :: cUnit_AR               , &
                                            cUnit_VL
  CHARACTER(LEN=f_iTimeStampLength),SAVE :: cPrintBeginDateAndTime , &
                                            cPrintEndDateAndTime
  
  
  ! -------------------------------------------------------------
  ! --- MISC. VARIABLES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                   :: ModNameLen = 17
  CHARACTER(LEN=ModNameLen),PARAMETER :: ModName    = 'ZBudgetControls::'



CONTAINS



  ! -------------------------------------------------------------
  ! --- READ IN THE MAIN CONTROL DATA
  ! -------------------------------------------------------------
  SUBROUTINE ProcessZBudgets(cFileName,LenFileName,iStat)
    INTEGER,INTENT(IN)                    :: LenFileName
    CHARACTER(LEN=LenFileName),INTENT(IN) :: cFileName
    INTEGER,INTENT(OUT)                   :: iStat

    !Local variables
    CHARACTER(LEN=ModNameLen+18) :: ThisProcedure = ModName // 'GetMainControlData'
    CHARACTER(LEN=1000)          :: cMainFileName,ALine
    TYPE(GenericFileType)        :: MainControlFile
    INTEGER                      :: NZBudget,indxBudget,CacheLimit
    
    !Initialize
    iStat = 0

    !Prompt user for the name of the main input file
    IF (cFileName .NE. '') THEN
        cMainFileName = cFileName
    ELSE
        CALL Print_screen('Program: Z-Budget',IWFM_Core)
        CALL Get_Main_File(' Enter the Name of the Main Input File >  ',cMainFileName)
        IF (TRIM(cMainFileName) .EQ. '-about') THEN
            CALL PrintVersionNumbers()
            STOP
        END IF
    END IF

    !Initialize main control file
    CALL MainControlFile%New(FileName=cMainFileName,InputFile=.TRUE.,Descriptor='Main control input',FileType='TXT',iStat=iStat)  
    IF (iStat .EQ. -1) RETURN

    !Conversion factors and units
    CALL MainControlFile%ReadData(Fact_AR,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL MainControlFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN ; CALL CleanSpecialCharacters(ALine) ; cUnit_AR = ADJUSTL(StripTextUntilCharacter(ALine,'/'))
    CALL MainControlFile%ReadData(Fact_VL,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL MainControlFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN ; CALL CleanSpecialCharacters(ALine) ; cUnit_VL = ADJUSTL(StripTextUntilCharacter(ALine,'/'))
    
    !Cache size
    CALL MainControlFile%ReadData(CacheLimit,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL SetCacheLimit(CacheLimit)
    
    !Print-out begin and end dates
    CALL MainControlFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN  ;  ALine = ADJUSTL(StripTextUntilCharacter(ALine,'/',Back=.TRUE.)) 
    IF (IsTimeStampValid(ALine)) cPrintBeginDateAndTime = StripTimeStamp(ALine)
    CALL MainControlFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN  ;  ALine = ADJUSTL(StripTextUntilCharacter(ALine,'/',Back=.TRUE.)) 
    IF (IsTimeStampValid(ALine)) cPrintEndDateAndTime = StripTimeStamp(ALine)
    IF (cPrintBeginDateAndTime .TSGT. cPrintEndDateAndTime) CALL LogMessage('Print-out end date and time cannot be less than the beginning date and time!',f_iFatal,ThisProcedure)

    !Number of Z-Budgets to process
    CALL MainControlFile%ReadData(NZBudget,iStat)  ;  IF (iStat .EQ. -1) RETURN
    
    !Process Z-Budgets
    DO indxBudget=1,NZBudget
        CALL ProcessZBudget(MainControlFile,iStat)
        IF (iStat .EQ. -1) RETURN
    END DO
        
    !Close main control file
    CALL MainControlFile%Kill()
    
  END SUBROUTINE ProcessZBudgets
  
  
  ! -------------------------------------------------------------
  ! --- PROCESS A ZONE BUDGET
  ! -------------------------------------------------------------
  SUBROUTINE ProcessZBudget(MainControlFile,iStat)
      TYPE(GenericFileType) :: MainControlFile
      INTEGER,INTENT(OUT)   :: iStat
      
      !Local variables
      CHARACTER(LEN=ModNameLen+14),PARAMETER :: ThisProcedure = ModName // 'ProcessZBudget'
      CHARACTER                              :: cHDFFileName*1000,cOutFileName*1000,cPrintInterval*100,cZoneDefFileName*1000
      INTEGER                                :: NZPrint,iZPrint,indxPrint,iLoc,NTimeSteps,ErrorCode
      INTEGER,ALLOCATABLE                    :: iZonesToProcess(:),iTempZonesToProcess(:)
      TYPE(TimeStepType)                     :: TimeStep
      TYPE(ZBudgetType)                      :: ZBudget
      TYPE(ZoneListType)                     :: ZoneList
      
      !Initailize
      iStat = 0
      
      !Read zone definition file name
      CALL MainControlFile%ReadData(cZoneDefFileName,iStat)  ;  IF (iStat .EQ. -1) RETURN  ;  cZoneDefFileName   = ADJUSTL(StripTextUntilCharacter(cZoneDefFileName,'/'))    ;  CALL CleanSpecialCharacters(cZoneDefFileName)

      !If no zone definition file is defined, return
      IF (cZoneDefFileName .EQ. '') RETURN
      
      !Read Z-Budget control data
      CALL MainControlFile%ReadData(cHDFFileName,iStat)  ;  IF (iStat .EQ. -1) RETURN    ;  cHDFFileName   = ADJUSTL(StripTextUntilCharacter(cHDFFileName,'/'))    ;  CALL CleanSpecialCharacters(cHDFFileName)
      CALL MainControlFile%ReadData(cOutFileName,iStat)  ;  IF (iStat .EQ. -1) RETURN    ;  cOutFileName   = ADJUSTL(StripTextUntilCharacter(cOutFileName,'/'))    ;  CALL CleanSpecialCharacters(cOutFileName)
      CALL MainControlFile%ReadData(cPrintInterval,iStat)  ;  IF (iStat .EQ. -1) RETURN  ;  cPrintInterval = ADJUSTL(StripTextUntilCharacter(cPrintInterval,'/'))  ;  CALL CleanSpecialCharacters(cPrintInterval)
      CALL MainControlFile%ReadData(NZPrint,iStat)  ;  IF (iStat .EQ. -1) RETURN
      
      !Return if no zones are to be processed
      IF (NZPrint .EQ. 0) RETURN
      
      !Return if no output file is defined
      IF (cOutFileName .EQ. '') RETURN
      
      !Check that Z-Budget input file is an HDF file and instantate the Z-Budget object
      IF (iGetFileType_FromName(cHDFFileName) .NE. f_iHDF) CALL LogMessage(TRIM(cHDFFileName)//' is not an HDF5 file!',f_iFatal,ThisProcedure)
      CALL ZBudget%New(cHDFFileName,iStat)
      IF (iStat .EQ. -1) RETURN
      
      !If the print interval is provided as empty, set it to the time interval of the data
      IF (cPrintInterval .EQ. '') THEN
          CALL ZBudget%GetTimeStepRelatedData(NTimeSteps,TimeStep)
          cPrintInterval = TimeStep%Unit
      END IF

      !Let the user know
      CALL LogMessage('Processing '//TRIM(LowerCase(ZBudget%Header%cDescriptor)),f_iMessage,'',Destination=f_iSCREEN_FILE)
      
      !Create the zone list
      CALL ZoneList%New(ZBudget%Header%iNData,ZBudget%Header%lFaceFlows_Defined,ZBudget%SystemData,TRIM(cZoneDefFileName),iStat)
      IF (iStat .EQ. -1) RETURN
      
      !Read the zones to be processed
      IF (NZPrint .EQ. 1) THEN
          CALL MainControlFile%ReadData(iZPrint,iStat)  ;  IF (iStat .EQ. -1) RETURN
          IF (iZPrint .EQ. 0) RETURN
          IF (iZPrint .EQ. -1) THEN
              NZPrint = ZoneList%GetNNodes()
              CALL ZoneList%GetOrderedKeyList(iTempZonesToProcess)
              iLoc = LocateInList(f_iUndefinedZone , iTempZonesToProcess)
              IF (iLoc .GT. 0) THEN
                  NZPrint = NZPrint - 1
                  ALLOCATE (iZonesToProcess(NZPrint))
                  iZonesToProcess(1:iLoc-1) = iTempZonesToProcess(1:iLoc-1)
                  iZonesToProcess(iLoc:) = iTempZonesToProcess(iLoc+1:)
              ELSE
                  CALL MOVE_ALLOC(iTempZonesToProcess , iZonesToProcess)
              END IF
          ELSE
              ALLOCATE (iZonesToProcess(NZPrint))
              iZonesToProcess = iZPrint
          END IF
      ELSE
          ALLOCATE (iZonesToProcess(NZPrint))
          DO indxPrint=1,NZPrint
              CALL MainControlFile%ReadData(iZonesToProcess(indxPrint),iStat)  ;  IF (iStat .EQ. -1) RETURN
          END DO
      END IF
      CALL ShellSort(iZonesToProcess)
      
      !Make sure that undefined zones is not asked for processing
      iLoc = LocateInList(f_iUndefinedZone,iZonesToProcess)
      IF (iLoc .GT. 0) THEN
          CALL LogMessage('An undefined zone (-99) cannot be processed!',f_iWarn,ThisProcedure)
          IF (SIZE(iZonesToProcess)-1 .EQ. 0) GOTO 100
          DEALLOCATE(iTempZonesToProcess,STAT=ErrorCode)
          ALLOCATE (iTempZonesToProcess(SIZE(iZonesToProcess)-1))
          iTempZonesToProcess(1:iLoc-1) = iZonesToProcess(1:iLoc-1)
          iTempZonesToProcess(iLoc:)    = iZonesToProcess(iLoc+1:)
          CALL MOVE_ALLOC(iTempZonesToProcess,iZonesToProcess)
      END IF
            
      !Print out zone flows
      CALL ZBudget%PrintZoneData(ZoneList,cOutFileName,cPrintBeginDateAndTime,cPrintEndDateAndTime,cPrintInterval,iZonesToProcess,Fact_VL,Fact_AR,cUnit_VL,cUnit_AR,iStat)
                  
      !Kill ZBudget and output file and zone list
100   CALL ZBudget%Kill()
      CALL ZoneList%Kill()
      
  END SUBROUTINE ProcessZBudget
  
  
  ! -------------------------------------------------------------
  ! --- SUBROUTINE THAT PRINTS OUT COMPONENT VERSION NUMBERS
  ! -------------------------------------------------------------
  SUBROUTINE PrintVersionNumbers()
    
    MessageArray(1) = NEW_LINE('x')//'VERSION NUMBERS FOR IWFM AND ITS COMPONENTS:'//NEW_LINE('x')
    MessageArray(2) = '  IWFM Core                 : '//TRIM(IWFM_Core%GetVersion())
    MessageArray(3) = '  IWFM_Util.lib             : '//TRIM(IWFM_Util%GetVersion())
    MessageArray(4) = '  Package_Misc.lib          : '//TRIM(Package_Misc_GetVersion())
    MessageArray(5) = '  Package_Discretization.lib: '//TRIM(Package_Discretization_GetVersion())
    MessageArray(6) = '  Package_Budget.lib        : '//TRIM(Package_Budget_GetVersion())
    MessageArray(7) = '  Package_ZBudget.lib       : '//TRIM(Package_ZBudget_GetVersion())

    CALL LogMessage(MessageArray(1:7),f_iMessage,'',Destination=f_iSCREEN)
  
  END SUBROUTINE PrintVersionNumbers
  
  
  ! -------------------------------------------------------------
  ! --- SUBROUTINE THAT ENDS PROGRAM EXECUTION PROPERLY
  ! -------------------------------------------------------------
  SUBROUTINE EndExecution(StandardOutputFile,iStat)
    TYPE(GenericFileType) :: StandardOutputFile
    INTEGER,INTENT(IN)    :: iStat
    
    IF (iStat .EQ. -1) THEN
        CALL LogLastMessage()
    ELSE
        CALL LogMessage(f_cLineFeed//'Program completed successfully.',f_iMessage,'')
    END IF
    
    CALL StopTimer()
    CALL PrintRunTime()
    CALL StandardOutputFile%Kill()
    STOP

  END SUBROUTINE EndExecution
  
END MODULE