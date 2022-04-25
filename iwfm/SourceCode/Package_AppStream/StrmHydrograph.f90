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
MODULE StrmHydrograph
  USE MessageLogger          , ONLY: SetLastMessage                , &
                                     LogMessage                    , &
                                     MessageArray                  , &
                                     f_iFatal                      , &
                                     f_iInfo
  USE IOInterface            , ONLY: GenericFileType               , &
                                     iGetFileType_FromName         , &
                                     f_iDSS                        , &
                                     f_iUNKNOWN                    , &
                                     f_iDataset                      
  USE GeneralUtilities       , ONLY: LocateInList                  , &
                                     StripTextUntilCharacter       , &
                                     IntToText                     , &
                                     ArrangeText                   , &
                                     CleanSpecialCharacters        , &
                                     EstablishAbsolutePathFilename , &
                                     PrepareTitle                  , &
                                     UpperCase                     , &
                                     ConvertID_To_Index
  USE TimeSeriesUtilities    , ONLY: TimeStepType                  , &
                                     IncrementTimeStamp
  USE Package_Misc           , ONLY: RealTSDataInFileType          , &
                                     PrepareTSDOutputFile          , &
                                     ReadTSData                    , &
                                     f_iDataUnitType_Length        , &
                                     f_iDataUnitType_Volume
  USE Class_StrmState 
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
  PUBLIC :: StrmHydrographType , &
            iHydFlow           , &
            iHydStage          , &
            iHydBoth
  
  
  ! -------------------------------------------------------------
  ! --- TYPES OF STREAM HYDROGRAPHS
  ! -------------------------------------------------------------
  INTEGER,PARAMETER :: iHydStage = 1 , &
                       iHydFlow  = 0 , &
                       iHydBoth  = 2
                       
                       
  ! -------------------------------------------------------------
  ! --- STREAM HYDROGRAPH PRINT DATA TYPE
  ! -------------------------------------------------------------
  TYPE StrmHydrographType
    TYPE(GenericFileType)         :: HydFile
    TYPE(RealTSDataInFileType)    :: HydFile_ForInquiry
    LOGICAL                       :: HydFile_Defined    = .FALSE.
    INTEGER                       :: iHydType           = iHydFlow
    REAL(8)                       :: FactFlow           = 1.0
    CHARACTER(LEN=10)             :: UnitFlow           = ''
    REAL(8)                       :: FactElev           = 1.0
    CHARACTER(LEN=10)             :: UnitElev           = ''
    INTEGER                       :: NHyd               = 0
    INTEGER,ALLOCATABLE           :: iHydNodes(:)
    CHARACTER(LEN=30),ALLOCATABLE :: cHydNames(:)
  CONTAINS
    PROCEDURE,PASS :: New                      
    PROCEDURE,PASS :: Kill                     
    PROCEDURE,PASS :: ReadStrmHydrograph_AtNode
    PROCEDURE,PASS :: PrintResults     
    PROCEDURE,PASS :: IsOutFileDefined 
    PROCEDURE,PASS :: IsHydrographAtNodeRequested
    PROCEDURE,PASS :: GetFileName
    PROCEDURE,PASS :: GetNHydrographs
    PROCEDURE,PASS :: GetHydrographNodes
    PROCEDURE,PASS :: GetCoordinates
    PROCEDURE,PASS :: GetNames
    PROCEDURE,PASS :: Transfer_To_HDF
  END TYPE StrmHydrographType
  
  
  ! -------------------------------------------------------------
  ! --- MISC. ENTITIES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                   :: ModNameLen    = 16
  CHARACTER(LEN=ModNameLen),PARAMETER :: ModName       = 'StrmHydrograph::'

  
  
  
CONTAINS

  

! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** CONSTRUCTORS
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- INSTANTIATE STREAM HYDROGRAPH PRINT DATA
  ! -------------------------------------------------------------
  SUBROUTINE New(StrmHyd,IsRoutedStreams,IsForInquiry,cWorkingDirectory,NStrmNodes,iStrmNodeIDs,TimeStep,InFile,iStat) 
    CLASS(StrmHydrographType),INTENT(OUT) :: StrmHyd
    LOGICAL,INTENT(IN)                    :: IsRoutedStreams,IsForInquiry
    CHARACTER(LEN=*),INTENT(IN)           :: cWorkingDirectory
    INTEGER,INTENT(IN)                    :: NStrmNodes,iStrmNodeIDs(NStrmNodes)
    TYPE(TimeStepType),INTENT(IN)         :: TimeStep
    TYPE(GenericFileType)                 :: InFile
    INTEGER,INTENT(OUT)                   :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+3) :: ThisProcedure = ModName // 'New'
    INTEGER                     :: NHyd,indx,ErrorCode,iNode
    CHARACTER                   :: ALine*1000,cHydOutFile*1000,cNumber*7
    INTEGER,ALLOCATABLE         :: iHydNodeIDs(:)
    CHARACTER(:),ALLOCATABLE    :: cAbsPathFileName
    
    !Read data
    CALL InFile%ReadData(NHyd,iStat)  ;  IF (iStat .EQ. -1) RETURN  ;  StrmHyd%NHyd = NHyd
    IF (NHyd .EQ. 0) THEN
        DO indx=1,6
            CALL InFile%ReadData(ALine,iStat)  
            IF (iStat .EQ. -1) RETURN
        END DO
        RETURN
    END IF
    CALL InFile%ReadData(StrmHyd%iHydType,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(StrmHyd%FactFlow,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN  ; CALL CleanSpecialCharacters(ALine) ; ALine = StripTextUntilCharacter(ALine,'/')
    StrmHyd%UnitFlow = ADJUSTL(TRIM(ALine))
    CALL InFile%ReadData(StrmHyd%FactElev,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN ; CALL CleanSpecialCharacters(ALine) ; ALine = StripTextUntilCharacter(ALine,'/')
    StrmHyd%UnitElev = ADJUSTL(TRIM(ALine))
    
    !Output file name
    CALL InFile%ReadData(cHydOutFile,iStat)  ;  IF (iStat .EQ. -1) RETURN ; cHydOutFile = StripTextUntilCharacter(cHydOutFile,'/') ; CALL CleanSpecialCharacters(cHydOutFile)
    IF (cHydOutFile .EQ. '') THEN
        IF (IsRoutedStreams) CALL LogMessage('Stream hydrograph printing is suppressed because an output file name is not specified!',f_iInfo,ThisProcedure)
        DO indx=1,NHyd
            CALL InFile%ReadData(iNode,iStat)  
            IF (iStat .EQ. -1) RETURN
        END DO
        StrmHyd%NHyd = 0
        RETURN
    END IF
    
    !Allocate memory
    ALLOCATE (StrmHyd%iHydNodes(StrmHyd%NHyd)  ,  StrmHyd%cHydNames(StrmHyd%NHyd) , iHydNodeIDs(StrmHyd%NHyd))
    StrmHyd%cHydNames = ''
    
    !Read the node numbers and hydrograph names for hydrograph printing
    DO indx=1,NHyd
        CALL InFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN  ;  CALL CleanSpecialCharacters(ALine)  ;  ALine = StripTextUntilCharacter(ALine,'/')  ;  ALine = ADJUSTL(ALine)
        READ (ALine,*,IOSTAT=ErrorCode) iHydNodeIDs(indx)
        CALL ConvertID_To_Index(iHydNodeIDs(indx),iStrmNodeIDs,iNode)
        IF (iNode .EQ. 0) THEN
            CALL SetLastMessage('Stream node ID '//TRIM(IntToText(iHydNodeIDs(indx)))//' listed for hydrograph printing is not in the model!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        cNumber = ''  ;  cNumber = IntToText(iHydNodeIDs(indx))  ;  cNumber = ADJUSTL(cNumber)
        StrmHyd%cHydNames(indx) = ADJUSTL(ALine(LEN_TRIM(cNumber)+1:))  ;  CALL CleanSpecialCharacters(StrmHyd%cHydNames(indx))
        IF (ErrorCode .NE. 0) THEN
            MessageArray(1) = 'Error in reading stream hydrograph print data!'
            MessageArray(2) = TRIM(ALine)
            CALL SetLastMessage(MessageArray(1:2),f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        StrmHyd%iHydNodes(indx) = iNode
    END DO
    
    IF (IsRoutedStreams) THEN
        !Prepare hydrograph file
        CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(cHydOutFile)),cWorkingDirectory,cAbsPathFileName)
        IF (IsForInquiry) THEN
            CALL PrepStrmHydFile_ForInquiry(TimeStep,cAbsPathFileName,StrmHyd%iHydType,StrmHyd%NHyd,iHydNodeIDs,StrmHyd%HydFile_ForInquiry,iStat)
        ELSE
            CALL PrepStrmHydFile(TimeStep,cAbsPathFileName,StrmHyd%UnitFlow,StrmHyd%UnitElev,StrmHyd%iHydType,StrmHyd%NHyd,iHydNodeIDs,StrmHyd%HydFile,iStat)
        END IF
        IF (iStat .EQ. -1) RETURN
        
        !Set the flag that checks if the hydrograph output file is defined
        StrmHyd%HydFile_Defined = .TRUE.
    ELSE
        CALL StrmHyd%Kill()
    END IF
            
  END SUBROUTINE New
  
  
 
  
! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** DESTRUCTOR
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- KILL STREAM HYDROGRAPH PRINT DATA OBJECT
  ! -------------------------------------------------------------
  SUBROUTINE Kill(StrmHyd)
    CLASS(StrmHydrographType) :: StrmHyd
    
    !Local variables
    INTEGER                  :: ErrorCode
    TYPE(StrmHydrographType) :: Dummy
    
    !Deallocate array attributes
    DEALLOCATE (StrmHyd%iHydNodes , StrmHyd%cHydNames , STAT=ErrorCode)
    
    !Close output (or input) file
    CALL StrmHyd%HydFile%Kill()
    CALL StrmHyd%HydFile_ForInquiry%Close()
    
    !Set attributes to their default values
    SELECT TYPE (p => StrmHyd)
        TYPE IS (StrmHydrographType)
            p = Dummy
    END SELECT
    
  END SUBROUTINE Kill
  
  
  
  
! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** GETTERS
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- GET OUTPUT FILE NAME
  ! -------------------------------------------------------------
  SUBROUTINE GetFileName(StrmHyd,cFileName)
    CLASS(StrmHydrographType)            :: StrmHyd
    CHARACTER(:),ALLOCATABLE,INTENT(OUT) :: cFileName
    
    !Local variables
    INTEGER :: ErrorCode
    
    !Initialize
    DEALLOCATE (cFileName , STAT=ErrorCode)
    
    IF (StrmHyd%HydFile_Defined) THEN
        IF (StrmHyd%HydFile%iGetFileType() .NE. f_iUNKNOWN) THEN
            CALL StrmHyd%HydFile%GetName(cFileName)
        ELSE
            CALL StrmHyd%HydFile_ForInquiry%GetFileName(cFileName)
        END IF
    END IF
    
  END SUBROUTINE GetFileName
  
  
  ! -------------------------------------------------------------
  ! --- GET STREAM NODE INDICES WHERE HYDROGRAPHS ARE PRINTED
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetHydrographNodes(StrmHyd,iNodes)
    CLASS(StrmHydrographType),INTENT(IN) :: StrmHyd
    INTEGER,INTENT(OUT)                  :: iNodes(:)
    
    IF (STrmHyd%NHyd .GT. 0) iNodes = StrmHyd%iHydNodes
    
  END SUBROUTINE GetHydrographNodes

  
  ! -------------------------------------------------------------
  ! --- GET NUMBER OF HYDROGRAPHS
  ! -------------------------------------------------------------
  FUNCTION GetNHydrographs(StrmHyd) RESULT(NHydrographs)
    CLASS(StrmHydrographType) :: StrmHyd
    INTEGER                   :: NHydrographs
    
    NHydrographs = StrmHyd%NHyd
    
  END FUNCTION GetNHydrographs

  
  ! -------------------------------------------------------------
  ! --- GET HYDROGRAPH COORDINATES
  ! -------------------------------------------------------------
  SUBROUTINE GetCoordinates(StrmHyd,iGWNodes,GridX,GridY,XHyd,YHyd)
    CLASS(StrmHydrographType) :: StrmHyd
    INTEGER,INTENT(IN)        :: iGWNodes(:)
    REAL(8),INTENT(IN)        :: GridX(:),GridY(:)
    REAL(8),INTENT(OUT)       :: XHyd(:),YHyd(:)
    
    !Local variables
    INTEGER :: indx,iGWNode
    
    DO indx=1,StrmHyd%NHyd
        iGWNode    = iGWNodes(StrmHyd%iHydNodes(indx))
        XHyd(indx) = GridX(iGWNode)
        YHyd(indx) = GridY(iGWNode)
    END DO
    
  END SUBROUTINE GetCoordinates

  
  ! -------------------------------------------------------------
  ! --- GET HYDROGRAPH NAMES
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetNames(StrmHyd,cNamesList)
    CLASS(StrmHydrographType),INTENT(IN) :: StrmHyd
    CHARACTER(LEN=*),INTENT(OUT)         :: cNamesList(:)   !Assumes array is previously dimensioned according to the number of hydrographs
    
    cNamesList = StrmHyd%cHydNames
    
  END SUBROUTINE GetNames

  
  
  
! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** DATA READERS
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- READ STREAM HYDROGRAPH FOR A STREAM NODE FOR A TIME RANGE
  ! -------------------------------------------------------------
  SUBROUTINE ReadStrmHydrograph_AtNode(StrmHyd,iNodeID,iNode,iHydType,cOutputBeginDateAndTime,cOutputEndDateAndTime,rFact_LT,rFact_VL,iDataUnitType,nActualOutput,rOutputDates,rOutputValues,iStat)
    CLASS(StrmHydrographType)   :: StrmHyd
    INTEGER,INTENT(IN)          :: iNodeID,iNode,iHydType
    CHARACTER(LEN=*),INTENT(IN) :: cOutputBeginDateAndTime,cOutputEndDateAndTime
    REAL(8),INTENT(IN)          :: rFact_LT,rFact_VL
    INTEGER,INTENT(OUT)         :: iDataUnitType,nActualOutput
    REAL(8),INTENT(OUT)         :: rOutputDates(:),rOutputValues(:)
    INTEGER,INTENT(OUT)         :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+25),PARAMETER :: ThisProcedure = ModName // 'ReadStrmHydrograph_AtNode'
    INTEGER                                :: ErrorCode,iHydIndex
    REAL(8)                                :: rEffectiveFactor
    
    !Convert node ID to hydrograph index
    iHydIndex = LocateInList(iNode,StrmHyd%iHydNodes)
    IF (iHydIndex .EQ. 0) THEN
        CALL SetLastMessage('Stream node ID '//TRIM(IntToText(iNodeID))//' does not have a hydrograph printed as model results!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    !Read data
    IF (StrmHyd%iHydType .EQ. iHydBoth) THEN
        IF (iHydType .EQ. iHydFlow) THEN
            CALL StrmHyd%HydFile_ForInquiry%ReadData(iHydIndex,cOutputBeginDateAndTime,cOutputEndDateAndTime,nActualOutput,rOutputValues,rOutputDates,ErrorCode,iStat)  
        ELSE
            CALL StrmHyd%HydFile_ForInquiry%ReadData(2*StrmHyd%NHyd+iHydIndex,cOutputBeginDateAndTime,cOutputEndDateAndTime,nActualOutput,rOutputValues,rOutputDates,ErrorCode,iStat)  
        END IF
    ELSE
        CALL StrmHyd%HydFile_ForInquiry%ReadData(iHydIndex,cOutputBeginDateAndTime,cOutputEndDateAndTime,nActualOutput,rOutputValues,rOutputDates,ErrorCode,iStat)  
    END IF
    IF (iStat .EQ. -1) RETURN

    !Convert flow/elevation data to simulation units, then to output units
    IF (StrmHyd%iHydType .EQ. iHydFlow) THEN
        iDataUnitType    = f_iDataUnitType_Volume
        rEffectiveFactor = rFact_VL / StrmHyd%FactFlow
        IF (rEffectiveFactor .NE. 1d0) rOutputValues(1:nActualOutput) = rOutputValues(1:nActualOutput) * rEffectiveFactor
    ELSEIF (StrmHyd%iHydType .EQ. iHydStage) THEN
        iDataUnitType    = f_iDataUnitType_Length
        rEffectiveFactor = rFact_LT / StrmHyd%FactElev
        IF (rEffectiveFactor .NE. 1d0) rOutputValues(1:nActualOutput) = rOutputValues(1:nActualOutput) * rEffectiveFactor
    ELSE
        IF (iHydType .EQ. iHydFlow) THEN
            iDataUnitType    = f_iDataUnitType_Volume
            rEffectiveFactor = rFact_VL / StrmHyd%FactFlow
        ELSE
            iDataUnitType    = f_iDataUnitType_Length
            rEffectiveFactor = rFact_LT / StrmHyd%FactElev
        END IF
        IF (rEffectiveFactor .NE. 1d0) rOutputValues(1:nActualOutput) = rOutputValues(1:nActualOutput) * rEffectiveFactor
    END IF
    
    !Rewind file
    CALL StrmHyd%HydFile_ForInquiry%File%RewindFile_To_BeginningOfTSData(iStat)
        
  END SUBROUTINE ReadStrmHydrograph_AtNode
  
  
  
  
! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** DATA WRITERS
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- PRINT OUT HYDROGRAPHS
  ! -------------------------------------------------------------
  SUBROUTINE PrintResults(StrmHyd,StrmState,BottomElev,TimeStep,lEndOfSimulation)
    CLASS(StrmHydrographType)      :: StrmHyd
    TYPE(StrmStateType),INTENT(IN) :: StrmState(:)
    REAL(8),INTENT(IN)             :: BottomElev(:)
    TYPE(TimeStepType),INTENT(IN)  :: TimeStep
    LOGICAL,INTENT(IN)             :: lEndOfSimulation
    
    !Local variables
    INTEGER           :: NHyd
    REAL(8)           :: rValues(2*StrmHyd%NHyd)
    CHARACTER(LEN=21) :: SimulationTime
    
    !Initialize
    NHyd = StrmHyd%NHyd
   
    !Create the simulation time
    IF (TimeStep%TrackTime) THEN
        SimulationTime=ADJUSTL(TimeStep%CurrentDateAndTime)
    ELSE
        WRITE(SimulationTime,'(F10.2,1X,A10)') TimeStep%CurrentTime,ADJUSTL(TimeStep%Unit)
    END IF

    !Print-out
    SELECT CASE (StrmHyd%iHydType)
      !Flow hydrograph
      CASE (iHydFlow)
        rValues(1:NHyd) = StrmState(StrmHyd%iHydNodes)%Flow * StrmHyd%FactFlow
        CALL StrmHyd%HydFile%WriteData(SimulationTime,rValues(1:NHyd),FinalPrint=lEndOfSimulation)
        
      !Stage hydrograph
      CASE (iHydStage)
        rValues(1:NHyd) = (StrmState(StrmHyd%iHydNodes)%Head - BottomElev(StrmHyd%iHydNodes)) * StrmHyd%FactElev
        CALL StrmHyd%HydFile%WriteData(SimulationTime,rValues(1:NHyd),FinalPrint=lEndOfSimulation)
        
      !Both flow and stage hydrographs
      CASE (iHydBoth)
        rValues(1:NHyd)  = StrmState(StrmHyd%iHydNodes)%Flow * StrmHyd%FactFlow
        rValues(NHyd+1:) = (StrmState(StrmHyd%iHydNodes)%Head - BottomElev(StrmHyd%iHydNodes)) * StrmHyd%FactElev
        CALL StrmHyd%HydFile%WriteData(SimulationTime,rValues,FinalPrint=lEndOfSimulation)
 
    END SELECT
      
  END SUBROUTINE PrintResults




! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** MISC. METHODS
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- CHECK IF OUTPUT FILE IS DEFINED
  ! -------------------------------------------------------------
  PURE FUNCTION IsOutFileDefined(StrmHyd) RESULT(lDefined)
    CLASS(StrmHydrographType),INTENT(IN) :: StrmHyd
    LOGICAL                              :: lDefined
    
    lDefined = StrmHyd%HydFile_Defined
    
  END FUNCTION IsOutFileDefined
  
  
  ! -------------------------------------------------------------
  ! --- CHECK IF HYDROGRAPH AT A SPECIFIC NODE IS REQUESTED
  ! -------------------------------------------------------------
  FUNCTION IsHydrographAtNodeRequested(StrmHyd,iStrmNode) RESULT(lRequested)
    CLASS(StrmHydrographType),INTENT(IN) :: StrmHyd
    INTEGER,INTENT(IN)                   :: iStrmNode
    LOGICAL                              :: lRequested
    
    IF (LocateInList(iStrmNode,StrmHyd%iHydNodes) .GT. 0) THEN
        lRequested = .TRUE.
    ELSE
        lRequested = .FALSE.
    END IF
    
  END FUNCTION IsHydrographAtNodeRequested
  
  
  ! -------------------------------------------------------------
  ! --- NEW STREAM HYDROGRAPH OUTPUT FILE
  ! -------------------------------------------------------------
  SUBROUTINE PrepStrmHydFile(TimeStep,cOutFileName,cUnitFlow,cUnitElev,iHydType,NHyd,iHydNodes,HydFile,iStat) 
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    CHARACTER(LEN=*),INTENT(IN)   :: cOutFileName,cUnitFlow,cUnitElev
    INTEGER,INTENT(IN)            :: iHydType,NHyd,iHydNodes(NHyd)
    TYPE(GenericFileType)         :: HydFile
    INTEGER,INTENT(OUT)           :: iStat
    
    !Local variables
    CHARACTER             :: Text*20,FormatSpec*500,FPart(1)*32,HeaderFormat(3)*500,  &
                             WorkArray(2)*3000,TitleLines(1)*325
    INTEGER               :: NColumnsOfData,NRowsOfData,indx,ErrorCode
    LOGICAL               :: OverwriteNColumnsOfData,PrintColumnNo
    CHARACTER,ALLOCATABLE :: cPart*32(:),DataUnit*10(:),DataType*10(:),Header*50(:,:)
    INTEGER,ALLOCATABLE   :: iHydNodes_Local(:)
    
    !Initialize
    iStat = 0
    
    !Open file
    CALL HydFile%New(FileName=cOutFileName,InputFile=.FALSE.,IsTSFile=.TRUE.,Descriptor='stream hydrographs output file',iStat=iStat)
    IF (iStat .EQ. -1) RETURN

    !Prepare file
    IF (iHydType .EQ. iHydBoth) THEN
        NColumnsOfData = 2 * NHyd
        ALLOCATE (CPart(NColumnsOfData) , DataUnit(NColumnsOfData) , DataType(NColumnsOfData) , Header(3,1+NColumnsOfData) , iHydNodes_Local(NColumnsOfData))    
        CPart                    = ''
        CPart(1:NHyd)            = ADJUSTL('FLOW')
        CPart(NHyd+1:)           = ADJUSTL('FLOW_DEPTH')
        DataUnit                 = ''
        DataUnit(1:NHyd)         = ADJUSTL(cUnitFlow)
        DataUnit(NHyd+1:)        = ADJUSTL(cUnitElev)
        DataType                 = ''
        DataType(1:NHyd)         = 'PER-AVER'
        DataType(NHyd+1:)        = 'INST-VAL'
        iHydNodes_Local(1:NHyd)  = iHydNodes 
        iHydNodes_Local(NHyd+1:) = iHydNodes 
    ELSE
        NColumnsOfData = NHyd
        ALLOCATE (CPart(NColumnsOfData) , DataUnit(NColumnsOfData) , DataType(NColumnsOfData) , Header(3,1+NColumnsOfData) , iHydNodes_Local(NColumnsOfData))   
        iHydNodes_Local = iHydNodes 
        CPart           = ''
        DataUnit        = ''
        DataType        = ''
        IF (iHydType .EQ. iHydFlow) THEN
            CPart    = ADJUSTL('FLOW')
            DataUnit = ADJUSTL(cUnitFlow) 
            DataType = 'PER-AVER'
        ELSE
            CPart    = ADJUSTL('FLOW_DEPTH')
            DataUnit = ADJUSTL(cUnitElev) 
            DataType = 'INST-VAL'
        END IF
    END IF
    Text                    = IntToText(NColumnsOfData)
    NRowsOfData             = 1
    OverwriteNColumnsOfData = .TRUE.
    PrintColumnNo           = .FALSE.
    FormatSpec              = '(A21,'//TRIM(Text)//'(2X,F12.2))'
    FPart(1)      = 'STREAM_HYDROGRAPHS'

    !Prepare header lines
    WorkArray(1) = ArrangeText('STREAM HYDROGRAPH',37)
    IF (iHydType .EQ. iHydFlow) THEN
        WorkArray(2) = ArrangeText('(UNIT=',cUnitFlow,')',37)
    ELSEIF (iHydType .EQ. iHydStage) THEN
        WorkArray(2) = ArrangeText('(UNIT=',cUnitElev,')',37)
    ELSE
        WorkArray(2) = ArrangeText('(UNIT=',TRIM(ADJUSTL(cUnitFlow))//' or '//TRIM(ADJUSTL(cUnitElev)),')',37)
    END IF
    CALL PrepareTitle(TitleLines(1),WorkArray(1:2),39,42)
    Header = ''
    WRITE (Header(1,1),'(A1,7X,A13)') '*','HYDROGRAPH ID'
    WRITE (Header(2,1),'(A1,15X,A5)') '*','NODES'
    WRITE (Header(3,1),'(A1,8X,A4)') '*','TIME'
    IF (iHydType .EQ. iHydBoth) THEN
        DO indx=1,NHyd
            WRITE (Header(1,indx+1),'(I12)') indx
            WRITE (Header(2,indx+1),'(I12)') iHydNodes(indx)
        END DO
        DO indx=NHyd+1,NColumnsOfData
            WRITE (Header(1,indx+1),'(I12)') indx
            WRITE (Header(2,indx+1),'(I12)') iHydNodes(indx-NHyd)
        END DO
    ELSE
        DO indx=1,NColumnsOfData
            WRITE (Header(1,indx+1),'(I12)') indx
            WRITE (Header(2,indx+1),'(I12)') iHydNodes(indx)
        END DO
    END IF
    HeaderFormat(1)='(A21,'//TRIM(Text)//'(2X,A12))'
    HeaderFormat(2)='(A21,'//TRIM(Text)//'(2X,A12))'
    HeaderFormat(3)='(A13,8X,'//TRIM(Text)//'(A))'
    
    !Prepare the time series output file
    CALL PrepareTSDOutputFile(HydFile                          , &
                              NColumnsOfData                   , &
                              NRowsOfData                      , &
                              OverwriteNColumnsOfData          , &
                              FormatSpec                       , &
                              TitleLines                       , &
                              Header                           , &
                              HeaderFormat                     , &
                              PrintColumnNo                    , &
                              DataUnit                         , &
                              DataType                         , &
                              CPart                            , &
                              FPart                            , &
                              TimeStep%Unit                    , &
                              StrmNodes=iHydNodes_Local        , &
                              iStat=iStat                      )
    
    !Clear memory
    DEALLOCATE (cPart , DataUnit , DataType , Header , iHydNodes_Local , STAT=ErrorCode)
    
  END SUBROUTINE PrepStrmHydFile
  
  
  ! -------------------------------------------------------------
  ! --- NEW STREAM HYDROGRAPH FILE AS INPUT FILE FOR POST-PROCESSING
  ! -------------------------------------------------------------
  SUBROUTINE PrepStrmHydFile_ForInquiry(TimeStep,cFileName,iHydType,NHyd,iHydNodes,InFile,iStat)
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    CHARACTER(LEN=*),INTENT(IN)   :: cFileName
    INTEGER,INTENT(IN)            :: iHydType,NHyd,iHydNodes(NHyd)
    TYPE(RealTSDataInFileType)    :: InFile
    INTEGER,INTENT(OUT)           :: iStat
    
    !Local variables
    INTEGER               :: indx,ErrorCode
    CHARACTER             :: BPart*32,EandFParts*64
    CHARACTER,ALLOCATABLE :: cPathNames*80(:),CPart*32(:)
    
    !Initialize
    iStat = 0
    
    !Pathname parts
    IF (iHydType .EQ. iHydBoth) THEN
        ALLOCATE (cPathNames(2*NHyd) , CPart(2*NHyd))
        cPathNames     = ''
        CPart          = ''
        CPart(1:NHyd)  = 'FLOW'
        CPart(NHyd+1:) = 'FLOW_DEPTH'
    ELSE
        ALLOCATE (cPathNames(NHyd) , CPart(NHyd))
        cPathNames = ''
        CPart      = ''
        IF (iHydType .EQ. iHydFlow) THEN
            CPart = 'FLOW'
        ELSE
            CPart ='FLOW_DEPTH'
        END IF
    END IF
    EandFParts = UpperCase(TRIM(TimeStep%Unit)) // '/STREAM_HYDROGRAPHS/'
    
    !Instantiate file according to its type
    IF (iGetFileType_FromName(cFileName) .EQ. f_iDSS) THEN
        !Form pathnames
        IF (iHydType .EQ. iHydBoth) THEN
            DO indx=1,NHyd
                BPart            = 'R' // TRIM(IntToText(iHydNodes(indx))) 
                cPathnames(indx) = '/IWFM/' // TRIM(BPart) // '/' // TRIM(CPart(indx)) // '//' // TRIM(EandFParts)
            END DO
            DO indx=NHyd+1,2*NHyd
                BPart            = 'R' // TRIM(IntToText(iHydNodes(indx-NHyd))) 
                cPathnames(indx) = '/IWFM/' // TRIM(BPart) // '/' // TRIM(CPart(indx)) // '//' // TRIM(EandFParts)
            END DO
            CALL InFile%Init(cFileName,'streamflow hydrograph at selected locations',TimeStep%TrackTime,nCol=2*NHyd,cPathNames=cPathNames,iStat=iStat)
        ELSE
            DO indx=1,NHyd
                BPart            = 'R' // TRIM(IntToText(iHydNodes(indx))) 
                cPathnames(indx) = '/IWFM/' // TRIM(BPart) // '/' // TRIM(CPart(indx)) // '//' // TRIM(EandFParts)
            END DO
            CALL InFile%Init(cFileName,'streamflow hydrograph at selected locations',TimeStep%TrackTime,nCol=NHyd,cPathNames=cPathNames,iStat=iStat)
        END IF
    ELSE
        IF (iHydType .EQ. iHydBoth) THEN
            CALL InFile%Init(cFileName,'streamflow hydrograph at selected locations',BlocksToSkip=0,nCol=2*NHyd,iStat=iStat)
        ELSE
            CALL InFile%Init(cFileName,'streamflow hydrograph at selected locations',BlocksToSkip=0,nCol=NHyd,iStat=iStat)
        END IF
    END IF
    
    !Clear memory
    DEALLOCATE (cPathNames , CPart , STAT=ErrorCode)
    
  END SUBROUTINE PrepStrmHydFile_ForInquiry

  
  ! -------------------------------------------------------------
  ! --- TRANSFER HYDROGRAPHS FROM TEXT/DSS FILE TO HDF FILE
  ! -------------------------------------------------------------
  SUBROUTINE Transfer_To_HDF(StrmHyd,NTIME,TimeStep,iStat)
    CLASS(StrmHydrographType)      :: StrmHyd
    INTEGER,INTENT(IN)             :: NTIME
    TYPE(TimeStepType),INTENT(IN)  :: TimeStep
    INTEGER,INTENT(OUT)            :: iStat
    
    !Local variables
    INTEGER                  :: NColumns(1),FileReadCode,indxTime,ErrorCode
    CHARACTER                :: cDataSetName(1)*50,cHDFFileName*500
    TYPE(GenericFileType)    :: OutFile
    CHARACTER(:),ALLOCATABLE :: cFileName
    REAL(8),ALLOCATABLE      :: rData(:,:),rConvFactor(:)
    TYPE(TimeStepType)       :: TimeStep_Local
    
    !Initialize
    iStat = 0
    
    !Return if no output
    IF (.NOT. StrmHyd%HydFile_Defined) RETURN
    
    !Get the name of the text/DSS file 
    CALL StrmHyd%HydFile_ForInquiry%GetFileName(cFileName)
    
    !Name for the HDF file
    cHDFFileName = TRIM(ADJUSTL(StripTextUntilCharacter(cFileName,'.',Back=.TRUE.))) // '.hdf'
    
    !Open output file HDF file
    CALL OutFile%New(FileName=TRIM(cHDFFileName),InputFile=.FALSE.,IsTSFile=.TRUE.,iStat=iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Advance time to t=1
    TimeStep_Local                    = TimeStep
    TimeStep_Local%CurrentTimeStep    = TimeStep_Local%CurrentTimeStep + 1
    TimeStep_Local%CurrentDateAndTime = IncrementTimeStamp(TimeStep_Local%CurrentDateAndTime,TimeStep_Local%DELTAT_InMinutes)
    
    !Create dataset
    IF (StrmHyd%iHydType .EQ. iHydBoth) THEN
        NColumns(1) = 2 * StrmHyd%NHyd
    ELSE
        NColumns(1) = StrmHyd%NHyd
    END IF
    cDataSetName(1) = '/Stream_Hydrographs'
    CALL OutFile%CreateHDFDataSet(cPathNames=cDataSetName,NColumns=NColumns,NTime=NTIME,TimeStep=TimeStep_Local,DataType=0d0,iStat=iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Write the type of hydrograph
    CALL OutFile%WriteData(f_iDataset,'/Stream_Hydrographs','HydrographType',ScalarAttrData=StrmHyd%iHydType)
    
    !Conversion factor used when printing out results
    IF (StrmHyd%iHydType .EQ. iHydBoth) THEN
        ALLOCATE (rConvFactor(2*StrmHyd%NHyd))
        rConvFactor(1:StrmHyd%NHyd)  = 1d0 / StrmHyd%FactFlow
        rConvFactor(StrmHyd%NHyd+1:) = 1d0 / StrmHyd%FactElev
    ELSE
        ALLOCATE (rConvFactor(StrmHyd%NHyd))
        IF (StrmHyd%iHydType .EQ. iHydFlow) THEN
            rConvFactor = 1d0 / StrmHyd%FactFlow
        ELSE
            rConvFactor = 1d0 / StrmHyd%FactElev
        END IF
    END IF
    
    !Transfer heads to HDF file
    IF (StrmHyd%iHydType .EQ. iHydBoth) THEN
        ALLOCATE (rData(2*StrmHyd%NHyd,1))
    ELSE
        ALLOCATE (rData(StrmHyd%NHyd,1))
    END IF
    DO indxTime=1,NTIME
        !Read data
        CALL ReadTSData(TimeStep_Local,'stream hydrographs',StrmHyd%HydFile_ForInquiry,FileReadCode,iStat)
        IF (iStat .EQ. -1) RETURN
        
        !Transfer  values to matrix to be written to HDF file
        rData(:,1) = StrmHyd%HydFile_ForInquiry%rValues * rConvFactor
        
        !Store heads into HDF file
        CALL OutFile%WriteData(rData)
        
        !Advance time
        TimeStep_Local%CurrentTimeStep    = TimeStep_Local%CurrentTimeStep + 1
        TimeStep_Local%CurrentDateAndTime = IncrementTimeStamp(TimeStep_Local%CurrentDateAndTime,TimeStep_Local%DELTAT_InMinutes)
    END DO
    
    !Rewind input file
    CALL StrmHyd%HydFile_ForInquiry%File%RewindFile_To_BeginningOfTSData(iStat)
    
    !Close HDF file
    CALL OutFile%Kill()
    
    !Clear memory
    DEALLOCATE (rData,rConvFactor,STAT=ErrorCode)
    
  END SUBROUTINE Transfer_To_HDF

END MODULE