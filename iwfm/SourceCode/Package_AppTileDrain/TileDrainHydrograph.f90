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
MODULE TileDrainHydrograph
  USE MessageLogger           , ONLY: SetLastMessage                , &
                                      LogMessage                    , &
                                      f_iFatal                      , &
                                      f_iWarn                         
  USE GeneralUtilities        , ONLY: ConvertID_To_Index            , &
                                      IntToText                     , &
                                      StripTextUntilCharacter       , &
                                      ArrangeText                   , &
                                      PrepareTitle                  , &
                                      EstablishAbsolutePathFileName , &
                                      CleanSpecialCharacters
  USE TimeSeriesUtilities     , ONLY: TimeStepType                  , &
                                      IncrementTimeStamp
  USE IOInterface             , ONLY: GenericFileType               , &
                                      iGetFileType_FromName         , &
                                      f_iDSS
  USE Package_Misc            , ONLY: RealTSDataInFileType          , &
                                      ReadTSData                    , &
                                      PrepareTSDOutputFile
  USE AppTileDrain_Parameters , ONLY: f_iTileDrain                  , &
                                      f_iSubIrig
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
  PUBLIC :: TileDrainHydrographType  


  ! -------------------------------------------------------------
  ! --- TILE DRAIN HYDROGRAPH DATA TYPE
  ! -------------------------------------------------------------
  TYPE TileDrainHydrographType
    TYPE(GenericFileType)         :: OutFile
    LOGICAL                       :: OutFile_Defined           = .FALSE.
    TYPE(RealTSDataInFileType)    :: InFile_ForInquiry
    LOGICAL                       :: InFile_ForInquiry_Defined = .FALSE.
    REAL(8)                       :: FactFlow                  = 1.0
    CHARACTER(LEN=10)             :: UnitFlow                  = ''
    INTEGER                       :: NHyd                      = 0
    INTEGER,ALLOCATABLE           :: iHydIndices(:)
    INTEGER,ALLOCATABLE           :: iHydTypes(:)
    CHARACTER(LEN=30),ALLOCATABLE :: cHydNames(:)
  CONTAINS
    PROCEDURE,PASS :: New         
    PROCEDURE,PASS :: Kill     
    PROCEDURE,PASS :: ReadTDHyd_AtHydrographLocation
    PROCEDURE,PASS :: PrintResults
    PROCEDURE,PASS :: IsOutFileDefined
    PROCEDURE,PASS :: GetOutFileName
    PROCEDURE,PASS :: GetNHydrographs
    PROCEDURE,PASS :: GetHydrographIDs
    PROCEDURE,PASS :: GetHydrographCoordinates
    PROCEDURE,PASS :: GetHydrographNames
    PROCEDURE,PASS :: Transfer_To_HDF
  END TYPE TileDrainHydrographType
  
  
  ! -------------------------------------------------------------
  ! --- MISC. ENTITIES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                   :: ModNameLen    = 21
  CHARACTER(LEN=ModNameLen),PARAMETER :: ModName       = 'TileDrainHydrograph::'

  
  
  
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
  ! --- READ TILE DRAIN HYDROGRAPH PRINT DATA FROM FILE
  ! -------------------------------------------------------------
  SUBROUTINE New(TDHyd,IsForInquiry,cWorkingDirectory,NodeIDs,SubIrigIDs,DrainIDs,SubIrigNodes,DrainNodes,TimeStep,InFile,iStat)
    CLASS(TileDrainHydrographType),INTENT(OUT) :: TDHyd
    LOGICAL,INTENT(IN)                         :: IsForInquiry
    CHARACTER(LEN=*),INTENT(IN)                :: cWorkingDirectory
    INTEGER,INTENT(IN)                         :: NodeIDs(:),SubIrigIDs(:),DrainIDs(:),SubIrigNodes(:),DrainNodes(:)
    TYPE(TimeStepType),INTENT(IN)              :: TimeStep
    TYPE(GenericFileType)                      :: InFile
    INTEGER,INTENT(OUT)                        :: iStat
  
    !Local variables
    CHARACTER(LEN=ModNameLen+3) :: ThisProcedure = ModName // 'New'
    INTEGER                     :: NHyd,indx,NDrain,NSubIrig,ErrorCode,ID
    CHARACTER                   :: cTDOutFile*1000,ALine*1000
    CHARACTER(:),ALLOCATABLE    :: cAbsPathFileName
    
    !Initialize
    iStat    = 0
    NDrain   = SIZE(DrainNodes)
    NSubIrig = SIZE(SubIrigNodes)
    
    !Read data
    CALL InFile%ReadData(NHyd,iStat)  ;  IF (iStat .EQ. -1) RETURN  ;  TDHyd%NHyd = NHyd
      IF (NHyd .EQ. 0) RETURN
    CALL InFile%ReadData(TDHyd%FactFlow,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN ; CALL CleanSpecialCharacters(ALine) ; ALine = StripTextUntilCharacter(ALine,'/')
      TDHyd%UnitFlow = ADJUSTL(TRIM(ALine))

    !Output file name
    CALL InFile%ReadData(cTDOutFile,iStat)  ;  IF (iStat .EQ. -1) RETURN ; cTDOutFile = StripTextUntilCharacter(cTDOutFile,'/') ; CALL CleanSpecialCharacters(cTDOutFile)
    IF (cTDOutFile .EQ. '') THEN
        CALL LogMessage('Tile drain hydrograph printing is suppressed because an output file name is not specified!',f_iWarn,ThisProcedure)
        TDHyd%NHyd = 0
        RETURN
    END IF
     
    !Allocate memory
    ALLOCATE (TDHyd%iHydIndices(NHyd) , TDHyd%iHydTypes(NHyd) , TDHyd%cHydNames(NHyd))
    TDHyd%cHydNames = ''
    
    !Read the hydrograph IDs for printing
    DO indx=1,NHyd
        CALL InFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN  ;  CALL CleanSpecialCharacters(ALine)  ;  ALine = StripTextUntilCharacter(ALine,'/')  ;  ALine = ADJUSTL(ALine)
        READ (ALine,*,IOSTAT=ErrorCode) ID , TDHyd%iHydTypes(indx) , TDHyd%cHydNames(indx)
        IF (ErrorCode .NE. 0) THEN
            CALL SetLastMessage('Error in reading tile drain/subsurface irrigation hydrograph data for hydrograph ID '//TRIM(IntToText(indx))//'!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        
        !Make sure hydrograph type is recognized
        IF (TDHyd%iHydTypes(indx).NE.f_iTileDrain  .AND.  TDHyd%iHydTypes(indx).NE.f_iSubIrig) THEN
            CALL SetLastMessage('Hydrograph type for tile drain/subsurface irrigation hydrograph printing is not recognized!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        
        !Convert hydrograph ID to index
        IF (TDHyd%iHydTypes(indx) .EQ. f_iTileDrain) THEN
            CALL ConvertID_To_Index(ID,DrainIDs,TDHyd%iHydIndices(indx))
            IF (TDHyd%iHydIndices(indx) .EQ. 0) THEN
                CALL SetLastMessage('Tile drain ID '//TRIM(IntToText(ID))//' listed for hydrograph printing is not simulated!',f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
        ELSE
            CALL ConvertID_To_Index(ID,SubIrigIDs,TDHyd%iHydIndices(indx))
            IF (TDHyd%iHydIndices(indx) .EQ. 0) THEN
                CALL SetLastMessage('Subsurface irrigation ID '//TRIM(IntToText(ID))//' listed for hydrograph printing is not simulated!',f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
        END IF
        
    END DO
    
    !Prepare output file
    CALL EstablishAbsolutePathFileName(TRIM(ADJUSTL(cTDOutFile)),cWorkingDirectory,cAbsPathFileName)
    IF (IsForInquiry) THEN
        CALL PrepTileDrainHydInFile_ForInquiry(TimeStep,NodeIDs,SubIrigNodes,DrainNodes,cAbsPathFileName,TDHyd,iStat)
    ELSE
        CALL PrepTileDrainHydOutFile(TimeStep,NodeIDs,SubIrigIDs,DrainIDs,SubIrigNodes,DrainNodes,cAbsPathFileName,TDHyd,iStat)
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
  ! --- KILL TILE DRAIN HYDROGRAPH PRINT DATA 
  ! -------------------------------------------------------------
  SUBROUTINE Kill(TDHyd)
    CLASS(TileDrainHydrographType) :: TDHyd
    
    !Local variables
    INTEGER                       :: ErrorCode
    TYPE(TileDrainHydrographType) :: Dummy
    
    IF (TDHyd%OutFile_Defined) CALL TDHyd%OutFile%Kill() 
    IF (TDHyd%InFile_ForInquiry_Defined) CALL TDHyd%InFile_ForInquiry%Close()
    DEALLOCATE (TDHyd%iHydIndices , TDHyd%iHydTypes , TDHyd%cHydNames , STAT=ErrorCode)
    
    SELECT TYPE (p => TDHyd)
        TYPE IS (TileDrainHydrographType)
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
  ! --- GET THE OUTPUT FILE NAME
  ! -------------------------------------------------------------
  SUBROUTINE GetOutFileName(TDHyd,cName)
    CLASS(TileDrainHydrographType),INTENT(IN) :: TDHyd
    CHARACTER(:),ALLOCATABLE,INTENT(OUT)      :: cName
    
    !Local variables
    INTEGER :: ErrorCode
    
    !Initialize
    DEALLOCATE (cName , STAT=ErrorCode)
    
    IF (TDHyd%OutFile_Defined) THEN
        CALL TDHyd%OutFile%GetName(cName)
    ELSEIF (TDHyd%InFile_ForInquiry_Defined) THEN
        CALL TDHyd%InFile_ForInquiry%GetFileName(cName)
    END IF
    
  END SUBROUTINE GetOutFileName

  
  ! -------------------------------------------------------------
  ! --- GET NUMBER OF TILE DRAIN HYDROGRAPHS 
  ! -------------------------------------------------------------
  PURE FUNCTION GetNHydrographs(TDHyd,iHydType) RESULT(NHyd)
    CLASS(TileDrainHydrographType),INTENT(IN) :: TDHyd
    INTEGER,INTENT(IN)                        :: iHydType
    INTEGER                                   :: NHyd
    
    !No tile drain hydrograph is defined
    IF (TDHyd%NHyd .EQ. 0) THEN
        NHyd = 0
        RETURN
    END IF
    
    !Return number of tile drain/sub irrigtaion hydrographs
    NHyd = COUNT(TDHyd%iHydTypes .EQ. iHydType)
    
  END FUNCTION GetNHydrographs

  
  ! -------------------------------------------------------------
  ! --- GET TILE DRAIN HYDROGRAPH IDS 
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetHydrographIDs(TDHyd,iHydType,iAllIDs,IDs)
    CLASS(TileDrainHydrographType),INTENT(IN) :: TDHyd
    INTEGER,INTENT(IN)                        :: iHydType,iAllIDs(:)
    INTEGER,INTENT(OUT)                       :: IDs(:)
    
    !Local variables
    INTEGER :: iCount,indx
    
    iCount = 0
    DO indx=1,TDHyd%NHyd
        IF (TDHyd%iHydTypes(indx) .EQ. iHydType) THEN
            iCount      = iCount + 1
            IDs(iCount) = iAllIDs(TDHyd%iHydIndices(indx))
        END IF
    END DO
        
  END SUBROUTINE GetHydrographIDs

  
  ! -------------------------------------------------------------
  ! --- GET HYDROGRAPH COORDINATES 
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetHydrographCoordinates(TDHyd,iHydType,iNodes,GridX,GridY,XHyd,YHyd)
    CLASS(TileDrainHydrographType),INTENT(IN) :: TDHyd
    INTEGER,INTENT(IN)                        :: iHydType,iNodes(:)
    REAL(8),INTENT(IN)                        :: GridX(:),GridY(:)
    REAL(8),INTENT(OUT)                       :: XHyd(:),YHyd(:)
    
    !Local variables
    INTEGER :: iCount,indx,iNode
    
    iCount = 0
    DO indx=1,TDHyd%NHyd
        IF (TDHyd%iHydTypes(indx) .EQ. iHydType) THEN
            iCount       = iCount + 1
            iNode        = iNodes(TDHyd%iHydIndices(indx))
            XHyd(iCount) = GridX(iNode)
            YHyd(iCount) = GridY(iNode)
        END IF
    END DO
        
  END SUBROUTINE GetHydrographCoordinates

  
  ! -------------------------------------------------------------
  ! --- GET TILE DRAIN HYDROGRAPH NAMES 
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetHydrographNames(TDHyd,cNamesList)
    CLASS(TileDrainHydrographType),INTENT(IN) :: TDHyd
    CHARACTER(LEN=*),INTENT(OUT)              :: cNamesList(:)   !Assumes array is previously dimensioned based on the number of tile drain hydrographs
    
    !Local variables
    INTEGER :: indx,iCount
    
    !Get only tile drain hydrograph names
    iCount = 1
    DO indx=1,TDHyd%NHyd
        IF (TDHyd%iHydTypes(indx) .EQ. f_iTileDrain) THEN
            cNamesList(iCount) = TDHyd%cHydNames(indx)
            iCount             = iCount + 1
        END IF
    END DO
    
  END SUBROUTINE GetHydrographNames

  
  
  
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
  ! --- READ SIMULATED TILE DRAIN/SUBSURFACE IRRIGATION AT A HYDROGRAPH LOCATION
  ! --- Note: Assumes InFile_ForInquiry file exists
  ! -------------------------------------------------------------
  SUBROUTINE ReadTDHyd_AtHydrographLocation(TDHyd,iHydIDs,iHydType,iHydID,cOutputBeginDateAndTime,cOutputEndDateAndTime,rConversionFactor,nActualOutput,rOutputDates,rOutputValues,iStat)
    CLASS(TileDrainHydrographType) :: TDHyd
    INTEGER,INTENT(IN)             :: iHydIDs(:),iHydType,iHydID
    CHARACTER(LEN=*),INTENT(IN)    :: cOutputBeginDateAndTime,cOutputEndDateAndTime
    REAL(8),INTENT(IN)             :: rConversionFactor
    INTEGER,INTENT(OUT)            :: nActualOutput
    REAL(8),INTENT(OUT)            :: rOutputDates(:),rOutputValues(:)
    INTEGER,INTENT(OUT)            :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+30),PARAMETER :: ThisProcedure = ModName // 'ReadTDHyd_AtHydrographLocation'
    INTEGER                                :: ErrorCode,iHydIndex,iPointer,indx
    REAL(8)                                :: rEffectiveFactor
    
    !Initialize
    iStat = 0
    
    !Find the index of hydrograph
    iHydIndex = 0
    DO indx=1,TDHyd%NHyd
        IF (iHydType .EQ. TDHyd%iHydTypes(indx)) THEN
            iPointer = TDHyd%iHydIndices(indx)
            IF (iHydID .EQ. iHydIDs(iPointer)) THEN
                iHydIndex = indx
                EXIT
            END IF
        END IF
    END DO
    IF (iHydIndex .EQ. 0) THEN
        CALL SetLastMessage('Tile drain/subsurface irrigation hydrograph ID '//TRIM(IntToText(iHydID))//' for results retrieval is not in the model!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    !Read data
    CALL TDHyd%InFile_ForInquiry%ReadData(iHydIndex,cOutputBeginDateAndTime,cOutputEndDateAndTime,nActualOutput,rOutputValues,rOutputDates,ErrorCode,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Convert unit
    rEffectiveFactor = rConversionFactor / TDHyd%FactFlow
    IF (rEffectiveFactor .NE. 1d0) rOutputValues = rOutputValues * rEffectiveFactor
    
    !Rewind file
    CALL TDHyd%InFile_ForInquiry%File%RewindFile_To_BeginningOfTSData(iStat)
    
  END SUBROUTINE ReadTDHyd_AtHydrographLocation

  
  
  
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
  SUBROUTINE PrintResults(TDHyd,rDrainFlows,rSubIrigFlows,TimeStep,lEndOfSimulation)
    CLASS(TileDrainHydrographType) :: TDHyd
    REAL(8),INTENT(IN)             :: rDrainFlows(:),rSubIrigFlows(:)
    TYPE(TimeStepType),INTENT(IN)  :: TimeStep
    LOGICAL,INTENT(IN)             :: lEndOfSimulation
    
    !Local variables
    INTEGER           :: indxHyd,indx
    REAL(8)           :: rFlows(TDHyd%NHyd),rFactor
    CHARACTER(LEN=21) :: SimulationTime

    !Initialize
    rFactor = TDHyd%FactFlow
    
    !Create the simulation time
    IF (TimeStep%TrackTime) THEN
      SimulationTime=ADJUSTL(TimeStep%CurrentDateAndTime)
    ELSE
      WRITE(SimulationTime,'(F10.2,1X,A10)') TimeStep%CurrentTime,ADJUSTL(TimeStep%Unit)
    END IF

    !Compile flows
    DO indxHyd=1,TDHyd%NHyd
        indx = TDHyd%iHydIndices(indxHyd)
        IF (TDHyd%iHydTypes(indxHyd) .EQ. f_iTileDrain) THEN
            rFlows(indxHyd) = rDrainFlows(indx) * rFactor
        ELSE
            rFlows(indxHyd) = rSubIrigFlows(indx) * rFactor
        END IF
    END DO

    !Print out the results
    CALL TDHyd%OutFile%WriteData(SimulationTime,rFlows,FinalPrint=lEndOfSimulation)
    
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
  ! ---CHECK IF OUTPUT FILE IS DEFINED
  ! -------------------------------------------------------------
  FUNCTION IsOutFileDefined(TDHyd) RESULT(lDefined)
    CLASS(TileDrainHydrographType),INTENT(IN) :: TDHyd
    LOGICAL                                   :: lDefined
    
    lDefined = TDHyd%OutFile_Defined .OR. TDHyd%InFile_ForInquiry_Defined
    
  END FUNCTION IsOutFileDefined
  
  
  ! -------------------------------------------------------------
  ! --- NEW TILE DRAIN HYDROGRAPH OUTPUT FILE
  ! -------------------------------------------------------------
  SUBROUTINE PrepTileDrainHydOutFile(TimeStep,NodeIDs,SubIrigIDs,DrainIDs,SubIrigNodes,DrainNodes,cOutFileName,TDHyd,iStat) 
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    INTEGER,INTENT(IN)            :: NodeIDs(:),SubIrigIDs(:),DrainIDs(:),SubIrigNodes(:),DrainNodes(:)
    CHARACTER(LEN=*),INTENT(IN)   :: cOutFileName
    TYPE(TileDrainHydrographType) :: TDHyd
    INTEGER,INTENT(OUT)           :: iStat
    
    !Local variables
    CHARACTER :: Text*20,FormatSpec*500,DataUnit(1)*10,DataType(1)*10,      &
                 CPart(1)*32,FPart(TDHyd%NHyd)*32,Header(3,1+TDHyd%NHyd)*50, &
                 HeaderFormat(3)*500,WorkArray(4)*3000,TitleLines(1)*3000
    INTEGER   :: NColumnsOfData,NRowsOfData,indx,IOUTTD(TDHyd%NHyd)
    LOGICAL   :: OverwriteNColumnsOfData,PrintColumnNo
    
    !Initialize
    iStat = 0
    
    !Open file
    CALL TDHyd%OutFile%New(FileName=cOutFileName,InputFile=.FALSE.,IsTSFile=.TRUE.,Descriptor='tile drain and subsurface irrigation hydrographs output file',iStat=iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Prepare output file
    Text                    = IntToText(TDHyd%NHyd)
    NColumnsOfData          = TDHyd%NHyd
    NRowsOfData             = 1
    OverwriteNColumnsOfData = .TRUE.
    PrintColumnNo           = .FALSE.
    FormatSpec              = '(A21,'//TRIM(Text)//'(2X,F10.2))'
    DataUnit(1)             = ADJUSTL(TDHyd%UnitFlow)
    DataType(1)             = 'PER-CUM'
    CPart(1)                = ADJUSTL('FLOW')
    DO indx=1,TDHyd%NHyd
      IF (TDHyd%iHydTypes(indx) .EQ. f_iTileDrain) THEN
        FPart(indx) = 'TILE_DRAIN_HYDROGRAPH'
      ELSE
        FPart(indx) = 'SUBSURFACE_IRIG_HYDROGRAPH'
      END IF
    END DO

    !Prepare header lines
    WorkArray(1)=ArrangeText('TILE DRAIN/SUBSURFACE IRRIGATION HYDROGRAPH',47)
    WorkArray(2)=ArrangeText('(UNIT=',TDHyd%UnitFlow,')',47)
    WorkArray(3)=ArrangeText('[(+): SUBSURFACE IRRIGATION INFLOW]',47)
    WorkArray(4)=ArrangeText('[(-): TILE DRAIN OUTFLOW          ]',47)
    CALL PrepareTitle(TitleLines(1),WorkArray(1:4),49,42)
    Header = ''
    WRITE (Header(1,1),'(A1,5X,A13)') '*','HYDROGRAPH ID'
    WRITE (Header(2,1),'(A1,14X,A4)') '*','NODE'
    WRITE (Header(3,1),'(A1,8X,A4)') '*','TIME'
    DO indx=1,TDHyd%NHyd
        IF (TDHyd%iHydTypes(indx) .EQ.  f_iSubIrig) THEN
            WRITE (Header(1,indx+1),'(I10)') SubIrigIDs(TDHyd%iHydIndices(indx))
            WRITE (Header(2,indx+1),'(I10)') NodeIDs(SubIrigNodes(TDHyd%iHydIndices(indx)))
            IOUTTD(indx) = NodeIDs(SubIrigNodes(TDHyd%iHydIndices(indx)))
        ELSE
            WRITE (Header(1,indx+1),'(I10)') DrainIDs(TDHyd%iHydIndices(indx))
            WRITE (Header(2,indx+1),'(I10)') NodeIDs(DrainNodes(TDHyd%iHydIndices(indx)))
            IOUTTD(indx) = NodeIDs(DrainNodes(TDHyd%iHydIndices(indx)))
        END IF  
    END DO
    HeaderFormat(1) = '(A19,2X,'//TRIM(Text)//'(2X,A10))'
    HeaderFormat(2) = '(A19,2X,'//TRIM(Text)//'(2X,A10))'
    HeaderFormat(3) = '(A13,'//TRIM(Text)//'(A))'
    
    !Prepare the time series output file
    CALL PrepareTSDOutputFile(TDHyd%OutFile            , &
                              NColumnsOfData           , &
                              NRowsOfData              , &
                              OverwriteNColumnsOfData  , &
                              FormatSpec               , &
                              TitleLines               , &
                              Header                   , &
                              HeaderFormat             , &
                              PrintColumnNo            , &
                              DataUnit                 , &
                              DataType                 , &
                              CPart                    , &
                              FPart                    , &
                              TimeStep%Unit            , &
                              GWNodes=IOUTTD           , &
                              iStat=iStat              )
    IF (iStat .EQ. -1) RETURN
    
    !Set the flag
    TDHyd%OutFile_Defined = .TRUE.

  END SUBROUTINE PrepTileDrainHydOutFile
  
  
  ! -------------------------------------------------------------
  ! --- NEW FILE FOR TILE DRAIN HYDROGRAPHS OPENED FOR INPUT FOR POST-PROCESSING
  ! -------------------------------------------------------------
  SUBROUTINE PrepTileDrainHydInFile_ForInquiry(TimeStep,NodeIDs,SubIrigNodes,DrainNodes,cFileName,TDHyd,iStat) 
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    INTEGER,INTENT(IN)            :: NodeIDs(:),SubIrigNodes(:),DrainNodes(:)
    CHARACTER(LEN=*),INTENT(IN)   :: cFileName
    TYPE(TileDrainHydrographType) :: TDHyd
    INTEGER,INTENT(OUT)           :: iStat
    
    !Local variables
    INTEGER           :: indx
    CHARACTER         :: cPathNames(TDHyd%NHyd)*80
    CHARACTER(LEN=32) :: BPart,EPart,FPart
    
    !Initialize
    iStat = 0
    
    !Instantiate file according to its type
    IF (iGetFileType_FromName(cFileName) .EQ. f_iDSS) THEN
        EPart = TimeStep%Unit
        !Form pathnames
        DO indx=1,TDHyd%NHyd
           IF (TDHyd%iHydTypes(indx) .EQ. f_iTileDrain) THEN
               BPart = 'GW' // TRIM(IntToText(NodeIDS(DrainNodes(TDHyd%iHydIndices(indx)))))
               FPart = 'TILE_DRAIN_HYDROGRAPH'
           ELSE
               BPart = 'GW' // TRIM(IntToText(NodeIDs(SubIrigNodes(TDHyd%iHydIndices(indx)))))
               FPart = 'SUBSURFACE_IRIG_HYDROGRAPH'
           END IF
            cPathnames(indx) = '/IWFM/' // TRIM(BPart) // '/FLOW//' // TRIM(EPart) // '/' //TRIM(FPart) // '/'
        END DO
        CALL TDHyd%InFile_ForInquiry%Init(cFileName,'tile drain and subsurface irrigation hydrographs file',TimeStep%TrackTime,nCol=TDHyd%NHyd,cPathNames=cPathNames,iStat=iStat) 
        IF (iStat .EQ. -1) RETURN
    ELSE
        CALL TDHyd%InFile_ForInquiry%Init(cFileName,'tile drain and subsurface irrigation hydrographs file',BlocksToSkip=0,nCol=TDHyd%NHyd,iStat=iStat) 
        IF (iStat .EQ. -1) RETURN
    END IF

    !Set the flag
    TDHyd%InFile_ForInquiry_Defined = .TRUE.
    
  END SUBROUTINE PrepTileDrainHydInFile_ForInquiry
  
  
  ! -------------------------------------------------------------
  ! --- TRANSFER HYDROGRAPHS FROM TEXT/DSS FILE TO HDF FILE
  ! -------------------------------------------------------------
  SUBROUTINE Transfer_To_HDF(TDHyd,NTIME,TimeStep,iStat)
    CLASS(TileDrainHydrographType) :: TDHyd
    INTEGER,INTENT(IN)             :: NTIME
    TYPE(TimeStepType),INTENT(IN)  :: TimeStep
    INTEGER,INTENT(OUT)            :: iStat
    
    !Local variables
    INTEGER                  :: NColumns(1),FileReadCode,indxTime
    REAL(8)                  :: rGWHeads(TDHyd%NHyd,1),rConvFactor
    CHARACTER                :: cDataSetName(1)*50,cHDFFileName*500
    TYPE(GenericFileType)    :: OutFile
    CHARACTER(:),ALLOCATABLE :: cFileName
    TYPE(TimeStepType)       :: TimeStep_Local
    
    !Initialize
    iStat = 0
    
    !Return if no output
    IF (.NOT. TDHyd%InFile_ForInquiry_Defined) RETURN
    
    !Get the name of the text/DSS file 
    CALL TDHyd%InFile_ForInquiry%GetFileName(cFileName)
    
    !Name for the HDF file
    cHDFFileName = TRIM(ADJUSTL(StripTextUntilCharacter(cFileName,'.',Back=.TRUE.))) // '.hdf'
    
    !Open output file HDF file
    CALL OutFile%New(FileName=TRIM(cHDFFileName),InputFile=.FALSE.,IsTSFile=.TRUE.,iStat=iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Create dataset
    NColumns(1)     = TDHyd%NHyd
    cDataSetName(1) = '/Tile_Drain_Hydrographs'
    CALL OutFile%CreateHDFDataSet(cPathNames=cDataSetName,NColumns=NColumns,NTime=NTIME,TimeStep=TimeStep,DataType=0d0,iStat=iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Conversion factor used when printing out results
    rConvFactor = 1.0 / TDHyd%FactFlow

    !Transfer heads to HDF file
    TimeStep_Local = TimeStep
    DO indxTime=1,NTIME
        !Read data
        CALL ReadTSData(TimeStep_Local,'tile drain hydrographs',TDHyd%InFile_ForInquiry,FileReadCode,iStat)
        IF (iStat .EQ. -1) RETURN
        
        !Transfer  values to matrix to be written to HDF file
        rGWHeads(:,1) = TDHyd%InFile_ForInquiry%rValues
        
        !Convert unit back to simulation units
        IF (TDHyd%FactFlow .NE. 1.0) rGWHeads = rGWHeads * rConvFactor
        
        !Store heads into HDF file
        CALL OutFile%WriteData(rGWHeads)
        
        !Advance time
        TimeStep_Local%CurrentTimeStep    = TimeStep_Local%CurrentTimeStep + 1
        TimeStep_Local%CurrentDateAndTime = IncrementTimeStamp(TimeStep_Local%CurrentDateAndTime,TimeStep_Local%DELTAT_InMinutes)
    END DO
    
    !Rewind input file
    CALL TDHyd%InFile_ForInquiry%File%RewindFile_To_BeginningOfTSData(iStat)
    
    !Close HDF file
    CALL OutFile%Kill()
    
  END SUBROUTINE Transfer_To_HDF


END MODULE