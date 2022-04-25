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
MODULE GWHydrograph
  USE MessageLogger          , ONLY: SetLastMessage            , &
                                     EchoProgress              , &
                                     MessageArray              , &
                                     f_iFatal
  USE IOInterface            , ONLY: GenericFileType           , &
                                     iGetFileType_FromName     , &
                                     f_iUNKNOWN                , &
                                     f_iTXT                    , &
                                     f_iDSS
  USE GeneralUtilities       , ONLY: ArrangeText               , &
                                     f_cLineFeed               , &
                                     StripTextUntilCharacter   , &
                                     CleanSpecialCharacters    , &
                                     PrepareTitle              , &
                                     FirstLocation             , &
                                     IntToText                 , &
                                     UpperCase                 , &
                                     LocateInList
  USE TimeSeriesUtilities    , ONLY: TimeStepType              , &
                                     TimeStampToJulian         , &
                                     IncrementTimeStamp        , &
                                     OPERATOR(.TSGT.)          
  USE Package_Misc           , ONLY: BaseHydrographType        , &
                                     HydOutputType             , &
                                     TecplotOutputType         , &
                                     Real2DTSDataInFileType    , &
                                     PrepareTSDOutputFile      , &
                                     ReadTSData                , &
                                     f_iHyd_GWHead             
  USE Package_Discretization , ONLY: AppGridType               , &
                                     StratigraphyType          , &
                                     AppFaceType               
  USE Class_GWState          , ONLY: GWStateType
  USE Class_AppBC            , ONLY: AppBCType
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
  PUBLIC :: GWHydrographType 
  
  
  ! -------------------------------------------------------------
  ! --- ELEMENT FACE FLOW HYDROGRAPH OUTPUT DATA TYPES
  ! -------------------------------------------------------------
  TYPE,EXTENDS(BaseHydrographType) :: FaceFlowHydType
      INTEGER :: iFace     = 0        !Face number for which flow hydrograph is to be printed
  END TYPE FaceFlowHydType
  
  TYPE FaceFlowOutputType
      TYPE(GenericFileType)             :: OutFile           !Output file for the face flow hydrographs
      INTEGER                           :: NHyd    = 0       !Number of hydrographs defined
      TYPE(FaceFlowHydType),ALLOCATABLE :: HydList(:)        !List of face flow hydrographs
  END TYPE FaceFlowOutputType
  
      
  ! -------------------------------------------------------------
  ! --- GW HYDROGRAPH DATA TYPE
  ! -------------------------------------------------------------
  TYPE GWHydrographType
      PRIVATE
      TYPE(HydOutputType),ALLOCATABLE          :: GWHydOutput                             !Groundwater hydrograph at user-defined locations print-out data 
      TYPE(GenericFileType),ALLOCATABLE        :: AllHeadOutFile                          !File to print heads at all nodes and layers
      TYPE(Real2DTSDataInFileType),ALLOCATABLE :: AllHeadOutFile_ForInquiry               !File that stores heads at all nodes and layers to be opened for post-processing
      TYPE(GenericFileType),ALLOCATABLE        :: CellVelocityOutFile                     !File to print-out velocities at cell centroids
      TYPE(FaceFlowOutputType),ALLOCATABLE     :: FaceFlowOutput                          !Element face flow print-out information
      TYPE(TecplotOutputType),ALLOCATABLE      :: HeadTecplotFile                         !File to print heads in Tecplot-readable format
      TYPE(TecplotOutputType),ALLOCATABLE      :: VelocityTecplotFile                     !File to print velocities in Tecplot-readable format
      REAL(8),ALLOCATABLE                      :: ElemCentroid_X(:)                       !X-coordinate of element centroid 
      REAL(8),ALLOCATABLE                      :: ElemCentroid_Y(:)                       !Y-coordinate of element centroid
      LOGICAL                                  :: lGWHydOutput_Defined         = .FALSE.  !Flag to check if an output file is defined
      LOGICAL                                  :: lAllHeadOutFile_Defined      = .FALSE.  !Flag to check if this output file is defined  
      LOGICAL                                  :: lCellVelocityOutFile_Defined = .FALSE.  !Flag to check if cell velocity output is defined 
      LOGICAL                                  :: lFaceFlowOutput_Defined      = .FALSE.  !Flag to check if element face flow output is defined 
      LOGICAL                                  :: lHeadTecplotFile_Defined     = .FALSE.  !Flag to check if this output file is defined
      LOGICAL                                  :: lVelocityTecplotFile_Defined = .FALSE.  !Flag to check if this output file is defined
  CONTAINS
      PROCEDURE,PASS :: New
      PROCEDURE,PASS :: Kill
      PROCEDURE,PASS :: ReadGWHeadsAll_ForALayer
      PROCEDURE,PASS :: ReadGWHead_AtNodeLayer
      PROCEDURE,PASS :: ReadGWHead_AtWell
      PROCEDURE,PASS :: PrintInitialValues
      PROCEDURE,PASS :: PrintResults
      PROCEDURE,PASS :: IsCellVelocityOutputDefined
      PROCEDURE,PASS :: IsVelocityTecplotDefined
      PROCEDURE,PASS :: IsFaceFlowOutputDefined
      PROCEDURE,PASS :: IsAllHeadOutputDefined
      PROCEDURE,PASS :: IsGWHydOutputDefined
      PROCEDURE,PASS :: GetAllHeadOutputFileName
      PROCEDURE,PASS :: GetGWHydOutputFileName
      PROCEDURE,PASS :: GetNGWHeadHydrographs
      PROCEDURE,PASS :: GetGWHeadHydrographIDs
      PROCEDURE,PASS :: GetGWHeadHydrographCoordinates
      PROCEDURE,PASS :: GetGWHeadHydrographNames
      PROCEDURE,PASS :: TransferOutputToHDF
  END TYPE GWHydrographType
  
  
  ! -------------------------------------------------------------
  ! --- MISC. ENTITIES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                   :: ModNameLen = 14
  CHARACTER(LEN=ModNameLen),PARAMETER :: ModName    = 'GWHydrograph::'
  
  
  
  
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
  ! --- NEW GW HYDROGRAPH DATA
  ! -------------------------------------------------------------
  SUBROUTINE New(GWHydData,IsForInquiry,AppGrid,Stratigraphy,cWorkingDirectory,iGWNodeIDs,FACTLTOU,UNITLTOU,UNITVLOU,UNITVROU,cAllHeadOutFileName,cCellVelocityFileName,cHeadTecplotFileName,cVelTecplotFileName,TimeStep,InFile,iStat) 
    CLASS(GWHydrographType),INTENT(OUT) :: GWHydData
    LOGICAL,INTENT(IN)                  :: IsForInquiry
    TYPE(AppGridType),INTENT(IN)        :: AppGrid
    TYPE(StratigraphyType),INTENT(IN)   :: Stratigraphy
    INTEGER,INTENT(IN)                  :: iGWNodeIDs(:)
    REAL(8),INTENT(IN)                  :: FACTLTOU
    CHARACTER(LEN=*),INTENT(IN)         :: cWorkingDirectory,UNITLTOU,UNITVLOU,UNITVROU,cAllHeadOutFileName,cCellVelocityFileName,cHeadTecplotFileName,cVelTecplotFileName
    TYPE(TimeStepType),INTENT(IN)       :: TimeStep
    TYPE(GenericFileType)               :: InFile
    INTEGER,INTENT(OUT)                 :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+3) :: ThisProcedure = ModName // 'New'
    INTEGER                     :: ErrorCode,NLayers,NElements
    CHARACTER                   :: cErrorMsg*300
    
    !Initialize
    iStat = 0
    
    !Inform user
    CALL EchoProgress('   Instantiating groundwater hydrograph print-out data...')
    
    !Initialize
    NLayers   = Stratigraphy%NLayers
    NElements = AppGrid%NElements
    
    !Instantiate file for the output of all heads
    IF (cAllHeadOutFileName .NE. '') THEN
        IF (IsForInquiry) THEN
            CALL AllHeadOutFile_ForInquiry_New(cAllHeadOutFileName,AppGrid%NNodes,NLayers,iGWNodeIDs,TimeStep,GWHydData%AllHeadOutFile_ForInquiry,iStat)
        ELSE
            CALL AllHeadOutFile_New(cAllHeadOutFileName,UNITLTOU,AppGrid%NNodes,NLayers,iGWNodeIDs,TimeStep,GWHydData%AllHeadOutFile,iStat)
        END IF
        IF (iStat .EQ. -1) RETURN
        GWHydData%lAllHeadOutFile_Defined = .TRUE.
    END IF
    
    !Instantiate file for the output of velocities at cell centers
    IF (cCellVelocityFileName .NE. '') THEN
        ALLOCATE (GWHydData%CellVelocityOutFile        , &
                  GWHydData%ElemCentroid_X(NElements)  , &
                  GWHydData%ElemCentroid_Y(NElements)  , &
                  STAT=ErrorCode ,ERRMSG=cErrorMsg     )
        IF (ErrorCode .NE. 0) THEN
            CALL SetLastMessage('Error in allocating memory for the print-out of groundwater velocities at cell centroids!'//NEW_LINE('')//TRIM(cErrorMsg),f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        CALL CellVelocityFile_New(IsForInquiry,cCellVelocityFileName,NLayers,AppGrid,GWHydData%CellVelocityOutFile,UNITLTOU,UNITVROU,FACTLTOU,GWHydData%ElemCentroid_X,GWHydData%ElemCentroid_Y,iStat)
        IF (iStat .EQ. -1) RETURN
        GWHydData%lCellVelocityOutFile_Defined = .TRUE.
    END IF
    
    !Instantiate file for head Tecplot output
    IF (cHeadTecplotFileName .NE. '') THEN
        ALLOCATE (GWHydData%HeadTecplotFile , STAT=ErrorCode ,ERRMSG=cErrorMsg)
        IF (ErrorCode .NE. 0) THEN
            CALL SetLastMessage('Error in allocating memory for groundwater head print-out for TecPlot!'//NEW_LINE('')//TRIM(cErrorMsg),f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        CALL GWHydData%HeadTecplotFile%New(IsForInquiry,cHeadTecplotFileName,'groundwater head print-out for TecPlot',iStat)  ;  IF (iStat .EQ. -1) RETURN
        GWHydData%lHeadTecplotFile_Defined = .TRUE.
    END IF
    
    !Instantiate file for velocity Tecplot output
    IF (cVelTecplotFileName .NE. '') THEN
        ALLOCATE (GWHydData%VelocityTecplotFile , STAT=ErrorCode ,ERRMSG=cErrorMsg)
        IF (ErrorCode .NE. 0) THEN
            CALL SetLastMessage('Error in allocating memory for groundwater velocities print-out for TecPlot!'//NEW_LINE('')//TRIM(cErrorMsg),f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        CALL GWHydData%VelocityTecplotFile%New(IsForInquiry,cVelTecplotFileName,'groundwater velocities print-out for TecPlot',iStat=iStat)
        IF (iStat .EQ. -1) RETURN
        GWHydData%lVelocityTecplotFile_Defined = .TRUE.
    END IF
    
    !Instantiate the user-specified hydrographs output data
    ALLOCATE (GWHydData%GWHydOutput , STAT=ErrorCode ,ERRMSG=cErrorMsg)
    IF (ErrorCode .NE. 0) THEN
        CALL SetLastMessage('Error in allocating memory for groundwater hydrograph printing at user-specified locations!'//NEW_LINE('')//TRIM(cErrorMsg),f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    CALL GWHydData%GWHydOutput%New(IsForInquiry,InFile,cWorkingDirectory,AppGrid,Stratigraphy,iGWNodeIDs,f_iHyd_GWHead,UNITLTOU,'HEAD',TimeStep,iStat)
    IF (iStat .EQ. -1) RETURN
    GWHydData%lGWHydOutput_Defined = GWHydData%GWHydOutput%IsDefined()
    IF (.NOT. GWHydData%lGWHydOutput_Defined) DEALLOCATE (GWHydData%GWHydOutput , STAT=ErrorCode)

    !Instantiate face flow hydrographs output data
    ALLOCATE (GWHydData%FaceFlowOutput , STAT=ErrorCode ,ERRMSG=cErrorMsg)
    IF (ErrorCode .NE. 0) THEN
        CALL SetLastMessage('Error in allocating memory for face flow hydrograph printing!'//NEW_LINE('')//TRIM(cErrorMsg),f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    CALL FaceFlowOutput_New(IsForInquiry,InFile,cWorkingDirectory,AppGrid%AppFace,NLayers,AppGrid%AppElement%ID,UNITVLOU,TimeStep,GWHydData%FaceFlowOutput,iStat)
    IF (iStat .EQ. -1) RETURN
    IF (GWHydData%FaceFlowOutput%OutFile%iGetFileType() .EQ. f_iUNKNOWN) THEN
        DEALLOCATE (GWHydData%FaceFlowOutput , STAT=ErrorCode)
    ELSE
        GWHydData%lFaceFlowOutput_Defined = .TRUE.
    END IF
   
  END SUBROUTINE New
  
  
  ! -------------------------------------------------------------
  ! --- NEW OUTPUT FILE FOR CELL VELOCITIES
  ! --- Note: Assumes a filename is specified
  ! -------------------------------------------------------------
  SUBROUTINE CellVelocityFile_New(IsForInquiry,cFileName,NLayers,AppGrid,CellVelocityFile,UNITLTOU,UNITVROU,FACTLTOU,XC,YC,iStat)
    LOGICAL,INTENT(IN)           :: IsForInquiry
    CHARACTER(LEN=*),INTENT(IN)  :: cFileName,UNITLTOU,UNITVROU
    INTEGER,INTENT(IN)           :: NLayers
    TYPE(AppGridType),INTENT(IN) :: AppGrid
    TYPE(GenericFileType)        :: CellVelocityFile
    REAL(8),INTENT(IN)           :: FACTLTOU
    REAL(8),INTENT(OUT)          :: XC(:),YC(:)
    INTEGER,INTENT(OUT)          :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+20) :: ThisProcedure = ModName // 'CellVelocityFile_New'
    INTEGER                      :: indxElem,indxLayer
    CHARACTER                    :: Text*1000
    REAL(8)                      :: XCL,YCL
    
    !Initialize
    iStat = 0
    
    !Open file
    IF (IsForInquiry) THEN
        CALL CellVelocityFile%New(FileName=cFileName,InputFile=.TRUE.,Descriptor='groundwater velocities at cell centroids output',iStat=iStat)
        RETURN
    ELSE
        CALL CellVelocityFile%New(FileName=cFileName,InputFile=.FALSE.,Descriptor='groundwater velocities at cell centroids output',iStat=iStat)
        IF (iStat .EQ. -1) RETURN
    END IF

    !Make sure that the file is a text file
    IF (CellVelocityFile%iGetFileType() .NE. f_iTXT) THEN
        CALL SetLastMessage('Output file ('//TRIM(cFileName)//') for cell groundwater velocities must be a text file!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    !Print coordinates of cell centroids
    CALL CellVelocityFile%WriteData('*'//REPEAT(' ',30)//REPEAT('*',50))
    CALL CellVelocityFile%WriteData('*'//REPEAT(' ',30)//'*        COORDINATES OF ELEMENT CENTROIDS        *')
    CALL CellVelocityFile%WriteData('*'//REPEAT(' ',30)//'*'//ArrangeText('(UNIT='//TRIM(UNITLTOU)//')',48)//'*')
    CALL CellVelocityFile%WriteData('*'//REPEAT(' ',30)//REPEAT('*',50))
    CALL CellVelocityFile%WriteData('*  ELEMENT                 X                 Y')
    DO indxElem=1,AppGrid%NElements
        CALL AppGrid%Centroid(indxElem,XCL,YCL)
        WRITE (Text,'(I7,5X,2(F16.4,2X))') AppGrid%AppElement(indxElem)%ID,XCL*FACTLTOU,YCL*FACTLTOU
        XC(indxElem) = XCL
        YC(indxElem) = YCL
        CALL CellVelocityFile%WriteData(TRIM(Text))        
    END DO
    CALL CellVelocityFile%WriteData(f_cLineFeed)
    
    !Print title lines for cell velocities
    CALL CellVelocityFile%WriteData('*'//REPEAT(' ',30)//REPEAT('*',50))
    CALL CellVelocityFile%WriteData('*'//REPEAT(' ',30)//'*          VELOCITIES AT CELL CENTROIDS          *')
    CALL CellVelocityFile%WriteData('*'//REPEAT(' ',30)//'*'//ArrangeText('(UNIT='//TRIM(UNITVROU)//')',48)//'*')
    CALL CellVelocityFile%WriteData('*'//REPEAT(' ',30)//REPEAT('*',50))
    WRITE (Text,'(A,100I18)') '*                   LAYER',(indxLayer,indxLayer,indxLayer,indxLayer=1,NLayers)
    CALL CellVelocityFile%WriteData(TRIM(Text))
    WRITE (Text,'(A,100A)')'*     TIME        ELEMENT',('                VX                VY                VZ',indxLayer=1,NLayers)
    CALL CellVelocityFile%WriteData(TRIM(Text))
    
  END SUBROUTINE CellVelocityFile_New
  
  
  ! -------------------------------------------------------------
  ! --- NEW OUTPUT FOR FACE FLOW HYDROGRAPH PRINTING
  ! -------------------------------------------------------------
  SUBROUTINE FaceFlowOutput_New(IsForInquiry,InFile,cWorkingDirectory,AppFace,NLayers,ElementIDs,UNITVLOU,TimeStep,FaceFlowOutput,iStat)
    LOGICAL,INTENT(IN)            :: IsForInquiry
    TYPE(GenericFileType)         :: InFile
    CHARACTER(LEN=*),INTENT(IN)   :: cWorkingDirectory
    TYPE(AppFaceType),INTENT(IN)  :: AppFace
    INTEGER,INTENT(IN)            :: NLayers,ElementIDs(:)
    CHARACTER(LEN=*),INTENT(IN)   :: UNITVLOU
    TYPE(TImeStepType),INTENT(IN) :: TimeStep
    TYPE(FaceFlowOutputType)      :: FaceFlowOutput
    INTEGER,INTENT(OUT)           :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+18) :: ThisProcedure = ModName // 'FaceFlowOutput_New'
    INTEGER                      :: NOUTF,indx,indx1,ErrorCode,iDummyArray(4),iNodes(2),  &
                                    ID,iHydLayer,iLoc,iFace
    CHARACTER                    :: cFileName*1200,ALine*1000,cErrorMsg*300
    
    !Read the general data
    CALL InFile%ReadData(NOUTF,iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(cFileName,iStat)  ;  IF (iStat .EQ. -1) RETURN  ;  cFileName = StripTextUntilCharacter(cFileName,'/')  ;  CALL CleanSpecialCharacters(cFileName)  
    cFileName = TRIM(cWorkingDirectory) // TRIM(ADJUSTL(cFileName))
    
    !If the filename is empty or the number of hydrographs is set to zero, return
    IF (cFileName .EQ. '') THEN
        DO indx=1,NOUTF
            CALL InFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN
        END DO
        RETURN
    ELSE
        IF (NOUTF .EQ. 0) RETURN
    END IF
    
    !Set the NHyd variable
    FaceFlowOutput%NHyd = NOUTF
    
    !Allocate memory
    ALLOCATE (FaceFlowOutput%HydList(NOUTF) ,STAT=ErrorCode , ERRMSG=cErrorMsg)
    IF (ErrorCode .NE. 0) THEN
        CALL SetLastMessage('Error in allocating memory for face flow hydrograph list!'//NEW_LINE(' ')//TRIM(cErrorMsg),f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF

    !Read and process data
    DO indx=1,NOUTF
        CALL InFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN  ;  ALine = StripTextUntilCharacter(ALine,'/')  ;  CALL CleanSpecialCharacters(ALine)  
        ALine = ADJUSTL(ALine)
        DO indx1=1,4
            READ (ALine,*) iDummyArray(indx1)
            iLoc = FirstLocation(' ',ALine)
            IF (iLoc .EQ. 0) THEN
                CALL SetLastMessage('Error in data entry for face flow hydrograph specification '//TRIM(IntToText(indx))//'!',f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
            ALine = ADJUSTL(ALine(iLoc:LEN_TRIM(ALine)))
        END DO
        ID        = iDummyArray(1)
        iHydLayer = iDummyArray(2)
        iNodes(1) = iDummyArray(3)
        iNodes(2) = iDummyArray(4)

        !Make sure hydrographs are entered sequentially
        IF (ID .NE. indx) THEN
            MessageArray(1) = 'Face flow hydrograph print-out specifications must be entered sequentially.'
            MessageArray(2) = 'Expected hydrograph ID = ' // TRIM(IntToText(indx))
            MessageArray(3) = 'Entered hydrograph ID  = ' // TRIM(IntToText(ID))
            CALL SetLastMessage(MessageArray(1:3),f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        
        !Make sure layer is modeled
        IF (iHydLayer .LT. 1   .OR.   iHydLayer .GT. NLayers) THEN
            CALL SetLastMessage('Face flow hydrograph layer listed for hydrograph ID '//TRIM(IntToText(ID))//' is outside model bounds!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        FaceFlowOutput%HydList(indx)%iLayer = iHydLayer
        
        !Find the element face id that corresponds to given nodes
        iFace = AppFace%GetFaceGivenNodes(iNodes)
        IF (iFace .EQ. 0) THEN
            MessageArray(1) = 'Groundwater nodes '//TRIM(IntToText(iNodes(1)))//' and '//TRIM(IntToText(iNodes(2)))//' do not define a face'
            MessageArray(2) = 'for element face flow printing!'
            CALL SetLastMessage(MessageArray(1:2),f_iFatal,ThisProcedure)            
            iStat = -1
            RETURN
        END IF 
        FaceFlowOutput%HydList(indx)%iFace = iFace
        
        !Name of the hydrograph
        FaceFlowOutput%HydList(indx)%cName = TRIM(ALine)
        
    END DO
    
    !Instantiate the output file 
    CALL PrepFaceFlowOutFile(IsForInquiry,cFileName,ElementIDs,UNITVLOU,NOUTF,FaceFlowOutput%HydList,AppFace,TimeStep,FaceFlowOutput%OutFile,iStat)
    
  END SUBROUTINE FaceFlowOutput_New
  
  
  ! -------------------------------------------------------------
  ! --- NEW OUTPUT FILE FOR ALL HEADS
  ! -------------------------------------------------------------
  SUBROUTINE AllHeadOutFile_New(cFileName,UNITLTOU,NNodes,NLayers,NodeIDs,TimeStep,OutFile,iStat)
    CHARACTER(LEN=*),INTENT(IN)       :: cFileName,UNITLTOU
    INTEGER,INTENT(IN)                :: NNodes,NLayers,NodeIDs(NNodes)
    TYPE(TimeStepType),INTENT(IN)     :: TimeStep
    TYPE(GenericFileType),ALLOCATABLE :: OutFile
    INTEGER,INTENT(OUT)               :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+18) :: ThisProcedure = ModName // 'AllHeadOutFile_New'
    INTEGER                      :: indx,I,J,ErrorCode
    CHARACTER                    :: Text*20,FormatSpec*500,DataUnit(1)*10,DataType(1)*10,    &
                                    CPart(1)*32,FPart(1)*32,Header(2,1+NNodes)*50,           &
                                    HeaderFormat(2)*500,WorkArray(2)*300,TitleLines(1)*3000, &
                                    cErrorMsg*300
    
    !Initialize
    iStat = 0
    
    !Inform user
    CALL EchoProgress('  Instantiating output file for groundwater heads at all nodes...') 
    
    !Initialize
    Header     = ''
    TitleLines = ''
    
    !Allocate memory for the file
    ALLOCATE (OutFile , STAT=ErrorCode ,ERRMSG=cErrorMsg)
    IF (ErrorCode .NE. 0) THEN
        CALL SetLastMessage('Error in allocating memory for groundwater head print-out at all nodes!'//NEW_LINE('')//TRIM(cErrorMsg),f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    !Open file
    CALL OutFile%New(FileName=cFileName,InputFile=.FALSE.,IsTSFile=.TRUE.,Descriptor='groundwater head at all nodes output',iStat=iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Make sure DSS file is used only if it is a time-tracking simulation
    IF (OutFile%iGetFileType() .EQ. f_iDSS) THEN
        IF (.NOT. TimeStep%TrackTime) THEN
            CALL SetLastMessage('DSS files for groundwater head printing at all nodes can only be used for time-tracking simulations.',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
    END IF

    !Prepare file for print-out
    Text = TRIM(IntToText(NNodes))//'(2X,F10.4)'
    SELECT CASE (NLayers)
      CASE (1)
        FormatSpec='(A21,'//TRIM(Text)//')'

      CASE DEFAULT
        FormatSpec=                             '(A21,'//TRIM(Text) // ","  //  &
                    TRIM(IntToText(NLayers-1))//'(/,21X,'//TRIM(Text)//'))' 

    END SELECT
    DataUnit(1) = UNITLTOU
    DataType(1) = 'INST-VAL'
    CPart(1)    = ADJUSTL('HEAD')
    FPart(1)    = 'GW_HEAD_AT_ALL_NODES'

    !Prepare header lines
    Text         = IntToText(NNodes)
    WorkArray(1) = ArrangeText('GROUNDWATER HEAD AT ALL NODES',37)
    WorkArray(2) = ArrangeText('(UNIT=',UNITLTOU,')',37)
    CALL PrepareTitle(TitleLines(1),WorkArray,39,42)
    WRITE (Header(1,1),'(A1,28X,A4)') '*','NODE'
    WRITE (Header(2,1),'(A1,8X,A4)')  '*','TIME'
    DO indx=1,NNodes
      WRITE (Header(2,indx+1),'(I7)') NodeIDs(indx)
    END DO
    HeaderFormat(1)='(A33,'//TRIM(Text)//'(A))'
    HeaderFormat(2)='(A13,8X,'//TRIM(Text)//'(5X,A7))'

    !Prepare the time series output file
    CALL PrepareTSDOutputFile(OutFile                                           , &
                              NColumnsOfData          = NNodes                  , &
                              NRowsOfData             = NLayers                 , &
                              OverwriteNColumnsOfData = .FALSE.                 , &
                              FormatSpec              = FormatSpec              , &
                              Title                   = TitleLines              , &
                              Header                  = Header                  , &
                              HeaderFormat            = HeaderFormat            , &
                              PrintColumnNo           = .FALSE.                 , &
                              DataUnit                = DataUnit                , &
                              DataType                = DataType                , &
                              CPart                   = CPart                   , &
                              FPart                   = FPart                   , &
                              UnitT                   = TimeStep%Unit           , &
                              Layers=[((I,J=1,NNodes),I=1,NLayers)]             , &
                              GWNodes=[((NodeIDs(I),I=1,NNodes),J=1,NLayers)]   , &
                              iStat=iStat                                       )

  END SUBROUTINE AllHeadOutFile_New
  
  
  ! -------------------------------------------------------------
  ! --- NEW FILE FOR ALL HEADS OPENED FOR INPUT FOR POST-PROCESSING
  ! -------------------------------------------------------------
  SUBROUTINE AllHeadOutFile_ForInquiry_New(cFileName,NNodes,NLayers,NodeIDs,TimeStep,InFile,iStat)
    CHARACTER(LEN=*),INTENT(IN)              :: cFileName
    INTEGER,INTENT(IN)                       :: NNodes,NLayers,NodeIDs(NNodes)
    TYPE(TimeStepType),INTENT(IN)            :: TimeStep
    TYPE(Real2DTSDataInFileType),ALLOCATABLE :: InFile
    INTEGER,INTENT(OUT)                      :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+29),PARAMETER :: ThisProcedure = ModName // 'AllHeadOutFile_ForInquiry_New'
    INTEGER                                :: ErrorCode,iCount,indxLayer,indxNode
    CHARACTER                              :: cErrorMsg*300,cPathNames(NNodes*NLayers)*80
    
    !Initialize
    iStat = 0
    
    !Allocate memory for the file
    ALLOCATE (InFile , STAT=ErrorCode , ERRMSG=cErrorMsg)
    IF (ErrorCode .NE. 0) THEN
        CALL SetLastMessage('Error in allocating memory for groundwater head print-out at all nodes!'//NEW_LINE('')//TRIM(cErrorMsg),f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
      
    !Instantiate file according to its type
    IF (iGetFileType_FromName(cFileName) .EQ. f_iDSS) THEN
        !Form pathnames
        iCount = 1
        DO indxLayer=1,NLayers
            DO indxNode=1,NNodes
                cPathnames(iCount) = '/IWFM/L' // TRIM(IntToText(indxLayer)) // ':GW' // TRIM(IntToText(NodeIDs(indxNode))) // '/HEAD//' // UpperCase(TRIM(TimeStep%Unit)) // '/GW_HEAD_AT_ALL_NODES/' 
                iCount             = iCount + 1
            END DO
        END DO
        CALL InFile%Init(cFileName,'groundwater head at all nodes',TimeStep%TrackTime,nRow=NLayers,nCol=NNodes,cPathNames=cPathNames,iStat=iStat)
    ELSE
        CALL InFile%Init(cFileName,'groundwater head at all nodes',BlocksToSkip=0,nRow=NLayers,nCol=NNodes,iStat=iStat)
    END IF
    
  END SUBROUTINE AllHeadOutFile_ForInquiry_New

    
  
  
  
! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** DESTRUCTORS
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- GATEWAY DESTRUCTOR METHOD
  ! -------------------------------------------------------------
  SUBROUTINE Kill(GWHydData)
    CLASS(GWHydrographType) :: GWHydData
    
    !Local variables
    INTEGER :: ErrorCode
    
    !Kill groundwater hydrograph output data
    IF (GWHydData%lGWHydOutput_Defined) THEN
        CALL GWHydData%GWHydOutput%Kill()
        DEALLOCATE (GWHydData%GWHydOutput , STAT=ErrorCode)
        GWHydData%lGWHydOutput_Defined = .FALSE.
    END IF
    
    !Kill groundwater-head-at-all-nodes output data
    IF (GWHydData%lAllHeadOutFile_Defined) THEN
        IF (ALLOCATED(GWHydData%AllHeadOutFile)) THEN
            CALL GWHydData%AllHeadOutFile%Kill()
            DEALLOCATE (GWHydData%AllHeadOutFile ,STAT=ErrorCode)
        END IF
        IF (ALLOCATED(GWHydData%AllHeadOutFile_ForInquiry)) THEN
            CALL GWHydData%AllHeadOutFile_ForInquiry%Close()
            DEALLOCATE (GWHydData%AllHeadOutFile_ForInquiry , STAT=ErrorCode)
        END IF
        GWHydData%lAllHeadOutFile_Defined = .FALSE.
    END IF
    
    !Kill velocity file
    IF (GWHydData%lCellVelocityOutFile_Defined) THEN
        CALL GWHydData%CellVelocityOutFile%Kill()
        DEALLOCATE (GWHydData%CellVelocityOutFile , STAT=ErrorCode)
        GWHydData%lCellVelocityOutFile_Defined = .FALSE.
    END IF
    
    !Kill head Tecplot output data
    IF (GWHydData%lHeadTecplotFile_Defined) THEN
        CALL GWHydData%HeadTecplotFile%Kill()
        DEALLOCATE (GWHydData%HeadTecplotFile ,STAT=ErrorCode)
        GWHydData%lHeadTecplotFile_Defined = .FALSE.
    END IF
    
    !Kill face flow output data
    IF (GWHydData%lFaceFlowOutput_Defined) THEN
        CALL FaceFlowOutput_Kill(GWHydData%FaceFlowOutput)
        DEALLOCATE (GWHydData%FaceFlowOutput ,STAT=ErrorCode)
        GWHydData%lFaceFlowOutput_Defined = .FALSE.
    END IF
    
    !Kill velocity Tecplot output data
    IF (GWHydData%lVelocityTecplotFile_Defined) THEN
        CALL GWHydData%VelocityTecplotFile%Kill()
        DEALLOCATE (GWHydData%VelocityTecplotFile ,STAT=ErrorCode)
        GWHydData%lVelocityTecplotFile_Defined = .FALSE.
    END IF
    
  END SUBROUTINE Kill
  
  
  ! -------------------------------------------------------------
  ! --- KILL FACE FLOW OUTPUT DATA
  ! -------------------------------------------------------------
  SUBROUTINE FaceFlowOutput_Kill(FaceFlowOutput)
    TYPE(FaceFlowOutputType) :: FaceFlowOutput
    
    !Local variables
    INTEGER :: ErrorCode
    
    !Close file
    CALL FaceFlowOutput%OutFile%Kill()
    
    !Clear memory
    DEALLOCATE (FaceFlowOutput%HydList , STAT=ErrorCode)
    FaceFlowOutput%NHyd = 0
    
  END SUBROUTINE FaceFlowOutput_Kill



  
! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** PREDICATES
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- CHECK IF CELL VELOCITY OUTPUT IS DEFINED
  ! -------------------------------------------------------------
  PURE FUNCTION IsCellVelocityOutputDefined(GWHydData) RESULT(lDefined)
    CLASS(GWHydrographType),INTENT(IN) :: GWHydData
    LOGICAL                            :: lDefined
    
    lDefined = GWHydData%lCellVelocityOutFile_Defined
    
  END FUNCTION IsCellVelocityOutputDefined
  
  
  ! -------------------------------------------------------------
  ! --- CHECK IF VELOCITY TECPLOT OUTPUT IS DEFINED
  ! -------------------------------------------------------------
  PURE FUNCTION IsVelocityTecplotDefined(GWHydData) RESULT(lDefined)
    CLASS(GWHydrographType),INTENT(IN) :: GWHydData
    LOGICAL                            :: lDefined
    
    lDefined = GWHydData%lVelocityTecplotFile_Defined
    
  END FUNCTION IsVelocityTecplotDefined
  
  
  ! -------------------------------------------------------------
  ! --- CHECK IF FACE FLOW OUTPUT IS DEFINED
  ! -------------------------------------------------------------
  PURE FUNCTION IsFaceFlowOutputDefined(GWHydData) RESULT(lDefined)
    CLASS(GWHydrographType),INTENT(IN) :: GWHydData
    LOGICAL                            :: lDefined
    
    lDefined = GWHydData%lFaceFlowOutput_Defined
    
  END FUNCTION IsFaceFlowOutputDefined
  
  
  ! -------------------------------------------------------------
  ! --- CHECK IF ALL HEAD OUTPUT IS DEFINED
  ! -------------------------------------------------------------
  PURE FUNCTION IsAllHeadOutputDefined(GWHydData) RESULT(lDefined)
    CLASS(GWHydrographType),INTENT(IN) :: GWHydData
    LOGICAL                            :: lDefined
    
    lDefined = GWHydData%lAllHeadOutFile_Defined
    
  END FUNCTION IsAllHeadOutputDefined


  ! -------------------------------------------------------------
  ! --- CHECK IF GW HYDROGRAPH OUTPUT IS DEFINED
  ! -------------------------------------------------------------
  PURE FUNCTION IsGWHydOutputDefined(GWHydData) RESULT(lDefined)
    CLASS(GWHydrographType),INTENT(IN) :: GWHydData
    LOGICAL                            :: lDefined
    
    lDefined = GWHydData%lGWHydOutput_Defined
    
  END FUNCTION IsGWHydOutputDefined
    
  
  
  
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
  ! --- GET FILENAME FOR THE HEAD-AT-ALL-NODES FILE
  ! -------------------------------------------------------------
  SUBROUTINE GetAllHeadOutputFileName(GWHydData,cFileName)
    CLASS(GWHydrographType),INTENT(IN)   :: GWHydData
    CHARACTER(:),ALLOCATABLE,INTENT(OUT) :: cFileName
    
    !Local variables
    INTEGER :: ErrorCode
    
    !Initialize
    DEALLOCATE (cFileName , STAT=ErrorCode)
    
    IF (GWHydData%lAllHeadOutFile_Defined) THEN
        IF (ALLOCATED(GWHydData%AllHeadOutFile)) THEN
            CALL GWHydData%AllHeadOutFile%GetName(cFileName)
        ELSE
            CALL GWHydData%AllHeadOutFile_ForInquiry%GetFileName(cFileName)
        END IF
    END IF
    
  END SUBROUTINE GetAllHeadOutputFileName
  
  
  ! -------------------------------------------------------------
  ! --- GET FILENAME FOR THE GW HYDROGRAPH OUTPUT FILE
  ! -------------------------------------------------------------
  SUBROUTINE GetGWHydOutputFileName(GWHydData,cFileName)
    CLASS(GWHydrographType),INTENT(IN)   :: GWHydData
    CHARACTER(:),ALLOCATABLE,INTENT(OUT) :: cFileName
    
    !Local variables
    INTEGER :: ErrorCode
    
    DEALLOCATE (cFileName , STAT=ErrorCode)
    CALL GWHydData%GWHydOutput%GetFileName(cFileName)

  END SUBROUTINE GetGWHydOutputFileName

  
  ! -------------------------------------------------------------
  ! --- GET NUMBER OF GW HEAD HYDROGRAPHS
  ! -------------------------------------------------------------
  FUNCTION GetNGWHeadHydrographs(GWHydData) RESULT(NHydrographs)
    CLASS(GWHydrographType),INTENT(IN) :: GWHydData
    INTEGER                            :: NHydrographs
    
    IF (GWHydData%lGWHydOutput_Defined) THEN
        NHydrographs = GWHydData%GWHydOutput%GetNHydrographs()
    ELSE
        NHydrographs = 0
    END IF
    
  END FUNCTION GetNGWHeadHydrographs
  
  
  ! -------------------------------------------------------------
  ! --- GET GW HEAD HYDROGRAPH IDS
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetGWHeadHydrographIDs(GWHydData,IDs)
    CLASS(GWHydrographType),INTENT(IN) :: GWHydData
    INTEGER,INTENT(OUT)                :: IDs(:)
    
    IF (GWHydData%lGWHydOutput_Defined) CALL GWHydData%GWHydOutput%GetHydrographIDs(IDs)
    
  END SUBROUTINE GetGWHeadHydrographIDs
  
  
  ! -------------------------------------------------------------
  ! --- GET GW HEAD HYDROGRAPH COORDINATES
  ! -------------------------------------------------------------
  SUBROUTINE GetGWHeadHydrographCoordinates(GWHydData,GridX,GridY,XHyd,YHyd)
    CLASS(GWHydrographType),INTENT(IN) :: GWHydData
    REAL(8),INTENT(IN)                 :: GridX(:),GridY(:)
    REAL(8),INTENT(OUT)                :: XHyd(:),YHyd(:)
    
    IF (GWHydData%lGWHydOutput_Defined) THEN
        CALL GWHydData%GWHydOutput%GetHydrographCoordinates(GridX,GridY,XHyd,YHyd)
    END IF
    
  END SUBROUTINE GetGWHeadHydrographCoordinates

  
  ! -------------------------------------------------------------
  ! --- GET GW HEAD HYDROGRAPH NAMES
  ! -------------------------------------------------------------
  SUBROUTINE GetGWHeadHydrographNames(GWHydData,cNamesList)
    CLASS(GWHydrographType),INTENT(IN) :: GWHydData
    CHARACTER(LEN=*),INTENT(OUT)       :: cNamesList(:)  !Assumes array is dimensioned previously based on the number of hydrographs
    
    IF (GWHydData%lGWHydOutput_Defined) THEN
        CALL GWHydData%GWHydOutput%GetHydrographNames(cNamesList)
    END IF
    
  END SUBROUTINE GetGWHeadHydrographNames
  
  
  
  
  
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
  ! --- READ SIMULATED GW HEADS AT ALL NODES AT A LAYER FOR A PERIOD OF TIME DIRECTLY FROM FILE
  ! --- Note: Assumes AllHeadOutFile_ForInquiry file exists
  ! -------------------------------------------------------------
  SUBROUTINE ReadGWHeadsAll_ForALayer(GWHydData,iNNodes,iLayer,cOutputBeginDateAndTime,cOutputEndDateAndTime,rOutputFactor,rConversionFactor,rOutputDates,rGWHeads,iStat)
    CLASS(GWHydrographType)     :: GWHydData
    INTEGER,INTENT(IN)          :: iNNodes,iLayer
    CHARACTER(LEN=*),INTENT(IN) :: cOutputBeginDateAndTime,cOutputEndDateAndTime
    REAL(8),INTENT(IN)          :: rOutputFactor,rConversionFactor   !rOutputFactor is the conversion factor when the heads were being printed out during simulation; rConversionFactor is the factor to convert the heads in the simulation units to the output units for post-processing
    REAL(8),INTENT(OUT)         :: rOutputDates(:),rGWHeads(:,:)     !rGWHeads are given in (node,time) format 
    INTEGER,INTENT(OUT)         :: iStat
    
    !Local variables
    INTEGER :: ErrorCode,iPathNameIndex,indxNode,nActualOutput,iOffset
    REAL(8) :: rEffectiveFactor,rValues(SIZE(rOutputDates)),rGWHeadsTemp(SIZE(rGWHeads,DIM=2),SIZE(rGWHeads,DIM=1))
    
    !Initialize
    iOffset = (iLayer - 1) * GWHydData%AllHeadOutFile_ForInquiry%nCol
    
    !Loop over nodes
    DO indxNode=1,iNNodes
        !Define the pathname index to read from DSS file
        iPathNameIndex = iOffset + indxNode
        
        !Rewind file
        CALL GWHydData%AllHeadOutFile_ForInquiry%File%RewindFile_To_BeginningOfTSData(iStat)
    
        !Read data
        CALL GWHydData%AllHeadOutFile_ForInquiry%ReadData(iLayer,indxNode,iPathNameIndex,cOutputBeginDateAndTime,cOutputEndDateAndTime,nActualOutput,rValues,rOutputDates,ErrorCode,iStat)  
        IF (iStat .EQ. -1) RETURN
        
        !Transfer to temporary array
        rGWHeadsTemp(:,indxNode) = rValues
    END DO
    
    !Transpose and store heads in persistent array
    rGWHeads = TRANSPOSE(rGWHeadsTemp)
    
    !Convert unit
    rEffectiveFactor = rOutputFactor / rConversionFactor 
    IF (rEffectiveFactor .NE. 1d0) rGWHeads = rGWHeads * rEffectiveFactor
    
    !Rewind file
    CALL GWHydData%AllHeadOutFile_ForInquiry%File%RewindFile_To_BeginningOfTSData(iStat)
    
  END SUBROUTINE ReadGWHeadsAll_ForALayer
  
  
  ! -------------------------------------------------------------
  ! --- READ SIMULATED GW HEADS AT A NODE AT A LAYER WHEN FILE IS PART OF GWHydrographType OBJECT
  ! --- Note: Assumes AllHeadOutFile_ForInquiry file exists
  ! -------------------------------------------------------------
  SUBROUTINE ReadGWHead_AtNodeLayer(GWHydData,iNode,iLayer,cOutputBeginDateAndTime,cOutputEndDateAndTime,rOutputFactor,rConversionFactor,nActualOutput,rOutputDates,rOutputValues,iStat)
    CLASS(GWHydrographType)     :: GWHydData
    INTEGER,INTENT(IN)          :: iNode,iLayer
    CHARACTER(LEN=*),INTENT(IN) :: cOutputBeginDateAndTime,cOutputEndDateAndTime
    REAL(8),INTENT(IN)          :: rOutputFactor,rConversionFactor   !rOutputFactor is the conversion factor when the heads were being printed out during simulation; rConversionFactor is the factor to convert the heads in the simulation units to the output units for post-processing
    INTEGER,INTENT(OUT)         :: nActualOutput
    REAL(8),INTENT(OUT)         :: rOutputDates(:),rOutputValues(:)
    INTEGER,INTENT(OUT)         :: iStat
    
    !Local variables
    INTEGER :: ErrorCode,iPathNameIndex
    REAL(8) :: rEffectiveFactor
    
    !Define the pathname index to read from DSS file
    iPathNameIndex = (iLayer - 1) * GWHydData%AllHeadOutFile_ForInquiry%nCol + iNode
    
    !Read data
    CALL GWHydData%AllHeadOutFile_ForInquiry%ReadData(iLayer,iNode,iPathNameIndex,cOutputBeginDateAndTime,cOutputEndDateAndTime,nActualOutput,rOutputValues,rOutputDates,ErrorCode,iStat)  
    IF (iStat .EQ. -1) RETURN
    
    !Convert unit
    rEffectiveFactor = rOutputFactor / rConversionFactor 
    IF (rEffectiveFactor .NE. 1d0) rOutputValues = rOutputValues * rEffectiveFactor
    
    !Rewind file
    CALL GWHydData%AllHeadOutFile_ForInquiry%File%RewindFile_To_BeginningOfTSData(iStat)
    
  END SUBROUTINE ReadGWHead_AtNodeLayer
  
  
  ! -------------------------------------------------------------
  ! --- READ SIMULATED GW HEADS AT A WELL
  ! --- Note: Assumes GWHydOutput_ForInquiry file exists
  ! -------------------------------------------------------------
  SUBROUTINE ReadGWHead_AtWell(GWHydData,iWellID,cOutputBeginDateAndTime,cOutputEndDateAndTime,rOutputFactor,rConversionFactor,nActualOutput,rOutputDates,rOutputValues,iStat)
    CLASS(GWHydrographType)      :: GWHydData
    INTEGER,INTENT(IN)           :: iWellID
    CHARACTER(LEN=*),INTENT(IN)  :: cOutputBeginDateAndTime,cOutputEndDateAndTime
    REAL(8),INTENT(IN)           :: rOutputFactor,rConversionFactor
    INTEGER,INTENT(OUT)          :: nActualOutput,iStat
    REAL(8),INTENT(OUT)          :: rOutputDates(:),rOutputValues(:)
    
    CALL GWHydData%GWHydOutput%ReadHydrograph_AtLocation(iWellID,cOutputBeginDateAndTime,cOutputEndDateAndTime,rOutputFactor,rConversionFactor,nActualOutput,rOutputDates,rOutputValues,iStat)
    
  END SUBROUTINE ReadGWHead_AtWell

  
  
  
  
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
  ! --- GATEWAY TO PRINTING INITIAL VALUES
  ! -------------------------------------------------------------
  SUBROUTINE PrintInitialValues(GWHydData,AppGrid,Stratigraphy,Heads,FACTLTOU,FACTVROU,StrmConnectivity,TimeStep)
    CLASS(GWHydrographType)           :: GWHydData
    TYPE(AppGridType),INTENT(IN)      :: AppGrid
    TYPE(StratigraphyType),INTENT(IN) :: Stratigraphy
    REAL(8),INTENT(IN)                :: Heads(:,:),FACTLTOU,FACTVROU
    COMPLEX,INTENT(IN)                :: StrmConnectivity(:)
    TYPE(TimeStepType),INTENT(IN)     :: TimeStep
    
    !Local variables
    INTEGER                     :: NLayers,indxLayer
    REAL(8)                     :: FactorsHead(Stratigraphy%NLayers),FactorsVel(3*Stratigraphy%NLayers),          &
                                   DummyVels(AppGrid%NNodes,3*Stratigraphy%NLayers)
    CHARACTER                   :: cVarNamesHead(Stratigraphy%NLayers)*10,cVarNamesVel(3*Stratigraphy%NLayers)*10
    CHARACTER(LEN=26),PARAMETER :: cFormatHead = '(50(F12.3,2X))' , cFormatVel = '(2(F12.3,2X),75(F12.5,2X))'
    
    !Initialize
    NLayers = Stratigraphy%NLayers
    
    !Print user-specified hydrographs
    IF (GWHydData%lGWHydOutput_Defined)  &
        CALL GWHydData%GWHydOutput%PrintResults(Stratigraphy,f_iHyd_GWHead,Heads,FACTLTOU,TimeStep,.FALSE.)
    
    !Print heads at all nodes
    IF (GWHydData%lAllHeadOutFile_Defined)  &
        CALL AllHeadOutFile_PrintResults(Heads,FACTLTOU,TimeStep,.FALSE.,GWHydData%AllHeadOutFile)
    
    !Print Head Tecplot output
    IF (GWHydData%lHeadTecplotFile_Defined) THEN
        cVarNamesHead = [('GWHEAD'//TRIM(IntToText(indxLayer)) , indxLayer=1,NLayers)]
        FactorsHead   = FACTLTOU
        CALL GWHydData%HeadTecplotFile%PrintInitialValues(AppGrid,StrmConnectivity,Heads,FactorsHead,cFormatHead,cVarNamesHead,TimeStep)
    END IF
    
    !Print Velocity Tecplot output
    IF (GWHydData%lVelocityTecplotFile_Defined) THEN
        cVarNamesVel(1::3) = [('VX_L'//TRIM(IntToText(indxLayer)) , indxLayer=1,NLayers)]
        cVarNamesVel(2::3) = [('VY_L'//TRIM(IntToText(indxLayer)) , indxLayer=1,NLayers)]
        cVarNamesVel(3::3) = [('VZ_L'//TRIM(IntToText(indxLayer)) , indxLayer=1,NLayers)]
        FactorsVel         = FACTVROU
        DummyVels          = 0.0
        CALL GWHydData%VelocityTecplotFile%PrintInitialValues(AppGrid,StrmConnectivity,DummyVels,FactorsVel,cFormatVel,cVarNamesVel,TimeStep)
    END IF
    
  END SUBROUTINE PrintInitialValues
  
  
  ! -------------------------------------------------------------
  ! --- GATEWAY TO ALL HYDROGRAPH PRINTING
  ! -------------------------------------------------------------
  SUBROUTINE PrintResults(GWHydData,AppGrid,Stratigraphy,AppBC,GWState,FaceFlows,SWShedFaceFlows,FACTLTOU,FACTVLOU,FACTVROU,TimeStep,lEndOfSimulation)
    CLASS(GWHydrographType)           :: GWHydData
    TYPE(AppGridType),INTENT(IN)      :: AppGrid
    TYPE(StratigraphyType),INTENT(IN) :: Stratigraphy
    TYPE(AppBCType),INTENT(IN)        :: AppBC
    TYPE(GWStateType),INTENT(IN)      :: GWState
    REAL(8),INTENT(IN)                :: FaceFlows(:,:),SWShedFaceFlows(:,:),FACTLTOU,FACTVLOU,FACTVROU
    TYPE(TimeStepType),INTENT(IN)     :: TimeStep
    LOGICAL,INTENT(IN)                :: lEndOfSimulation
    
    !Local variables
    INTEGER :: indx,indxLayer
    REAL(8) :: Vels(AppGrid%NNodes,3*Stratigraphy%NLayers)
    
    !Print user-specified hydrographs
    IF (GWHydData%lGWHydOutput_Defined)  &
        CALL GWHydData%GWHydOutput%PrintResults(Stratigraphy,f_iHyd_GWHead,GWState%Head,FACTLTOU,TimeStep,lEndOfSimulation)
    
    !Print heads at all nodes
    IF (GWHydData%lAllHeadOutFile_Defined)  &
        CALL AllHeadOutFile_PrintResults(GWState%Head,FACTLTOU,TimeStep,lEndOfSimulation,GWHydData%AllHeadOutFile)
    
    !Print head Tecplot output
    IF (GWHydData%lHeadTecplotFile_Defined)  &
        CALL GWHydData%HeadTecplotFile%PrintResults(GWState%Head,FACTLTOU,TimeStep)
    
    !Print velocity Tecplot output
    IF (GWHydData%lVelocityTecplotFile_Defined) THEN
        indx = 1
        DO indxLayer=1,3*Stratigraphy%NLayers,3
            Vels(:,indxLayer)   = GWState%Vx(:,indx)
            Vels(:,indxLayer+1) = GWState%Vy(:,indx)
            Vels(:,indxLayer+2) = GWState%Vz(:,indx)
            indx                = indx + 1
        END DO
        CALL GWHydData%VelocityTecplotFile%PrintResults(Vels,FACTVROU,TimeStep)
    END IF
    
    !Print velocities at cell centroids
    IF (GWHydData%lCellVelocityOutFile_Defined)  &
        CALL CellVelocitiesOutput_PrintResults(AppGrid,Stratigraphy%NLayers,TimeStep,GWState,FACTVROU,GWHydData%ElemCentroid_X,GWHydData%ElemCentroid_Y,GWHydData%CellVelocityOutFile)
    
    !Print face flow output
    IF (GWHydData%lFaceFlowOutput_Defined)  &
        CALL FaceFlowOutput_PrintResults(AppGrid,AppBC,FACTVLOU,FaceFlows,SWShedFaceFlows(:,:),TimeStep,lEndOfSimulation,GWHydData%FaceFlowOutput)
    
  END SUBROUTINE PrintResults
  
  
  ! -------------------------------------------------------------
  ! --- PRINT CELL VELOCITIES
  ! -------------------------------------------------------------
  SUBROUTINE CellVelocitiesOutput_PrintResults(AppGrid,NLayers,TimeStep,GWState,FACTVROU,ElemCentroid_X,ElemCentroid_Y,CellVelocityFile)
    TYPE(AppGridType),INTENT(IN)  :: AppGrid
    INTEGER,INTENT(IN)            :: NLayers
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    TYPE(GWStateType),INTENT(IN)  :: GWState
    REAL(8),INTENT(IN)            :: FACTVROU,ElemCentroid_X(AppGrid%NElements),ElemCentroid_Y(AppGrid%NElements)
    TYPE(GenericFileType)         :: CellVelocityFile
    
    !Local variables
    INTEGER                                      :: indxLayer,indxElem,NVertex,Vertex(4),NElements
    REAL(8)                                      :: Coeff(4)
    CHARACTER                                    :: Text*1000
    REAL(8),DIMENSION(NLayers,AppGrid%NElements) :: Vx,Vy,Vz
    
    !Initialize
    NElements = AppGrid%NElements
    
    !Compute velocities at cell centroids using FE interpolation when there are more than 1 aquifer layers
    IF (NLayers .GT. 1) THEN
        DO indxElem=1,NElements
            NVertex = AppGrid%NVertex(indxElem)
            Vertex  = AppGrid%Vertex(:,indxElem)
            CALL AppGrid%FEInterpolate_AtCell(indxElem,ElemCentroid_X(indxElem),ElemCentroid_Y(indxElem),Coeff(1:NVertex))
            DO indxLayer=1,NLayers
                Vx(indxLayer,indxElem) = SUM(Coeff(1:NVertex) * GWState%Vx(Vertex(1:NVertex),indxLayer)) * FACTVROU
                Vy(indxLayer,indxElem) = SUM(Coeff(1:NVertex) * GWState%Vy(Vertex(1:NVertex),indxLayer)) * FACTVROU
                Vz(indxLayer,indxElem) = SUM(Coeff(1:NVertex) * GWState%Vz(Vertex(1:NVertex),indxLayer)) * FACTVROU
           END DO
        END DO
    
    !Compute velocities at cell centroids using FE interpolation when there is only 1 aquifer layer
    ELSE
        Vz = 0.0
        DO indxElem=1,NElements
            NVertex = AppGrid%NVertex(indxElem)
            Vertex  = AppGrid%Vertex(:,indxElem)
            CALL AppGrid%FEInterpolate_AtCell(indxElem,ElemCentroid_X(indxElem),ElemCentroid_Y(indxElem),Coeff(1:NVertex))
            Vx(1,indxElem) = SUM(Coeff(1:NVertex) * GWState%Vx(Vertex(1:NVertex),1)) * FACTVROU
            Vy(1,indxElem) = SUM(Coeff(1:NVertex) * GWState%Vy(Vertex(1:NVertex),1)) * FACTVROU
        END DO
    END IF
    
    !First line
    WRITE (Text,'(A16,I9,2X,100(F16.4,2x))') TimeStep%CurrentDateAndTime,AppGrid%AppElement(1)%ID,(Vx(indxLayer,1),Vy(indxLayer,1),Vz(indxLayer,1),indxLayer=1,NLayers)
    CALL CellVelocityFile%WriteData(TRIM(Text))
    
    !Rest of the lines
    DO indxElem=2,NElements
        WRITE (Text,'(16X,I9,2X,100(F16.4,2x))') AppGrid%AppElement(indxElem)%ID,(Vx(indxLayer,indxElem),Vy(indxLayer,indxElem),Vz(indxLayer,indxElem),indxLayer=1,NLayers)
        CALL CellVelocityFile%WriteData(TRIM(Text))        
    END DO
    
  END SUBROUTINE CellVelocitiesOutput_PrintResults
  
  
  ! -------------------------------------------------------------
  ! --- PRINT-OUT FACE FLOWS
  ! -------------------------------------------------------------
  SUBROUTINE FaceFlowOutput_PrintResults(AppGrid,AppBC,FACTVLOU,FaceFlows,SWShedFaceFlows,TimeStep,lEndOfSimulation,FaceFlowOutput)
    TYPE(AppGridType),INTENT(IN)  :: AppGrid
    TYPE(AppBCType),INTENT(IN)    :: AppBC
    REAL(8),INTENT(IN)            :: FACTVLOU,FaceFlows(:,:),SWShedFaceFlows(:,:)
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    LOGICAL,INTENT(IN)            :: lEndOfSimulation
    TYPE(FaceFlowOutputType)      :: FaceFlowOutput
    
    !Local variables
    INTEGER   :: indx,iLayer,iFace,indx1
    REAL(8)   :: DummyArray(FaceFlowOutput%NHyd),rBndFaceFlow
    CHARACTER :: SimulationTime*16
    
    !Compile data
    DO indx=1,FaceFlowOutput%NHyd
      iLayer           = FaceFlowOutput%HydList(indx)%iLayer
      iFace            = FaceFlowOutput%HydList(indx)%iFace
      DummyArray(indx) = FaceFlows(iFace,iLayer)
      
      !Add flows due to boundary conditions
      IF (AppBC%IsDefined()) THEN
          IF (AppGrid%AppFace%BoundaryFace(iFace)) THEN
              CALL AppBC%GetBoundaryFlowAtFaceLayer(AppGrid,iFace,iLayer,rBndFaceFlow)
              DummyArray(indx) = DummyArray(indx) + rBndFaceFlow
              !Add base flows from small watersheds
              indx1            = LocateInList(iFace,AppGrid%BoundaryFaceList)
              DummyArray(indx) = DummyArray(indx) + SWShedFaceFlows(indx1,iLayer)
          END IF
      END IF
      
    END DO
    
    !Scale print-out variables
    DummyArray = DummyArray * FACTVLOU

    !Create the simulation time
    IF (TimeStep%TrackTime) THEN
      SimulationTime = ADJUSTL(TimeStep%CurrentDateAndTime)
    ELSE
      WRITE(SimulationTime,'(F10.2,1X,A10)') TimeStep%CurrentTime,ADJUSTL(TimeStep%Unit)
    END IF

    !Print out the results
    CALL FaceFlowOutput%OutFile%WriteData(SimulationTime,DummyArray,FinalPrint=lEndOfSimulation)
        
  END SUBROUTINE FaceFlowOutput_PrintResults
  
  
  ! -------------------------------------------------------------
  ! --- PRINT-OUT GW HEADS AT ALL NODES
  ! -------------------------------------------------------------
  SUBROUTINE AllHeadOutFile_PrintResults(Heads,FACTLTOU,TimeStep,lEndOfSimulation,OutFile)
    REAL(8),INTENT(IN)            :: Heads(:,:),FACTLTOU
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    LOGICAL,INTENT(IN)            :: lEndOfSimulation
    TYPE(GenericFileType)         :: OutFile
    
    !Local variables
    INTEGER   :: indxLayer,indxNode,NNodes,NLayers
    REAL(8)   :: DummyArray(SIZE(Heads,DIM=2),SIZE(Heads,DIM=1))
    CHARACTER :: SimulationTime*21
    
    !Initialize
    NNodes  = SIZE(Heads , DIM=1)
    NLayers = SIZE(Heads , DIM=2)
    
    !Prepare data for printing
    DO indxLayer=1,NLayers
      DO indxNode=1,NNodes
        DummyArray(indxLayer,indxNode) = Heads(indxNode,indxLayer) * FACTLTOU
      END DO
    END DO
    
    !Create the simulation time
    IF (TimeStep%TrackTime) THEN
      SimulationTime = ADJUSTL(TimeStep%CurrentDateAndTime)
    ELSE
      WRITE(SimulationTime,'(F10.2,1X,A10)') TimeStep%CurrentTime,ADJUSTL(TimeStep%Unit)
    END IF

    !Print out the results
    CALL OutFile%WriteData(SimulationTime,DummyArray,FinalPrint=lEndOfSimulation)
    
  END SUBROUTINE AllHeadOutFile_PrintResults
  
  
  
  
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
  ! --- PREPARE FACE FLOW HYDROGRAPH OUTPUT FILE FOR PRINT-OUT
  ! -------------------------------------------------------------
  SUBROUTINE PrepFaceFlowOutFile(IsForInquiry,cFileName,ElementIDs,UNITVLOU,NHyd,HydList,AppFace,TimeStep,OutFile,iStat)
    LOGICAL,INTENT(IN)               :: IsForInquiry
    CHARACTER(LEN=*),INTENT(IN)      :: cFileName,UNITVLOU
    INTEGER,INTENT(IN)               :: ElementIDs(:),NHyd
    TYPE(FaceFlowHydType),INTENT(IN) :: HydList(NHyd)
    TYPE(AppFaceType),INTENT(IN)     :: AppFace
    TYPE(TimeStepType),INTENT(IN)    :: TimeStep
    TYPE(GenericFileType)            :: OutFile
    INTEGER,INTENT(OUT)              :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+19) :: ThisProcedure = ModName // 'PrepFaceFlowOutFile'
    INTEGER                      :: iElem1,iElem2,iFace,iLayers(NHyd),indx
    CHARACTER                    :: Text*20,cFormatSpec*30,DataUnit(1)*10,DataType(1)*10,CPart(1)*32,  &
                                    FPart(1)*32,Header(3,1+NHyd)*50,HeaderFormat(3)*500,               &
                                    DummyCharArray(NHyd)*32,TitleLines(1)*500,WorkArray(2)*500
    
    !Initialize
    iStat   = 0
    iLayers = HydList%iLayer
    Header  = ''
    
    !Open file
    IF (IsForInquiry) THEN
        CALL OutFile%New(FileName=cFileName,InputFile=.TRUE.,IsTSFile=.TRUE.,Descriptor='element face flow output',iStat=iStat)
        RETURN
    ELSE
        CALL OutFile%New(FileName=cFileName,InputFile=.FALSE.,IsTSFile=.TRUE.,Descriptor='element face flow output',iStat=iStat)
        IF (iStat .EQ. -1) RETURN
    END IF
    
    !Make sure that DSS file is used only if it is a time tracking simulation
    IF (OutFile%iGetFileType() .EQ. f_iDSS) THEN
        IF (.NOT. TimeStep%TrackTime) THEN
            CALL SetLastMessage('DSS files for face flow hydrograph printing can only be used for time-tracking simulations.',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
    END IF
    
    !Prepare data for ASCII output
    Text        = IntToText(NHyd)
    cFormatSpec = '(A21,'//TRIM(Text)//'(2X,F14.2))'
    DO indx=1,NHyd
      iFace                = HydList(indx)%iFace
      iElem1               = ElementIDs(AppFace%Element(1,iFace))
      iElem2               = ElementIDs(AppFace%Element(2,iFace))
      DummyCharArray(indx) = 'E'//TRIM(IntToText(iElem1))//'-E'//TRIM(IntToText(iElem2))
    END DO
    WorkArray(1) = ArrangeText('ELEMENT FACE FLOW',37)
    WorkArray(2) = ArrangeText('(UNIT=',UNITVLOU,')',37)
    CALL PrepareTitle(TitleLines(1),WorkArray,39,42)
    WRITE (Header(1,1),'(A1,15X,A5)') '*','LAYER'
    WRITE (Header(2,1),'(A1,16X,A4)') '*','FACE'
    WRITE (Header(3,1),'(A1,8X,A4)')  '*','TIME'
    DO indx=1,NHyd
        WRITE (Header(1,indx+1),'(I14)') HydList(indx)%iLayer
        WRITE (Header(2,indx+1),'(A14)') TRIM(DummyCharArray(indx))
        WRITE (Header(3,indx+1),'(A14)') TRIM(HydList(indx)%cName)
    END DO
    HeaderFormat(1) = '(A21,'//TRIM(Text)//'(2X,A14))'
    HeaderFormat(2) = '(A21,'//TRIM(Text)//'(2X,A14))'
    HeaderFormat(3) = '(A13,8X'//TRIM(Text)//'(2X,A14))'

    !Prepare data for DSS output
    DataUnit(1) = ADJUSTL(UNITVLOU)
    DataType(1) = 'PER-AVER'
    CPart(1)    = ADJUSTL('FLOW')
    FPart(1)    = 'element_face_flow'

    !Prepare the time series output file
    CALL PrepareTSDOutputFile(OutFile                                   , &
                              NColumnsOfData           = Nhyd           , &
                              NRowsOfData              = 1              , &
                              OverwriteNColumnsOfData  = .TRUE.         , &
                              FormatSpec               = cFormatSpec    , &
                              Title                    = TitleLines     , &
                              Header                   = Header         , &
                              HeaderFormat             = HeaderFormat   , &
                              PrintColumnNo            = .FALSE.        , &
                              DataUnit                 = DataUnit       , &
                              DataType                 = DataType       , &
                              CPart                    = CPart          , &
                              FPart                    = FPart          , &
                              UnitT                    = TimeStep%Unit  , &
                              Layers                   = iLayers        , &
                              MiscArray                = DummyCharArray , &
                              iStat                    = iStat          )
    
  END SUBROUTINE PrepFaceFlowOutFile
  
  
  ! -------------------------------------------------------------
  ! --- GATEWAY PROCEDURE TO TRANSFER HYDROGRAPHS TO HDF
  ! -------------------------------------------------------------
  SUBROUTINE TransferOutputToHDF(GWHyd,NNodes,NLayers,NTIME,TimeStep,rFactHead,iStat)
    CLASS(GWHydrographType)       :: GWHyd
    INTEGER,INTENT(IN)            :: NNodes,NLayers,NTIME
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    REAL(8),INTENT(IN)            :: rFactHead
    INTEGER,INTENT(OUT)           :: iStat
    
    !Heads at all nodes
    IF (GWHyd%lAllHeadOutFile_Defined) CALL TransferAllHeadOut_To_HDF(GWHyd%AllHeadOutFile_ForInquiry,NNodes,NLayers,NTIME,TimeStep,rFactHead,iStat)
    IF (iStat .EQ. -1) RETURN
    
    !GW hydrographs at user-defined locations
    IF (GWHyd%lGWHydOutput_Defined) CALL GWHyd%GWHydOutput%Transfer_To_HDF('groundwater hydrographs at user-defined locations','/cGWHydrographs',NTIME,TimeStep,rFactHead,iStat)
    
  END SUBROUTINE TransferOutputToHDF
    
  
  ! -------------------------------------------------------------
  ! --- TRANSFER HEADS AT ALL NODES FROM TEXT/DSS FILE TO HDF FILE
  ! -------------------------------------------------------------
  SUBROUTINE TransferAllHeadOut_To_HDF(AllHeadsInFile,NNodes,NLayers,NTIME,TimeStep,rFactHead,iStat)
    TYPE(Real2DTSDataInFileType)  :: AllHeadsInFile
    INTEGER,INTENT(IN)            :: NNodes,NLayers,NTIME
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    REAL(8),INTENT(IN)            :: rFactHead
    INTEGER,INTENT(OUT)           :: iStat
    
    !Local variables
    INTEGER                  :: NColumns(1),FileReadCode,indxTime
    REAL(8)                  :: rGWHeads(NNodes*NLayers,1),rConvFactor
    CHARACTER                :: cDataSetName(1)*16,cHDFFileName*500
    TYPE(GenericFileType)    :: OutFile
    CHARACTER(:),ALLOCATABLE :: cFileName
    TYPE(TimeStepType)       :: TimeStep_Local
    
    !Get the name of the text/DSS file 
    CALL AllHeadsInFile%GetFileName(cFileName)
    
    !Name for the HDF file
    cHDFFileName = TRIM(ADJUSTL(StripTextUntilCharacter(cFileName,'.',Back=.TRUE.))) // '.hdf'
    
    !Open output file HDF file
    CALL OutFile%New(FileName=TRIM(cHDFFileName),InputFile=.FALSE.,IsTSFile=.TRUE.,iStat=iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Create dataset; since GWHeadAll output includes the initial conditions, NTIME+1 is used
    NColumns(1)     = NNodes*NLayers
    cDataSetName(1) = '/GWHeadAtAllNodes'
    CALL OutFile%CreateHDFDataSet(cPathNames=cDataSetName,NColumns=NColumns,NTime=NTIME+1,TimeStep=TimeStep,DataType=0d0,iStat=iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Conversion factor used when printing out results
    rConvFactor = 1.0 / rFactHead

    !Transfer heads to HDF file; NTIME+1 because GWHeadsAll output includes the initial conditions as well
    TimeStep_Local = TimeStep
    DO indxTime=1,NTIME+1
        !Read data
        CALL ReadTSData(TimeStep_Local,'gw-heads-at-all-nodes file',AllHeadsInFile,FileReadCode,iStat)
        IF (iStat .EQ. -1) RETURN
        
        !Transfer  values to matrix to be written to HDF file
        rGWHeads(:,1) = PACK(TRANSPOSE(AllHeadsInFile%rValues) , MASK=.TRUE.)
        
        !Convert units back to simulation units
        IF (rConvFactor .NE. 1.0) rGWHeads = rGWHeads * rConvFactor
        
        !Store heads into HDF file
        CALL OutFile%WriteData(rGWHeads)
        
        !Advance time
        TimeStep_Local%CurrentTimeStep    = TimeStep_Local%CurrentTimeStep + 1
        TimeStep_Local%CurrentDateAndTime = IncrementTimeStamp(TimeStep_Local%CurrentDateAndTime,TimeStep_Local%DELTAT_InMinutes)
    END DO
    
    !Rewind input file
    CALL AllHeadsInFile%File%RewindFile_To_BeginningOfTSData(iStat)
    
    !Close HDF file
    CALL OutFile%Kill()
    
  END SUBROUTINE TransferAllHeadOut_To_HDF

  
END MODULE