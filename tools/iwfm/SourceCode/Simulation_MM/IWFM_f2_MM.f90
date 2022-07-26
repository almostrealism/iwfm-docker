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
!
!  NOTES:
!
!  We are using the co-array feature of Fortran to concurrently run 
!  multiple models. 
!  Image 1             : Controller image that reads global data,  
!                        coordinates model runs and the convergence 
!                        between them.
!  Images 2 to NModel+1: Model runner images
!  Image *             : Last available image that is used to
!                        track error condition and to terminate
!                        all images
!
!***********************************************************************
PROGRAM IWFM_f2_MultiModel
  USE ProgramTimer           , ONLY: StartTimer              , &
                                     StopTimer               
  USE MessageLogger          , ONLY: PrintRunTime            , &
                                     SetLogFileName          , &
                                     KillLogFile             , &
                                     LogMessage              , &
                                     SetLastMessage          , &  
                                     LogLastMessage          , &
                                     GetLastMessage          , &
                                     MessageArray            , & 
                                     f_iInfo                 , &
                                     f_iMessage              , &
                                     f_iFatal                , &
                                     f_iFILE                 , &
                                     f_iSCREEN
  USE GeneralUtilities       , ONLY: IntToText               , &
                                     StripTextUntilCharacter , & 
                                     CleanSpecialCharacters  , &
                                     GetFileDirectory        , &
                                     ReplaceString           , &
                                     ArrangeText             , &
                                     FirstLocation           , &
                                     LocateInList            , &
                                     NormalizeArray          , &
                                     f_cLineFeed      
  USE TimeSeriesUtilities    , ONLY: TimeStepType            , &
                                     IncrementTimeStamp
  USE IOInterface            , ONLY: GenericFileType     
  USE Package_Misc           , ONLY: Print_Screen            , &
                                     Get_Main_File           , &
                                     f_iFlowDest_Outside     , &
                                     f_iFlowDest_StrmNode    , &
                                     f_iFlowDest_Lake        , &
                                     f_iLocationType_Bypass
  USE IWFM_Core_Version      , ONLY: IWFM_Core                                      
  USE Package_Model          , ONLY: ModelType               
  USE Coarray_Debugging
  IMPLICIT NONE
  
  !Model with ID type
  TYPE,EXTENDS(ModelType) :: ModelWithIDType
      INTEGER,ALLOCATABLE :: iNodeIDs(:)
      INTEGER,ALLOCATABLE :: iStrmNodeIDs(:)
  END TYPE ModelWithIDType
  
  !Data type listing model connections
  INTEGER,PARAMETER :: f_iMaxConnectedNodes = 500
  TYPE ModelConnectionType
      INTEGER             :: iMaxIter             = 100     !Maximum number of iterations between models 
      REAL(8)             :: rConvergence         = 0.1     !Convergence creteria (change in flow/flow)
      !Connections in the aquifer
      INTEGER             :: iNLinkNodesGW        = 0       !Number of connections in the aquifer
      REAL(8),ALLOCATABLE :: rFlowGW(:)                     !Current estimate of boundary flow at each node at each (connection)
      REAL(8),ALLOCATABLE :: rFlowGW_P(:)                   !Previous boundary flow at each node at each (connection)
      INTEGER,ALLOCATABLE :: iModelGW(:)                    !Index of first connecting model for each (connection)    
      INTEGER,ALLOCATABLE :: iNodeGW(:)                     !Index of node in first model for each (connection)
      INTEGER,ALLOCATABLE :: iLayerGW(:)                    !Aquifer layer in first model for each (connection) 
      REAL(8),ALLOCATABLE :: rHeadGW(:)                     !Head at the node in first model for each (connection)
      REAL(8),ALLOCATABLE :: rEffectiveConductanceGW(:)     !Effective conductance between above model, node, layer and all the connecting nodes at each (connection)
      INTEGER,ALLOCATABLE :: iNConnectedNodesGW(:)          !Total number of nodes connecting to above model, node, layer at each connection(:)
      INTEGER,ALLOCATABLE :: iModelConnectedGW(:,:)         !List of models connecting to above model, node, layer at each (:,connection)
      INTEGER,ALLOCATABLE :: iNodeConnectedGW(:,:)          !List of nodes connecting to above model, node, layer at each (:,connection)
      INTEGER,ALLOCATABLE :: iLayerConnectedGW(:,:)         !List of aquifer  layers connecting to above model, node, layer at each (:,connection)
      REAL(8),ALLOCATABLE :: rConductanceConnectedGW(:,:)   !List of conductance between connecting nodes at each (:,connection) 
      REAL(8),ALLOCATABLE :: rHeadConnectedGW(:,:)          !List of heads at connecting nodes at each (:,connection) 
      !Connections in streams
      INTEGER             :: iNLinkNodesST        = 0       !Number of connections in the streams
      REAL(8),ALLOCATABLE :: rFlowST(:)                     !Current estimate of flow at each outflowing stream node at each (connection) 
      REAL(8),ALLOCATABLE :: rFlowST_P(:)                   !Previous flow at each outflowing stream node at each (connection)
      INTEGER,ALLOCATABLE :: iModelST(:)                    !Index of first connecting model for each (connection); model that flow goes out    
      INTEGER,ALLOCATABLE :: iNodeST(:)                     !Index of stream node in first model for each (connection); stream node that flow goes out
      INTEGER,ALLOCATABLE :: iModelConnectedST(:)           !Index of the second model for each (connection); model that receives flow
      INTEGER,ALLOCATABLE :: iNodeConnectedST(:)            !Index of the stream node in the second model for each (connection); node that receives flow
      REAL(8),ALLOCATABLE :: rInflowConnectedReadST(:)      !Inflow read from a file at the connected stream node
      !Connections in bypasses
      INTEGER             :: iNLinkNodesBP        = 0       !Number of connections for bypasses 
      REAL(8),ALLOCATABLE :: rFlowBP(:)                     !Current estimate of bypass flow at each (connection)
      INTEGER,ALLOCATABLE :: iModelBP(:)                    !Index of the model that the bypass originates from at each (connection)
      INTEGER,ALLOCATABLE :: iBP(:)                         !Index of the bypass at each (connection)
      INTEGER,ALLOCATABLE :: iModelConnectedBP(:)           !Index of the model that the bypass goes to at each (connection)
      INTEGER,ALLOCATABLE :: iConnectedBP(:)                !Index of the bypass in the receiving model at each (connection)
      !Connections in diversions
      INTEGER             :: iNLinkNodesDiv       = 0       !Number of connections for diversions
      REAL(8),ALLOCATABLE :: rFlowDiv(:)                    !Current estimate of diversion at each (connection)
      INTEGER,ALLOCATABLE :: iModelDiv(:)                   !Index of the model that the diversion originates from at each (connection)
      INTEGER,ALLOCATABLE :: iDiv(:)                        !Index of the diversion at each (connection)
      INTEGER,ALLOCATABLE :: iModelConnectedDiv(:)          !Index of the model that the diversion goes to at each (connection)
      INTEGER,ALLOCATABLE :: iConnectedDiv(:)               !Index of the diversion in the receiving model at each (connection)
  END TYPE ModelConnectionType
  
  !GHB conductance computation related data (used temporarily)
  TYPE GHBConductanceTempDataType
      REAL(8),ALLOCATABLE :: rK1(:),rK2(:)
      REAL(8),ALLOCATABLE :: rX1(:),rY1(:),rX2(:),rY2(:)
      REAL(8),ALLOCATABLE :: rLength1(:),rLength2(:),rThick1(:),rThick2(:)
  END TYPE GHBConductanceTempDataType
  
  !Co-variables
  INTEGER,PARAMETER                      :: f_iMaxNModel  = 50 , &  !Assume maximum of 50 models can be run
                                            f_iModNameLen = 10
  CHARACTER(LEN=f_iModNameLen),PARAMETER :: f_cModName = 'IWFM_f2_MM'
  TYPE(ModelWithIDType)                  :: Model[*]
  CHARACTER                              :: cSimFileName[*]*500,cErrorMessage[*]*1000,cModelNames(f_iMaxNModel)[*]*30
  INTEGER                                :: iNModels[*],iNTime[*],iStat[*],iErrModelIndex[*]
  INTEGER                                :: iModelIDs(f_iMaxNModel)[*]  
  TYPE(TimeStepType)                     :: TimeStep[*]
  LOGICAL                                :: lConverged[*]
  TYPE(ModelConnectionType)              :: Connections[*]
  TYPE(GHBConductanceTempDataType)       :: ConductanceTempData[*]
  
  !Local variables to the image
  INTEGER             :: indxTime,iErrImage,iFlowBCID,iDummy,iModelIndex,iIter,indx,iNode
  INTEGER,ALLOCATABLE :: iActiveImages(:)
  LOGICAL             :: lMaster,lRunner,lFirstCall
  
  !Initialize
  iStat        = 0
  iErrImage    = NUM_IMAGES()
  cSimFileName = ''
  lMaster      = .FALSE.
  lRunner      = .FALSE.
  
  !Flow BC ID used by the Model class
  CALL Model%GetGWBCFlags(iFlowBCID,iDummy,iDummy,iDummy)
  
  !****REMOVE THIS WHEN DONE DEBUGGING****
  !CALL Enable_Coarray_Debugging
  !Prepare control parameters for the run
  CALL PrepareForRun(cSimFileName,cModelNames,iNModels,iModelIndex,iModelIDs,iActiveImages,Connections,iStat)
  IF (iStat .NE. 0) iStat[iErrImage] = iStat
  SYNC ALL
  IF (iStat[iErrImage] .NE. 0) CALL EndExecution()
   
  !Use only active images
  IF (LocateInList(THIS_IMAGE(),iActiveImages) .EQ. 0) CALL EndExecution()

  !Instantiate model
  CALL InstantiateModel(cSimFileName,Model,TimeStep,iNTime,iStat)
  IF (iStat .NE. 0) iStat[iErrImage] = iStat
  SYNC IMAGES(iActiveImages)
  IF (iStat[iErrImage] .NE. 0) CALL EndExecution()
  
  !Check that model time related data are the same for all models
  CALL CheckForModelConsistency(iNModels,iNTime,TimeStep,iStat)
  IF (iStat .NE. 0) iStat[iErrImage] = iStat
  SYNC IMAGES(iActiveImages)
  IF (iStat[iErrImage] .NE. 0) CALL EndExecution()
  
  !Convert model link node IDs to indices and prepare connections to pass flows between models
  CALL PrepConnections(Model,cModelNames,iModelIndex,Connections,iStat)
  IF (iStat .NE. 0) iStat[iErrImage] = iStat
  SYNC IMAGES(iActiveImages)
  IF (iStat[iErrImage] .NE. 0) CALL EndExecution()
  
  !Compute conductances between each connected node pairs
  CALL ComputeConductances(Model,iModelIDs,iModelIndex,ConductanceTempData,Connections,iStat)
  IF (iStat .NE. 0) iStat[iErrImage] = iStat
  SYNC IMAGES(iActiveImages)
  IF (iStat[iErrImage] .NE. 0) CALL EndExecution()
  
  !Simulate
  DO indxTime=1,iNTime
      !Advance time
      CALL AdvanceTime(Model,TimeStep)
      SYNC IMAGES (iActiveImages)

      
      !Read timeseries data
      IF (lRunner) THEN
          CALL Model%ReadTSData(iStat) 
          IF (iStat .NE. 0) iStat[iErrImage] = iStat
          !Get stream inflow read at connected nodes (this is in case the receiiving stream node at a model connection also has pre-defined inflow from other reaches)
          DO indx=1,Connections[1]%iNLinkNodesST
              IF (Connections[1]%iModelConnectedST(indx) .EQ. iModelIndex) THEN
                  iNode                                       = Connections[1]%iNodeConnectedST(indx)
                  Connections[1]%rInflowConnectedReadST(indx) = Model%GetStrmInflow_AtANode(iNode)
              END IF
          END DO 
      END IF
      SYNC IMAGES(iActiveImages)
      IF (iStat[iErrImage] .NE. 0) CALL EndExecution()
      
          
      !Simulate until convergence between models
      lConverged = .FALSE.
      lFirstCall = .TRUE.
      iIter      = 1
      DO
          !Set boundary flows
          CALL SetFlowBC(Model,iModelIndex,lFirstCall,Connections,iStat)
          IF (iStat .NE. 0) iStat[iErrImage] = iStat
          SYNC IMAGES(iActiveImages)
          IF (iStat[iErrImage] .NE. 0) CALL EndExecution()
          
          !Simulate
          IF (lRunner) THEN
              CALL Model%SimulateOneTimeStep(iStat)
              IF (iStat .NE. 0) iStat[iErrImage] = iStat
          END IF
          SYNC IMAGES(iActiveImages)
          IF (iStat[iErrImage] .NE. 0) CALL EndExecution()
          
          !Check convergence
          lConverged = CheckConvergence(Model,cModelNames,iModelIndex,iIter,Connections)
          SYNC IMAGES(iActiveImages)
          IF (lConverged[1]) EXIT
          
          !Advance number of iterations
          CALL AdvanceIterations(Connections,iIter,iStat)
          IF (iStat .NE. 0) iStat[iErrImage] = iStat
          SYNC IMAGES(iActiveImages)
          IF (iStat[iErrImage] .NE. 0) CALL EndExecution()
          
          !Flag to check if this is the first iteration between the models in a given timestep
          lFirstCall = .FALSE.
      END DO
          
      !Print results
      IF (lRunner) CALL Model%PrintResults()
                       
      !Advance state of the model
      IF (lRunner) CALL Model%AdvanceState()  
          
  END DO
      
  !Kill model and close its message file
  IF (lRunner)  THEN
      CALL Model%Kill()
      CALL KillLogFile()
  END IF
  
  !Finish run
  CALL EndExecution()
  

CONTAINS

    
  ! -------------------------------------------------------------
  ! --- PREPARE FOR RUN
  ! -------------------------------------------------------------
  SUBROUTINE PrepareForRun(cSimFileName,cModelNames,iNModels,iModelIndex,iModelIDs,iActiveImages,Connections,iStat)
    CHARACTER(LEN=*),INTENT(OUT)    :: cSimFileName[*],cModelNames(:)
    INTEGER,INTENT(OUT)             :: iNModels[*]
    INTEGER,INTENT(OUT)             :: iModelIndex,iModelIDs(:)
    INTEGER,ALLOCATABLE,INTENT(OUT) :: iActiveImages(:)
    TYPE(ModelConnectionType)       :: Connections
    INTEGER,INTENT(OUT)             :: iStat
    
    !Local variables
    CHARACTER(LEN=f_iModNameLen+15),PARAMETER :: ThisProcedure = f_cModName // '::PrepareForRun'
    CHARACTER,ALLOCATABLE                     :: cSimFileNames(:)*500
    INTEGER                                   :: indxModel,iImage,indx,iNActiveImages
    CHARACTER                                 :: RDATE*30,RTIME*30 
    
    !Initialize
    iStat = 0
    
    !Assign Master
    IF (THIS_IMAGE() .EQ. 1) lMaster = .TRUE.
    
    !Skip to the end if not Master
    IF (.NOT. lMaster) GOTO 10
    
    !Start program timer
    CALL StartTimer()  
  
    !Standard output file
    CALL SetLogFileName('SimulationMessages_MM.out',iStat)
    IF (iStat .EQ. -1) GOTO 10
    
    !Print title
    CALL LogMessage(REPEAT('*',50)//f_cLineFeed//ArrangeText('IWFM',50)//f_cLineFeed//ArrangeText('Multi-Model Simulation',50)//f_cLineFeed//REPEAT('*',50),f_iMessage,'',f_iFILE)
    
    !Get the current date and time, and display
    CALL DATE_AND_TIME(DATE=RDATE,TIME=RTIME)
    CALL LogMessage(f_cLineFeed,f_iMessage,'',f_iFILE)
    CALL LogMessage(' THIS RUN IS MADE ON '//                       &
                    RDATE(5:6)//'/'//RDATE(7:8)//'/'//RDATE(1:4)//  &
                    ' AT '                                      //  &
                    RTIME(1:2)//':'//RTIME(3:4)//':'//RTIME(5:6),f_iMessage,'',f_iFILE)
    
    !Read control data
    CALL ReadControlData_MAIN('',cSimFileNames,cModelNames,iModelIDs,iNModels,Connections,iStat)
    IF (iStat .NE. 0) GOTO 10
    
    !Print out models
    CALL LogMessage(f_cLineFeed,f_iMessage,'',f_iFILE)
    CALL LogMessage(' THE FOLLOWING MODELS WERE USED IN THIS RUN:',f_iMessage,'',f_iFILE)
    DO indx=1,iNModels
        WRITE (MessageArray(1),'(I3,4X,A10,4X,A)') iModelIDs(indx),cModelNames(indx),TRIM(cSimFileNames(indx))
        CALL LogMessage(MessageArray(1),f_iMessage,'',f_iFILE)
    END DO
    CALL LogMessage(f_cLineFeed//' *** CHECK THE RESPECTIVE SimulationMessages.out FILE FOR MODEL-SPECIFIC MESSAGES.',f_iMessage,'',f_iFILE)
    
    !Active images (First one is the master that controls communication between models, last one is the error checker)
    iNActiveImages = iNModels + 2
    
    !Make sure there are enough processors 
    IF (iNActiveImages .GT. NUM_IMAGES()) THEN
        CALL SetLastMessage('There should be at least '//TRIM(IntToText(iNActiveImages))//' processor to run this many models concurrently!',f_iFatal,ThisProcedure)
        iStat = -1
        GOTO 10
    END IF
    
    !Simulation filenames and model IDs to individual model running images
    DO indxModel=1,iNModels
        iImage               = indxModel + 1
        cSimFileName[iImage] = cSimFileNames(indxModel)
    END DO
    
10  SYNC ALL
    IF (iStat .NE. 0) RETURN
    
    !Indices of active images
    iNModels       = iNModels[1]
    iNActiveImages = iNModels + 2
    ALLOCATE (iActiveImages(iNActiveImages))
    iActiveImages(1:iNActiveImages-1) = [(indx,indx=1,iNActiveImages-1)]
    iActiveImages(iNActiveImages)     = NUM_IMAGES()
    
    !Identify model running images
    IF (THIS_IMAGE() .GT. 1) THEN
        IF (THIS_IMAGE() .LE. iNModels+1) lRunner = .TRUE.
    END IF
    
    !Model index
    iModelIndex = THIS_IMAGE() - 1
    
  END SUBROUTINE PrepareForRun
  
  
  ! -------------------------------------------------------------
  ! --- READ IN SIMULATION MAIN CONTROL DATA (GATEWAY PROCEDURE)
  ! -------------------------------------------------------------
  SUBROUTINE ReadControlData_MAIN(cFileName,cSimFileNames,cModelNames,iModelIDs,iNModels,Connections,iStat)
    CHARACTER(LEN=*),INTENT(IN)   :: cFileName
    CHARACTER(LEN=*),ALLOCATABLE  :: cSimFileNames(:)
    CHARACTER(LEN=*) ,INTENT(OUT) :: cModelNames(:)
    INTEGER                       :: iModelIDs(:)
    INTEGER,INTENT(OUT)           :: iNModels
    TYPE(ModelConnectionType)     :: Connections
    INTEGER,INTENT(OUT)           :: iStat
    
    !Local variables
    CHARACTER(LEN=f_iModNameLen+22),PARAMETER :: ThisProcedure = f_cModName // '::ReadControlData_MAIN'
    INTEGER                                   :: indx,iLoc
    CHARACTER                                 :: cMainFileName*500,ALine*1000
    TYPE(GenericFileType)                     :: MainControlFile
    TYPE(ModelType)                           :: aModel
    
    !Initialize
    iStat     = 0
    iModelIDs = 0
    
    !Prompt user for the name of the main input file
    IF (cFileName .NE. '') THEN
        cMainFileName = cFileName
    ELSE
        CALL Print_screen('Program: Simulation_MultiModel',IWFM_Core)
        CALL Get_Main_File(' Enter the Name of the Main Input File >  ',cMainFileName)
        IF (TRIM(cMainFileName) .EQ. '-about') THEN
            CALL aModel%PrintVersionNumbers()
            iStat = -2  !Stat code to stop the program without any message
            RETURN
        END IF
    END IF
    
    !Open main control file
    CALL MainControlFile%New(FileName=cMainFileName,InputFile=.TRUE.,Descriptor='Simulation main control input',FileType='TXT',iStat=iStat)
    IF (iStat .EQ. -1) RETURN
  
    !Number of models
    CALL MainControlFile%ReadData(iNModels,iStat)  ;  IF (iStat .EQ. -1) RETURN
    
    !Read model IDs and Simulation main filenames
    ALLOCATE (cSimFileNames(iNModels))
    DO indx=1,iNModels
        CALL MainControlFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN
        CALL CleanSpecialCharacters(ALine)  ;  ALine = ADJUSTL(ALine)  ;  CALL ReplaceString(ALine,',',' ',iStat)  ;  IF (iStat .EQ. -1) RETURN
        !Model ID
        READ (ALine,*) iModelIDs(indx)
        !Model name
        iLoc              = FirstLocation(' ',ALine)
        ALine             = ADJUSTL(ALine(iLoc:))
        iLoc              = FirstLocation(' ',ALine)
        cModelNames(indx) = ALine(1:iLoc-1)
        !Simulation filename
        ALine               = ALine(iLoc:)
        cSimFileNames(indx) = ADJUSTL(StripTextUntilCharacter(ALine,'/'))
    END DO
    
    !Convergence creteria
    CALL MainControlFile%ReadData(Connections%iMaxIter,iStat)     ;  IF (iStat .EQ. -1) RETURN
    CALL MainControlFile%ReadData(Connections%rConvergence,iStat) ;  IF (iStat .EQ. -1) RETURN
    
    !Read connecting nodes, models and layers for GW
    CALL ReadControlData_GW(MainControlFile,iModelIDs,Connections,iStat)  ;  IF (iStat .EQ. -1) RETURN
    
    !Read stream connections
    CALL ReadControlData_STREAM(MainControlFile,iModelIDs,Connections,iStat)  ;  IF (iStat .EQ. -1) RETURN
    
    !Read bypass connections
    CALL ReadControlData_BYPASS(MainControlFile,iModelIDs,Connections,iStat)  ;  IF (iStat .EQ. -1) RETURN

    !Read diversion connections
    CALL ReadControlData_DIVERSION(MainControlFile,iModelIDs,Connections,iStat)  ;  IF (iStat .EQ. -1) RETURN

    !Close main control file
    CALL MainControlFile%Kill()
    
  END SUBROUTINE ReadControlData_MAIN
  
  
  ! -------------------------------------------------------------
  ! --- READ IN SIMULATION MAIN CONTROL DATA FOR GROUNDWATER CONNECTIONS
  ! -------------------------------------------------------------
  SUBROUTINE ReadControlData_GW(MainControlFile,iModelIDs,Connections,iStat)
    TYPE(GenericFileType)     :: MainControlFile
    INTEGER,INTENT(IN)        :: iModelIDs(:)
    TYPE(ModelConnectionType) :: Connections
    INTEGER,INTENT(OUT)       :: iStat
    
    !Local variables
    CHARACTER(LEN=f_iModNameLen+20),PARAMETER :: ThisProcedure = f_cModName // '::ReadControlData_GW'
    INTEGER                                   :: iNRows,indx,iModel1,iNode1,iModel2,iNode2,iNLayerLink, &
                                                 iLinkLayers(f_iMaxConnectedNodes),iIndex,indxLayer,    &
                                                 iLayer1,iLayer2
    CHARACTER                                 :: ALine*1000
    
    CALL MainControlFile%ReadData(iNRows,iStat) ;  IF (iStat .EQ. -1) RETURN
    DO indx=1,iNRows
        CALL MainControlFile%ReadData(ALine,iStat)  ;  IF (iStat .EQ. -1) RETURN
        READ (ALine,*) iModel1,iNode1,iModel2,iNode2,iNLayerLink,iLinkLayers(1:2*iNLayerLink)
        
        !Can't link model to itself
        IF (iModel1 .EQ. iModel2) THEN
            CALL SetLastMessage('Model '//TRIM(IntToText(iModel1))//' is being linked to itself at nodes '//TRIM(IntToText(iNode1))//' and '//TRIM(IntToText(iNode2))//'!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        
        !Convert first model ID to index (can't convert node IDs yet since models have not been instantiated)
        iIndex = LocateInList(iModel1 , iModelIDs)
        IF (iIndex .EQ. 0) THEN
            CALL SetLastMessage('Model ID '//TRIM(IntToText(iModel1))//' listed for model linkage data for groundwater is not defined!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        iModel1 = iIndex
        
        !Convert second model ID to index (can't convert node IDs yet since models have not been instantiated)
        iIndex = LocateInList(iModel2 , iModelIDs)
        IF (iIndex .EQ. 0) THEN
            CALL SetLastMessage('Model ID '//TRIM(IntToText(iModel2))//' listed for model linkage data for groundwater is not defined!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        iModel2 = iIndex
        
        !Add linkage between model1,node1,layer1 and model2,node2,layer2 to database
        DO indxLayer=1,2*iNLayerLink,2
            iLayer1 = iLinkLayers(indxLayer)
            iLayer2 = iLinkLayers(indxLayer+1)
            CALL AddConnection(iModel1,iNode1,iLayer1,iModel2,iNode2,iLayer2,Connections)
        END DO
    END DO
    !Allocate memory for the conductance, head terms, flows
    ALLOCATE (Connections%rHeadGW(Connections%iNLinkNodesGW)                                      , &
              Connections%rFlowGW(Connections%iNLinkNodesGW)                                      , &
              Connections%rFlowGW_P(Connections%iNLinkNodesGW)                                    , &
              Connections%rEffectiveConductanceGW(Connections%iNLinkNodesGW)                      , &
              Connections%rHeadConnectedGW(f_iMaxConnectedNodes,Connections%iNLinkNodesGW)        , &
              Connections%rConductanceConnectedGW(f_iMaxConnectedNodes,Connections%iNLinkNodesGW) )
    Connections%rFlowGW   = 0.0
    Connections%rFlowGW_P = 0.0

  END SUBROUTINE ReadControlData_GW
  
  
  ! -------------------------------------------------------------
  ! --- READ IN SIMULATION MAIN CONTROL DATA FOR STREAM CONNECTIONS
  ! -------------------------------------------------------------
  SUBROUTINE ReadControlData_STREAM(MainControlFile,iModelIDs,Connections,iStat)
    TYPE(GenericFileType)     :: MainControlFile
    INTEGER,INTENT(IN)        :: iModelIDs(:)
    TYPE(ModelConnectionType) :: Connections
    INTEGER,INTENT(OUT)       :: iStat
    
    !Local variables
    CHARACTER(LEN=f_iModNameLen+24),PARAMETER :: ThisProcedure = f_cModName // '::ReadControlData_STREAM'
    INTEGER                                   :: iNRows,indx,iDummyArray4(4),iIndex
    
    CALL MainControlFile%ReadData(iNRows,iStat) ;  IF (iStat .EQ. -1) RETURN
    Connections%iNLinkNodesST = iNRows
    ALLOCATE (Connections%rFlowST(iNRows)                , &
              Connections%rFlowST_P(iNRows)              , &
              Connections%iModelST(iNRows)               , &
              Connections%iNodeST(iNRows)                , &
              Connections%iModelConnectedST(iNRows)      , &
              Connections%iNodeConnectedST(iNRows)       , &
              Connections%rInflowConnectedReadST(iNRows) )
    
    !Read data
    DO indx=1,iNRows
        CALL MainControlFile%ReadData(iDummyArray4,iStat)  ;  IF (iStat .EQ. -1) RETURN
        Connections%iModelST(indx)          = iDummyArray4(1)
        Connections%iNodeST(indx)           = iDummyArray4(2)
        Connections%iModelConnectedST(indx) = iDummyArray4(3)
        Connections%iNodeConnectedST(indx)  = iDummyArray4(4)
        
        !Convert first model ID to index; can't convert stream node IDs yet since models have not been instantiated
        iIndex = LocateInList(Connections%iModelST(indx) , iModelIDs)
        IF (iIndex .EQ. 0) THEN
            CALL SetLastMessage('Model ID '//TRIM(IntToText(Connections%iModelST(indx)))//' listed for model linkage data for streams is not defined!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        Connections%iModelST(indx) = iIndex
        
        !Convert second model ID to index; can't convert stream node IDs yet since models have not been instantiated
        iIndex = LocateInList(Connections%iModelConnectedST(indx) , iModelIDs)
        IF (iIndex .EQ. 0) THEN
            CALL SetLastMessage('Model ID '//TRIM(IntToText(Connections%iModelConnectedST(indx)))//' listed for model linkage data for streams is not defined!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        Connections%iModelConnectedST(indx) = iIndex
    END DO

  END SUBROUTINE ReadControlData_STREAM
  
  
  ! -------------------------------------------------------------
  ! --- READ IN SIMULATION MAIN CONTROL DATA FOR BYPASS CONNECTIONS
  ! -------------------------------------------------------------
  SUBROUTINE ReadControlData_BYPASS(MainControlFile,iModelIDs,Connections,iStat)
    TYPE(GenericFileType)     :: MainControlFile
    INTEGER,INTENT(IN)        :: iModelIDs(:)
    TYPE(ModelConnectionType) :: Connections
    INTEGER,INTENT(OUT)       :: iStat
    
    !Local variables
    CHARACTER(LEN=f_iModNameLen+24),PARAMETER :: ThisProcedure = f_cModName // '::ReadControlData_BYPASS'
    INTEGER                                   :: iNRows,indx,iIndex,iDummyArray4(4)
    
    CALL MainControlFile%ReadData(iNRows,iStat) ;  IF (iStat .EQ. -1) RETURN
    Connections%iNLinkNodesBP = iNRows
    ALLOCATE (Connections%rFlowBP(iNRows)           , &
              Connections%iModelBP(iNRows)          , &
              Connections%iBP(iNRows)               , &
              Connections%iModelConnectedBP(iNRows) , &
              Connections%iConnectedBP(iNRows)      )
    
    !Read data
    DO indx=1,iNRows
        CALL MainControlFile%ReadData(iDummyArray4,iStat)  ;  IF (iStat .EQ. -1) RETURN
        Connections%iModelBP(indx)          = iDummyArray4(1)
        Connections%iBP(indx)               = iDummyArray4(2)
        Connections%iModelConnectedBP(indx) = iDummyArray4(3)
        Connections%iConnectedBP(indx)      = iDummyArray4(4)
        
        !Convert first model ID to index; can't convert other IDs yet since models have not been instantiated
        iIndex = LocateInList(Connections%iModelBP(indx) , iModelIDs)
        IF (iIndex .EQ. 0) THEN
            CALL SetLastMessage('Model ID '//TRIM(IntToText(Connections%iModelBP(indx)))//' listed for model linkage data for bypasses is not defined!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        Connections%iModelBP(indx) = iIndex
        
        !Convert second model ID to index; can't convert other IDs yet since models have not been instantiated
        iIndex = LocateInList(Connections%iModelConnectedBP(indx) , iModelIDs)
        IF (iIndex .EQ. 0) THEN
            CALL SetLastMessage('Model ID '//TRIM(IntToText(Connections%iModelConnectedBP(indx)))//' listed for model linkage data for bypasses is not defined!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        Connections%iModelConnectedBP(indx) = iIndex
        
    END DO

  END SUBROUTINE ReadControlData_BYPASS
  
  
  ! -------------------------------------------------------------
  ! --- READ IN SIMULATION MAIN CONTROL DATA FOR DIVERSION CONNECTIONS
  ! -------------------------------------------------------------
  SUBROUTINE ReadControlData_DIVERSION(MainControlFile,iModelIDs,Connections,iStat)
    TYPE(GenericFileType)     :: MainControlFile
    INTEGER,INTENT(IN)        :: iModelIDs(:)
    TYPE(ModelConnectionType) :: Connections
    INTEGER,INTENT(OUT)       :: iStat
    
    !Local variables
    CHARACTER(LEN=f_iModNameLen+27),PARAMETER :: ThisProcedure = f_cModName // '::ReadControlData_DIVERSION'
    INTEGER                                   :: iNRows,indx,iIndex,iDummyArray4(4)
    
    CALL MainControlFile%ReadData(iNRows,iStat) ;  IF (iStat .EQ. -1) RETURN
    Connections%iNLinkNodesDiv = iNRows
    ALLOCATE (Connections%rFlowDiv(iNRows)           , &
              Connections%iModelDiv(iNRows)          , &
              Connections%iDiv(iNRows)               , &
              Connections%iModelConnectedDiv(iNRows) , &
              Connections%iConnectedDiv(iNRows)      )
    
    !Read data
    DO indx=1,iNRows
        CALL MainControlFile%ReadData(iDummyArray4,iStat)  ;  IF (iStat .EQ. -1) RETURN
        Connections%iModelDiv(indx)          = iDummyArray4(1)
        Connections%iDiv(indx)               = iDummyArray4(2)
        Connections%iModelConnectedDiv(indx) = iDummyArray4(3)
        Connections%iConnectedDiv(indx)      = iDummyArray4(4)
        
        !Convert first model ID to index; can't convert other IDs yet since models have not been instantiated
        iIndex = LocateInList(Connections%iModelDiv(indx) , iModelIDs)
        IF (iIndex .EQ. 0) THEN
            CALL SetLastMessage('Model ID '//TRIM(IntToText(Connections%iModelDiv(indx)))//' listed for model linkage data for diversions is not defined!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        Connections%iModelDiv(indx) = iIndex
        
        !Convert second model ID to index; can't convert other IDs yet since models have not been instantiated
        iIndex = LocateInList(Connections%iModelConnectedDiv(indx) , iModelIDs)
        IF (iIndex .EQ. 0) THEN
            CALL SetLastMessage('Model ID '//TRIM(IntToText(Connections%iModelConnectedDiv(indx)))//' listed for model linkage data for diversions is not defined!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        Connections%iModelConnectedDiv(indx) = iIndex
        
    END DO

  END SUBROUTINE ReadControlData_DIVERSION
  
  
  ! -------------------------------------------------------------
  ! --- ADD CONNECTION
  ! -------------------------------------------------------------
  SUBROUTINE AddConnection(iModel1,iNode1,iLayer1,iModel2,iNode2,iLayer2,Connections)
    INTEGER,INTENT(IN)        :: iModel1,iNode1,iLayer1,iModel2,iNode2,iLayer2
    TYPE(ModelConnectionType) :: Connections
    
    !Local variables
    INTEGER                              :: indx1,indx2,indx,iNData
    LOGICAL                              :: lListed1,lListed2,lIncluded
    INTEGER,ALLOCATABLE                  :: iTempModel(:),iTempNode(:),iTempLayer(:),iTempNConnectedNodes(:), &
                                            iTempModelConnected(:,:),iTempNodeConnected(:,:),iTempLayerConnected(:,:)
    
    !First, check if the first node, model, layer is in the database
    lListed1 = .FALSE.
    DO indx1=1,Connections%iNLinkNodesGW
        IF (Connections%iModelGW(indx1) .EQ. iModel1) THEN
            IF (Connections%iNodeGW(indx1) .EQ. iNode1) THEN
                IF (Connections%iLayerGW(indx1) .EQ. iLayer1) THEN
                    lListed1 = .TRUE.
                    EXIT
                END IF
            END IF
        END IF
    END DO
    
    !Then, check if the second node, model, layer is in the database
    lListed2 = .FALSE.
    DO indx2=1,Connections%iNLinkNodesGW
        IF (Connections%iModelGW(indx2) .EQ. iModel2) THEN
            IF (Connections%iNodeGW(indx2) .EQ. iNode2) THEN
                IF (Connections%iLayerGW(indx2) .EQ. iLayer2) THEN
                    lListed2 = .TRUE.
                    EXIT
                END IF
            END IF
        END IF
    END DO  
    
    !Process second node as a connection to first node
    IF (lListed1) THEN
        !Node 1 is already listed in the database
        !Check if the second node is already listed for the first node
        lIncluded =.FALSE.
        DO indx=1,Connections%iNConnectedNodesGW(indx1)
            IF (Connections%iModelConnectedGW(indx,indx1) .EQ. iModel2) THEN
                IF (Connections%iNodeConnectedGW(indx,indx1) .EQ. iNode2) THEN
                    IF (Connections%iLayerConnectedGW(indx,indx1) .EQ. iLayer2) THEN
                        lIncluded = .TRUE.
                        EXIT
                    END IF
                END IF
            END IF
        END DO
        !If not, add
        IF (.NOT. lIncluded) THEN
            iNData                                      = Connections%iNConnectedNodesGW(indx1) + 1
            Connections%iNConnectedNodesGW(indx1)       = iNData
            Connections%iModelConnectedGW(iNData,indx1) = iModel2
            Connections%iNodeConnectedGW(iNData,indx1)  = iNode2
            Connections%iLayerConnectedGW(iNData,indx1) = iLayer2
        END IF
    ELSE
        !Node 1 is not listed in the database, add that and node 2 as linkage
        iNData                  = Connections%iNLinkNodesGW
        Connections%iNLinkNodesGW = iNData + 1
        ALLOCATE (iTempModel(iNData+1)                               , &
                  iTempNode(iNData+1)                                , &
                  iTempLayer(iNData+1)                               , &
                  iTempNConnectedNodes(iNData+1)                     , &
                  iTempModelConnected(f_iMaxConnectedNodes,iNData+1) , &
                  iTempNodeConnected(f_iMaxConnectedNodes,iNData+1)  , &
                  iTempLayerConnected(f_iMaxConnectedNodes,iNData+1) )
        iTempModel(1:iNData)            = Connections%iModelGW            ;  iTempModel(iNData+1)            = iModel1
        iTempNode(1:iNData)             = Connections%iNodeGW             ;  iTempNode(iNData+1)             = iNode1
        iTempLayer(1:iNData)            = Connections%iLayerGW            ;  iTempLayer(iNData+1)            = iLayer1
        iTempNConnectedNodes(1:iNData)  = Connections%iNConnectedNodesGW  ;  iTempNConnectedNodes(iNData+1)  = 1
        iTempModelConnected(:,1:iNData) = Connections%iModelConnectedGW   ;  iTempModelConnected(1,iNData+1) = iModel2   
        iTempNodeConnected(:,1:iNData)  = Connections%iNodeConnectedGW    ;  iTempNodeConnected(1,iNData+1)  = iNode2   
        iTempLayerConnected(:,1:iNData) = Connections%iLayerConnectedGW   ;  iTempLayerConnected(1,iNData+1) = iLayer2   
        CALL MOVE_ALLOC(iTempModel           , Connections%iModelGW)
        CALL MOVE_ALLOC(iTempNode            , Connections%iNodeGW)
        CALL MOVE_ALLOC(iTempLayer           , Connections%iLayerGW)
        CALL MOVE_ALLOC(iTempNConnectedNodes , Connections%iNConnectedNodesGW)
        CALL MOVE_ALLOC(iTempModelConnected  , Connections%iModelConnectedGW)
        CALL MOVE_ALLOC(iTempNodeConnected   , Connections%iNodeConnectedGW)
        CALL MOVE_ALLOC(iTempLayerConnected  , Connections%iLayerConnectedGW)
    END IF
  
    !Process first node as a connection to second node
    IF (lListed2) THEN
        !Node 2 is already listed in the database
        !Check if the first node is already listed for the second node
        lIncluded =.FALSE.
        DO indx=1,Connections%iNConnectedNodesGW(indx2)
            IF (Connections%iModelConnectedGW(indx,indx2) .EQ. iModel1) THEN
                IF (Connections%iNodeConnectedGW(indx,indx2) .EQ. iNode1) THEN
                    IF (Connections%iLayerConnectedGW(indx,indx2) .EQ. iLayer1) THEN
                        lIncluded = .TRUE.
                        EXIT
                    END IF
                END IF
            END IF
        END DO
        !If not, add
        IF (.NOT. lIncluded) THEN
            iNData                                      = Connections%iNConnectedNodesGW(indx2) + 1
            Connections%iNConnectedNodesGW(indx2)       = iNData
            Connections%iModelConnectedGW(iNData,indx2) = iModel1
            Connections%iNodeConnectedGW(iNData,indx2)  = iNode1
            Connections%iLayerConnectedGW(iNData,indx2) = iLayer1
        END IF
    ELSE
        !Node 2 is not listed in the database, add that and node 1 as linkage
        iNData                    = Connections%iNLinkNodesGW
        Connections%iNLinkNodesGW = iNData + 1
        ALLOCATE (iTempModel(iNData+1)                               , &
                  iTempNode(iNData+1)                                , &
                  iTempLayer(iNData+1)                               , &
                  iTempNConnectedNodes(iNData+1)                     , &
                  iTempModelConnected(f_iMaxConnectedNodes,iNData+1) , &
                  iTempNodeConnected(f_iMaxConnectedNodes,iNData+1)  , &
                  iTempLayerConnected(f_iMaxConnectedNodes,iNData+1) )
        iTempModel(1:iNData)            = Connections%iModelGW            ;  iTempModel(iNData+1)            = iModel2
        iTempNode(1:iNData)             = Connections%iNodeGW             ;  iTempNode(iNData+1)             = iNode2
        iTempLayer(1:iNData)            = Connections%iLayerGW            ;  iTempLayer(iNData+1)            = iLayer2
        iTempNConnectedNodes(1:iNData)  = Connections%iNConnectedNodesGW  ;  iTempNConnectedNodes(iNData+1)  = 1
        iTempModelConnected(:,1:iNData) = Connections%iModelConnectedGW   ;  iTempModelConnected(1,iNData+1) = iModel1   
        iTempNodeConnected(:,1:iNData)  = Connections%iNodeConnectedGW    ;  iTempNodeConnected(1,iNData+1)  = iNode1   
        iTempLayerConnected(:,1:iNData) = Connections%iLayerConnectedGW   ;  iTempLayerConnected(1,iNData+1) = iLayer1   
        CALL MOVE_ALLOC(iTempModel           , Connections%iModelGW)
        CALL MOVE_ALLOC(iTempNode            , Connections%iNodeGW)
        CALL MOVE_ALLOC(iTempLayer           , Connections%iLayerGW)
        CALL MOVE_ALLOC(iTempNConnectedNodes , Connections%iNConnectedNodesGW)
        CALL MOVE_ALLOC(iTempModelConnected  , Connections%iModelConnectedGW)
        CALL MOVE_ALLOC(iTempNodeConnected   , Connections%iNodeConnectedGW)
        CALL MOVE_ALLOC(iTempLayerConnected  , Connections%iLayerConnectedGW)
    END IF
  
  END SUBROUTINE AddConnection
  
  
  ! -------------------------------------------------------------
  ! --- INSTANTIATE MODEL
  ! -------------------------------------------------------------
  SUBROUTINE InstantiateModel(cSimFileName,Model,TimeStep,iNTime,iStat)
    CHARACTER(LEN=*),INTENT(IN)       :: cSimFileName
    TYPE(ModelWithIDType),INTENT(OUT) :: Model
    TYPE(TimeStepType),INTENT(OUT)    :: TimeStep
    INTEGER,INTENT(OUT)               :: iNTime
    INTEGER,INTENT(OUT)               :: iStat
    
    !Local variables
    CHARACTER(:),ALLOCATABLE :: cSimDir
    INTEGER                  :: iNNodes
    
    !Initialize
    iStat = 0
    
    !Return if not a model running image
    IF (.NOT. lRunner) RETURN
    
    !Standard output file for the model
    CALL GetFileDirectory(cSimFileName,cSimDir)
    CALL SetLogFileName(cSimDir // 'SimulationMessages.out',iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Instantiate model
    CALL Model%New(TRIM(cSimFileName),lForInquiry=.FALSE.,iStat=iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Obtain node IDs
    iNNodes = Model%GetNNodes()
    ALLOCATE (Model%iNodeIDs(iNNodes))
    CALL Model%GetNodeIDs(Model%iNodeIDs)
    
    !Obtain stream node IDs
    iNNodes = Model%GetNStrmNodes()
    ALLOCATE (Model%iStrmNodeIDs(iNNodes))
    CALL Model%GetStrmNodeIDs(Model%iStrmNodeIDs)
    
    !Get model time related data
    CALL Model%GetTimeSpecs(TimeStep,iNTime)
    
  END SUBROUTINE InstantiateModel
  
  
  ! -------------------------------------------------------------
  ! --- CHECK FOR CONSISTENCY BETWEEN MODELS
  ! -------------------------------------------------------------
  SUBROUTINE CheckForModelConsistency(iNModels,iNTime,TimeStep,iStat)
    INTEGER,INTENT(IN)  :: iNModels
    INTEGER             :: iNTime[*] 
    TYPE(TimeStepType)  :: TimeStep[*]
    INTEGER,INTENT(OUT) :: iStat
    
    !Local variables
    CHARACTER(LEN=f_iModNameLen+26),PARAMETER :: ThisProcedure = f_cModName // '::CheckForModelConsistency'
    INTEGER                                   :: iNTime_Check,indxModel,iImage
    TYPE(TimeStepType)                        :: TimeStep_Check
    
    !Initialize
    iStat = 0
    
    !Return if a Runner image
    IF (lRunner) RETURN
    
    !Return if there is only 1 model
    IF (iNModels .EQ. 1) RETURN
    
    !Check for consistency in simulation period and timestep
    TimeStep_Check = TimeStep[2]
    iNTime_Check   = iNTime[2]
    DO indxModel=2,iNModels
        iImage = indxModel + 1
        !Check beginning time 
        IF (TRIM(TimeStep_Check%CurrentDateAndTime) .NE. TRIM(TimeStep[iImage]%CurrentDateAndTime)) THEN
            CALL SetLastMessage('Simulation beginning date must be the same for all models!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        
        !Check timestep length
        IF (TRIM(TimeStep_Check%Unit) .NE. TRIM(TimeStep[iImage]%Unit)) THEN
            CALL SetLastMessage('Simulation timestep length must be the same for all models!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF

        !Check ending time
        IF (iNTime_Check .NE. iNTime[iImage]) THEN
            CALL SetLastMessage('Simulation end date must be the same for all models!',f_iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
    END DO
    
    !Store timestep information in Master image
    iNTime   = iNTime[2]
    TimeStep = TimeStep[2]

  END SUBROUTINE CheckForModelConsistency
  
  
  ! -------------------------------------------------------------
  ! --- PREPARE NODES TO PASS FLOW B.C.s
  ! -------------------------------------------------------------
  SUBROUTINE PrepConnections(Model,cModelNames,iModelIndex,Connections,iStat)
    TYPE(ModelWithIDType),INTENT(IN)        :: Model
    CHARACTER(LEN=*),INTENT(IN)             :: cModelNames(:)[*]
    INTEGER,INTENT(IN)                      :: iModelIndex
    TYPE(ModelConnectionType),INTENT(INOUT) :: Connections[*]
    INTEGER,INTENT(OUT)                     :: iStat 
    
    !Local variables
    CHARACTER(LEN=f_iModNameLen+17),PARAMETER :: ThisProcedure = f_cModName // '::PrepConnections'
    INTEGER                                   :: iNodeID,iNodeIndex,indx,indx1,iCount,iNBypasses,iIndex,iDest,iDestType, &
                                                 iBPID,iConnectedBPID,iStrmNodeExport,iNDiver,iDivID,iConnectedDivID
    INTEGER,ALLOCATABLE                       :: iBCNodes(:),iBCLayers(:),iBypassIDs(:),iDiverIDs(:)
    
    !Initialize
    iStat = 0
    
    !Return if this is not a model running image
    IF (.NOT. lRunner) RETURN
    
    !Allocate memory
    ALLOCATE (iBCNodes(Connections[1]%iNLinkNodesGW) , iBCLayers(Connections[1]%iNLinkNodesGW))
    iCount = 0
    
    !=====================================
    !=== Prepare gw nodes
    !=====================================
    DO indx=1,Connections[1]%iNLinkNodesGW
        IF (Connections[1]%iModelGW(indx) .EQ. iModelIndex) THEN
            iCount     = iCount + 1 
            iNodeID    = Connections[1]%iNodeGW(indx)
            iNodeIndex = LocateInList(iNodeID,Model%iNodeIDs)
            !Is node ID legit?
            IF (iNodeIndex .EQ. 0) THEN
                CALL SetLastMessage('Node '//TRIM(IntTotext(iNodeID))//' listed for model linkage is not in '//TRIM(cModelNames(iModelIndex)[1])//' model!',f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
            !Node must be a boundary node
            IF (.NOT. Model%IsBoundaryNode(iNodeIndex)) THEN
                CALL SetLastMessage('Node '//TRIM(IntTotext(iNodeID))//' listed for model linkage in '//TRIM(cModelNames(iModelIndex)[1])//' model is not a boundary node!',f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
            !Assign node index
            Connections[1]%iNodeGW(indx) = iNodeIndex
            
            !Save b.c. node and layer to be removed
            iBCNodes(iCount)  = iNodeIndex
            iBCLayers(iCount) = Connections[1]%iLayerGW(indx)
        END IF
        
        !Loop over connected nodes
        DO indx1=1,Connections[1]%iNConnectedNodesGW(indx) 
            IF (Connections[1]%iModelConnectedGW(indx1,indx) .EQ. iModelIndex) THEN
                iNodeID    = Connections[1]%iNodeConnectedGW(indx1,indx)
                iNodeIndex = LocateInList(iNodeID,Model%iNodeIDs)
                !Is node ID legit?
                IF (iNodeIndex .EQ. 0) THEN
                    CALL SetLastMessage('Groundwater node '//TRIM(IntTotext(iNodeID))//' listed for model linkage is not in model '//TRIM(cModelNames(iModelIndex)[1])//'!',f_iFatal,ThisProcedure)
                    iStat = -1
                    RETURN
                END IF
                !Node must be a boundary node
                IF (.NOT. Model%IsBoundaryNode(iNodeIndex)) THEN
                    CALL SetLastMessage('Groundwater node '//TRIM(IntTotext(iNodeID))//' listed for model linkage in '//TRIM(cModelNames(iModelIndex)[1])//' model is not a boundary node!',f_iFatal,ThisProcedure)
                    iStat = -1
                    RETURN
                END IF
                !Assign node index
                Connections[1]%iNodeConnectedGW(indx1,indx) = iNodeIndex
            END IF
        END DO
    END DO
  
    !Remove previously defined b.c. nodes from model
    CALL Model%RemoveGWBC(iBCNodes(1:iCount),iBCLayers(1:iCount),iStat)  ;  IF (iStat .NE. 0) RETURN
    
    !Add new b.c. nodes as specified-flow b.c.
    CALL Model%SetGWBCNodes(iBCNodes(1:iCount),iBCLayers(1:iCount),iFlowBCID,iStat)  ;  IF (iStat .NE. 0) RETURN
    
    
    !=====================================
    !=== Prepare stream nodes
    !=====================================
    DO indx=1,Connections[1]%iNLinkNodesST
        !First stream node
        IF (Connections[1]%iModelST(indx) .EQ. iModelIndex) THEN
             iNodeID    = Connections[1]%iNodeST(indx)
             iNodeIndex = LocateInList(iNodeID,Model%iStrmNodeIDs)
             !Is node ID legit?
             IF (iNodeIndex .EQ. 0) THEN
                 CALL SetLastMessage('Stream node '//TRIM(IntTotext(iNodeID))//' listed for model linkage is not in model '//TRIM(cModelNames(iModelIndex)[1])//'!',f_iFatal,ThisProcedure)
                 iStat = -1
                 RETURN
             END IF
             !Assign node index
             Connections[1]%iNodeST(indx) = iNodeIndex
        END IF
        
        !Second stream node
        IF (Connections[1]%iModelConnectedST(indx) .EQ. iModelIndex) THEN
             iNodeID    = Connections[1]%iNodeConnectedST(indx)
             iNodeIndex = LocateInList(iNodeID,Model%iStrmNodeIDs)
             !Is node ID legit?
             IF (iNodeIndex .EQ. 0) THEN
                 CALL SetLastMessage('Stream node '//TRIM(IntTotext(iNodeID))//' listed for model linkage is not in model '//TRIM(cModelNames(iModelIndex)[1])//'!',f_iFatal,ThisProcedure)
                 iStat = -1
                 RETURN
             END IF
             !Assign node index
             Connections[1]%iNodeConnectedST(indx) = iNodeIndex
        END IF
    END DO
    
    
    !=====================================
    !=== Prepare bypasses
    !=====================================
    CALL Model%GetNBypasses(iNBypasses,iStat)  ;  IF (iStat .NE. 0) RETURN
    ALLOCATE (iBypassIDs(iNBypasses))
    CALL Model%GetBypassIDs(iBypassIDs)
    DO indx=1,Connections[1]%iNLinkNodesBP
        !Store bypass IDs
        iBPID          = Connections[1]%iBP(indx)
        iConnectedBPID = Connections[1]%iConnectedBP(indx)
        
        !Process bypass that is being exported
        IF (Connections[1]%iModelBP(indx) .EQ. iModelIndex) THEN
            !Convert bypass ID
            iIndex = LocateInList(iBPID , iBypassIDs)
            IF (iIndex .EQ. 0) THEN
                CALL SetLastMessage('Bypass '//TRIM(IntTotext(iBPID))//' listed for model linkage is not in the exporting model '//TRIM(cModelNames(iModelIndex)[1])//'!',f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
            Connections[1]%iBP(indx) = iIndex
            
            !Make sure bypass is destined to outside of the model that it is taken out of
            CALL Model%GetBypassDiversionOriginDestData(.TRUE.,iIndex,iStrmNodeExport,iDestType,iDest)
            IF (iDestType .NE. f_iFlowDest_Outside) THEN
                CALL SetLastMessage('Bypass '//TRIM(IntTotext(iBPID))//' listed for model linkage in model '//TRIM(cModelNames(iModelIndex)[1])//' is not delivered outside that model!',f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
        END IF
        
        !Process bypass that is being imported
        IF (Connections[1]%iModelConnectedBP(indx) .EQ. iModelIndex) THEN
            !Convert bypass ID
            iIndex = LocateInList(iConnectedBPID , iBypassIDs)
            IF (iIndex .EQ. 0) THEN
                CALL SetLastMessage('Bypass '//TRIM(IntTotext(iConnectedBPID))//' listed for model linkage is not in the importing model '//TRIM(cModelNames(iModelIndex)[1])//'!',f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
            Connections[1]%iConnectedBP(indx) = iIndex
            
            !Make sure bypass is being imported
            CALL Model%GetBypassDiversionOriginDestData(.TRUE.,iIndex,iStrmNodeExport,iDestType,iDest)
            IF (iStrmNodeExport .NE. 0) THEN
                CALL SetLastMessage('Bypass '//TRIM(IntTotext(iConnectedBPID))//' listed for model linkage in model '//TRIM(cModelNames(iModelIndex)[1])//' is not imported from outside the model area!',f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
        END IF
    END DO
    
    
    !=====================================
    !=== Prepare diversions
    !=====================================
    iNDiver = Model%GetNDiversions()  
    ALLOCATE (iDiverIDs(iNDiver))
    CALL Model%GetDiversionIDs(iDiverIDs)
    DO indx=1,Connections[1]%iNLinkNodesDiv
        !Store diversion IDs
        iDivID          = Connections[1]%iDiv(indx)
        iConnectedDivID = Connections[1]%iConnectedDiv(indx)
        
        !Process diversion that is being exported
        IF (Connections[1]%iModelDiv(indx) .EQ. iModelIndex) THEN
            !Convert diversion ID
            iIndex = LocateInList(iDivID , iDiverIDs)
            IF (iIndex .EQ. 0) THEN
                CALL SetLastMessage('Diversion '//TRIM(IntTotext(iDivID))//' listed for model linkage is not in the exporting model '//TRIM(cModelNames(iModelIndex)[1])//'!',f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
            Connections[1]%iDiv(indx) = iIndex
            
            !Make sure diversion is destined to outside of the model that it is taken out of
            CALL Model%GetBypassDiversionOriginDestData(.FALSE.,iIndex,iStrmNodeExport,iDestType,iDest)
            IF (iDestType .NE. f_iFlowDest_Outside) THEN
                CALL SetLastMessage('Diversion '//TRIM(IntTotext(iDivID))//' listed for model linkage in model '//TRIM(cModelNames(iModelIndex)[1])//' is not delivered outside that model!',f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
        END IF
        
        !Process diversion that is being imported
        IF (Connections[1]%iModelConnectedDiv(indx) .EQ. iModelIndex) THEN
            !Convert diversion ID
            iIndex = LocateInList(iConnectedDivID , iDiverIDs)
            IF (iIndex .EQ. 0) THEN
                CALL SetLastMessage('Diversion '//TRIM(IntTotext(iConnectedDivID))//' listed for model linkage is not in the importing model '//TRIM(cModelNames(iModelIndex)[1])//'!',f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
            Connections[1]%iConnectedDiv(indx) = iIndex
            
            !Make sure diversion is being imported
            CALL Model%GetBypassDiversionOriginDestData(.FALSE.,iIndex,iStrmNodeExport,iDestType,iDest)
            IF (iStrmNodeExport .NE. 0) THEN
                CALL SetLastMessage('Diversion '//TRIM(IntTotext(iConnectedDivID))//' listed for model linkage in model '//TRIM(cModelNames(iModelIndex)[1])//' is not imported from outside the model area!',f_iFatal,ThisProcedure)
                iStat = -1
                RETURN
            END IF
        END IF
    END DO
    
  END SUBROUTINE PrepConnections
  
  
  ! -------------------------------------------------------------
  ! --- COMPUTE CONDUCTANCES
  ! -------------------------------------------------------------
  SUBROUTINE ComputeConductances(Model,iModelIDs,iModelIndex,ConductanceTempData,Connections,iStat)
    TYPE(ModelWithIDType),INTENT(IN) :: Model[*]
    INTEGER,INTENT(IN)               :: iModelIDs(:),iModelIndex
    TYPE(GHBConductanceTempDataType) :: ConductanceTempData[*]
    TYPE(ModelConnectionType)        :: Connections[*]
    INTEGER,INTENT(OUT)              :: iStat
    
    !Local variables
    CHARACTER(LEN=f_iModNameLen+21),PARAMETER :: ThisProcedure = f_cModName // '::ComputeConductances'
    INTEGER                                   :: indx,indx1,ErrorCode,iNode1,iNode2,iLayer1,iLayer2,iNPairs,       &
                                                 iNLayers,iNNodes,iCount,iModel1,iModel2,iNConnectedNodes         
    REAL(8)                                   :: rDistance,rX1,rY1,rX2,rY2,rLength1,rLength2,rThick1,rThick2,      &
                                                 rK1,rK2,rLength,rConductanceInv1,rConductanceInv2,rThick
    REAL(8),ALLOCATABLE                       :: rKh(:,:),rX(:),rY(:),rTopElev(:,:),rBottomElev(:,:)
    
    !Initialize
    iStat = 0
    
    !Allocate memory for temp data
    IF (lMaster) THEN
        !Total number of node pairs considering aquifer layers
        iNPairs = SUM(Connections%iNConnectedNodesGW)
        
        !Allocate memory for data
        ALLOCATE (ConductanceTempData%rK1(Connections%iNLinkNodesGW)      , &
                  ConductanceTempData%rK2(iNPairs)                        , &
                  ConductanceTempData%rThick1(Connections%iNLinkNodesGW)  , &
                  ConductanceTempData%rThick2(iNPairs)                    , &
                  ConductanceTempData%rLength1(Connections%iNLinkNodesGW) , &
                  ConductanceTempData%rLength2(iNPairs)                   , &
                  ConductanceTempData%rX1(Connections%iNLinkNodesGW)      , &
                  ConductanceTempData%rY1(Connections%iNLinkNodesGW)      , &
                  ConductanceTempData%rX2(iNPairs)                        , &
                  ConductanceTempData%rY2(iNPairs)                        )
    END IF
    SYNC IMAGES (iActiveImages)
    
    !Retrieve relevant aquifer paremeters at linked nodes
    IF (lRunner) THEN
        iNLayers   = Model%GetNLayers()
        iNNodes    = SIZE(Model%iNodeIDs)
        ALLOCATE (rKh(iNNodes,iNLayers) , rX(iNNodes) , rY(iNNodes) , rTopElev(iNNodes,iNLayers) , rBottomElev(iNNodes,iNLayers))
        CALL Model%GetAquiferHorizontalK(rKh,iStat)  ;  IF (iStat .NE. 0) RETURN
        CALL Model%GetNodeXY(rX,rY)
        CALL Model%GetAquiferTopElev(rTopElev)
        CALL Model%GetAquiferBottomElev(rBottomElev)
        
        !Pick the parameters at the linked nodes
        iCount = 0
        DO indx=1,Connections[1]%iNLinkNodesGW
            iModel1 = Connections[1]%iModelGW(indx)
            !Node coordinates, boundary length,hydraulic conductivity and thickness for node 1 in model 1
            IF (iModel1 .EQ. iModelIndex) THEN
                iNode1                                = Connections[1]%iNodeGW(indx)
                iLayer1                               = Connections[1]%iLayerGW(indx)
                ConductanceTempData[1]%rX1(indx)      = rX(iNode1)
                ConductanceTempData[1]%rY1(indx)      = rY(iNode1)
                ConductanceTempData[1]%rLength1(indx) = Model%GetBoundaryLengthAtNode(iNode1)
                ConductanceTempData[1]%rK1(indx)      = rKh(iNode1,iLayer1)
                ConductanceTempData[1]%rThick1(indx)  = rTopElev(iNode1,iLayer1) - rBottomElev(iNode1,iLayer1)
            END IF
            !Data for connecting nodes
            DO indx1=1,Connections[1]%iNConnectedNodesGW(indx)
                iCount  = iCount + 1
                iModel2 = Connections[1]%iModelConnectedGW(indx1,indx)
                !Node coordinates, boundary length,hydraulic conductivity and thickness for node 2 in model 2
                IF (iModel2 .EQ. iModelIndex) THEN
                    iNode2                                  = Connections[1]%iNodeConnectedGW(indx1,indx)
                    iLayer2                                 = Connections[1]%iLayerConnectedGW(indx1,indx)
                    ConductanceTempData[1]%rX2(iCount)      = rX(iNode2)
                    ConductanceTempData[1]%rY2(iCount)      = rY(iNode2)
                    ConductanceTempData[1]%rLength2(iCount) = Model%GetBoundaryLengthAtNode(iNode2)
                    ConductanceTempData[1]%rK2(iCount)      = rKh(iNode2,iLayer2)
                    ConductanceTempData[1]%rThick2(iCount)  = rTopElev(iNode2,iLayer2) - rBottomElev(iNode2,iLayer2)
                END IF
            END DO
        END DO
    END IF
    SYNC IMAGES (iActiveImages)
    
    !Compute conductances at each node pair at each layer listed by user as well as effective conductances at each link node
    IF (lMaster) THEN
        !Compute conductances
        iCount = 0
        DO indx=1,Connections%iNLinkNodesGW
            rX1      = ConductanceTempData%rX1(indx)
            rY1      = ConductanceTempData%rY1(indx)
            rK1      = ConductanceTempData%rK1(indx)
            rLength1 = ConductanceTempData%rLength1(indx)
            rThick1  = ConductanceTempData%rThick1(indx)
            DO indx1=1,Connections%iNConnectedNodesGW(indx)
                iCount    = iCount + 1
                rX2       = ConductanceTempData%rX2(iCount)
                rY2       = ConductanceTempData%rY2(iCount)
                rK2       = ConductanceTempData%rK2(iCount)
                rLength2  = ConductanceTempData%rLength2(iCount)
                rThick2   = ConductanceTempData%rThick2(iCount)
                rDistance = SQRT((rX1-rX2)*(rX1-rX2) + (rY1-rY2)*(rY1-rY2))  ;  IF (rDistance .EQ. 0.0) rDistance = 1.0
                rLength   = MIN(rLength1 , rLength2) 
                rThick    = MIN(rThick1 , rThick2)
                rConductanceInv1 = rK1*rThick*rLength  ;  IF (rConductanceInv1 .GT. 0.0) rConductanceInv1 = 1D0 / rConductanceInv1
                rConductanceInv2 = rK2*rThick*rLength  ;  IF (rConductanceInv2 .GT. 0.0) rConductanceInv2 = 1D0 / rConductanceInv2
                IF (rConductanceInv1.EQ.0.0  .OR.  rConductanceInv2.EQ.0.0) THEN
                    Connections%rConductanceConnectedGW(indx1,indx) = 0.0
                ELSE
                    Connections%rConductanceConnectedGW(indx1,indx) = 2D0/(rDistance * (rConductanceInv1 + rConductanceInv2))
                END IF
            END DO
        END DO
        
        !Compute effective conductances at each node,layer,model combination
        DO indx=1,Connections%iNLinkNodesGW
            iNConnectedNodes                          = Connections%iNConnectedNodesGW(indx)
            Connections%rEffectiveConductanceGW(indx) = SUM(Connections%rConductanceConnectedGW(1:iNConnectedNodes,indx))
            IF (Connections%rEffectiveConductanceGW(indx) .EQ. 0.0) THEN
                iModel1         = iModelIDs(Connections%iModelGW(indx))
                iNode1          = Model[Connections%iModelGW(indx)+1]%iNodeIDs(Connections%iNodeGW(indx))
                iLayer1         = Connections%iLayerGW(indx)
                MessageArray(1) = 'The following (model, node, layer) has zero effective conductance!'
                MessageArray(2) = 'This means it is either an inactive node or being linked to all inactive nodes.'
                MessageArray(3) = '(' // TRIM(cModelNames(iModel1)) // ', ' // TRIM(IntToText(iNode1)) // ', ' // TRIM(IntToText(iLayer1)) // ')'
                CALL LogMessage(MessageArray(1:3),f_iInfo,ThisProcedure)
            END IF
        END DO
    END IF
    
    !Clear memory of the temp data
    IF (lMaster) THEN
        DEALLOCATE (ConductanceTempData%rK1      , &
                    ConductanceTempData%rK2      , &
                    ConductanceTempData%rX1      , &
                    ConductanceTempData%rY1      , &
                    ConductanceTempData%rX2      , &
                    ConductanceTempData%rY2      , &
                    ConductanceTempData%rLength1 , &
                    ConductanceTempData%rLength2 , &
                    ConductanceTempData%rThick1  , &
                    ConductanceTempData%rThick2  , &
                    STAT = ErrorCode             )
    END IF
    
  END SUBROUTINE ComputeConductances
  
  
  ! -------------------------------------------------------------
  ! --- ADVANCE TIME
  ! -------------------------------------------------------------
  SUBROUTINE AdvanceTime(Model,TimeStep)
    TYPE(ModelWithIDType) :: Model
    TYPE(TimeStepType)    :: TimeStep
    
    !Advance time in model
    IF (lRunner) CALL Model%AdvanceTime()
    
    !Advance time in Master and print it out
    IF (lMaster) THEN
        !Increment the time step counter and time stamp
        TimeStep%CurrentTimeStep    = TimeStep%CurrentTimeStep + 1
        TimeStep%CurrentDateAndTime = IncrementTimeStamp(TimeStep%CurrentDateAndTime,TimeStep%DELTAT_InMinutes)
        
        !Print time step
        WRITE (MessageArray(1),'(A)') '*   TIME STEP '//TRIM(IntToText(TimeStep%CurrentTimeStep))//' AT '//TRIM(TimeStep%CurrentDateAndTime)
        MessageArray(1) = f_cLineFeed//REPEAT('-',50)//f_cLineFeed//TRIM(MessageArray(1))//f_cLineFeed//REPEAT('-',50)
        CALL LogMessage(MessageArray(1),f_iMessage,'',Destination=f_iFILE)
        
        !Print title for iteration and convergence tracking
        CALL LogMessage('   ITER      CONVERGENCE         MAX.DIFF    VARIABLE      MODEL'//f_cLineFeed//REPEAT('-',65),f_iMessage,'',Destination=f_iFILE)

    END IF
    
  END SUBROUTINE AdvanceTime
  
  
  ! -------------------------------------------------------------
  ! --- SET BOUNDARY FLOWS 
  ! -------------------------------------------------------------
  SUBROUTINE SetFlowBC(Model,iModelIndex,lFirstCall,Connections,iStat)
    TYPE(ModelWithIDType)     :: Model[*]
    INTEGER,INTENT(IN)        :: iModelIndex
    LOGICAL,INTENT(IN)        :: lFirstCall
    TYPE(ModelConnectionType) :: Connections[*]
    INTEGER,INTENT(OUT)       :: iStat
    
    !Local variables
    INTEGER                  :: indx,iNode,iLayer
    LOGICAL,SAVE             :: lDataInitialized  = .FALSE. 
    INTEGER,SAVE             :: iNLinkNodesGW,iNLinkNodesST,iNLinkNodesBP,iNLinkNodesDiv
    INTEGER,ALLOCATABLE,SAVE :: iModelGW(:),iNodeGW(:),iLayerGW(:),iModelST(:),iNodeST(:)    , &
                                iModelConnectedST(:),iNodeConnectedST(:),iModelConnectedBP(:), &
                                iConnectedBP(:),iModelConnectedDiv(:),iConnectedDiv(:) 
    REAL(8),ALLOCATABLE,SAVE :: rFlowGW(:),rInflowConnectedReadST(:),rFlowST(:),rFlowBP(:),rFlowDiv(:)
    
    !Initailize
    iStat = 0
    
    !First collect heads from models
    IF (lRunner) CALL CollectHeadsFlows(Model,iModelIndex,Connections)
    SYNC IMAGES (iActiveImages)
    
    !Calculate gw flows and store the connection flows at the beginning of time step
    IF (lMaster) THEN
        CALL CalculateGWFlows(Connections)
        IF (lFirstCall) THEN
            Connections%rFlowGW_P = Connections%rFlowGW
            Connections%rFlowST_P = Connections%rFlowST
        END IF
    END IF
    SYNC IMAGES (iActiveImages)
  
    !Pass boundary flows to models
    IF (lRunner) THEN
        !Copy info from master (this proved to be faster than getting information one at a time)
        IF (.NOT. lDataInitialized) THEN
            iNLinkNodesGW  = Connections[1]%iNLinkNodesGW
            iNLinkNodesST  = Connections[1]%iNLinkNodesST
            iNLinkNodesBP  = Connections[1]%iNLinkNodesBP
            iNLinkNodesDiv = Connections[1]%iNLinkNodesDiv
            ALLOCATE (iModelGW(iNLinkNodesGW)               , &
                      iNodeGW(iNLinkNodesGW)                , &
                      iLayerGW(iNLinkNodesGW)               , &
                      rFlowGW(iNLinkNodesGW)                , &
                      iModelST(iNLinkNodesST)               , &
                      iNodeST(iNLinkNodesST)                , &
                      iModelConnectedST(iNLinkNodesST)      , &
                      iNodeConnectedST(iNLinkNodesST)       , &
                      rInflowConnectedReadST(iNLinkNodesST) , &
                      rFlowST(iNLinkNodesST)                , &
                      iModelConnectedBP(iNLinkNodesBP)      , &
                      iConnectedBP(iNLinkNodesBP)           , &
                      rFlowBP(iNLinkNodesBP)                , &
                      iModelConnectedDiv(iNLinkNodesDiv)    , &
                      iConnectedDiv(iNLinkNodesDiv)         , &
                      rFlowDiv(iNLinkNodesDiv)              )
            iModelGW           = Connections[1]%iModelGW
            iNodeGW            = Connections[1]%iNodeGW
            iLayerGW           = Connections[1]%iLayerGW
            iModelST           = Connections[1]%iModelST
            iNodeST            = Connections[1]%iNodeST
            iModelConnectedST  = Connections[1]%iModelConnectedST
            iNodeConnectedST   = Connections[1]%iNodeConnectedST
            iModelConnectedBP  = Connections[1]%iModelConnectedBP
            iConnectedBP       = Connections[1]%iConnectedBP
            iModelConnectedDiv = Connections[1]%iModelConnectedDiv
            iConnectedDiv      = Connections[1]%iConnectedDiv
            
            lDataInitialized  = .TRUE.
        END IF
        rFlowGW                = Connections[1]%rFlowGW
        rInflowConnectedReadST = Connections[1]%rInflowConnectedReadST
        rFlowST                = Connections[1]%rFlowST
        rFlowBP                = Connections[1]%rFlowBP
        rFlowDiv               = Connections[1]%rFlowDiv
        
        !Groundwater b.c.
        DO indx=1,iNLinkNodesGW
            IF (iModelGW(indx) .EQ. iModelIndex) THEN
                iNode  = iNodeGW(indx)
                iLayer = iLayerGW(indx)
                CALL Model%SetGWBC(iNode,iLayer,iFlowBCID,iStat,rFlow=rFlowGW(indx))
                IF (iStat .NE. 0) RETURN
            END IF
        END DO
        
        !Stream b.c.
        DO indx=1,iNLinkNodesST
            IF (iModelConnectedST(indx) .EQ. iModelIndex) THEN
                CALL Model%SetStreamInflow(iNodeConnectedST(indx),rInflowConnectedReadST(indx),.FALSE.,iStat)  ;  IF (iStat .NE. 0) RETURN
                CALL Model%SetStreamInflow(iNodeConnectedST(indx),rFlowST(indx),.TRUE.,iStat)                  ;  IF (iStat .NE. 0) RETURN
            END IF
        END DO
        
        !Bypasses
        DO indx=1,iNLinkNodesBP
            IF (iModelConnectedBP(indx) .EQ. iModelIndex) THEN
                CALL Model%SetBypassFlows_AtABypass(iConnectedBP(indx),rFlowBP(indx))
            END IF
        END DO
        
        !Diversions
        DO indx=1,iNLinkNodesDiv
            IF (iModelConnectedDiv(indx) .EQ. iModelIndex) THEN
                CALL Model%SetStreamDiversionRead(iConnectedDiv(indx),rFlowDiv(indx))
            END IF
        END DO
    END IF
    
  END SUBROUTINE SetFlowBC
    
  
  ! -------------------------------------------------------------
  ! --- COLLECT GW HEADS, FLOWS FROM MODELS TO PASS TO OTHER MODELS AS B.C.
  ! -------------------------------------------------------------
  SUBROUTINE CollectHeadsFlows(Model,iModelIndex,Connections)
    TYPE(ModelWithIDType),INTENT(IN) :: Model[*]
    INTEGER,INTENT(IN)               :: iModelIndex
    TYPE(ModelConnectionType)        :: Connections[*]
    
    !Local variables
    INTEGER                  :: indx,indx1,iNode,iLayer,iNConnectedNodes
    LOGICAL,SAVE             :: lDataInitialized = .FALSE.
    INTEGER,SAVE             :: iNLinkNodesGW,iNLinkNodesST,iNLinkNodesBP,iNLinkNodesDiv
    INTEGER,ALLOCATABLE,SAVE :: iModelGW(:),iNodeGW(:),iLayerGW(:),iModelConnectedGW(:,:), &
                                iNodeConnectedGW(:,:),iLayerConnectedGW(:,:),iModelST(:),  &
                                iNodeST(:),iModelBP(:),iBP(:),iModelDiv(:),iDiv(:)
    
    !Copy infor from master
    IF (.NOT. lDataInitialized) THEN
        iNLinkNodesGW  = Connections[1]%iNLinkNodesGW
        iNLinkNodesST  = Connections[1]%iNLinkNodesST
        iNLinkNodesBP  = Connections[1]%iNLinkNodesBP
        iNLinkNodesDiv = Connections[1]%iNLinkNodesDiv
        ALLOCATE (iModelGW(iNLinkNodesGW)                               , &
                  iLayerGW(iNLinkNodesGW)                               , &
                  iNodeGW(iNLinkNodesGW)                                , &
                  iModelConnectedGW(f_iMaxConnectedNodes,iNLinkNodesGW) , &
                  iNodeConnectedGW(f_iMaxConnectedNodes,iNLinkNodesGW)  , &
                  iLayerConnectedGW(f_iMaxConnectedNodes,iNLinkNodesGW) , &
                  iModelST(iNLinkNodesST)                               , &
                  iNodeST(iNLinkNodesST)                                , &
                  iModelBP(iNLinkNodesBP)                               , &
                  iBP(iNLinkNodesBP)                                    , &
                  iModelDiv(iNLinkNodesDiv)                             , &
                  iDiv(iNLinkNodesDiv)                                  )
        
        !Retrieve info from master (copying information from master in one shot proved to be much efficient than using one information at a time)
        iModelGW          = Connections[1]%iModelGW
        iNodeGW           = Connections[1]%iNodeGW
        iLayerGW          = Connections[1]%iLayerGW
        iModelConnectedGW = Connections[1]%iModelConnectedGW
        iNodeConnectedGW  = Connections[1]%iNodeConnectedGW
        iLayerConnectedGW = Connections[1]%iLayerConnectedGW
        iModelST          = Connections[1]%iModelST
        iNodeST           = Connections[1]%iNodeST
        iModelBP          = Connections[1]%iModelBP
        iBP               = Connections[1]%iBP
        iModelDiv         = Connections[1]%iModelDiv
        iDiv              = Connections[1]%iDiv
        
        lDataInitialized  = .TRUE.
    END IF
    
    !Collect heads from model
    DO indx=1,iNLinkNodesGW
        IF (iModelGW(indx) .EQ. iModelIndex) THEN
            iNode                        = iNodeGW(indx)
            iLayer                       = iLayerGW(indx)
            Connections[1]%rHeadGW(indx) = Model%GetGWHead_AtOneNodeLayer(iNode,iLayer,lPrevious=.FALSE.)
        END IF
        iNConnectedNodes = Connections[1]%iNConnectedNodesGW(indx)
        DO indx1=1,iNConnectedNodes
            IF (iModelConnectedGW(indx1,indx) .EQ. iModelIndex) THEN
                iNode                                       = iNodeConnectedGW(indx1,indx)
                iLayer                                      = iLayerConnectedGW(indx1,indx)
                Connections[1]%rHeadConnectedGW(indx1,indx) = Model%GetGWHead_AtOneNodeLayer(iNode,iLayer,lPrevious=.FALSE.)
            END IF
        END DO
    END DO
    
    !Collect stream flows from model (only upstream flows since downstream flows don't affect upstream)
    DO indx=1,iNLinkNodesST
        IF (iModelST(indx) .EQ. iModelIndex) THEN
            iNode                        = iNodeST(indx)
            Connections[1]%rFlowST(indx) = Model%GetStrmFlow(iNode)
        END IF
    END DO
    
    !Collect bypass flows
    DO indx=1,iNLinkNodesBP
        IF (iModelBP(indx) .EQ. iModelIndex) THEN
            Connections[1]%rFlowBP(indx) = Model%GetBypassReceived_FromABypass(iBP(indx))
        END IF
    END DO
    
    !Collect diversions
    DO indx=1,iNLinkNodesDiv
        IF (iModelDiv(indx) .EQ. iModelIndex) THEN
            Connections[1]%rFlowDiv(indx) = Model%GetStrmDiversionDelivery(iDiv(indx))
        END IF
    END DO
  END SUBROUTINE CollectHeadsFlows
  
  
  ! -------------------------------------------------------------
  ! --- CALCULATE GROUNDWATER FLOWS BETWEEN MODELS
  ! -------------------------------------------------------------
  SUBROUTINE CalculateGWFlows(Connections)
    TYPE(ModelConnectionType) :: Connections
    
    !Local variables
    INTEGER :: indx,iNConnectedNodes
    REAL(8) :: rEffConductance
    
    DO indx=1,Connections%iNLinkNodesGW
        rEffConductance = Connections%rEffectiveConductanceGW(indx)
        IF (rEffConductance .NE. 0.0) THEN
            iNConnectedNodes          = Connections%iNConnectedNodesGW(indx)
            Connections%rFlowGW(indx) = SUM(Connections%rConductanceConnectedGW(1:iNConnectedNodes,indx) * Connections%rHeadConnectedGW(1:iNConnectedNodes,indx)) - (Connections%rHeadGW(indx) * rEffConductance)
        END IF
    END DO

  END SUBROUTINE CalculateGWFlows
  
  
  ! -------------------------------------------------------------
  ! --- CHECK FOR CONVERGENCE
  ! -------------------------------------------------------------
  FUNCTION CheckConvergence(Model,cModelNames,iModelIndex,iIter,Connections) RESULT(lConverged)
    TYPE(ModelWithIDType),INTENT(IN) :: Model[*]
    CHARACTER(LEN=*),INTENT(IN)      :: cModelNames(:)[*]
    INTEGER,INTENT(IN)               :: iModelIndex,iIter
    TYPE(ModelConnectionType)        :: Connections[*]
    LOGICAL                          :: lConverged
    
    !Local variables
    INTEGER   :: indx,indxModel,iNode,iImage
    REAL(8)   :: rConverge,rDiff,rDiffMax
    CHARACTER :: cMessage*100,cVariable*10,cModel*30
    
    !Initialize
    lConverged = .TRUE.
    
    !Retrieve the latest heads from models
    IF (lRunner) CALL CollectHeadsFlows(Model,iModelIndex,Connections)
    SYNC IMAGES (iActiveImages)
    
    !Calculate flows based on new heads and check convergence
    IF (lMaster) THEN
        !Calculate gw flows
        CALL CalculateGWFlows(Connections)
        
        !Calculate convergence from groundwater
        rConverge = 0.0
        rDiffMax  = 0.0
        DO indx=1,Connections%iNLinkNodesGW
            IF (Connections%rFlowGW(indx) .EQ. 0.0) CYCLE
            rDiff     = ABS(Connections%rFlowGW(indx)-Connections%rFlowGW_P(indx)) / Connections%rFlowGW(indx)
            rConverge = rConverge + rDiff*rDiff
            IF (ABS(rDiff) .GT. rDiffMax) THEN
                rDiffMax  = rDiff
                indxModel = Connections%iModelGW(indx)
                iImage    = indxModel + 1
                iNode     = Connections%iNodeGW(indx)
                cVariable = TRIM(IntToText(Model[iImage]%iNodeIDs(iNode))) // '_GW'
                cModel    = TRIM(cModelNames(indxModel))
            END IF
        END DO
        
        !Convergence from stream flows
        DO indx=1,Connections%iNLinkNodesST
            IF (Connections%rFlowST(indx) .EQ. 0.0) CYCLE
            rDiff     = ABS(Connections%rFlowST(indx)-Connections%rFlowST_P(indx)) / Connections%rFlowST(indx)
            rConverge = rConverge + rDiff*rDiff
            IF (ABS(rDiff) .GT. rDiffMax) THEN
                rDiffMax  = rDiff
                indxModel = Connections%iModelST(indx)
                iImage    = indxModel + 1
                iNode     = Connections%iNodeST(indx)
                cVariable = TRIM(IntToText(Model[iImage]%iStrmNodeIDs(iNode))) // '_ST'
                cModel    = TRIM(cModelNames(indxModel))
            END IF
        END DO
        
        !Convergence from bypass flows 
        !Don't do anything; if stream flows converge so should bypasses
        
        !Print-out converegence
        WRITE (cMessage,'(I7,4X,G13.6,4X,G13.6,4X,A,4X,A)') iIter,SQRT(rConverge),rDiffMax,ADJUSTL(cVariable),TRIM(ADJUSTL(cModel))
        CALL LogMessage(TRIM(cMessage),f_iMessage,'',Destination=f_iFILE)
        
        !Did the models converge?
        IF (SQRT(rConverge) .GT. Connections%rConvergence) lConverged = .FALSE.
    END IF
    
  END FUNCTION CheckConvergence
  
  
  ! -------------------------------------------------------------
  ! --- ADVANCE ITERATIONS BETWEEN MODELS
  ! -------------------------------------------------------------
  SUBROUTINE AdvanceIterations(Connections,iIter,iStat)
    TYPE(ModelConnectionType) :: Connections
    INTEGER                   :: iIter
    INTEGER,INTENT(OUT)       :: iStat
    
    !Local variables
    CHARACTER(LEN=f_iModNameLen+19),PARAMETER :: ThisProcedure = f_cModName // '::AdvanceIterations'
    
    !Initialize
    iStat = 0
    
    IF (lRunner) RETURN
    
    iIter = iIter + 1
    IF (iIter .GT. Connections%iMaxIter) THEN
        CALL SetLastMessage('Models failed to converge at their interfaces!',f_iFatal,ThisProcedure)
        iStat = -1
    END IF
    Connections%rFlowGW_P = Connections%rFlowGW
    Connections%rFlowST_P = Connections%rFlowST

  END SUBROUTINE AdvanceIterations
  
  
  ! -------------------------------------------------------------
  ! --- END EXECUTION
  ! -------------------------------------------------------------
  SUBROUTINE EndExecution()
    
    !Local variables
    CHARACTER(LEN=LEN(cErrorMessage)) :: cMessage
    
    !Initialize
    cErrorMessage  = ''
    cMessage       = ''
    iErrModelIndex = 0
    
    !Handle error messages
    SYNC ALL
    IF (iStat[iErrImage] .EQ. -1) THEN
        IF (THIS_IMAGE() .NE. iErrImage) THEN
            IF (iStat .EQ. -1) THEN
                CALL LogLastMessage()
                !Pass error message to controller as well
                IF (.NOT. lMaster) THEN
                    CALL GetLastMessage(cMessage)
                    cErrorMessage[1]  = REPEAT('*',80) // f_cLineFeed // TRIM(cMessage) // f_cLineFeed // REPEAT('*',80)
                    iErrModelIndex[1] = THIS_IMAGE() - 1
                END IF
            END IF
        END IF
    END IF
    
    !Print error messages generated by runners to contoller's log file as well
    SYNC ALL
    IF (lMaster) THEN
        IF (cErrorMessage .NE. '') THEN
            IF (iErrModelIndex .GT. 0) THEN
                cMessage      = '* MESSAGE GENERATED BY MODEL '// TRIM(cModelNames(iErrModelIndex))
                CALL LogMessage(TRIM(cMessage)//f_cLineFeed//REPEAT('*',80),f_iMessage,'',f_iSCREEN)
                cErrorMessage = TRIM(cErrorMessage) // f_cLineFeed // TRIM(cMessage) // f_cLineFeed // REPEAT('*',80)
            END IF
            CALL LogMessage(TRIM(cErrorMessage),f_iMessage,'',Destination=f_iFILE)
        END IF
    END IF
    
    !Finish up run
    IF (lMaster) THEN
        CALL PrintRunTime()
        CALL KillLogFile()
    END IF
    
    !Stop the program
    STOP
    
  END SUBROUTINE EndExecution
END
    
    
