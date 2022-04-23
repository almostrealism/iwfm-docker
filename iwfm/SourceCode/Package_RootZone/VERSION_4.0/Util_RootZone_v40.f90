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
MODULE Util_RootZone_v40
  USE GeneralUtilities
  USE TimeSeriesUtilities
  USE IOInterface
  USE Package_Budget         , ONLY: BudgetType              , &
                                     BudgetHeaderType        , &
                                     VolumeUnitMarker        , &
                                     AreaUnitMarker          , &
                                     LocationNameMarker      , &
                                     AreaMarker              , &
                                     AR                      , &
                                     VR                      , &
                                     VLB                     , &
                                     VLE                     , &
                                     VR_lwu_PotCUAW          , &
                                     VR_lwu_AgSupplyReq      , &
                                     VR_lwu_AgPump           , &
                                     VR_lwu_AgDiv            , &
                                     VR_lwu_AgOthIn          , &
                                     VR_lwu_AgShort          , &
                                     PER_AVER                , &
                                     PER_CUM
  USE Util_Package_RootZone  , ONLY: WaterSupplyType         , &
                                     ReadRealData            , &
                                     ReadPointerData         , &
                                     AddStringToStringList   , &
                                     IrigPeriod              , &
                                     NoIrigPeriod            , &
                                     iDemandFromMoistAtBegin , &
                                     iDemandFromMoistAtEnd
  IMPLICIT NONE
  
  
  ! -------------------------------------------------------------
  ! --- PUBLIC ENTITIES
  ! -------------------------------------------------------------
  PRIVATE
  PUBLIC :: WaterSupplyType              , &
            ReadRealData                 , &
            ReadPointerData              , &
            AddStringToStringList        , &
            AgLWUseBudRawFile_New        , &
            AgRootZoneBudRawFile_New     , &
            LWUseBudRawFile_New          , &
            RootZoneBudRawFile_New       , &
            IrigPeriod                   , &
            NoIrigPeriod                 , &
            iDemandFromMoistAtBegin      , &
            iDemandFromMoistAtEnd        , &
            NAgLWUseBudColumns           , &
            NAgRootZoneBudColumns        , &
            NRootZoneBudColumns          , &
            NLWUseBudColumns             , &
            cLWUseBudgetColumnTitles     , &
            cRootZoneBudgetColumnTitles  
  
  
  ! -------------------------------------------------------------
  ! --- MISC. ENTITIES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER            :: NLWUseBudColumns                                 = 16  , &
                                  NRootZoneBudColumns                              = 46  , & 
                                  NAgLWUseBudColumns                               = 10  , &
                                  NAgRootZoneBudColumns                            = 17  
   CHARACTER(LEN=30),PARAMETER :: cLWUseBudgetColumnTitles(NLWUseBudColumns)       = ['Ag. Area'                           , &   
                                                                                      'Potential CUAW'                     , &   
                                                                                      'Ag. Supply Requirement'             , &   
                                                                                      'Ag. Pumping'                        , &   
                                                                                      'Ag. Deliveries'                     , &   
                                                                                      'Ag. Inflow as Surface Runoff'       , &   
                                                                                      'Ag. Shortage'                       , &   
                                                                                      'Ag. ETAW'                           , &   
                                                                                      'Ag. Effective Precipitation'        , &   
                                                                                      'Ag. ET from Other Sources'          , &   
                                                                                      'Urban Area'                         , &   
                                                                                      'Urban Supply Requirement'           , &   
                                                                                      'Urban Pumping'                      , &   
                                                                                      'Urban Deliveries'                   , &   
                                                                                      'Urban Inflow as Surface Runoff'     , &   
                                                                                      'Urban Shortage'                     ]     
   CHARACTER(LEN=53),PARAMETER :: cRootZoneBudgetColumnTitles(NRootZoneBudColumns) = ['Ag. Area'                                               , &     
                                                                                      'Ag. Potential ET'                                       , &     
                                                                                      'Ag. Precipitation'                                      , &     
                                                                                      'Ag. Runoff'                                             , &     
                                                                                      'Ag. Prime Applied Water'                                , &     
                                                                                      'Ag. Inflow as Surface Runoff'                           , &     
                                                                                      'Ag. Reused Water'                                       , &     
                                                                                      'Ag. Net Return Flow'                                    , &     
                                                                                      'Ag. Beginning Storage (+)'                              , &     
                                                                                      'Ag. Net Gain from Land Expansion (+)'                   , &     
                                                                                      'Ag. Infiltration (+)'                                   , &     
                                                                                      'Ag. Other Inflow (+)'                                   , &     
                                                                                      'Ag. Pond Drain (-)'                                     , &     
                                                                                      'Ag. Actual ET (-)'                                      , &     
                                                                                      'Ag. Percolation (-)'                                    , &     
                                                                                      'Ag. Ending Storage (-)'                                 , &     
                                                                                      'Ag. Discrepancy (=)'                                    , &     
                                                                                      'Urban Area'                                             , &     
                                                                                      'Urban Potential ET'                                     , &     
                                                                                      'Urban Precipitation'                                    , &     
                                                                                      'Urban Runoff'                                           , &     
                                                                                      'Urban Prime Applied Water'                              , &     
                                                                                      'Urban Inflow as Surface Runoff'                         , &     
                                                                                      'Urban Reused Water'                                     , &     
                                                                                      'Urban Net Return Flow'                                  , &     
                                                                                      'Urban Beginning Storage (+)'                            , &     
                                                                                      'Urban Net Gain from Land Expansion (+)'                 , &     
                                                                                      'Urban Infiltration (+)'                                 , &     
                                                                                      'Urban Other Inflow (+)'                                 , &     
                                                                                      'Urban Actual ET (-)'                                    , &     
                                                                                      'Urban Percolation (-)'                                  , &     
                                                                                      'Urban Ending Storage (-)'                               , &     
                                                                                      'Urban Discrepancy (=)'                                  , &     
                                                                                      'Native&Riparian Veg. Area'                              , &     
                                                                                      'Native&Riparian Veg. Potential ET'                      , &     
                                                                                      'Native&Riparian Veg. Precipitation'                     , &     
                                                                                      'Native&Riparian Veg. Inflow as Surface Runoff'          , &     
                                                                                      'Native&Riparian Veg. Runoff'                            , &     
                                                                                      'Native&Riparian Veg. Beginning Storage (+)'             , &     
                                                                                      'Native&Riparian Veg. Net Gain from Land Expansion (+)'  , &     
                                                                                      'Native&Riparian Veg. Infiltration (+)'                  , &     
                                                                                      'Native&Riparian Veg. Other Inflow (+)'                  , &     
                                                                                      'Native&Riparian Veg. Actual ET (-)'                     , &     
                                                                                      'Native&Riparian Veg. Percolation (-)'                   , &     
                                                                                      'Native&Riparian Veg. Ending Storage (-)'                , &     
                                                                                      'Native&Riparian Veg. Discrepancy (=)'                   ]       
                    
  
  
  
CONTAINS



  ! -------------------------------------------------------------
  ! --- NEW BINARY LAND AND WATER USE BUDGET FILE FOR POST-PROCESSING
  ! -------------------------------------------------------------
  SUBROUTINE LWUseBudRawFile_New(IsForInquiry,cFileName,TimeStep,NTIME,NRegion,RegionArea,cRegionNames,cDescriptor,cVersion,RawFile,iStat)
    LOGICAL,INTENT(IN)            :: IsForInquiry
    CHARACTER(LEN=*),INTENT(IN)   :: cFileName,cRegionNames(NRegion)
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    INTEGER,INTENT(IN)            :: NTIME,NRegion
    REAL(8),INTENT(IN)            :: RegionArea(NRegion)
    CHARACTER(LEN=*),INTENT(IN)   :: cDescriptor
    CHARACTER(LEN=*),INTENT(IN)   :: cVersion
    TYPE(BudgetType),INTENT(OUT)  :: RawFile
    INTEGER,INTENT(OUT)           :: iStat
    
    !Local variables
    TYPE(BudgetHeaderType) :: OutputData
    TYPE(TimeStepType)     :: TimeStepLocal
    INTEGER,PARAMETER      :: NTitles            = 6   , &
                              TitleLen           = 229 , &        
                              NColumnHeaderLines = 4   
    INTEGER                :: indxCol,indxLocation,iCount
    CHARACTER              :: UnitT*10,Text*17,Text1*13
    CHARACTER(LEN=6)       :: CParts(NLWUseBudColumns) = (/'AREA'   , &
                                                           'VOLUME' , &
                                                           'VOLUME' , &
                                                           'VOLUME' , &
                                                           'VOLUME' , &
                                                           'VOLUME' , &
                                                           'VOLUME' , &
                                                           'VOLUME' , &
                                                           'VOLUME' , &
                                                           'VOLUME' , &
                                                           'AREA'   , &
                                                           'VOLUME' , &
                                                           'VOLUME' , &
                                                           'VOLUME' , &
                                                           'VOLUME' , &
                                                           'VOLUME'/)
    CHARACTER(LEN=13)      :: FParts(NLWUseBudColumns) = (/'AG_AREA'         ,&
                                                           'AG_POTNL_CUAW'   ,&
                                                           'AG_SUP_REQ'      ,&    
                                                           'AG_PUMPING'      ,&
                                                           'AG_DELIVERY'     ,&
                                                           'AG_SR_INFLOW'    ,&
                                                           'AG_SHORTAGE'     ,&
                                                           'AG_ETAW'         ,&
                                                           'AG_EFF_PRECIP'   ,&
                                                           'AG_ET_OTH'       ,&
                                                           'URB_AREA'        ,&
                                                           'URB_SUP_REQ'     ,&       
                                                           'URB_PUMPING'     ,&
                                                           'URB_DELIVERY'    ,&
                                                           'URB_SR_INFLOW'   ,&
                                                           'URB_SHORTAGE'    /)
    
    !Initialize
    iStat = 0

    !Instantiate the land and water use raw file for when it is opened for inquiry
    IF (IsForInquiry) THEN
        CALL RawFile%New(cFileName,iStat)
        RETURN
    END IF
       
    !Budget descriptor
    OutputData%cBudgetDescriptor = cDescriptor
    
    !Increment the initial simulation time to represent the data begin date  
    TimeStepLocal = TimeStep
    IF (TimeStep%TrackTime) THEN
      TimeStepLocal%CurrentDateAndTime = IncrementTimeStamp(TimeStepLocal%CurrentDateAndTime,TimeStepLocal%DeltaT_InMinutes)
      UnitT                            = ''
    ELSE
      TimeStepLocal%CurrentTime        = TimeStepLocal%CurrentTime + TimeStepLocal%DeltaT
      UnitT                            = '('//TRIM(TimeStep%Unit)//')'
    END IF
    
    !Simulation time related data
    OutputData%NTimeSteps = NTIME
    OutputData%TimeStep   = TimeStepLocal
    
    !Areas
    ALLOCATE (OutputData%Areas(NRegion))
    OutputData%NAreas = NRegion
    OutputData%Areas  = RegionArea
    
    !Data for ASCII output
    ASSOCIATE (pASCIIOutput => OutputData%ASCIIOutput)
      pASCIIOutput%TitleLen           = TitleLen
      pASCIIOutput%NTitles            = NTitles
      ALLOCATE(pASCIIOutput%cTitles(NTitles)  ,  pASCIIOutput%lTitlePersist(NTitles))
      pASCIIOutput%cTitles(1)         = ArrangeText('IWFM ROOT ZONE PACKAGE ('//TRIM(cVersion)//')' , pASCIIOutput%TitleLen)
      pASCIIOutput%cTitles(2)         = ArrangeText('LAND AND WATER USE BUDGET IN '//VolumeUnitMarker//' FOR '//LocationNameMarker , pASCIIOutput%TitleLen)
      pASCIIOutput%cTitles(3)         = ArrangeText('SUBREGION AREA: '//AreaMarker//' '//AreaUnitMarker , pASCIIOutput%TitleLen)
      pASCIIOutput%cTitles(4)         = REPEAT('-',pASCIIOutput%TitleLen)
      pASCIIOutput%cTitles(5)         = REPEAT(' ',73)//'Agricultural Area'//REPEAT(' ',94)//'Urban Area'
      pASCIIOutput%cTitles(6)         = REPEAT(' ',17)//REPEAT('-',130)//REPEAT(' ',3)//REPEAT('-',78)
      pASCIIOutput%lTitlePersist(1:3) = .TRUE.
      pASCIIOutput%lTitlePersist(4:6) = .FALSE.
      pASCIIOutput%cFormatSpec        = ADJUSTL('(A16,1X,10(F12.1,1X),3X,6(F12.1,1X))')
      pASCIIOutput%NColumnHeaderLines = NColumnHeaderLines
    END ASSOCIATE 
    
    !Location names
    OutputData%NLocations = NRegion
    ALLOCATE (OutputData%cLocationNames(NRegion))
    OutputData%cLocationNames = cRegionNames  
        
    !Locations
    ALLOCATE (OutputData%Locations(1)                                                       , &
              OutputData%Locations(1)%cFullColumnHeaders(NLWUseBudColumns+1)                , &
              OutputData%Locations(1)%iDataColumnTypes(NLWUseBudColumns)                    , &
              OutputData%Locations(1)%iColWidth(NLWUseBudColumns+1)                         , &
              OutputData%Locations(1)%cColumnHeaders(NLWUseBudColumns+1,NColumnHeaderLines) , &
              OutputData%Locations(1)%cColumnHeadersFormatSpec(NColumnHeaderLines)          )
    ASSOCIATE (pLocation => OutputData%Locations(1))
      pLocation%NDataColumns           = NLWUseBudColumns
      pLocation%cFullColumnHeaders(1)  = 'Time'                               
      pLocation%cFullColumnHeaders(2:) = cLWUseBudgetColumnTitles
      pLocation%cFullColumnHeaders(2)  = TRIM(pLocation%cFullColumnHeaders(2))  // ' ('//AreaUnitMarker//')'    
      pLocation%cFullColumnHeaders(12) = TRIM(pLocation%cFullColumnHeaders(12)) // ' ('//AreaUnitMarker//')'    
      pLocation%iDataColumnTypes       = [AR                 ,&  !Ag area
                                          VR_lwu_PotCUAW     ,&  !Potential CUAW
                                          VR_lwu_AgSupplyReq ,&  !Ag supply req.
                                          VR_lwu_AgPump      ,&  !Pumping for ag
                                          VR_lwu_AgDiv       ,&  !Deliveries for ag
                                          VR_lwu_AgOthIn     ,&  !Ag inflow as surface runoff from upstream elements
                                          VR_lwu_AgShort     ,&  !Ag supply shortage
                                          VR                 ,&  !ETAW
                                          VR                 ,&  !ETP
                                          VR                 ,&  !ETOth
                                          AR                 ,&  !Urban area
                                          VR                 ,&  !Urban supply req.
                                          VR                 ,&  !Pumping for urban
                                          VR                 ,&  !Deliveries for urban
                                          VR                 ,&  !Urban inflow as surface runoff from upstream elements
                                          VR                 ]   !Urban supply shortage
      pLocation%iColWidth              = [17,12,14,(13,indxCol=1,8),12,14,(13,indxCol=1,4)]
      ASSOCIATE (pColumnHeaders => pLocation%cColumnHeaders           , &
                 pFormatSpecs   => pLocation%cColumnHeadersFormatSpec )
        Text                = ArrangeText(TRIM(UnitT),17)
        Text1               = '('//TRIM(AreaUnitMarker)//')'
        pColumnHeaders(:,1) = (/'                 ','            ','    Potential ',' Agricultural','             ','             ','  Inflow as  ','             ','             ','             ','      ET     ','            ','     Urban    ','             ','             ','  Inflow as  ','             '/)
        pColumnHeaders(:,2) = (/'      Time       ','        Area','      CUAW    ','    Supply   ','      Pumping','  Deliveries ',' Srfc. Runoff','     Shortage','             ','   Effective ','  from Other ','        Area','     Supply   ','      Pumping','  Deliveries ',' Srfc. Runoff','     Shortage'/)
        pColumnHeaders(:,3) = (/               Text,         Text1,'              ','  Requirement','        (-)  ','      (-)    ','     (-)     ','       (=)   ','       ETAW  ','    Precip   ','   Sources   ',         Text1,'   Requirement','        (-)  ','      (-)    ','     (-)     ','       (=)   '/)
        pColumnHeaders(:,4) = ''
        pFormatSpecs(1)     = '(A17,A12,A14,8A13,3X,A12,A14,4A13)'
        pFormatSpecs(2)     = '(A17,A12,A14,8A13,3X,A12,A14,4A13)'
        pFormatSpecs(3)     = '(A17,A12,A14,8A13,3X,A12,A14,4A13)'
        pFormatSpecs(4)     = '("'//REPEAT('-',TitleLen)//'",'//TRIM(IntToText(NLWUseBudColumns+1))//'A0)'
      END ASSOCIATE
    END ASSOCIATE
     
    !Data for DSS output  
    ASSOCIATE (pDSSOutput => OutputData%DSSOutput)
      ALLOCATE (pDSSOutput%cPathNames(NLWUseBudColumns*NRegion) , pDSSOutput%iDataTypes(NLWUseBudColumns))
      iCount = 1
      DO indxLocation=1,NRegion
        DO indxCol=1,NLWUseBudColumns
          pDSSOutput%cPathNames(iCount) = '/IWFM_L&W_USE_BUD/'                                           //  &  !A part
                                          TRIM(UpperCase(OutputData%cLocationNames(indxLocation)))//'/'  //  &  !B part
                                          TRIM(CParts(indxCol))//'/'                                     //  &  !C part
                                          '/'                                                            //  &  !D part
                                           TRIM(TimeStep%Unit)//'/'                                      //  &  !E part
                                          TRIM(FParts(indxCol))//'/'                                            !F part
          iCount = iCount+1
        END DO
      END DO
      pDSSOutput%iDataTypes = (/PER_AVER,(PER_CUM,indxCol=1,9),PER_AVER,(PER_CUM,indxCol=1,5)/)
    END ASSOCIATE
                                             
    !Instantiate the land and water use raw file
    CALL RawFile%New(cFileName,OutputData,iStat)
    
  END SUBROUTINE LWUseBudRawFile_New
  
  
  ! -------------------------------------------------------------
  ! --- NEW BINARY LAND AND WATER USE BUDGET FILE FOR POST-PROCESSING
  ! -------------------------------------------------------------
  SUBROUTINE AgLWUseBudRawFile_New(IsForInquiry,cFileName,TimeStep,NTIME,NRegion,RegionArea,cRegionNames,cDescriptor,cVersion,RawFile,iStat)
    LOGICAL,INTENT(IN)            :: IsForInquiry
    CHARACTER(LEN=*),INTENT(IN)   :: cFileName,cRegionNames(NRegion)
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    INTEGER,INTENT(IN)            :: NTIME,NRegion
    REAL(8),INTENT(IN)            :: RegionArea(NRegion)
    CHARACTER(LEN=*),INTENT(IN)   :: cDescriptor
    CHARACTER(LEN=*),INTENT(IN)   :: cVersion
    TYPE(BudgetType),INTENT(OUT)  :: RawFile
    INTEGER,INTENT(OUT)           :: iStat
    
    !Local variables
    TYPE(BudgetHeaderType) :: OutputData
    TYPE(TimeStepType)     :: TimeStepLocal
    INTEGER,PARAMETER      :: NTitles            = 4   , &
                              TitleLen           = 149 , &        
                              NColumnHeaderLines = 4   
    INTEGER                :: indxCol,indxLocation,iCount
    CHARACTER              :: UnitT*10,Text*17,Text1*13
    CHARACTER(LEN=6)       :: CParts(NAgLWUseBudColumns) = (/'AREA'   , &
                                                             'VOLUME' , &
                                                             'VOLUME' , &
                                                             'VOLUME' , &
                                                             'VOLUME' , &
                                                             'VOLUME' , &
                                                             'VOLUME' , &
                                                             'VOLUME' , &
                                                             'VOLUME' , &
                                                             'VOLUME' /)
    CHARACTER(LEN=10)      :: FParts(NAgLWUseBudColumns) = (/'AREA'         ,&
                                                             'POTNL_CUAW'   ,&
                                                             'SUP_REQ'      ,&    
                                                             'PUMPING'      ,&
                                                             'DELIVERY'     ,&
                                                             'SR_INFLOW'    ,&
                                                             'SHORTAGE'     ,&
                                                             'ETAW'         ,&
                                                             'EFF_PRECIP'   ,&
                                                             'ET_OTHER'     /)
    
    !Initialize
    iStat = 0
    
    !Instantiate the land and water use raw file for when it is opened for inquiry
    IF (IsForInquiry) THEN
        CALL RawFile%New(cFileName,iStat)
        RETURN
    END IF
    
    !Budget descriptor
    OutputData%cBudgetDescriptor = cDescriptor
    
    !Increment the initial simulation time to represent the data begin date  
    TimeStepLocal = TimeStep
    IF (TimeStep%TrackTime) THEN
      TimeStepLocal%CurrentDateAndTime = IncrementTimeStamp(TimeStepLocal%CurrentDateAndTime,TimeStepLocal%DeltaT_InMinutes)
      UnitT                            = ''
    ELSE
      TimeStepLocal%CurrentTime        = TimeStepLocal%CurrentTime + TimeStepLocal%DeltaT
      UnitT                            = '('//TRIM(TimeStep%Unit)//')'
    END IF
    
    !Simulation time related data
    OutputData%NTimeSteps = NTIME
    OutputData%TimeStep   = TimeStepLocal
    
    !Areas
    ALLOCATE (OutputData%Areas(NRegion))
    OutputData%NAreas = NRegion
    OutputData%Areas  = RegionArea
    
    !Data for ASCII output
    ASSOCIATE (pASCIIOutput => OutputData%ASCIIOutput)
      pASCIIOutput%TitleLen           = TitleLen
      pASCIIOutput%NTitles            = NTitles
        ALLOCATE(pASCIIOutput%cTitles(NTitles)  ,  pASCIIOutput%lTitlePersist(NTItles))
        pASCIIOutput%cTitles(1)         = ArrangeText('IWFM ROOT ZONE PACKAGE ('//TRIM(cVersion)//')' , pASCIIOutput%TitleLen)
        pASCIIOutput%cTitles(2)         = ArrangeText('LAND AND WATER USE BUDGET IN '//VolumeUnitMarker//' FOR '//LocationNameMarker , pASCIIOutput%TitleLen)
        pASCIIOutput%cTitles(3)         = ArrangeText('SUBREGION AREA: '//AreaMarker//' '//AreaUnitMarker , pASCIIOutput%TitleLen)
        pASCIIOutput%cTitles(4)         = REPEAT('-',pASCIIOutput%TitleLen)
        pASCIIOutput%lTitlePersist(1:3) = .TRUE.
        pASCIIOutput%lTItlePersist(4)   = .FALSE.
      pASCIIOutput%cFormatSpec        = ADJUSTL('(A16,1X,10(F12.1,1X))')
      pASCIIOutput%NColumnHeaderLines = NColumnHeaderLines
    END ASSOCIATE 
    
    !Location names
    OutputData%NLocations = NRegion
    ALLOCATE (OutputData%cLocationNames(NRegion))
    OutputData%cLocationNames = cRegionNames  
        
    !Locations
    ALLOCATE (OutputData%Locations(1)                                                         , &
              OutputData%Locations(1)%cFullColumnHeaders(NAgLWUseBudColumns+1)                , &
              OutputData%Locations(1)%iDataColumnTypes(NAgLWUseBudColumns)                    , &
              OutputData%Locations(1)%iColWidth(NAgLWUseBudColumns+1)                         , &
              OutputData%Locations(1)%cColumnHeaders(NAgLWUseBudColumns+1,NColumnHeaderLines) , &
              OutputData%Locations(1)%cColumnHeadersFormatSpec(NColumnHeaderLines)            )
    ASSOCIATE (pLocation => OutputData%Locations(1))
      pLocation%NDataColumns       = NAgLWUseBudColumns
      pLocation%cFullColumnHeaders = (/'Time'                           , &
                                       'Area ('//AreaUnitMarker//')'    , &
                                       'Potential CUAW'                 , &
                                       'Supply Requirement'             , &
                                       'Pumping'                        , &
                                       'Deliveries'                     , &
                                       'Inflow as Surface Runoff'       , &
                                       'Shortage'                       , &
                                       'ETAW'                           , &
                                       'Effective Precipitation'        , &
                                       'ET from Other Sources'          /)
      pLocation%iDataColumnTypes  = (/AR                 ,&      !Ag area
                                      VR_lwu_PotCUAW     ,&      !Potential CUAW
                                      VR_lwu_AgSupplyReq ,&      !Ag supply req.
                                      VR_lwu_AgPump      ,&      !Pumping for ag
                                      VR_lwu_AgDiv       ,&      !Deliveries for ag
                                      VR_lwu_AgOthIn     ,&      !Ag inflow as surface runoff from upstream elements
                                      VR_lwu_AgShort     ,&      !Ag supply shortage
                                      VR                 ,&      !ETAW
                                      VR                 ,&      !ETP
                                      VR                 /)      !ETOth
      pLocation%iColWidth       = (/17,12,14,(13,indxCol=1,7)/)
      ASSOCIATE (pColumnHeaders => pLocation%cColumnHeaders           , &
                 pFormatSpecs   => pLocation%cColumnHeadersFormatSpec )
        Text                = ArrangeText(TRIM(UnitT),17)
        Text1               = '('//TRIM(AreaUnitMarker)//')'
        pColumnHeaders(:,1) = (/'                 ','            ','    Potential ',' Agricultural','             ','             ','  Inflow as  ','             ','             ','             ','      ET     '/)
        pColumnHeaders(:,2) = (/'      Time       ','        Area','      CUAW    ','    Supply   ','      Pumping',' Deliveries  ',' Srfc. Runoff','     Shortage','             ','   Effective ','  from Other '/)
        pColumnHeaders(:,3) = (/               Text,         Text1,'              ','  Requirement','        (-)  ','     (-)     ','     (-)     ','       (=)   ','       ETAW  ','    Precip   ','    Sources  '/)
        pColumnHeaders(:,4) = ''
        pFormatSpecs(1)     = '(A17,A12,A14,8A13)'
        pFormatSpecs(2)     = '(A17,A12,A14,8A13)'
        pFormatSpecs(3)     = '(A17,A12,A14,8A13)'
        pFormatSpecs(4)     = '("'//REPEAT('-',TitleLen)//'",'//TRIM(IntToText(NAgLWUseBudColumns+1))//'A0)'
      END ASSOCIATE
    END ASSOCIATE
     
    !Data for DSS output  
    ASSOCIATE (pDSSOutput => OutputData%DSSOutput)
      ALLOCATE (pDSSOutput%cPathNames(NAgLWUseBudColumns*NRegion) , pDSSOutput%iDataTypes(NAgLWUseBudColumns))
      iCount = 1
      DO indxLocation=1,NRegion
        DO indxCol=1,NAgLWUseBudColumns
          pDSSOutput%cPathNames(iCount) = '/IWFM_L&W_USE_BUD/'                                           //  &  !A part
                                          TRIM(UpperCase(OutputData%cLocationNames(indxLocation)))//'/'  //  &  !B part
                                          TRIM(CParts(indxCol))//'/'                                     //  &  !C part
                                          '/'                                                            //  &  !D part
                                           TRIM(TimeStep%Unit)//'/'                                      //  &  !E part
                                          TRIM(FParts(indxCol))//'/'                                            !F part
          iCount = iCount+1
        END DO
      END DO
      pDSSOutput%iDataTypes = (/PER_AVER,(PER_CUM,indxCol=1,9)/)
    END ASSOCIATE
                                             
    !Instantiate the land and water use raw file
    CALL RawFile%New(cFileName,OutputData,iStat)
    
  END SUBROUTINE AgLWUseBudRawFile_New

  
  
  ! -------------------------------------------------------------
  ! --- NEW BINARY ROOT ZONE BUDGET FILE FOR POST-PROCESSING
  ! -------------------------------------------------------------
  SUBROUTINE RootZoneBudRawFile_New(IsForInquiry,cFileName,TimeStep,NTIME,NRegion,RegionArea,cRegionNames,cDescriptor,cVersion,RawFile,iStat)
    LOGICAL,INTENT(IN)            :: IsForInquiry
    CHARACTER(LEN=*),INTENT(IN)   :: cFileName,cRegionNames(NRegion)
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    INTEGER,INTENT(IN)            :: NTIME,NRegion
    REAL(8),INTENT(IN)            :: RegionArea(NRegion)
    CHARACTER(LEN=*),INTENT(IN)   :: cDescriptor
    CHARACTER(LEN=*),INTENT(IN)   :: cVersion
    TYPE(BudgetType),INTENT(OUT)  :: RawFile
    INTEGER,INTENT(OUT)           :: iStat

    !Local variables
    TYPE(BudgetHeaderType) :: OutputData
    TYPE(TimeStepType)     :: TimeStepLocal
    INTEGER,PARAMETER      :: NTitles            = 6   , &
                              TitleLen           = 713 , &        
                              NColumnHeaderLines = 4   
    INTEGER                :: indxCol,indxLocation,iCount
    CHARACTER              :: UnitT*10,Text*17,Text1*13
    CHARACTER(LEN=6)       :: CParts(NRootZoneBudColumns) = (/'AREA'   , &
                                                              'VOLUME' , &
                                                              'VOLUME' , &
                                                              'VOLUME' , &
                                                              'VOLUME' , &
                                                              'VOLUME' , &
                                                              'VOLUME' , &
                                                              'VOLUME' , &
                                                              'VOLUME' , &
                                                              'VOLUME' , &
                                                              'VOLUME' , &
                                                              'VOLUME' , &
                                                              'VOLUME' , &
                                                              'VOLUME' , &
                                                              'VOLUME' , &
                                                              'VOLUME' , &
                                                              'VOLUME' , &
                                                              'AREA'   , &
                                                              'VOLUME' , &
                                                              'VOLUME' , &
                                                              'VOLUME' , &
                                                              'VOLUME' , &
                                                              'VOLUME' , &
                                                              'VOLUME' , &
                                                              'VOLUME' , &
                                                              'VOLUME' , &
                                                              'VOLUME' , &
                                                              'VOLUME' , &
                                                              'VOLUME' , &
                                                              'VOLUME' , &
                                                              'VOLUME' , &
                                                              'VOLUME' , &
                                                              'VOLUME' , &
                                                              'VOLUME' , &
                                                              'AREA'   , &
                                                              'VOLUME' , &
                                                              'VOLUME' , &
                                                              'VOLUME' , &
                                                              'VOLUME' , &
                                                              'VOLUME' , &
                                                              'VOLUME' , &
                                                              'VOLUME' , &
                                                              'VOLUME' , &
                                                              'VOLUME' , &
                                                              'VOLUME' , &
                                                              'VOLUME'/)
    CHARACTER(LEN=15)      :: FParts(NRootZoneBudColumns) = (/'AG_AREA'           ,&
                                                              'AG_POT_ET'         ,&   
                                                              'AG_PRECIP'         ,&   
                                                              'AG_RUNOFF'         ,&   
                                                              'AG_PRM_H2O'        ,&
                                                              'AG_SR_INFLOW'      ,&
                                                              'AG_RE-USE'         ,&   
                                                              'AG_NT_RTRN_FLOW'   ,&   
                                                              'AG_BEGIN_STOR'     ,&   
                                                              'AG_GAIN_EXP'       ,&   
                                                              'AG_INFILTR'        ,& 
                                                              'AG_OTHER_INFLOW'   ,&
                                                              'AG_DRAIN'          ,&  
                                                              'AG_ET'             ,&   
                                                              'AG_PERC'           ,&   
                                                              'AG_END_STOR'       ,&  
                                                              'AG_DISCREPANCY'    ,& 
                                                              'URB_AREA'          ,&  
                                                              'URB_POT_ET'        ,&  
                                                              'URB_PRECIP'        ,&  
                                                              'URB_RUNOFF'        ,&  
                                                              'URB_PRM_H2O'       ,& 
                                                              'URB_SR_INFLOW'     ,&
                                                              'URB_RE-USE'        ,&     
                                                              'URB_NT_RTRN_FLOW'  ,&     
                                                              'URB_BEGIN_STOR'    ,&     
                                                              'URB_GAIN_EXP'      ,&     
                                                              'URB_INFILTR'       ,&     
                                                              'URB_OTHER_INFLOW'  ,&
                                                              'URB_ET'            ,&     
                                                              'URB_PERC'          ,&     
                                                              'URB_END_STOR'      ,& 
                                                              'URB_DISCREPANCY'   ,&    
                                                              'NRV_AREA'          ,&  
                                                              'NRV_POT_ET'        ,&
                                                              'NRV_PRECIP'        ,&
                                                              'NRV_SR_INFLOW'     ,&  
                                                              'NRV_RUNOFF'        ,&  
                                                              'NRV_BEGIN_STOR'    ,&     
                                                              'NRV_GAIN_EXP'      ,&     
                                                              'NRV_INFILTR'       ,&     
                                                              'NRV_OTHER_INFLOW'  ,&
                                                              'NRV_ET'            ,&     
                                                              'NRV_PERC'          ,&     
                                                              'NRV_END_STOR'      ,&
                                                              'NRV_DISCREPANCY'   /) 
    
    !Initailize
    iStat = 0
                                                  
    !Instantiate the root zone budget raw file for when it is opened for inquiry
    IF (IsForInquiry) THEN
        CALL RawFile%New(cFileName,iStat)
        RETURN
    END IF
    
    !Budget descriptor
    OutputData%cBudgetDescriptor = cDescriptor
    
    !Increment the initial simulation time to represent the data begin date  
    TimeStepLocal = TimeStep
    IF (TimeStep%TrackTime) THEN
      TimeStepLocal%CurrentDateAndTime = IncrementTimeStamp(TimeStepLocal%CurrentDateAndTime,TimeStepLocal%DeltaT_InMinutes)
      UnitT                            = ''
    ELSE
      TimeStepLocal%CurrentTime        = TimeStepLocal%CurrentTime + TimeStepLocal%DeltaT
      UnitT                            = '('//TRIM(TimeStep%Unit)//')'
    END IF

    !Simulation time related data
    OutputData%NTimeSteps = NTIME
    OutputData%TimeStep   = TimeStepLocal
    
    !Areas
    ALLOCATE (OutputData%Areas(NRegion))
    OutputData%NAreas = NRegion
    OutputData%Areas  = RegionArea
    
    !Data for ASCII output
    ASSOCIATE (pASCIIOutput => OutputData%ASCIIOutput)
      pASCIIOutput%TitleLen = TitleLen
      pASCIIOutput%NTitles  = NTitles
        ALLOCATE(pASCIIOutput%cTitles(NTitles)  ,  pASCIIOutput%lTitlePersist(NTitles))
        pASCIIOutput%cTitles(1)         = ArrangeText('IWFM ROOT ZONE PACKAGE ('//TRIM(cVersion)//')' , pASCIIOutput%TitleLen)
        pASCIIOutput%cTitles(2)         = ArrangeText('ROOT ZONE MOISTURE BUDGET IN '//VolumeUnitMarker//' FOR '//LocationNameMarker , pASCIIOutput%TitleLen)
        pASCIIOutput%cTitles(3)         = ArrangeText('SUBREGION AREA: '//AreaMarker//' '//AreaUnitMarker , pASCIIOutput%TitleLen)
        pASCIIOutput%cTitles(4)         = REPEAT('-',pASCIIOutput%TitleLen)
        pASCIIOutput%cTitles(5)         = REPEAT(' ',135)//'Agricultural Area'//REPEAT(' ',237)//'Urban Area'//REPEAT(' ',199)//'Native & Riparian Vegetation Area'
        pASCIIOutput%cTitles(6)         = REPEAT(' ',17)//REPEAT('-',255)//REPEAT(' ',3)//REPEAT('-',240)//REPEAT(' ',3)//REPEAT('-',195)
        pASCIIOutput%lTitlePersist(1:3) = .TRUE.
        pASCIIOutput%lTitlePersist(4:6) = .FALSE.
      pASCIIOutput%cFormatSpec        = ADJUSTL('(A16,1X,17(F14.1,1X),3X,16(F14.1,1X),3X,13(F14.1,1X))')
      pASCIIOutput%NColumnHeaderLines = NColumnHeaderLines
    END ASSOCIATE 
                                                     
    !Location names
    OutputData%NLocations = NRegion
    ALLOCATE (OutputData%cLocationNames(NRegion))
    OutputData%cLocationNames = cRegionNames  
        
    !Locations
    ALLOCATE (OutputData%Locations(1)                                                          , &
              OutputData%Locations(1)%cFullColumnHeaders(NRootZoneBudColumns+1)                , &
              OutputData%Locations(1)%iDataColumnTypes(NRootZoneBudColumns)                    , &
              OutputData%Locations(1)%iColWidth(NRootZoneBudColumns+1)                         , &
              OutputData%Locations(1)%cColumnHeaders(NRootZoneBudColumns+1,NColumnHeaderLines) , &
              OutputData%Locations(1)%cColumnHeadersFormatSpec(NColumnHeaderLines)             )
    ASSOCIATE (pLocation => OutputData%Locations(1))
      pLocation%NDataColumns       = NRootZoneBudColumns
      pLocation%cFullColumnHeaders = (/'Time'                                                   , &
                                       'Ag. Area ('//AreaUnitMarker//')'                        , &
                                       'Ag. Potential ET'                                       , &
                                       'Ag. Precipitation'                                      , &
                                       'Ag. Runoff'                                             , &
                                       'Ag. Prime Applied Water'                                , &
                                       'Ag. Inflow as Surface Runoff'                           , &
                                       'Ag. Reused Water'                                       , &
                                       'Ag. Net Return Flow'                                    , &
                                       'Ag. Beginning Storage (+)'                              , &
                                       'Ag. Net Gain from Land Expansion (+)'                   , &
                                       'Ag. Infiltration (+)'                                   , &
                                       'Ag. Other Inflow (+)'                                   , &
                                       'Ag. Pond Drain (-)'                                     , &
                                       'Ag. Actual ET (-)'                                      , &
                                       'Ag. Percolation (-)'                                    , &
                                       'Ag. Ending Storage (-)'                                 , &
                                       'Ag. Discrepancy (=)'                                    , &
                                       'Urban Area ('//AreaUnitMarker//')'                      , &
                                       'Urban Potential ET'                                     , &
                                       'Urban Precipitation'                                    , &
                                       'Urban Runoff'                                           , &
                                       'Urban Prime Applied Water'                              , &
                                       'Urban Inflow as Surface Runoff'                         , &
                                       'Urban Reused Water'                                     , &
                                       'Urban Net Return Flow'                                  , &
                                       'Urban Beginning Storage (+)'                            , &
                                       'Urban Net Gain from Land Expansion (+)'                 , &
                                       'Urban Infiltration (+)'                                 , &
                                       'Urban Other Inflow (+)'                                 , &
                                       'Urban Actual ET (-)'                                    , &
                                       'Urban Percolation (-)'                                  , &
                                       'Urban Ending Storage (-)'                               , &
                                       'Urban Discrepancy (=)'                                  , &
                                       'Native&Riparian Veg. Area ('//AreaUnitMarker//')'       , &
                                       'Native&Riparian Veg. Potential ET'                      , &
                                       'Native&Riparian Veg. Precipitation'                     , &
                                       'Native&Riparian Veg. Inflow as Surface Runoff'          , &
                                       'Native&Riparian Veg. Runoff'                            , &
                                       'Native&Riparian Veg. Beginning Storage (+)'             , &
                                       'Native&Riparian Veg. Net Gain from Land Expansion (+)'  , &
                                       'Native&Riparian Veg. Infiltration (+)'                  , &
                                       'Native&Riparian Veg. Other Inflow (+)'                  , &
                                       'Native&Riparian Veg. Actual ET (-)'                     , &
                                       'Native&Riparian Veg. Percolation (-)'                   , &
                                       'Native&Riparian Veg. Ending Storage (-)'                , &
                                       'Native&Riparian Veg. Discrepancy (=)'                   /)
      pLocation%iDataColumnTypes  = (/AR ,&  !Ag area
                                      VR ,&  !Ag potential ET
                                      VR ,&  !Ag precipitation
                                      VR ,&  !Ag runoff
                                      VR ,&  !Ag prime applied water
                                      VR ,&  !Ag applied water from upstream element surface runoff
                                      VR ,&  !Ag re-used water
                                      VR ,&  !Ag return flow
                                      VLB,&  !Ag beginning storage
                                      VR ,&  !Ag net gain from land expansion
                                      VR ,&  !Ag infiltration
                                      VR ,&  !Ag generic inflow
                                      VR ,&  !Ag pond drain
                                      VR ,&  !Ag actual ET
                                      VR ,&  !Ag perc
                                      VLE,&  !Ag ending storage
                                      VR ,&  !Ag discrepancy
                                      AR ,&  !Urban area
                                      VR ,&  !Urban potential ET
                                      VR ,&  !Urban precipitation
                                      VR ,&  !Urban runoff
                                      VR ,&  !Urban prime applied water
                                      VR ,&  !Urban applied water due to upstream element surface runoff
                                      VR ,&  !Urban re-used water
                                      VR ,&  !Urban return flow
                                      VLB,&  !Urban beginning storage
                                      VR ,&  !Urban net gain from land expansion
                                      VR ,&  !Urban infiltration
                                      VR ,&  !Urban generic inflow
                                      VR ,&  !Urban actual ET
                                      VR ,&  !Urban perc
                                      VLE,&  !Urban ending storage
                                      VR ,&  !Urban discrepancy
                                      AR ,&  !NV&RV area
                                      VR ,&  !NV&RV potential ET
                                      VR ,&  !NV&RV precipitation
                                      VR ,&  !NV&RV surface runoff from upstream elements/subregions
                                      VR ,&  !NV&RV runoff
                                      VLB,&  !NV&RV beginning storage
                                      VR ,&  !NV&RV net gain from land expansion
                                      VR ,&  !NV&RV infiltration
                                      VR ,&  !NV&RV generic inflow
                                      VR ,&  !NV&RV actual ET
                                      VR ,&  !NV&RV perc
                                      VLE,&  !NV&RV ending storage
                                      VR /)  !NV&RV discrepancy
      pLocation%iColWidth       = (/17,14,15,16,(15,indxCol=1,14),14,15,16,(15,indxCol=1,13),14,15,16,(15,indxCol=1,10)/)
      ASSOCIATE (pColumnHeaders => pLocation%cColumnHeaders           , &
                 pFormatSpecs   => pLocation%cColumnHeadersFormatSpec )
        Text                = ArrangeText(TRIM(UnitT),17)
        Text1               = '('//TRIM(AreaUnitMarker)//')'
        pColumnHeaders(:,1) = (/'                 ','              ','               ','                ','               ','       Prime   ','   Inflow as   ','         Reused','          Net  ','     Beginning ',' Net Gain from ','               ','        Other  ','          Pond ','         Actual','               ','        Ending ','               ','              ','               ','                ','               ','       Prime   ','   Inflow as   ','         Reused','          Net  ','     Beginning ',' Net Gain from ','               ','        Other  ','         Actual','               ','        Ending ','               ','              ','               ','                ','   Inflow as   ','               ','     Beginning ',' Net Gain from ','               ','        Other  ','         Actual','               ','        Ending ','               '/)
        pColumnHeaders(:,2) = (/'      Time       ','          Area','      Potential','   Precipitation','         Runoff','      Applied  ',' Surface Runoff','         Water ','         Return','      Storage  ',' Land Expansion','   Infiltration','        Inflow ','          Drain','           ET  ','    Percolation','        Storage','    Discrepancy','          Area','      Potential','   Precipitation','         Runoff','      Applied  ',' Surface Runoff','         Water ','         Return','      Storage  ',' Land Expansion','   Infiltration','        Inflow ','           ET  ','    Percolation','        Storage','    Discrepancy','          Area','      Potential','  Precipitation ',' Surface Runoff','        Runoff ','      Storage  ',' Land Expansion','   Infiltration','        Inflow ','           ET  ','    Percolation','        Storage','    Discrepancy'/)
        pColumnHeaders(:,3) = (/               Text,           Text1,'         ET    ','                ','               ','       Water   ','               ','               ','          Flow ','        (+)    ','       (+)     ','        (+)    ','          (+)  ','           (-) ','           (-) ','       (-)     ','          (-)  ','        (=)    ',           Text1,'         ET    ','                ','               ','       Water   ','               ','               ','          Flow ','        (+)    ','       (+)     ','        (+)    ','          (+)  ','           (-) ','       (-)     ','          (-)  ','        (=)    ',           Text1,'         ET    ','                ','               ','               ','        (+)    ','       (+)     ','        (+)    ','          (+)  ','           (-) ','       (-)     ','          (-)  ','        (=)    '/)
        pColumnHeaders(:,4) = ''
        pFormatSpecs(1)     = '(A17,A14,A15,A16,14A15,3X,A14,A15,A16,13A15,3X,A14,A15,A16,10A15)'
        pFormatSpecs(2)     = '(A17,A14,A15,A16,14A15,3X,A14,A15,A16,13A15,3X,A14,A15,A16,10A15)'
        pFormatSpecs(3)     = '(A17,A14,A15,A16,14A15,3X,A14,A15,A16,13A15,3X,A14,A15,A16,10A15)'
        pFormatSpecs(4)     = '('//TRIM(IntToText(TitleLen))//'(1H-),'//TRIM(IntToText(NRootZoneBudColumns+1))//'A0)'
      END ASSOCIATE
    END ASSOCIATE

    !Data for DSS output  
    ASSOCIATE (pDSSOutput => OutputData%DSSOutput)
      ALLOCATE (pDSSOutput%cPathNames(NRootZoneBudColumns*NRegion) , pDSSOutput%iDataTypes(NRootZoneBudColumns))
      iCount = 1
      DO indxLocation=1,NRegion
        DO indxCol=1,NRootZoneBudColumns
          pDSSOutput%cPathNames(iCount) = '/IWFM_ROOTZN_BUD/'                                            //  &  !A part
                                          TRIM(UpperCase(OutputData%cLocationNames(indxLocation)))//'/'  //  &  !B part
                                          TRIM(CParts(indxCol))//'/'                                     //  &  !C part
                                          '/'                                                            //  &  !D part
                                           TRIM(TimeStep%Unit)//'/'                                      //  &  !E part
                                          TRIM(FParts(indxCol))//'/'                                            !F part
          iCount = iCount+1
        END DO
      END DO
      pDSSOutput%iDataTypes = (/PER_AVER,(PER_CUM,indxCol=1,16),PER_AVER,(PER_CUM,indxCol=1,15),PER_AVER,(PER_CUM,indxCol=1,12)/)
    END ASSOCIATE
                                             
    !Instantiate the root zone budget file
    CALL RawFile%New(cFileName,OutputData,iStat)
    
    !Free memory
    CALL OutputData%Kill()
    
  END SUBROUTINE RootZoneBudRawFile_New
  
  
  ! -------------------------------------------------------------
  ! --- NEW BINARY ROOT ZONE BUDGET FILE FOR POST-PROCESSING OF AG LANDS
  ! -------------------------------------------------------------
  SUBROUTINE AgRootZoneBudRawFile_New(IsForInquiry,cFileName,TimeStep,NTIME,NRegion,RegionArea,cRegionNames,cDescriptor,cVersion,RawFile,iStat)
    LOGICAL,INTENT(IN)            :: IsForInquiry
    CHARACTER(LEN=*),INTENT(IN)   :: cFileName,cRegionNames(NRegion)
    TYPE(TimeStepType),INTENT(IN) :: TimeStep
    INTEGER,INTENT(IN)            :: NTIME,NRegion
    REAL(8),INTENT(IN)            :: RegionArea(NRegion)
    CHARACTER(LEN=*),INTENT(IN)   :: cDescriptor
    CHARACTER(LEN=*),INTENT(IN)   :: cVersion
    TYPE(BudgetType),INTENT(OUT)  :: RawFile
    INTEGER,INTENT(OUT)           :: iStat

    !Local variables
    TYPE(BudgetHeaderType) :: OutputData
    TYPE(TimeStepType)     :: TimeStepLocal
    INTEGER,PARAMETER      :: NTitles            = 4   , &
                              TitleLen           = 274 , &        
                              NColumnHeaderLines = 4   
    INTEGER                :: indxCol,indxLocation,iCount
    CHARACTER              :: UnitT*10,Text*17,Text1*13
    CHARACTER(LEN=6)       :: CParts(NAgRootZoneBudColumns) = (/'AREA'   , &
                                                                'VOLUME' , &
                                                                'VOLUME' , &
                                                                'VOLUME' , &
                                                                'VOLUME' , &
                                                                'VOLUME' , &
                                                                'VOLUME' , &
                                                                'VOLUME' , &
                                                                'VOLUME' , &
                                                                'VOLUME' , &
                                                                'VOLUME' , &
                                                                'VOLUME' , &
                                                                'VOLUME' , &
                                                                'VOLUME' , &
                                                                'VOLUME' , &
                                                                'VOLUME' , &
                                                                'VOLUME'/)
    CHARACTER(LEN=11)      :: FParts(NAgRootZoneBudColumns) = (/'AREA'           ,&
                                                                'POT_ET'         ,&   
                                                                'PRECIP'         ,&   
                                                                'RUNOFF'         ,&   
                                                                'PRM_H2O'        ,&
                                                                'SR_INFLOW'      ,&
                                                                'RE-USE'         ,&   
                                                                'NET_RTRN_FLOW'  ,&   
                                                                'BEGIN_STOR'     ,&   
                                                                'GAIN_EXP'       ,&   
                                                                'INFILTR'        ,&
                                                                'OTHER_INFLOW'   ,&
                                                                'DRAIN'          ,&  
                                                                'ET'             ,&   
                                                                'PERC'           ,&   
                                                                'END_STOR'       ,&  
                                                                'DISCREPANCY'   /) 
    
    !Initialize
    iStat = 0
    
    !Instantiate the root zone budget raw file for when it is opened for inquiry
    IF (IsForInquiry) THEN
        CALL RawFile%New(cFileName,iStat)
        RETURN
    END IF

    !Budget descriptor
    OutputData%cBudgetDescriptor = cDescriptor
    
    !Increment the initial simulation time to represent the data begin date  
    TimeStepLocal = TimeStep
    IF (TimeStep%TrackTime) THEN
      TimeStepLocal%CurrentDateAndTime = IncrementTimeStamp(TimeStepLocal%CurrentDateAndTime,TimeStepLocal%DeltaT_InMinutes)
      UnitT                            = ''
    ELSE
      TimeStepLocal%CurrentTime        = TimeStepLocal%CurrentTime + TimeStepLocal%DeltaT
      UnitT                            = '('//TRIM(TimeStep%Unit)//')'
    END IF

    !Simulation time related data
    OutputData%NTimeSteps = NTIME
    OutputData%TimeStep   = TimeStepLocal
    
    !Areas
    ALLOCATE (OutputData%Areas(NRegion))
    OutputData%NAreas = NRegion
    OutputData%Areas  = RegionArea
    
    !Data for ASCII output
    ASSOCIATE (pASCIIOutput => OutputData%ASCIIOutput)
      pASCIIOutput%TitleLen = TitleLen
      pASCIIOutput%NTitles  = NTitles
        ALLOCATE(pASCIIOutput%cTitles(NTitles)  ,  pASCIIOutput%lTitlePersist(NTitles))
        pASCIIOutput%cTitles(1)         = ArrangeText('IWFM ROOT ZONE PACKAGE ('//TRIM(cVersion)//')' , pASCIIOutput%TitleLen)
        pASCIIOutput%cTitles(2)         = ArrangeText('ROOT ZONE MOISTURE BUDGET IN '//VolumeUnitMarker//' FOR '//LocationNameMarker , pASCIIOutput%TitleLen)
        pASCIIOutput%cTitles(3)         = ArrangeText('SUBREGION AREA: '//AreaMarker//' '//AreaUnitMarker , pASCIIOutput%TitleLen)
        pASCIIOutput%cTitles(4)         = REPEAT('-',pASCIIOutput%TitleLen)
        pASCIIOutput%lTitlePersist(1:3) = .TRUE.
        pASCIIOutput%lTitlePersist(4)   = .FALSE.
      pASCIIOutput%cFormatSpec        = ADJUSTL('(A16,1X,17(F14.1,1X))')
      pASCIIOutput%NColumnHeaderLines = NColumnHeaderLines
    END ASSOCIATE 
                                                     
    !Location names
    OutputData%NLocations = NRegion
    ALLOCATE (OutputData%cLocationNames(NRegion))
    OutputData%cLocationNames = cRegionNames  
        
    !Locations
    ALLOCATE (OutputData%Locations(1)                                                            , &
              OutputData%Locations(1)%cFullColumnHeaders(NAgRootZoneBudColumns+1)                , &
              OutputData%Locations(1)%iDataColumnTypes(NAgRootZoneBudColumns)                    , &
              OutputData%Locations(1)%iColWidth(NAgRootZoneBudColumns+1)                         , &
              OutputData%Locations(1)%cColumnHeaders(NAgRootZoneBudColumns+1,NColumnHeaderLines) , &
              OutputData%Locations(1)%cColumnHeadersFormatSpec(NColumnHeaderLines)               )
    ASSOCIATE (pLocation => OutputData%Locations(1))
      pLocation%NDataColumns       = NAgRootZoneBudColumns
      pLocation%cFullColumnHeaders = (/'Time'                                               , &
                                       'Area ('//AreaUnitMarker//')'                        , &
                                       'Potential ET'                                       , &
                                       'Precipitation'                                      , &
                                       'Runoff'                                             , &
                                       'Prime Applied Water'                                , &
                                       'Inflow as Surface Runoff'                           , &
                                       'Reused Water'                                       , &
                                       'Net Return Flow'                                    , &
                                       'Beginning Storage (+)'                              , &
                                       'Net Gain from Land Expansion (+)'                   , &
                                       'Infiltration (+)'                                   , &
                                       'Other Inflow (+)'                                   , &
                                       'Pond Drain (-)'                                     , &
                                       'Actual ET (-)'                                      , &
                                       'Percolation (-)'                                    , &
                                       'Ending Storage (-)'                                 , &
                                       'Discrepancy (=)'                                   /)
      pLocation%iDataColumnTypes  = (/AR ,&  !Ag area
                                      VR ,&  !Ag potential ET
                                      VR ,&  !Ag precipitation
                                      VR ,&  !Ag runoff
                                      VR ,&  !Ag prime applied water
                                      VR ,&  !Ag applied water from upstream element surface runoff
                                      VR ,&  !Ag re-used water
                                      VR ,&  !Ag return flow
                                      VLB,&  !Ag beginning storage
                                      VR ,&  !Ag net gain from land expansion
                                      VR ,&  !Ag infiltration
                                      VR ,&  !Ag generic inflow
                                      VR ,&  !Ag pond drain
                                      VR ,&  !Ag actual ET
                                      VR ,&  !Ag perc
                                      VLE,&  !Ag ending storage
                                      VR /)  !Ag discrepancy
      pLocation%iColWidth       = (/17,14,15,16,(15,indxCol=1,14)/)
      ASSOCIATE (pColumnHeaders => pLocation%cColumnHeaders           , &
                 pFormatSpecs   => pLocation%cColumnHeadersFormatSpec )
        Text                = ArrangeText(TRIM(UnitT),17)
        Text1               = '('//TRIM(AreaUnitMarker)//')'
        pColumnHeaders(:,1) = (/'                 ','              ','               ','                ','               ','       Prime   ','   Inflow as   ','         Reused','          Net  ','     Beginning ',' Net Gain from ','               ','        Other  ','          Pond ','         Actual','               ','        Ending ','               '/)
        pColumnHeaders(:,2) = (/'      Time       ','          Area','      Potential','   Precipitation','         Runoff','      Applied  ',' Surface Runoff','         Water ','         Return','      Storage  ',' Land Expansion','   Infiltration','        Inflow ','          Drain','           ET  ','    Percolation','        Storage','    Discrepancy'/)
        pColumnHeaders(:,3) = (/               Text,           Text1,'         ET    ','                ','               ','       Water   ','               ','               ','          Flow ','        (+)    ','       (+)     ','        (+)    ','          (+)  ','           (-) ','           (-) ','       (-)     ','          (-)  ','        (=)    '/)
        pColumnHeaders(:,4) = ''
        pFormatSpecs(1)     = '(A17,A14,A15,A16,14A15)'
        pFormatSpecs(2)     = '(A17,A14,A15,A16,14A15)'
        pFormatSpecs(3)     = '(A17,A14,A15,A16,14A15)'
        pFormatSpecs(4)     = '('//TRIM(IntToText(TitleLen))//'(1H-),'//TRIM(IntToText(NAgRootZoneBudColumns+1))//'A0)'
      END ASSOCIATE
    END ASSOCIATE

    !Data for DSS output  
    ASSOCIATE (pDSSOutput => OutputData%DSSOutput)
      ALLOCATE (pDSSOutput%cPathNames(NAgRootZoneBudColumns*NRegion) , pDSSOutput%iDataTypes(NAgRootZoneBudColumns))
      iCount = 1
      DO indxLocation=1,NRegion
        DO indxCol=1,NAgRootZoneBudColumns
          pDSSOutput%cPathNames(iCount) = '/IWFM_ROOTZN_BUD/'                                            //  &  !A part
                                          TRIM(UpperCase(OutputData%cLocationNames(indxLocation)))//'/'  //  &  !B part
                                          TRIM(CParts(indxCol))//'/'                                     //  &  !C part
                                          '/'                                                            //  &  !D part
                                           TRIM(TimeStep%Unit)//'/'                                      //  &  !E part
                                          TRIM(FParts(indxCol))//'/'                                            !F part
          iCount = iCount+1
        END DO
      END DO
      pDSSOutput%iDataTypes = (/PER_AVER,(PER_CUM,indxCol=1,16)/)
    END ASSOCIATE
                                             
    !Instantiate the root zone budget raw file
    CALL RawFile%New(cFileName,OutputData,iStat)
    
  END SUBROUTINE AgRootZoneBudRawFile_New


  
END MODULE