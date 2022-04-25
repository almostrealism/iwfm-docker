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
MODULE Util_RootZone_v40
  USE GeneralUtilities       , ONLY: ArrangeText                          , &
                                     IntToText                            , &
                                     UpperCase                            
  USE TimeSeriesUtilities    , ONLY: TimeStepType                         , &
                                     IncrementTimeStamp                   
  USE Package_Budget         , ONLY: BudgetType                           , &
                                     BudgetHeaderType                     , &
                                     f_cVolumeUnitMarker                  , &
                                     f_cAreaUnitMarker                    , &
                                     f_cLocationNameMarker                , &
                                     f_cAreaMarker                        , &
                                     f_iAR                                , &
                                     f_iVR                                , &
                                     f_iVLB                               , &
                                     f_iVLE                               , &
                                     f_iVR_lwu_PotCUAW                    , &
                                     f_iVR_lwu_AgSupplyReq                , &
                                     f_iVR_lwu_AgPump                     , &
                                     f_iVR_lwu_AgDiv                      , &
                                     f_iVR_lwu_AgOthIn                    , &
                                     f_iVR_lwu_AgShort                    , &
                                     f_iPER_AVER                          , &
                                     f_iPER_CUM                              
  IMPLICIT NONE
  
  
  ! -------------------------------------------------------------
  ! --- PUBLIC ENTITIES
  ! -------------------------------------------------------------
  PRIVATE
  PUBLIC :: AgLWUseBudRawFile_New          , &
            AgRootZoneBudRawFile_New       , &
            LWUseBudRawFile_New            , &
            RootZoneBudRawFile_New         , &
            f_cLWUseBudgetColumnTitles     , &
            f_cRootZoneBudgetColumnTitles  , &
            f_iNLWUseBudColumns            , &
            f_iNRootZoneBudColumns         , &
            f_iNAgLWUseBudColumns          , &
            f_iNAgRootZoneBudColumns       
            
  
  ! -------------------------------------------------------------
  ! --- MISC. ENTITIES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER            :: f_iNLWUseBudColumns                                   = 16  , &
                                  f_iNRootZoneBudColumns                                = 46  , & 
                                  f_iNAgLWUseBudColumns                                 = 10  , &
                                  f_iNAgRootZoneBudColumns                              = 17  
   CHARACTER(LEN=30),PARAMETER :: f_cLWUseBudgetColumnTitles(f_iNLWUseBudColumns)       = ['Ag. Area'                           , &   
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
   CHARACTER(LEN=53),PARAMETER :: f_cRootZoneBudgetColumnTitles(f_iNRootZoneBudColumns) = ['Ag. Area'                                               , &     
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
    INTEGER,PARAMETER      :: f_iNTitles            = 6   , &
                              f_iTitleLen           = 229 , &        
                              f_iNColumnHeaderLines = 4   
    INTEGER                :: indxCol,indxLocation,iCount
    CHARACTER              :: UnitT*10,Text*17,Text1*13
    CHARACTER(LEN=6)       :: CParts(f_iNLWUseBudColumns) = ['AREA'   , &
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
                                                             'VOLUME' ]
    CHARACTER(LEN=13)      :: FParts(f_iNLWUseBudColumns) = ['AG_AREA'         ,&
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
                                                             'URB_SHORTAGE'    ]
    
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
      pASCIIOutput%TitleLen           = f_iTitleLen
      pASCIIOutput%NTitles            = f_iNTitles
      ALLOCATE(pASCIIOutput%cTitles(f_iNTitles)  ,  pASCIIOutput%lTitlePersist(f_iNTitles))
      pASCIIOutput%cTitles(1)         = ArrangeText('IWFM ROOT ZONE PACKAGE ('//TRIM(cVersion)//')' , pASCIIOutput%TitleLen)
      pASCIIOutput%cTitles(2)         = ArrangeText('LAND AND WATER USE BUDGET IN '//f_cVolumeUnitMarker//' FOR '//f_cLocationNameMarker , pASCIIOutput%TitleLen)
      pASCIIOutput%cTitles(3)         = ArrangeText('SUBREGION AREA: '//f_cAreaMarker//' '//f_cAreaUnitMarker , pASCIIOutput%TitleLen)
      pASCIIOutput%cTitles(4)         = REPEAT('-',pASCIIOutput%TitleLen)
      pASCIIOutput%cTitles(5)         = REPEAT(' ',73)//'Agricultural Area'//REPEAT(' ',94)//'Urban Area'
      pASCIIOutput%cTitles(6)         = REPEAT(' ',17)//REPEAT('-',130)//REPEAT(' ',3)//REPEAT('-',78)
      pASCIIOutput%lTitlePersist(1:3) = .TRUE.
      pASCIIOutput%lTitlePersist(4:6) = .FALSE.
      pASCIIOutput%cFormatSpec        = ADJUSTL('(A16,1X,10(F12.1,1X),3X,6(F12.1,1X))')
      pASCIIOutput%NColumnHeaderLines = f_iNColumnHeaderLines
    END ASSOCIATE 
    
    !Location names
    OutputData%NLocations = NRegion
    ALLOCATE (OutputData%cLocationNames(NRegion))
    OutputData%cLocationNames = cRegionNames  
        
    !Locations
    ALLOCATE (OutputData%Locations(1)                                                             , &
              OutputData%Locations(1)%cFullColumnHeaders(f_iNLWUseBudColumns+1)                   , &
              OutputData%Locations(1)%iDataColumnTypes(f_iNLWUseBudColumns)                       , &
              OutputData%Locations(1)%iColWidth(f_iNLWUseBudColumns+1)                            , &
              OutputData%Locations(1)%cColumnHeaders(f_iNLWUseBudColumns+1,f_iNColumnHeaderLines) , &
              OutputData%Locations(1)%cColumnHeadersFormatSpec(f_iNColumnHeaderLines)             )
    ASSOCIATE (pLocation => OutputData%Locations(1))
      pLocation%NDataColumns           = f_iNLWUseBudColumns
      pLocation%cFullColumnHeaders(1)  = 'Time'                               
      pLocation%cFullColumnHeaders(2:) = f_cLWUseBudgetColumnTitles
      pLocation%cFullColumnHeaders(2)  = TRIM(pLocation%cFullColumnHeaders(2))  // ' ('//f_cAreaUnitMarker//')'    
      pLocation%cFullColumnHeaders(12) = TRIM(pLocation%cFullColumnHeaders(12)) // ' ('//f_cAreaUnitMarker//')'    
      pLocation%iDataColumnTypes       = [f_iAR                 ,&  !Ag area
                                          f_iVR_lwu_PotCUAW     ,&  !Potential CUAW
                                          f_iVR_lwu_AgSupplyReq ,&  !Ag supply req.
                                          f_iVR_lwu_AgPump      ,&  !Pumping for ag
                                          f_iVR_lwu_AgDiv       ,&  !Deliveries for ag
                                          f_iVR_lwu_AgOthIn     ,&  !Ag inflow as surface runoff from upstream elements
                                          f_iVR_lwu_AgShort     ,&  !Ag supply shortage
                                          f_iVR                 ,&  !ETAW
                                          f_iVR                 ,&  !ETP
                                          f_iVR                 ,&  !ETOth
                                          f_iAR                 ,&  !Urban area
                                          f_iVR                 ,&  !Urban supply req.
                                          f_iVR                 ,&  !Pumping for urban
                                          f_iVR                 ,&  !Deliveries for urban
                                          f_iVR                 ,&  !Urban inflow as surface runoff from upstream elements
                                          f_iVR                 ]   !Urban supply shortage
      pLocation%iColWidth              = [17,12,14,(13,indxCol=1,8),12,14,(13,indxCol=1,4)]
      ASSOCIATE (pColumnHeaders => pLocation%cColumnHeaders           , &
                 pFormatSpecs   => pLocation%cColumnHeadersFormatSpec )
        Text                = ArrangeText(TRIM(UnitT),17)
        Text1               = '('//TRIM(f_cAreaUnitMarker)//')'
        pColumnHeaders(:,1) = ['                 ','            ','    Potential ',' Agricultural','             ','             ','  Inflow as  ','             ','             ','             ','      ET     ','            ','     Urban    ','             ','             ','  Inflow as  ','             ']
        pColumnHeaders(:,2) = ['      Time       ','        Area','      CUAW    ','    Supply   ','      Pumping','  Deliveries ',' Srfc. Runoff','     Shortage','             ','   Effective ','  from Other ','        Area','     Supply   ','      Pumping','  Deliveries ',' Srfc. Runoff','     Shortage']
        pColumnHeaders(:,3) = [               Text,         Text1,'              ','  Requirement','        (-)  ','      (-)    ','     (-)     ','       (=)   ','       ETAW  ','    Precip   ','   Sources   ',         Text1,'   Requirement','        (-)  ','      (-)    ','     (-)     ','       (=)   ']
        pColumnHeaders(:,4) = ''
        pFormatSpecs(1)     = '(A17,A12,A14,8A13,3X,A12,A14,4A13)'
        pFormatSpecs(2)     = '(A17,A12,A14,8A13,3X,A12,A14,4A13)'
        pFormatSpecs(3)     = '(A17,A12,A14,8A13,3X,A12,A14,4A13)'
        pFormatSpecs(4)     = '("'//REPEAT('-',f_iTitleLen)//'",'//TRIM(IntToText(f_iNLWUseBudColumns+1))//'A0)'
      END ASSOCIATE
    END ASSOCIATE
     
    !Data for DSS output  
    ASSOCIATE (pDSSOutput => OutputData%DSSOutput)
      ALLOCATE (pDSSOutput%cPathNames(f_iNLWUseBudColumns*NRegion) , pDSSOutput%iDataTypes(f_iNLWUseBudColumns))
      iCount = 1
      DO indxLocation=1,NRegion
        DO indxCol=1,f_iNLWUseBudColumns
          pDSSOutput%cPathNames(iCount) = '/IWFM_L&W_USE_BUD/'                                           //  &  !A part
                                          TRIM(UpperCase(OutputData%cLocationNames(indxLocation)))//'/'  //  &  !B part
                                          TRIM(CParts(indxCol))//'/'                                     //  &  !C part
                                          '/'                                                            //  &  !D part
                                           TRIM(TimeStep%Unit)//'/'                                      //  &  !E part
                                          TRIM(FParts(indxCol))//'/'                                            !F part
          iCount = iCount+1
        END DO
      END DO
      pDSSOutput%iDataTypes = [f_iPER_AVER,(f_iPER_CUM,indxCol=1,9),f_iPER_AVER,(f_iPER_CUM,indxCol=1,5)]
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
    INTEGER,PARAMETER      :: f_iNTitles            = 4   , &
                              f_iTitleLen           = 149 , &        
                              f_iNColumnHeaderLines = 4   
    INTEGER                :: indxCol,indxLocation,iCount
    CHARACTER              :: UnitT*10,Text*17,Text1*13
    CHARACTER(LEN=6)       :: CParts(f_iNAgLWUseBudColumns) = ['AREA'   , &
                                                               'VOLUME' , &
                                                               'VOLUME' , &
                                                               'VOLUME' , &
                                                               'VOLUME' , &
                                                               'VOLUME' , &
                                                               'VOLUME' , &
                                                               'VOLUME' , &
                                                               'VOLUME' , &
                                                               'VOLUME' ]
    CHARACTER(LEN=10)      :: FParts(f_iNAgLWUseBudColumns) = ['AREA'         ,&
                                                               'POTNL_CUAW'   ,&
                                                               'SUP_REQ'      ,&    
                                                               'PUMPING'      ,&
                                                               'DELIVERY'     ,&
                                                               'SR_INFLOW'    ,&
                                                               'SHORTAGE'     ,&
                                                               'ETAW'         ,&
                                                               'EFF_PRECIP'   ,&
                                                               'ET_OTHER'     ]
    
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
      pASCIIOutput%TitleLen           = f_iTitleLen
      pASCIIOutput%NTitles            = f_iNTitles
        ALLOCATE(pASCIIOutput%cTitles(f_iNTitles)  ,  pASCIIOutput%lTitlePersist(f_iNTitles))
        pASCIIOutput%cTitles(1)         = ArrangeText('IWFM ROOT ZONE PACKAGE ('//TRIM(cVersion)//')' , pASCIIOutput%TitleLen)
        pASCIIOutput%cTitles(2)         = ArrangeText('LAND AND WATER USE BUDGET IN '//f_cVolumeUnitMarker//' FOR '//f_cLocationNameMarker , pASCIIOutput%TitleLen)
        pASCIIOutput%cTitles(3)         = ArrangeText('SUBREGION AREA: '//f_cAreaMarker//' '//f_cAreaUnitMarker , pASCIIOutput%TitleLen)
        pASCIIOutput%cTitles(4)         = REPEAT('-',pASCIIOutput%TitleLen)
        pASCIIOutput%lTitlePersist(1:3) = .TRUE.
        pASCIIOutput%lTItlePersist(4)   = .FALSE.
      pASCIIOutput%cFormatSpec        = ADJUSTL('(A16,1X,10(F12.1,1X))')
      pASCIIOutput%NColumnHeaderLines = f_iNColumnHeaderLines
    END ASSOCIATE 
    
    !Location names
    OutputData%NLocations = NRegion
    ALLOCATE (OutputData%cLocationNames(NRegion))
    OutputData%cLocationNames = cRegionNames  
        
    !Locations
    ALLOCATE (OutputData%Locations(1)                                                               , &
              OutputData%Locations(1)%cFullColumnHeaders(f_iNAgLWUseBudColumns+1)                   , &
              OutputData%Locations(1)%iDataColumnTypes(f_iNAgLWUseBudColumns)                       , &
              OutputData%Locations(1)%iColWidth(f_iNAgLWUseBudColumns+1)                            , &
              OutputData%Locations(1)%cColumnHeaders(f_iNAgLWUseBudColumns+1,f_iNColumnHeaderLines) , &
              OutputData%Locations(1)%cColumnHeadersFormatSpec(f_iNColumnHeaderLines)               )
    ASSOCIATE (pLocation => OutputData%Locations(1))
      pLocation%NDataColumns       = f_iNAgLWUseBudColumns
      pLocation%cFullColumnHeaders = ['Time'                           , &
                                      'Area ('//f_cAreaUnitMarker//')' , &
                                      'Potential CUAW'                 , &
                                      'Supply Requirement'             , &
                                      'Pumping'                        , &
                                      'Deliveries'                     , &
                                      'Inflow as Surface Runoff'       , &
                                      'Shortage'                       , &
                                      'ETAW'                           , &
                                      'Effective Precipitation'        , &
                                      'ET from Other Sources'          ]
      pLocation%iDataColumnTypes  =  [f_iAR                 ,&      !Ag area
                                      f_iVR_lwu_PotCUAW     ,&      !Potential CUAW
                                      f_iVR_lwu_AgSupplyReq ,&      !Ag supply req.
                                      f_iVR_lwu_AgPump      ,&      !Pumping for ag
                                      f_iVR_lwu_AgDiv       ,&      !Deliveries for ag
                                      f_iVR_lwu_AgOthIn     ,&      !Ag inflow as surface runoff from upstream elements
                                      f_iVR_lwu_AgShort     ,&      !Ag supply shortage
                                      f_iVR                 ,&      !ETAW
                                      f_iVR                 ,&      !ETP
                                      f_iVR                 ]       !ETOth
      pLocation%iColWidth       = [17,12,14,(13,indxCol=1,7)]
      ASSOCIATE (pColumnHeaders => pLocation%cColumnHeaders           , &
                 pFormatSpecs   => pLocation%cColumnHeadersFormatSpec )
        Text                = ArrangeText(TRIM(UnitT),17)
        Text1               = '('//TRIM(f_cAreaUnitMarker)//')'
        pColumnHeaders(:,1) = ['                 ','            ','    Potential ',' Agricultural','             ','             ','  Inflow as  ','             ','             ','             ','      ET     ']
        pColumnHeaders(:,2) = ['      Time       ','        Area','      CUAW    ','    Supply   ','      Pumping',' Deliveries  ',' Srfc. Runoff','     Shortage','             ','   Effective ','  from Other ']
        pColumnHeaders(:,3) = [               Text,         Text1,'              ','  Requirement','        (-)  ','     (-)     ','     (-)     ','       (=)   ','       ETAW  ','    Precip   ','    Sources  ']
        pColumnHeaders(:,4) = ''
        pFormatSpecs(1)     = '(A17,A12,A14,8A13)'
        pFormatSpecs(2)     = '(A17,A12,A14,8A13)'
        pFormatSpecs(3)     = '(A17,A12,A14,8A13)'
        pFormatSpecs(4)     = '("'//REPEAT('-',f_iTitleLen)//'",'//TRIM(IntToText(f_iNAgLWUseBudColumns+1))//'A0)'
      END ASSOCIATE
    END ASSOCIATE
     
    !Data for DSS output  
    ASSOCIATE (pDSSOutput => OutputData%DSSOutput)
      ALLOCATE (pDSSOutput%cPathNames(f_iNAgLWUseBudColumns*NRegion) , pDSSOutput%iDataTypes(f_iNAgLWUseBudColumns))
      iCount = 1
      DO indxLocation=1,NRegion
        DO indxCol=1,f_iNAgLWUseBudColumns
          pDSSOutput%cPathNames(iCount) = '/IWFM_L&W_USE_BUD/'                                           //  &  !A part
                                          TRIM(UpperCase(OutputData%cLocationNames(indxLocation)))//'/'  //  &  !B part
                                          TRIM(CParts(indxCol))//'/'                                     //  &  !C part
                                          '/'                                                            //  &  !D part
                                           TRIM(TimeStep%Unit)//'/'                                      //  &  !E part
                                          TRIM(FParts(indxCol))//'/'                                            !F part
          iCount = iCount+1
        END DO
      END DO
      pDSSOutput%iDataTypes = [f_iPER_AVER,(f_iPER_CUM,indxCol=1,9)]
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
    INTEGER,PARAMETER      :: f_iNTitles            = 6   , &
                              f_iTitleLen           = 713 , &        
                              f_iNColumnHeaderLines = 4   
    INTEGER                :: indxCol,indxLocation,iCount
    CHARACTER              :: UnitT*10,Text*17,Text1*13
    CHARACTER(LEN=6)       :: CParts(f_iNRootZoneBudColumns) = ['AREA'   , &
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
                                                                'VOLUME' ]
    CHARACTER(LEN=15)      :: FParts(f_iNRootZoneBudColumns) = ['AG_AREA'           ,&
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
                                                                'NRV_DISCREPANCY'   ] 
    
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
      pASCIIOutput%TitleLen = f_iTitleLen
      pASCIIOutput%NTitles  = f_iNTitles
        ALLOCATE(pASCIIOutput%cTitles(f_iNTitles)  ,  pASCIIOutput%lTitlePersist(f_iNTitles))
        pASCIIOutput%cTitles(1)         = ArrangeText('IWFM ROOT ZONE PACKAGE ('//TRIM(cVersion)//')' , pASCIIOutput%TitleLen)
        pASCIIOutput%cTitles(2)         = ArrangeText('ROOT ZONE MOISTURE BUDGET IN '//f_cVolumeUnitMarker//' FOR '//f_cLocationNameMarker , pASCIIOutput%TitleLen)
        pASCIIOutput%cTitles(3)         = ArrangeText('SUBREGION AREA: '//f_cAreaMarker//' '//f_cAreaUnitMarker , pASCIIOutput%TitleLen)
        pASCIIOutput%cTitles(4)         = REPEAT('-',pASCIIOutput%TitleLen)
        pASCIIOutput%cTitles(5)         = REPEAT(' ',135)//'Agricultural Area'//REPEAT(' ',237)//'Urban Area'//REPEAT(' ',199)//'Native & Riparian Vegetation Area'
        pASCIIOutput%cTitles(6)         = REPEAT(' ',17)//REPEAT('-',255)//REPEAT(' ',3)//REPEAT('-',240)//REPEAT(' ',3)//REPEAT('-',195)
        pASCIIOutput%lTitlePersist(1:3) = .TRUE.
        pASCIIOutput%lTitlePersist(4:6) = .FALSE.
      pASCIIOutput%cFormatSpec        = ADJUSTL('(A16,1X,17(F14.1,1X),3X,16(F14.1,1X),3X,13(F14.1,1X))')
      pASCIIOutput%NColumnHeaderLines = f_iNColumnHeaderLines
    END ASSOCIATE 
                                                     
    !Location names
    OutputData%NLocations = NRegion
    ALLOCATE (OutputData%cLocationNames(NRegion))
    OutputData%cLocationNames = cRegionNames  
        
    !Locations
    ALLOCATE (OutputData%Locations(1)                                                                , &
              OutputData%Locations(1)%cFullColumnHeaders(f_iNRootZoneBudColumns+1)                   , &
              OutputData%Locations(1)%iDataColumnTypes(f_iNRootZoneBudColumns)                       , &
              OutputData%Locations(1)%iColWidth(f_iNRootZoneBudColumns+1)                            , &
              OutputData%Locations(1)%cColumnHeaders(f_iNRootZoneBudColumns+1,f_iNColumnHeaderLines) , &
              OutputData%Locations(1)%cColumnHeadersFormatSpec(f_iNColumnHeaderLines)                )
    ASSOCIATE (pLocation => OutputData%Locations(1))
      pLocation%NDataColumns       = f_iNRootZoneBudColumns
      pLocation%cFullColumnHeaders =  ['Time'                                                   , &
                                       'Ag. Area ('//f_cAreaUnitMarker//')'                     , &
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
                                       'Urban Area ('//f_cAreaUnitMarker//')'                   , &
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
                                       'Native&Riparian Veg. Area ('//f_cAreaUnitMarker//')'    , &
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
      pLocation%iDataColumnTypes  =  [f_iAR ,&  !Ag area
                                      f_iVR ,&  !Ag potential ET
                                      f_iVR ,&  !Ag precipitation
                                      f_iVR ,&  !Ag runoff
                                      f_iVR ,&  !Ag prime applied water
                                      f_iVR ,&  !Ag applied water from upstream element surface runoff
                                      f_iVR ,&  !Ag re-used water
                                      f_iVR ,&  !Ag return flow
                                      f_iVLB,&  !Ag beginning storage
                                      f_iVR ,&  !Ag net gain from land expansion
                                      f_iVR ,&  !Ag infiltration
                                      f_iVR ,&  !Ag generic inflow
                                      f_iVR ,&  !Ag pond drain
                                      f_iVR ,&  !Ag actual ET
                                      f_iVR ,&  !Ag perc
                                      f_iVLE,&  !Ag ending storage
                                      f_iVR ,&  !Ag discrepancy
                                      f_iAR ,&  !Urban area
                                      f_iVR ,&  !Urban potential ET
                                      f_iVR ,&  !Urban precipitation
                                      f_iVR ,&  !Urban runoff
                                      f_iVR ,&  !Urban prime applied water
                                      f_iVR ,&  !Urban applied water due to upstream element surface runoff
                                      f_iVR ,&  !Urban re-used water
                                      f_iVR ,&  !Urban return flow
                                      f_iVLB,&  !Urban beginning storage
                                      f_iVR ,&  !Urban net gain from land expansion
                                      f_iVR ,&  !Urban infiltration
                                      f_iVR ,&  !Urban generic inflow
                                      f_iVR ,&  !Urban actual ET
                                      f_iVR ,&  !Urban perc
                                      f_iVLE,&  !Urban ending storage
                                      f_iVR ,&  !Urban discrepancy
                                      f_iAR ,&  !NV&RV area
                                      f_iVR ,&  !NV&RV potential ET
                                      f_iVR ,&  !NV&RV precipitation
                                      f_iVR ,&  !NV&RV surface runoff from upstream elements/subregions
                                      f_iVR ,&  !NV&RV runoff
                                      f_iVLB,&  !NV&RV beginning storage
                                      f_iVR ,&  !NV&RV net gain from land expansion
                                      f_iVR ,&  !NV&RV infiltration
                                      f_iVR ,&  !NV&RV generic inflow
                                      f_iVR ,&  !NV&RV actual ET
                                      f_iVR ,&  !NV&RV perc
                                      f_iVLE,&  !NV&RV ending storage
                                      f_iVR ]   !NV&RV discrepancy
      pLocation%iColWidth       = [17,14,15,16,(15,indxCol=1,14),14,15,16,(15,indxCol=1,13),14,15,16,(15,indxCol=1,10)]
      ASSOCIATE (pColumnHeaders => pLocation%cColumnHeaders           , &
                 pFormatSpecs   => pLocation%cColumnHeadersFormatSpec )
        Text                = ArrangeText(TRIM(UnitT),17)
        Text1               = '('//TRIM(f_cAreaUnitMarker)//')'
        pColumnHeaders(:,1) = ['                 ','              ','               ','                ','               ','       Prime   ','   Inflow as   ','         Reused','          Net  ','     Beginning ',' Net Gain from ','               ','        Other  ','          Pond ','         Actual','               ','        Ending ','               ','              ','               ','                ','               ','       Prime   ','   Inflow as   ','         Reused','          Net  ','     Beginning ',' Net Gain from ','               ','        Other  ','         Actual','               ','        Ending ','               ','              ','               ','                ','   Inflow as   ','               ','     Beginning ',' Net Gain from ','               ','        Other  ','         Actual','               ','        Ending ','               ']
        pColumnHeaders(:,2) = ['      Time       ','          Area','      Potential','   Precipitation','         Runoff','      Applied  ',' Surface Runoff','         Water ','         Return','      Storage  ',' Land Expansion','   Infiltration','        Inflow ','          Drain','           ET  ','    Percolation','        Storage','    Discrepancy','          Area','      Potential','   Precipitation','         Runoff','      Applied  ',' Surface Runoff','         Water ','         Return','      Storage  ',' Land Expansion','   Infiltration','        Inflow ','           ET  ','    Percolation','        Storage','    Discrepancy','          Area','      Potential','  Precipitation ',' Surface Runoff','        Runoff ','      Storage  ',' Land Expansion','   Infiltration','        Inflow ','           ET  ','    Percolation','        Storage','    Discrepancy']
        pColumnHeaders(:,3) = [               Text,           Text1,'         ET    ','                ','               ','       Water   ','               ','               ','          Flow ','        (+)    ','       (+)     ','        (+)    ','          (+)  ','           (-) ','           (-) ','       (-)     ','          (-)  ','        (=)    ',           Text1,'         ET    ','                ','               ','       Water   ','               ','               ','          Flow ','        (+)    ','       (+)     ','        (+)    ','          (+)  ','           (-) ','       (-)     ','          (-)  ','        (=)    ',           Text1,'         ET    ','                ','               ','               ','        (+)    ','       (+)     ','        (+)    ','          (+)  ','           (-) ','       (-)     ','          (-)  ','        (=)    ']
        pColumnHeaders(:,4) = ''
        pFormatSpecs(1)     = '(A17,A14,A15,A16,14A15,3X,A14,A15,A16,13A15,3X,A14,A15,A16,10A15)'
        pFormatSpecs(2)     = '(A17,A14,A15,A16,14A15,3X,A14,A15,A16,13A15,3X,A14,A15,A16,10A15)'
        pFormatSpecs(3)     = '(A17,A14,A15,A16,14A15,3X,A14,A15,A16,13A15,3X,A14,A15,A16,10A15)'
        pFormatSpecs(4)     = '('//TRIM(IntToText(f_iTitleLen))//'(1H-),'//TRIM(IntToText(f_iNRootZoneBudColumns+1))//'A0)'
      END ASSOCIATE
    END ASSOCIATE

    !Data for DSS output  
    ASSOCIATE (pDSSOutput => OutputData%DSSOutput)
      ALLOCATE (pDSSOutput%cPathNames(f_iNRootZoneBudColumns*NRegion) , pDSSOutput%iDataTypes(f_iNRootZoneBudColumns))
      iCount = 1
      DO indxLocation=1,NRegion
        DO indxCol=1,f_iNRootZoneBudColumns
          pDSSOutput%cPathNames(iCount) = '/IWFM_ROOTZN_BUD/'                                            //  &  !A part
                                          TRIM(UpperCase(OutputData%cLocationNames(indxLocation)))//'/'  //  &  !B part
                                          TRIM(CParts(indxCol))//'/'                                     //  &  !C part
                                          '/'                                                            //  &  !D part
                                           TRIM(TimeStep%Unit)//'/'                                      //  &  !E part
                                          TRIM(FParts(indxCol))//'/'                                            !F part
          iCount = iCount+1
        END DO
      END DO
      pDSSOutput%iDataTypes = [f_iPER_AVER,(f_iPER_CUM,indxCol=1,16),f_iPER_AVER,(f_iPER_CUM,indxCol=1,15),f_iPER_AVER,(f_iPER_CUM,indxCol=1,12)]
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
    INTEGER,PARAMETER      :: f_iNTitles            = 4   , &
                              f_iTitleLen           = 274 , &        
                              f_iNColumnHeaderLines = 4   
    INTEGER                :: indxCol,indxLocation,iCount
    CHARACTER              :: UnitT*10,Text*17,Text1*13
    CHARACTER(LEN=6)       :: CParts(f_iNAgRootZoneBudColumns) = ['AREA'   , &
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
                                                                  'VOLUME' ]
    CHARACTER(LEN=11)      :: FParts(f_iNAgRootZoneBudColumns) = ['AREA'           ,&
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
                                                                  'DISCREPANCY'    ] 
    
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
      pASCIIOutput%TitleLen = f_iTitleLen
      pASCIIOutput%NTitles  = f_iNTitles
        ALLOCATE(pASCIIOutput%cTitles(f_iNTitles)  ,  pASCIIOutput%lTitlePersist(f_iNTitles))
        pASCIIOutput%cTitles(1)         = ArrangeText('IWFM ROOT ZONE PACKAGE ('//TRIM(cVersion)//')' , pASCIIOutput%TitleLen)
        pASCIIOutput%cTitles(2)         = ArrangeText('ROOT ZONE MOISTURE BUDGET IN '//f_cVolumeUnitMarker//' FOR '//f_cLocationNameMarker , pASCIIOutput%TitleLen)
        pASCIIOutput%cTitles(3)         = ArrangeText('SUBREGION AREA: '//f_cAreaMarker//' '//f_cAreaUnitMarker , pASCIIOutput%TitleLen)
        pASCIIOutput%cTitles(4)         = REPEAT('-',pASCIIOutput%TitleLen)
        pASCIIOutput%lTitlePersist(1:3) = .TRUE.
        pASCIIOutput%lTitlePersist(4)   = .FALSE.
      pASCIIOutput%cFormatSpec        = ADJUSTL('(A16,1X,17(F14.1,1X))')
      pASCIIOutput%NColumnHeaderLines = f_iNColumnHeaderLines
    END ASSOCIATE 
                                                     
    !Location names
    OutputData%NLocations = NRegion
    ALLOCATE (OutputData%cLocationNames(NRegion))
    OutputData%cLocationNames = cRegionNames  
        
    !Locations
    ALLOCATE (OutputData%Locations(1)                                                                  , &
              OutputData%Locations(1)%cFullColumnHeaders(f_iNAgRootZoneBudColumns+1)                   , &
              OutputData%Locations(1)%iDataColumnTypes(f_iNAgRootZoneBudColumns)                       , &
              OutputData%Locations(1)%iColWidth(f_iNAgRootZoneBudColumns+1)                            , &
              OutputData%Locations(1)%cColumnHeaders(f_iNAgRootZoneBudColumns+1,f_iNColumnHeaderLines) , &
              OutputData%Locations(1)%cColumnHeadersFormatSpec(f_iNColumnHeaderLines)                  )
    ASSOCIATE (pLocation => OutputData%Locations(1))
      pLocation%NDataColumns       = f_iNAgRootZoneBudColumns
      pLocation%cFullColumnHeaders =  ['Time'                                               , &
                                       'Area ('//f_cAreaUnitMarker//')'                     , &
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
                                       'Discrepancy (=)'                                    ]
      pLocation%iDataColumnTypes  =  [f_iAR ,&  !Ag area
                                      f_iVR ,&  !Ag potential ET
                                      f_iVR ,&  !Ag precipitation
                                      f_iVR ,&  !Ag runoff
                                      f_iVR ,&  !Ag prime applied water
                                      f_iVR ,&  !Ag applied water from upstream element surface runoff
                                      f_iVR ,&  !Ag re-used water
                                      f_iVR ,&  !Ag return flow
                                      f_iVLB,&  !Ag beginning storage
                                      f_iVR ,&  !Ag net gain from land expansion
                                      f_iVR ,&  !Ag infiltration
                                      f_iVR ,&  !Ag generic inflow
                                      f_iVR ,&  !Ag pond drain
                                      f_iVR ,&  !Ag actual ET
                                      f_iVR ,&  !Ag perc
                                      f_iVLE,&  !Ag ending storage
                                      f_iVR ]   !Ag discrepancy
      pLocation%iColWidth       = [17,14,15,16,(15,indxCol=1,14)]
      ASSOCIATE (pColumnHeaders => pLocation%cColumnHeaders           , &
                 pFormatSpecs   => pLocation%cColumnHeadersFormatSpec )
        Text                = ArrangeText(TRIM(UnitT),17)
        Text1               = '('//TRIM(f_cAreaUnitMarker)//')'
        pColumnHeaders(:,1) = ['                 ','              ','               ','                ','               ','       Prime   ','   Inflow as   ','         Reused','          Net  ','     Beginning ',' Net Gain from ','               ','        Other  ','          Pond ','         Actual','               ','        Ending ','               ']
        pColumnHeaders(:,2) = ['      Time       ','          Area','      Potential','   Precipitation','         Runoff','      Applied  ',' Surface Runoff','         Water ','         Return','      Storage  ',' Land Expansion','   Infiltration','        Inflow ','          Drain','           ET  ','    Percolation','        Storage','    Discrepancy']
        pColumnHeaders(:,3) = [               Text,           Text1,'         ET    ','                ','               ','       Water   ','               ','               ','          Flow ','        (+)    ','       (+)     ','        (+)    ','          (+)  ','           (-) ','           (-) ','       (-)     ','          (-)  ','        (=)    ']
        pColumnHeaders(:,4) = ''
        pFormatSpecs(1)     = '(A17,A14,A15,A16,14A15)'
        pFormatSpecs(2)     = '(A17,A14,A15,A16,14A15)'
        pFormatSpecs(3)     = '(A17,A14,A15,A16,14A15)'
        pFormatSpecs(4)     = '('//TRIM(IntToText(f_iTitleLen))//'(1H-),'//TRIM(IntToText(f_iNAgRootZoneBudColumns+1))//'A0)'
      END ASSOCIATE
    END ASSOCIATE

    !Data for DSS output  
    ASSOCIATE (pDSSOutput => OutputData%DSSOutput)
      ALLOCATE (pDSSOutput%cPathNames(f_iNAgRootZoneBudColumns*NRegion) , pDSSOutput%iDataTypes(f_iNAgRootZoneBudColumns))
      iCount = 1
      DO indxLocation=1,NRegion
        DO indxCol=1,f_iNAgRootZoneBudColumns
          pDSSOutput%cPathNames(iCount) = '/IWFM_ROOTZN_BUD/'                                            //  &  !A part
                                          TRIM(UpperCase(OutputData%cLocationNames(indxLocation)))//'/'  //  &  !B part
                                          TRIM(CParts(indxCol))//'/'                                     //  &  !C part
                                          '/'                                                            //  &  !D part
                                           TRIM(TimeStep%Unit)//'/'                                      //  &  !E part
                                          TRIM(FParts(indxCol))//'/'                                            !F part
          iCount = iCount+1
        END DO
      END DO
      pDSSOutput%iDataTypes = [f_iPER_AVER,(f_iPER_CUM,indxCol=1,16)]
    END ASSOCIATE
                                             
    !Instantiate the root zone budget raw file
    CALL RawFile%New(cFileName,OutputData,iStat)
    
  END SUBROUTINE AgRootZoneBudRawFile_New


  
END MODULE