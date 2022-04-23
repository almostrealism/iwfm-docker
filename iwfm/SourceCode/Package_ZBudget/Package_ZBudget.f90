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
MODULE Package_ZBudget
  USE Class_Version          , ONLY: VersionType
  USE ZBudget_Parameters     , ONLY: iStorageType                , &
                                     iVerticalFlowType           , &
                                     iFaceFlowType               , &
                                     iElemDataType               , &
                                     iZoneHorizontal             , &
                                     iZoneVertical               , &
                                     iUndefinedZone              , &
                                     MarkerChar                  , &
                                     LengthUnitMarker            , &
                                     AreaUnitMarker              , &
                                     VolumeUnitMarker            , &
                                     ColumnHeaderLen             , &
                                     MaxLocationNameLen          , &
                                     AR                          , &
                                     VR                          , &
                                     VR_lwu_PotCUAW              , &
                                     VR_lwu_AgSupplyReq          , &
                                     VR_lwu_AgPump               , &
                                     VR_lwu_AgDiv                , &
                                     VR_lwu_AgOthIn              , &
                                     VR_lwu_AgShort              , &
                                     VLB                         , &
                                     VLE                         
  USE Class_ZBudgetHeader    , ONLY: ZBudgetHeaderType           
  USE Class_SystemData       , ONLY: SystemDataType
  USE Class_ZBudget          , ONLY: ZBudgetType                 , &
                                     Abstract_CallbackFun
  USE Class_ZoneList         , ONLY: ZoneType                    , &
                                     AdjacentZoneType            , &
                                     ZoneListType
  USE ZBudget_Util           , ONLY: IsZBudgetFile
  IMPLICIT NONE
  
  
  ! -------------------------------------------------------------
  ! --- PUBLIC ENTITIES
  ! -------------------------------------------------------------
  PRIVATE
  PUBLIC :: SystemDataType              , &
            ZBudgetHeaderType           , &
            ZBudgetType                 , &
            ZoneType                    , &
            AdjacentZoneType            , &
            ZoneListType                , &
            Package_ZBudget_GetVersion  , &
            IsZBudgetFile               , &
            iStorageType                , &
            iVerticalFlowType           , &
            iFaceFlowType               , &
            iElemDataType               , &
            iZoneHorizontal             , &
            iZoneVertical               , &
            iUndefinedZone              , &
            MarkerChar                  , &
            LengthUnitMarker            , &
            AreaUnitMarker              , &
            VolumeUnitMarker            , &
            ColumnHeaderLen             , &
            MaxLocationNameLen          , &
            AR                          , &
            VR                          , &
            VR_lwu_PotCUAW              , &
            VR_lwu_AgSupplyReq          , &
            VR_lwu_AgPump               , &
            VR_lwu_AgDiv                , &
            VR_lwu_AgOthIn              , &
            VR_lwu_AgShort              , &
            VLB                         , &
            VLE                         , &
            Abstract_CallbackFun
 
  
  ! -------------------------------------------------------------
  ! --- VERSION RELEATED DATA
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                    :: iLenVersion = 8
  CHARACTER(LEN=iLenVersion),PARAMETER :: cVersion    ='4.0.0000'
  INCLUDE 'Package_ZBudget_Revision.fi'
  
  
  
CONTAINS

    

  ! -------------------------------------------------------------
  ! --- GET VERSION NUMBER
  ! -------------------------------------------------------------
  FUNCTION Package_ZBudget_GetVersion() RESULT(cVrs)
    CHARACTER(:),ALLOCATABLE :: cVrs
    
    !Local variables
    TYPE(VersionType) :: MyVersion
    
    MyVersion = MyVersion%New(iLenVersion,cVersion,cRevision)
    cVrs      = TRIM(MyVersion%GetVersion()) 
    
  END FUNCTION Package_ZBudget_GetVersion

  
END MODULE