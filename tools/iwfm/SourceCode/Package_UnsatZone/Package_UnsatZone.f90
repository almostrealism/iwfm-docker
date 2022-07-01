MODULE Package_UnsatZone
  USE Class_Version  , ONLY: VersionType
  USE UnsatZoneOps   , ONLY: NonPondedLUMoistureRouter , &                             
                             PondedLUMoistureRouter    , &
                             VadoseZoneMoistureRouter  , &
                             NonPondedCropDemand       , &
                             PondedCropDemand           
  USE Class_Soil     , ONLY: SoilType                  , &
                             RootZoneSoilType          , &
                             f_iKunsatMethodList
  IMPLICIT NONE
  
  PRIVATE
  PUBLIC  :: SoilType                     , &
             RootZoneSoilType             , &
             NonPondedCropDemand          , &
             NonPondedLUMoistureRouter    , &
             VadoseZoneMoistureRouter     , &
             PondedCropDemand             , &
             PondedLUMoistureRouter       , &
             Package_UnsatZone_GetVersion , &
             f_iKunsatMethodList
  
  
  ! -------------------------------------------------------------
  ! --- VERSION RELEATED DATA
  ! -------------------------------------------------------------
  INTEGER,PRIVATE,PARAMETER                    :: iLenVersion = 8
  CHARACTER(LEN=iLenVersion),PRIVATE,PARAMETER :: cVersion ='4.0.0000'
  INCLUDE 'Package_UnsatZone_Revision.fi'
  
  
  
CONTAINS

    

  ! -------------------------------------------------------------
  ! --- GET VERSION NUMBER
  ! -------------------------------------------------------------
  FUNCTION Package_UnsatZone_GetVersion() RESULT(cVrs)
    CHARACTER(:),ALLOCATABLE :: cVrs
    
    !Local variables
    TYPE(VersionType) :: MyVersion
    
    MyVersion = MyVersion%New(iLenVersion,cVersion,cRevision)
    cVrs      = TRIM(MyVersion%GetVersion()) 
    
  END FUNCTION Package_UnsatZone_GetVersion


END MODULE