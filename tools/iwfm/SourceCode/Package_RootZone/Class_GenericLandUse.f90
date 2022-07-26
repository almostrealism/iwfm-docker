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
MODULE Class_GenericLandUse
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
  PUBLIC :: GenericLandUseType 
                                

  ! -------------------------------------------------------------
  ! --- ROOT ZONE CORE DATA TYPE
  ! -------------------------------------------------------------
  TYPE GenericLandUseType
      REAL(8),ALLOCATABLE :: SMax(:,:)                            !Maximum soil retention parameter
      INTEGER,ALLOCATABLE :: iColETc(:,:)                         !Column number in the ETc database
      REAL(8),ALLOCATABLE :: ETa(:,:)                             !Actual ET
      REAL(8),ALLOCATABLE :: Runoff(:,:)                          !Direct runoff due to precip
      REAL(8),ALLOCATABLE :: PrecipInfilt(:,:)                    !Infiltration due to precipitation
      REAL(8),ALLOCATABLE :: SoilM_Precip_P_BeforeUpdate(:,:)     !Soil moisture at the beginning of time step due to precipitation, but before it is possibly updated due to land use area change
      REAL(8),ALLOCATABLE :: SoilM_Precip_P(:,:)                  !Soil moisture at the beginning of time step due to precipitation
      REAL(8),ALLOCATABLE :: SoilM_Precip(:,:)                    !Soil moisture at the end of time step due to precipitation
      REAL(8),ALLOCATABLE :: SoilM_AW_P_BeforeUpdate(:,:)         !Soil moisture at the beginning of time step due to irrigation, but before it is possibly updated due to land use area change
      REAL(8),ALLOCATABLE :: SoilM_AW_P(:,:)                      !Soil moisture at the beginning of time step due to irrigation
      REAL(8),ALLOCATABLE :: SoilM_AW(:,:)                        !Soil moisture at the end of time step due to irrigation
      REAL(8),ALLOCATABLE :: SoilM_Oth_P_BeforeUpdate(:,:)        !Soil moisture at the beginning of time step due to user-defined source of water, but before it is possibly updated due to land use area change
      REAL(8),ALLOCATABLE :: SoilM_Oth_P(:,:)                     !Soil moisture at the beginning of time step due to user-defined source of water
      REAL(8),ALLOCATABLE :: SoilM_Oth(:,:)                       !Soil moisture at the end of time step due to user-defined source of water
      REAL(8),ALLOCATABLE :: SoilMCh(:,:)                         !Soil moisture chnage due to contraction/expansion of land use area
      REAL(8),ALLOCATABLE :: Area_P(:,:)                          !Land use area before update from land use area data file at the beginning of time step
      REAL(8),ALLOCATABLE :: Area(:,:)                            !Land use area after update from land use area data file at the beginning of time step
      REAL(8),ALLOCATABLE :: Perc(:,:)                            !Percolation
      REAL(8),ALLOCATABLE :: PercCh(:,:)                          !Percolation due to soil moisture exceeding total porosity when land use areas are modified and soil moisture redistributed
      REAL(8),ALLOCATABLE :: GMExcess(:,:)                        !Excess general moisture inflow due to stoarge capacity of the root zone
  CONTAINS
      PROCEDURE,PASS :: New
      PROCEDURE,PASS :: Kill
  END TYPE GenericLandUseType
        

  ! -------------------------------------------------------------
  ! --- MISC. ENTITIES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                   :: ModNameLen = 22
  CHARACTER(LEN=ModNameLen),PARAMETER :: ModName    = 'Class_GenericLandUse::'
  
  
  
CONTAINS
  
    

  
! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** CONSTRUCTOR
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************
    
  ! -------------------------------------------------------------
  ! --- ALLOCATE MEMORY FOR ARRAYS
  ! -------------------------------------------------------------
  SUBROUTINE New(GenericLandUse,iDim1,iDim2,iStat)
    CLASS(GenericLandUseType),INTENT(OUT) :: GenericLandUse
    INTEGER,INTENT(IN)                    :: iDim1,iDim2
    INTEGER,INTENT(OUT)                   :: iStat
    
    ALLOCATE(GenericLandUse%SMax(iDim1,iDim2)                        , &                            
             GenericLandUse%iColETc(iDim1,iDim2)                     , &                        
             GenericLandUse%ETa(iDim1,iDim2)                         , &                            
             GenericLandUse%Runoff(iDim1,iDim2)                      , &                         
             GenericLandUse%PrecipInfilt(iDim1,iDim2)                , &                   
             GenericLandUse%SoilM_Precip_P_BeforeUpdate(iDim1,iDim2) , &    
             GenericLandUse%SoilM_Precip_P(iDim1,iDim2)              , &                 
             GenericLandUse%SoilM_Precip(iDim1,iDim2)                , &                   
             GenericLandUse%SoilM_AW_P_BeforeUpdate(iDim1,iDim2)     , &        
             GenericLandUse%SoilM_AW_P(iDim1,iDim2)                  , &                     
             GenericLandUse%SoilM_AW(iDim1,iDim2)                    , &                       
             GenericLandUse%SoilM_Oth_P_BeforeUpdate(iDim1,iDim2)    , &       
             GenericLandUse%SoilM_Oth_P(iDim1,iDim2)                 , &                    
             GenericLandUse%SoilM_Oth(iDim1,iDim2)                   , &                      
             GenericLandUse%SoilMCh(iDim1,iDim2)                     , &                        
             GenericLandUse%Area_P(iDim1,iDim2)                      , &                         
             GenericLandUse%Area(iDim1,iDim2)                        , &                           
             GenericLandUse%Perc(iDim1,iDim2)                        , &                           
             GenericLandUse%PercCh(iDim1,iDim2)                      , &                         
             GenericLandUse%GMExcess(iDim1,iDim2)                    , &
             STAT=iStat                                              )  
    IF (iStat .NE. 0) RETURN
    
    !Initialize all to zero
    GenericLandUse%SMax                        = 0.0                                                   
    GenericLandUse%iColETc                     = 0                                           
    GenericLandUse%ETa                         = 0.0                                                    
    GenericLandUse%Runoff                      = 0.0                                              
    GenericLandUse%PrecipInfilt                = 0.0                                  
    GenericLandUse%SoilM_Precip_P_BeforeUpdate = 0.0    
    GenericLandUse%SoilM_Precip_P              = 0.0                              
    GenericLandUse%SoilM_Precip                = 0.0                                  
    GenericLandUse%SoilM_AW_P_BeforeUpdate     = 0.0            
    GenericLandUse%SoilM_AW_P                  = 0.0                                      
    GenericLandUse%SoilM_AW                    = 0.0                                          
    GenericLandUse%SoilM_Oth_P_BeforeUpdate    = 0.0          
    GenericLandUse%SoilM_Oth_P                 = 0.0                                    
    GenericLandUse%SoilM_Oth                   = 0.0                                        
    GenericLandUse%SoilMCh                     = 0.0                                            
    GenericLandUse%Area_P                      = 0.0                                              
    GenericLandUse%Area                        = 0.0                                                  
    GenericLandUse%Perc                        = 0.0                                                  
    GenericLandUse%PercCh                      = 0.0                                              
    GenericLandUse%GMExcess                    = 0.0    
    
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
  ! --- DEALLOCATE MEMORY 
  ! -------------------------------------------------------------
  SUBROUTINE Kill(GenericLandUse)
    CLASS(GenericLandUseType) :: GenericLandUse
    
    !LOcal variables
    INTEGER :: iErrorCode
    
    DEALLOCATE(GenericLandUse%SMax                         , &                            
               GenericLandUse%iColETc                      , &                        
               GenericLandUse%ETa                          , &                            
               GenericLandUse%Runoff                       , &                         
               GenericLandUse%PrecipInfilt                 , &                   
               GenericLandUse%SoilM_Precip_P_BeforeUpdate  , &    
               GenericLandUse%SoilM_Precip_P               , &                 
               GenericLandUse%SoilM_Precip                 , &                   
               GenericLandUse%SoilM_AW_P_BeforeUpdate      , &        
               GenericLandUse%SoilM_AW_P                   , &                     
               GenericLandUse%SoilM_AW                     , &                       
               GenericLandUse%SoilM_Oth_P_BeforeUpdate     , &       
               GenericLandUse%SoilM_Oth_P                  , &                    
               GenericLandUse%SoilM_Oth                    , &                      
               GenericLandUse%SoilMCh                      , &                        
               GenericLandUse%Area_P                       , &                         
               GenericLandUse%Area                         , &                           
               GenericLandUse%Perc                         , &                           
               GenericLandUse%PercCh                       , &                         
               GenericLandUse%GMExcess                     , &
               STAT=iErrorCode                             )     
    
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

    


! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** MISC. METHODS
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************


END MODULE