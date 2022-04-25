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
MODULE Class_GWState
  USE MessageLogger , ONLY: SetLastMessage , &
                            MessageArray   , &
                            f_iFatal 
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
  PUBLIC :: GWStateType
  
  
  ! -------------------------------------------------------------
  ! --- GW STATE DATA TYPE
  ! -------------------------------------------------------------
  TYPE GWStateType
      REAL(8),ALLOCATABLE :: Head(:,:)               !Groundwater head at current time step at each (node,layer)
      REAL(8),ALLOCATABLE :: Head_P(:,:)             !Groundwater head at the previous time step at each (node,layer)
      REAL(8),ALLOCATABLE :: Vx(:,:)                 !Groundwater velocity in x-direction at each (node,layer)
      REAL(8),ALLOCATABLE :: Vy(:,:)                 !Groundwater velocity in y-direction at each (node,layer)
      REAL(8),ALLOCATABLE :: Vz(:,:)                 !Groundwater velocity in z-direction at each (node,layer)
      REAL(8),ALLOCATABLE :: Storativity_P(:,:)      !Storage coefficient used at the previous time step at each (node,layer)
  CONTAINS
      PROCEDURE,PASS :: New
      PROCEDURE,PASS :: Kill
  END TYPE GWStateType
  
  
  ! -------------------------------------------------------------
  ! --- MISC. ENTITIES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                   :: ModNameLen = 15
  CHARACTER(LEN=ModNameLen),PARAMETER :: ModName    = 'Class_GWState::'
  
  
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
  ! --- INSTANTIATE GW STATE DATA
  ! -------------------------------------------------------------
  SUBROUTINE New(GWState,NNodes,NLayers,iStat)
    CLASS(GWStateType)  :: GWState
    INTEGER,INTENT(IN)  :: NNodes,NLayers
    INTEGER,INTENT(OUT) :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+3),PARAMETER :: ThisProcedure = ModName // 'New'
    INTEGER                               :: ErrorCode
    CHARACTER                             :: cErrorMsg*250
    
    ALLOCATE(GWState%Head(NNodes,NLayers)            , &
             GWState%Head_P(NNodes,NLayers)          , &
             GWState%Vx(NNodes,NLayers)              , &
             GWState%Vy(NNodes,NLayers)              , &
             GWState%Vz(NNodes,NLayers)              , &
             GWState%Storativity_P(NNodes,NLayers)   , &
             STAT = ErrorCode                        , &
             ERRMSG = cErrorMsg                      )
    IF (ErrorCode .NE. 0) THEN
        MessageArray(1) = 'Error in allocating memory for the groundwater component.'
        MessageArray(2) = cErrorMsg
        CALL SetLastMessage(MessageArray(1:2),f_iFatal,ThisProcedure)
        iStat = -1
    ELSE
        iStat = 0
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
  ! --- KILL GW STATE DATA
  ! -------------------------------------------------------------
  SUBROUTINE Kill(GWState)
    CLASS(GWStateType) :: GWState
    
    !Local variables
    INTEGER :: ErrorCode
    
    !Deallocate allocatable arrays
    DEALLOCATE (GWState%Head          , &
                GWState%Head_P        , &
                GWState%Vx            , &
                GWState%Vy            , &
                GWState%Vz            , &
                GWState%Storativity_P , &
                STAT=ErrorCode        )
    
    END SUBROUTINE Kill

END MODULE