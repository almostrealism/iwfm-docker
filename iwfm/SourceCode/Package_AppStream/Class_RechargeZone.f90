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
MODULE Class_RechargeZone
  USE MessageLogger     , ONLY: SetLastMessage , &
                                MessageArray   , &
                                iFatal
  USE GeneralUtilities
  USE IOInterface
  USE Package_Misc
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
  PUBLIC :: RechargeZoneType   , &
            RechargeZone_New
  

  ! -------------------------------------------------------------
  ! --- RECHARGE ZONE DATA TYPE
  ! -------------------------------------------------------------
  TYPE RechargeZoneType
    INTEGER             :: NZones        = 0
    INTEGER,ALLOCATABLE :: Zones(:)
    REAL(8),ALLOCATABLE :: Fracs(:)
  END TYPE RechargeZoneType
  
  
  ! -------------------------------------------------------------
  ! --- MISC. ENTITIES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                   :: ModNameLen = 20
  CHARACTER(LEN=ModNameLen),PARAMETER :: ModName    = 'Class_RechargeZone::'
    


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
  ! --- READ FROM FILE
  ! -------------------------------------------------------------
  SUBROUTINE RechargeZone_New(NDiver,InFile,RechargeZones,iStat)
    INTEGER,INTENT(IN)            :: NDiver
    TYPE(GenericFileType)         :: InFile
    TYPE(RechargeZoneType),TARGET :: RechargeZones(NDiver)
    INTEGER,INTENT(OUT)           :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+16)   :: ThisProcedure = ModName // 'RechargeZone_New'
    INTEGER                        :: NZones,indxDiver,ID,iZone,ErrorCode
    REAL(8)                        :: DummyArray(4),DummyArray2(2)
    TYPE(RechargeZoneType),POINTER :: pZone
    
    !Initialize
    iStat = 0
    
    !Iterate over diversions
    DO indxDiver=1,NDiver
        pZone => RechargeZones(indxDiver)
        
        !Read data
        CALL InFile%ReadData(DummyArray,iStat)  
        IF (iStat .EQ. -1) RETURN
        
        !Diversion ID
        ID = INT(DummyArray(1))
        IF (ID .NE. indxDiver) THEN
            MessageArray(1) = 'Diversions for recharge zone data should be entered sequentialy.'
            MessageArray(2) = 'Diversion number expected='//TRIM(IntToText(indxDiver))
            MessageArray(3) = 'Diversion number entered ='//TRIM(IntToText(ID))
            CALL SetLastMessage(MessageArray(1:3),iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        
        !Number of zones
        NZones       = INT(DummyArray(2))
        pZone%NZones = NZones
        IF (NZones .EQ. 0) CYCLE
        
        !Allocate memory
        ALLOCATE (pZone%Zones(NZones) , pZone%Fracs(NZones) ,STAT=ErrorCode)
        IF (ErrorCode .NE. 0) THEN
            CALL SetLastMessage('Error allocating memory for recharge zones for diversion '//TRIM(IntToText(indxDiver))//'!',iFatal,ThisProcedure)
            iStat = -1
            RETURN
        END IF
        pZone%Zones(1) = INT(DummyArray(3))
        pZone%Fracs(1) =     DummyArray(4)
        
        !Read and assign rest of data
        DO iZone=2,NZones
            CALL InFile%ReadData(DummyArray2,iStat)  ;  IF (iStat .EQ. -1) RETURN
            pZone%Zones(iZone) = INT(DummyArray2(1))
            pZone%Fracs(iZone) =     DummyArray2(2)
        END DO
        
        !Normalize the fractions
        IF (NZones .GT. 0) CALL NormalizeArray(pZone%Fracs)
      
    END DO
    
  END SUBROUTINE RechargeZone_New
  
END MODULE