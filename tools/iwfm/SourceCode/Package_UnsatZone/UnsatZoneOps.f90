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
MODULE UnsatZoneOps
  USE GeneralUtilities   , ONLY: FEXP
  USE RainfallRunoff     , ONLY: SCSMethod_HELP
  USE Class_Soil         , ONLY: f_iCampbell        , &
                                 f_ivanGenuchten
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
  PUBLIC :: NonPondedLUMoistureRouter  , &
            PondedLUMoistureRouter     , &
            VadoseZoneMoistureRouter   , &
            NonPondedCropDemand        , &
            PondedCropDemand           


  ! -------------------------------------------------------------
  ! --- FRCATION OF TOLERANCE TO START MODIFYING van Genucten CURVE NEAR SATURATION
  ! -------------------------------------------------------------
  REAL(8),PARAMETER :: f_rTolerFrac  = 1d-1




CONTAINS




! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** MOISTURE ROUTERS
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- MOISTURE ROUTING FOR VADOSE ZONE BELOW ROOTING DEPTH
  ! -------------------------------------------------------------
  !  *** Note *** All values are assumed to be converted to depths
  !               before being sent to this subroutine
  ! --------------------------------------------------------------
  SUBROUTINE VadoseZoneMoistureRouter(Inflow,K,TN,PSDI,SMI,Tolerance,KunsatMethod,IterMax,SMN,Excess,Outflow,Delta)
    INTEGER,INTENT(IN)  :: IterMax,KunsatMethod
    REAL(8),INTENT(IN)  :: Inflow,K,TN,PSDI,SMI,Tolerance
    REAL(8),INTENT(OUT) :: SMN,Excess,Outflow,Delta

    !Local variables
    REAL(8) :: SMHigh,SMLow,FC
    
    !Correction for full saturation (total porosity)
    Excess = TN - SMI - Inflow  + K
    Excess = MAX(0d0 , -Excess)
    
    !If Excess is greater than zero, no need for iterative solution
    IF (Excess .GT. 0.0) THEN
        Delta   = 0.0
        SMN     = TN
        Outflow = K
        RETURN
    END IF

    !Iterative moisture routing
    SMLow  = 0.0
    SMHigh = TN
    FC     = 0.5 * TN  !When calling the solver, set FC to half of TN so that there won't be any division-by-zero errors. ETc and hence FC are not used anyways.
    CALL MixedIterMethod(SMI,0d0,0d0,FC,TN,PSDI,K,Inflow,Excess,Tolerance,KunsatMethod,IterMax,SMHigh,SMLow,SMN,Delta)
    
    !If converged compute additional terms
    IF (Delta .EQ. 0.0) Outflow = Kunsat(SMN,TN,PSDI,K,Tolerance,KunsatMethod)

  END SUBROUTINE VadoseZoneMoistureRouter


  ! -------------------------------------------------------------
  ! --- MOISTURE ROUTING FOR NON-PONDED LAND USE
  ! -------------------------------------------------------------
  !  *** Note *** All values are assumed to be converted to depths
  !               before being sent to this subroutine
  ! --------------------------------------------------------------
  SUBROUTINE NonPondedLUMoistureRouter(Precip,SMAX,SMI,ETc,K,TN,FC,WP,PSDI,Inflow,Tolerance,KunsatMethod,IterMax,SMN,Runoff,FILTRN,ET,DP,Excess,Delta)
    REAL(8),INTENT(IN)  :: Precip,SMAX,SMI,ETc,K,TN,FC,WP,PSDI,Inflow,Tolerance
    INTEGER,INTENT(IN)  :: KunsatMethod,IterMax
    REAL(8),INTENT(OUT) :: SMN,Runoff,FILTRN,DP,ET,Excess,Delta

    !Local variables
    REAL(8) :: SMHigh,SMLow,TotalInflow

    !Compute infiltration and runoff of precipitation
    CALL SCSMethod_HELP(Precip,SMI,SMAX,FC,TN,Runoff,FILTRN)
    
    !Total inflow
    TotalInflow = Inflow + FILTRN

    !Correction for full saturation (total porosity)
    Excess = TN - SMI - TotalInflow + ETc + K
    Excess = MAX(0.0 , -Excess)
    
    !If Excess is greater than zero, than no need to solve iteratively
    IF (Excess .GT. 0.0) THEN
        Delta = 0.0
        SMN   = TN
        DP    = K
        ET    = ETc
        RETURN
    END IF
    
    !Iterative moisture routing
    SMLow       = 0.0
    SMHigh      = TN
    CALL MixedIterMethod(SMI,ETc,WP,FC,TN,PSDI,K,TotalInflow,Excess,Tolerance,KunsatMethod,IterMax,SMHigh,SMLow,SMN,Delta)

    !If converged, compute additional terms
    IF (Delta .EQ. 0.0) THEN
      ET = calcET(ETc,WP,FC,SMN)
      DP = Kunsat(SMN,TN,PSDI,K,Tolerance,KunsatMethod)
    END IF  
    
 END SUBROUTINE NonPondedLUMoistureRouter
 
 
 
 
  ! -------------------------------------------------------------
  ! --- MOISTURE ROUTING FOR PONDED LAND USE
  ! -------------------------------------------------------------
  !  *** Note *** All values are assumed to be converted to depths
  !               before being sent to this subroutine
  ! --------------------------------------------------------------
  SUBROUTINE PondedLUMoistureRouter(Precip,SMAX,PondDepth,SMI,ETc,K,TN,FC,WP,PSDI,Inflow,Tolerance,KunsatMethod,IterMax,SMN,Runoff,Drain,FILTRN,ET,DP,Excess,Delta)
    REAL(8),INTENT(IN)  :: Precip,SMAX,PondDepth,SMI,ETc,K,TN,FC,WP,PSDI,Inflow,Tolerance
    INTEGER,INTENT(IN)  :: KunsatMethod,IterMax
    REAL(8),INTENT(OUT) :: SMN,Runoff,Drain,FILTRN,DP,ET,Excess,Delta

    !Local variables
    REAL(8) :: SMLow,SMHigh,TotalInflow,TotalOutflow,SMMax
    
    !Set SMMax (safegurad for user entering a negative pond depth)
    SMMax = MAX(TN+PondDepth , TN)
    
    !Compute infiltration and runoff of precipitation
    IF (Precip .EQ. 0.0) THEN
        Runoff = 0.0
        FILTRN = 0.0
    ELSE
        IF (SMI .GE. SMMax) THEN
            Runoff = Precip
            FILTRN = 0.0
        ELSEIF (SMI .LT. SMMax  .AND.  SMI .GT. TN) THEN
            Runoff = MAX(SMI + Precip - SMMax  ,  0.0)
            FILTRN = MIN(MAX(Precip - Runoff , 0.0) , Precip)
        ELSEIF (SMI .EQ. TN) THEN
            Runoff = MAX(SMI + Precip - SMMax  ,  0.0)
            FILTRN = MIN(MAX(Precip - Runoff , 0.0) , Precip)
        ELSEIF (SMI .LT. TN) THEN
            CALL SCSMethod_HELP(Precip,SMI,SMAX,FC,TN,Runoff,FILTRN)
            IF (PondDepth .GT. 0.0) THEN
              FILTRN = FILTRN + MIN(Runoff , PondDepth)
              Runoff = MAX(Runoff - PondDepth  , 0.0)
            END IF
        END IF
        Runoff = Precip - FILTRN
    END IF

    !Pond drainage
    Drain = MAX(SMI - SMMax  ,  0.0)
    
    !Total inflow
    TotalInflow = Inflow + FILTRN
    
    !Correction for maximum moisture (total porosity + ponding depth)
    Excess = SMMax - SMI - TotalInflow + ETc + K + Drain
    Excess = MAX(0.0 , -Excess)
    
    !If Excess greater than zero, no need for itertiave solution
    IF (Excess .GT. 0.0) THEN
        Delta = 0.0
        SMN   = SMMax
        DP    = K
        ET    = ETc
        RETURN
    END IF
        
    !Total outflow 
    TotalOutflow = Drain + Excess
    
    !Iterative moisture routing
    SMLow  = 0.0
    SMHigh = SMMax
    CALL MixedIterMethod(SMI,ETc,WP,FC,TN,PSDI,K,TotalInflow,TotalOutflow,Tolerance,KunsatMethod,IterMax,SMHigh,SMLow,SMN,Delta)
    
    !If converged, compute additional terms
    IF (Delta .EQ. 0.0) THEN
      ET   = calcET(ETc,WP,FC,SMN)
      DP   = Kunsat(SMN,TN,PSDI,K,Tolerance,KunsatMethod)
    END IF

  END SUBROUTINE PondedLUMoistureRouter
  
  
  ! -------------------------------------------------------------
  ! --- SOIL MOISTURE FUNCTION
  ! -------------------------------------------------------------
  FUNCTION SM_Function(SMI,ETc,WP,FC,TN,PSDI,K,Inflow,Outflow,SM,Toler,KunsatMethod) RESULT(rValue)
    REAL(8),INTENT(IN) :: SMI,ETc,WP,FC,TN,PSDI,K,Inflow,Outflow,SM,Toler
    INTEGER,INTENT(IN) :: KunsatMethod
    REAL(8)            :: rValue
    
    rValue = SM - SMI - Inflow + calcET(ETc,WP,FC,SM) + Kunsat(SM,TN,PSDI,K,Toler,KunsatMethod) + Outflow
    
  END FUNCTION SM_Function


  ! -------------------------------------------------------------
  ! --- DERIVATIVE OF SOIL MOISTURE FUNCTION
  ! -------------------------------------------------------------
  FUNCTION dSM_Function(ETc,WP,FC,TN,PSDI,K,SM,Toler,KunsatMethod) RESULT(rValue)
    REAL(8),INTENT(IN) :: ETc,WP,FC,TN,PSDI,K,SM,Toler
    INTEGER,INTENT(IN) :: KunsatMethod
    REAL(8)            :: rValue
    
    rValue = 1d0 + dET(ETc,WP,FC,SM) + dKunsat(SM,TN,PSDI,K,Toler,KunsatMethod)
    
  END FUNCTION dSM_Function


  
! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** DEMAND CALCULATORS
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- WATER DEMAND COMPUTATION FOR NON-PONDED AG CROP
  ! -------------------------------------------------------------
  !  *** Note *** All values are assumed to be converted to depths
  !               before being sent to this subroutine
  ! --------------------------------------------------------------
  SUBROUTINE NonPondedCropDemand(Precip,SMAX,Inflow,fRF,fRU,LF,ETc,K,TN,FC,WP,TRGSM,PSDI,SMI,Tolerance,KunsatMethod,IterMax,DemandRaw,Demand,Delta)
    REAL(8),INTENT(IN)  :: Precip,SMAX,Inflow,fRF,fRU,LF,ETc,K,TN,FC,WP,TRGSM,PSDI,SMI,Tolerance
    INTEGER,INTENT(IN)  :: KunsatMethod,IterMax
    REAL(8),INTENT(OUT) :: DemandRaw,Demand,Delta

    !Local variables
    REAL(8) :: F,DF,DP,DPmin,Runoff,FILTRN,SM,ET
    INTEGER :: Iter

    !Compute infiltration and runoff of precipitation
    CALL SCSMethod_HELP(Precip,SMI,SMAX,FC,TN,Runoff,FILTRN)
    
    !Compute ET at target soil moisture
    ET = calcET(ETc,WP,FC,TRGSM)

    DP        = Kunsat(TRGSM,TN,PSDI,K,Tolerance,KunsatMethod)
    DemandRaw = TRGSM+ET+DP-FILTRN-Inflow-SMI       !This is equivalent to the infiltrated amount of applied water
    DPmin     = MIN(DemandRaw*LF , K)

    !If Demand is negative, there is no real demand for water
    IF (DemandRaw .LT. 0d0) DemandRaw = 0d0

    !Check if minimum deep percolation for leaching is achieved
    IF (DP .GE. DPmin) THEN
      Demand = DemandRaw/(1d0-(fRF-fRU))  !Increase demand by the net return flow amount to get the actual demand for applied water
      Delta  = 0d0
      RETURN
    END IF

    !If at total porosity, deep perc is still not enough, then that is the most we get
    DemandRaw = TN+ETc+K-FILTRN-Inflow-SMI
    IF (K .LT. DemandRaw*LF) THEN
      Demand = DemandRaw/(1d0-(fRF-fRU))  !Increase demand by the net return flow amount to get the actual demand for applied water
      Delta  = 0d0
      RETURN
    END IF  
    
    !If leaching requirement is not met, solve for demand that meets it
    Iter     = 0
    SM       = TN-f_rTolerFrac*Tolerance   !Initial estimate is the total porosity less some tolerence; moisture should end up being larger than field capacity anyways
    DO
      F      = SM - SMI - FILTRN - Inflow + calcET(ETc,WP,FC,SM) + (1d0-1d0/LF)*Kunsat(SM,TN,PSDI,K,Tolerance,KunsatMethod) 
      dF     = 1d0 + dET(ETc,WP,FC,SM) + (1d0-1d0/LF)*dKunsat(SM,TN,PSDI,K,Tolerance,KunsatMethod)
      Delta  = F/DF
      SM     = SM-Delta
      IF (MAX(ABS(F),ABS(Delta)) .GE. Tolerance) THEN
        Iter = Iter+1
        IF (Iter .GT. IterMax) EXIT
      ELSE
        DemandRaw = Kunsat(SM,TN,PSDI,K,Tolerance,KunsatMethod)/LF 
        Demand    = DemandRaw/(1d0-(fRF-fRU))              !Increase Demand to include the net return flow
        Delta     = 0d0
        EXIT
      END IF
    END DO

  END SUBROUTINE NonPondedCropDemand
  
  
  ! -------------------------------------------------------------
  ! --- WATER DEMAND COMPUTATION FOR PONDED AG CROP
  ! -------------------------------------------------------------
  !  *** Note *** All values are assumed to be converted to depths
  !               before being sent to this subroutine
  ! --------------------------------------------------------------
  SUBROUTINE PondedCropDemand(Precip,SMAX,Inflow,dRF,dRU,PondDepth,ETc,K,TN,FC,SMI,DemandRaw,Demand)
    REAL(8),INTENT(IN)  :: Precip,SMAX,Inflow,dRF,dRU,PondDepth,ETc,K,TN,FC,SMI
    REAL(8),INTENT(OUT) :: DemandRaw,Demand

    !Local variables
    REAL(8) :: Runoff,FILTRN,Drain,SMMax
    
    SMMax = MAX(TN+PondDepth , TN)

    !Compute infiltration and runoff of precipitation
    IF (Precip .EQ. 0.0) THEN
        Runoff = 0.0
        FILTRN = 0.0
    ELSE
        IF (SMI .GE. SMMax) THEN
            Runoff = Precip
            FILTRN = 0.0
        ELSEIF (SMI .LT. SMMax  .AND.  SMI .GT. TN) THEN
            Runoff = MAX(SMI + Precip - SMMax  ,  0.0)
            FILTRN = MIN(MAX(Precip - Runoff , 0.0) , Precip)
        ELSEIF (SMI .EQ. TN) THEN
            Runoff = MAX(SMI + Precip - SMMax  ,  0.0)
            FILTRN = MIN(MAX(Precip - Runoff , 0.0) , Precip)
        ELSEIF (SMI .LT. TN) THEN
            CALL SCSMethod_HELP(Precip,SMI,SMAX,FC,TN,Runoff,FILTRN)
            IF (PondDepth .GT. 0.0) THEN
                FILTRN = FILTRN + MIN(Runoff , PondDepth)
                Runoff = MAX(Runoff - PondDepth  , 0.0)
            END IF
        END IF
        Runoff = Precip - FILTRN
    END IF

    !Raw demand; if it is negative, there is no real demand for water
    Drain     = MAX(SMI - SMMax  ,  0.0)
    DemandRaw = MAX(SMMax + Drain + ETc + K - FILTRN - Inflow - SMI  ,  0.0)
    
    !Compute total demand that inludes the return flow and re-use
    Demand = MAX(DemandRaw + dRF - dRU, 0.0)

  END SUBROUTINE PondedCropDemand




! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** HYDRAULIC CUNDUCTIVITY CALCULATORS
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- GATEWAY FUNCTION FOR UNSATURATED HYDRAULIC CONDUCTIVITY
  ! -------------------------------------------------------------
  FUNCTION Kunsat(SM,TN,PSDI,Ksat,Toler,KunsatMethod) RESULT(Ku)
    REAL(8),INTENT(IN) :: SM,TN,PSDI,Ksat,Toler
    INTEGER,INTENT(IN) :: KunsatMethod
    REAL(8)            :: Ku

    SELECT CASE(KunsatMethod)

      !Campbell's approach
      CASE (f_iCampbell)
        Ku = Kunsat_Campbell(SM,TN,PSDI,Ksat)

      !van Genuchten approach
      CASE (f_ivanGenuchten)
        Ku = Kunsat_vanGenuchten(SM,TN,PSDI,Ksat,Toler)

    END SELECT

  END FUNCTION Kunsat


  ! -------------------------------------------------------------
  ! --- GATEWAY FUNCTION FOR SLOPE OF UNSATURATED HYDRAULIC CONDUCTIVITY
  ! -------------------------------------------------------------
  FUNCTION dKunsat(SM,TN,PSDI,Ksat,Toler,KunsatMethod) RESULT(dKu)
    REAL(8),INTENT(IN) :: SM,TN,PSDI,Ksat,Toler
    INTEGER,INTENT(IN) :: KunsatMethod
    REAL(8)            :: dKu

    SELECT CASE(KunsatMethod)

      !Campbell's approach
      CASE (f_iCampbell)
        dKu = dKunsat_Campbell(SM,TN,PSDI,Ksat)

      !van Genuchten approach
      CASE (f_ivanGenuchten)
        dKu = dKunsat_vanGenuchten(SM,TN,PSDI,Ksat,Toler)

    END SELECT

  END FUNCTION dKunsat


  ! -------------------------------------------------------------
  ! --- UNSATURATED HYDRAULIC CONDUCTIVITY - CAMPBELL APPROACH
  ! -------------------------------------------------------------
  FUNCTION Kunsat_Campbell(SM,TN,PSDI,Ksat) RESULT(Kunsat)
    REAL(8),INTENT(IN)  :: SM,TN,PSDI,Ksat
    REAL(8)             :: Kunsat

    !Local variables
    REAL(8) :: A

    !Unsaturated hydraulic conductivity
    IF (SM .LE. 0.0) THEN
      Kunsat = 0.0
    ELSEIF (SM .GT. TN) THEN
      Kunsat = Ksat
    ELSE
      A      = 3d0 + 2d0/PSDI
      Kunsat = Ksat * FEXP(A*LOG(SM/TN))  ! = Ksat*(SM/TN)**A
    END IF

  END FUNCTION Kunsat_Campbell


  ! -------------------------------------------------------------
  ! --- DERIVATIVE OF UNSATURATED HYDRAULIC CONDUCTIVITY - CAMPBELL APPROACH
  ! -------------------------------------------------------------
  FUNCTION dKunsat_Campbell(SM,TN,PSDI,Ksat) RESULT(dKunsat)
    REAL(8),INTENT(IN)  :: SM,TN,PSDI,Ksat
    REAL(8)             :: dKunsat

    !Local variables
    REAL(8) :: A

    !Derivative of unsaturated hydraulic conductivity w.r.t. soil moisture
    IF (SM .LE. 0.0) THEN
      dKunsat = 0.0
    ELSEIF (SM .GT. TN) THEN
      dKunsat = 0.0
    ELSE
      A       = 3d0 + 2d0/PSDI
      dKunsat = Ksat * A/TN * FEXP((A-1d0)*LOG(SM/TN))  ! = Ksat*A/TN*(SM/TN)**(A-1d0)
    END IF

  END FUNCTION dKunsat_Campbell


  ! -------------------------------------------------------------
  ! --- UNSATURATED HYDRAULIC CONDUCTIVITY - VAN GENUCHTEN APPROACH
  ! -------------------------------------------------------------
  FUNCTION Kunsat_vanGenuchten(SM,TN,PSDI,Ksat,Toler) RESULT(Kunsat)
    REAL(8),INTENT(IN)  :: SM,TN,PSDI,Ksat,Toler
    REAL(8)             :: Kunsat

    !Local variables
    REAL(8) :: m,SM_Check,Kunsat_Check

    !If moisture is above TN, Kunsat = Ksat
    IF (SM .GE. TN) THEN
      Kunsat = Ksat

    !If SM<0, Kunsat = 0
    ELSEIF (SM .LE. 0.0) THEN
      Kunsat = 0.0

    !If moisture is below TN and above zero, proceed normally 
    ELSE
      m        = PSDI/(PSDI+1d0)
      SM_Check = TN - f_rTolerFrac*Toler
      IF (SM .LT. SM_Check) THEN
        Kunsat = KunsatFunc(SM,TN,Ksat,m)
      ELSE
        Kunsat_Check = KunsatFunc(SM_Check,TN,Ksat,m)
        Kunsat       = (SM-SM_Check) * (Kunsat_Check-Ksat) / (SM_Check-TN) + Kunsat_Check
      END IF
    END IF
    
  
  CONTAINS
  
  
    FUNCTION KunsatFunc(SM,TN,Ksat,m) RESULT(Ret)
      REAL(8),INTENT(IN) :: SM,TN,Ksat,m
      REAL(8)            :: Ret
      
      !Local variables
      REAL(8) :: ratio,F1,F2
      
      ratio = SM/TN
      F2    = 1d0 - FEXP((1d0/m)*LOG(ratio))  ! = 1d0 - ratio**(1d0/m) 
      F1    = 1d0 - FEXP(m*LOG(F2))           ! = 1d0 - F2**m
      Ret   = Ksat * SQRT(ratio) * F1*F1

    END FUNCTION KunsatFunc              

  END FUNCTION Kunsat_vanGenuchten


  ! -------------------------------------------------------------
  ! --- DERIVATIVE OF UNSATURATED HYDRAULIC CONDUCTIVITY - VAN GENUCHTEN APPROACH
  ! -------------------------------------------------------------
  FUNCTION dKunsat_vanGenuchten(SM,TN,PSDI,Ksat,Toler) RESULT(dKunsat)
    REAL(8),INTENT(IN) :: SM,TN,PSDI,Ksat,Toler
    REAL(8)            :: dKunsat

    !Local variables
    REAL(8) :: m,ratio,F1,dF1,F2,dF2,SM_Check,Kunsat_Check,rValue,rPowF2

    !If SM>TN slope is zero
    IF (SM .GE. TN) THEN
      dKunsat = 0.0
    
    !If SM<0.0 slope is zero
    ELSEIF (SM .LE. 0.0) THEN
      dKunsat = 0.0
      
    !otherwise
    ELSE
      m        = PSDI/(PSDI+1d0)
      SM_Check = TN - f_rTolerFrac*Toler
      IF (SM .LT. SM_Check) THEN
        ratio   = SM/TN
        rValue  = FEXP((1d0/m)*LOG(ratio))  ! = ratio**(1d0/m)
        F2      = 1d0 - rValue  ;  dF2 = -1d0/(TN*m) * (rValue/ratio)
        rPowF2  = FEXP(m*LOG(F2))           ! = F2**m
        F1      = 1d0 - rPowF2  ;  dF1 = -m * (rPowF2/F2) * dF2
        dKunsat =  Ksat * 1d0/SQRT(ratio) * F1*F1 / (2d0 * TN)  &
                 + 2d0 * Ksat * SQRT(ratio) * F1 * dF1
      ELSE
        Kunsat_Check = Kunsat(SM,TN,PSDI,Ksat,Toler,f_ivanGenuchten)
        dKunsat      = (Kunsat_Check - Ksat) / (SM_Check - TN)
      END IF
    END IF
    
  END FUNCTION dKunsat_vanGenuchten



! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** ET CALCULATORS
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- ET
  ! -------------------------------------------------------------
  FUNCTION calcET(ETc,WP,FC,SM) RESULT(valET)
    REAL(8),INTENT(IN) :: ETc,WP,FC,SM
    REAL(8)            :: valET
    
    IF (SM .GT. WP) THEN
      valET = ETc * MIN(1d0 , 2d0*(SM-WP)/(FC-WP))
    ELSE
      valET = 0.0
    END IF
    
  END FUNCTION calcET
  
  
  ! -------------------------------------------------------------
  ! --- DERIVATIVE OF ET
  ! -------------------------------------------------------------
  FUNCTION dET(ETc,WP,FC,SM) RESULT(ETDeriv)
    REAL(8),INTENT(IN) :: ETc,WP,FC,SM
    REAL(8)            :: ETDeriv
    
    ETDeriv = 0.0
    IF (SM.GE.WP .AND. SM.LT.5d-1*(FC+WP)) THEN
      ETDeriv = 2d0 * ETc / (FC-WP)
    ELSE
      ETDeriv = 0.0
    END IF
    
  END FUNCTION dET
  



! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** ITERATIVE SOLVER
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- MIXED NEWTON-RAPHSON AND BISECTION METHODS
  ! -------------------------------------------------------------
  SUBROUTINE MixedIterMethod(SMI,ETc,WP,FC,TN,PSDI,K,Inflow,Outflow,Toler,KunsatMethod,IterMax,SMHigh,SMLow,SM,Delta)
    REAL(8),INTENT(IN)  :: SMI,ETc,WP,FC,TN,PSDI,K,Inflow,Outflow,Toler
    REAL(8)             :: SMHigh,SMLow
    INTEGER,INTENT(IN)  :: KunsatMethod,IterMax
    REAL(8),INTENT(OUT) :: SM,Delta
    
    !Local variables
    INTEGER :: iter
    REAL(8) :: SM_test,F_SM_test,DF_SM_test,F_SMLow,SM_test_Check
    LOGICAL :: Converged,UseNewtonRaphson

    !Initialize 
    SM               = SMI
    Converged        = .FALSE.
    UseNewtonRaphson = .TRUE.
    
    !Set SM_test_Check based on the method used to compute Kunsat 
    SELECT CASE (KunsatMethod)
      CASE (f_iCampbell)
        SM_test_Check = 0.0
      
      CASE (f_ivanGenuchten)
        SM_test_Check = f_rTolerFrac * Toler
    END SELECT
    
    !Check if high-end or low-end value of SM satisfies function
    IF (ABS(SM_Function(SMI,ETc,WP,FC,TN,PSDI,K,Inflow,Outflow,SMHigh,Toler,KunsatMethod)) .LT. Toler) THEN
      SM        = SMHigh
      Converged = .TRUE.
    ELSEIF (ABS(SM_Function(SMI,ETc,WP,FC,TN,PSDI,K,Inflow,Outflow,SMLow,Toler,KunsatMethod)) .LT. Toler) THEN
      SM        = SMLow
      Converged = .TRUE.
    ELSE
      iter = 0
      DO 
        !Take a Newton-Raphson step
        IF (UseNewtonRaphson) THEN
          F_SM_test  = SM_Function(SMI,ETc,WP,FC,TN,PSDI,K,Inflow,Outflow,SM,Toler,KunsatMethod)
          DF_SM_test = dSM_Function(ETc,WP,FC,TN,PSDI,K,SM,Toler,KunsatMethod)
          Delta      = F_SM_test /DF_SM_test
          !Check if SM is already close enough to the root; if it is don't update it   
          IF (ABS(Delta) .LT. Toler) THEN
            IF (ABS(F_SM_test) .LT. Toler) THEN
              Converged = .TRUE.
              EXIT
            END IF
          END IF
          IF (iter .LT. 11) THEN
              SM_test = SM - Delta
          ELSEIF (iter .LT. 21) THEN
              SM_test = SM - (0.5 * Delta)
              IF (F_SM_Test .GT. 0.0) THEN
                  IF (SM .LT. SMHigh) SMHigh = SM
              ELSE
                  IF (SM .GT. SMLow) SMLow = SM
              END IF
          ELSE
              SM_test          = SM - (0.5 * Delta)
              UseNewtonRaphson = .FALSE.
          END IF
          IF (SM_test.GT.TN-SM_test_Check .OR. SM_test.LT.0.0) UseNewtonRaphson = .FALSE.
      
        !Check if new moisture is beyond limits; if so take a bisection step
        ELSE
          SM_test   = (SMHigh+SMLow)/2d0
          F_SM_Test = SM_Function(SMI,ETc,WP,FC,TN,PSDI,K,Inflow,Outflow,SM_test,Toler,KunsatMethod)
          F_SMLow   = SM_Function(SMI,ETc,WP,FC,TN,PSDI,K,Inflow,Outflow,SMLow,Toler,KunsatMethod)
          IF (F_SM_Test*F_SMLow .GE. 0.0) THEN
            SMLow  = SM_test
          ELSE
            SMHigh = SM_test
          END IF
          Delta = ABS(SMHigh-SMLow)
        END IF
        
        SM = SM_test
        
        !Convergence achieved because we are exactly at the root
        IF (F_SM_test .EQ. 0.0) THEN
          Converged = .TRUE.
          EXIT
        END IF
        
        !Convergence achieved because we are close to the root
        IF (ABS(Delta) .LT. Toler) THEN
          IF (ABS(F_SM_test) .LT. Toler) THEN
            Converged = .TRUE.
            EXIT
          END IF
        END IF
        
        !Convergence is not achieved
        iter = iter+1
        IF (iter .GT. IterMax) EXIT
                     
      END DO
    END IF
    
    IF (Converged) THEN
      Delta = 0.0
    ELSE
      Delta = MAX(ABS(Delta) , ABS(F_SM_test))
    END IF
       
  END SUBROUTINE MixedIterMethod
  
END MODULE