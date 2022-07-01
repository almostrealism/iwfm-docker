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
MODULE RainfallRunoff
  IMPLICIT NONE


  ! -------------------------------------------------------------
  ! --- PUBLIC ENTITIES
  ! -------------------------------------------------------------
  PRIVATE
  PUBLIC :: SCSMethod_HELP




CONTAINS



  ! -------------------------------------------------------------
  ! --- SCS METHOD WITH HELP MODEL MODIFICATION (Schroeder, et. al, 1994)
  ! -------------------------------------------------------------
  SUBROUTINE SCSMethod_HELP(RAIN,SMI,SMAX,FC,TN,ROFF,FILTRN)
    REAL(8),INTENT(IN) ::RAIN ,SMI ,SMAX ,FC ,TN
    REAL(8),INTENT(OUT)::ROFF ,FILTRN

    !Local variables
    REAL(8)::S ,SI ,SP, SMI_Work 

    !Initialize variables
    ROFF   = 0.0
    FILTRN = RAIN

    !Compute runoff and infiltration based on modified SCS method
    IF (RAIN.GT.0.0) THEN
      IF (SMI.GT.FC/2.0) THEN
        SMI_Work = MIN(SMI,TN)
        S        = SMAX*(1.0-(SMI_Work-FC/2.0)/(TN-FC/2.0))
      ELSE
        S = SMAX
      END IF
      SI = 0.2*S
      SP = 0.8*S
      IF (RAIN.GT.SI) THEN
        ROFF   = RAIN-SI
        ROFF   = MIN(ROFF*ROFF/(RAIN+SP) , RAIN)
        FILTRN = RAIN-ROFF
      END IF
    END IF

  END SUBROUTINE SCSMethod_HELP



END MODULE