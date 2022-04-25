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
MODULE Class_PairedData
  USE MessageLogger     , ONLY: SetLastMessage        , &
                                f_iFatal
  USE IOInterface
  USE AbstractFunction  , ONLY: AbstractFunctionType
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
  PUBLIC :: PairedDataType             


  ! -------------------------------------------------------------
  ! --- PAIRED DATA TYPE
  ! -------------------------------------------------------------
  TYPE,EXTENDS(AbstractFunctionType) :: PairedDataType
    INTEGER             :: NPoints   =  0        !Number of data points
    REAL(8),ALLOCATABLE :: XPoint(:)             !X value
    REAL(8),ALLOCATABLE :: YPoint(:)             !Y value corresponding to X value
  CONTAINS
    PROCEDURE,PASS,PRIVATE :: PairedData_
    PROCEDURE,PASS,PRIVATE :: PairedData_ReadFromFile
    PROCEDURE,PASS         :: Kill                      => PairedData_Kill  
    PROCEDURE,PASS         :: GetNPoints
    PROCEDURE,PASS         :: GetYPoints
    PROCEDURE,PASS         :: GetXPoints
    PROCEDURE,PASS         :: Evaluate                  => PairedData_Interpolate                
    PROCEDURE,PASS         :: InverseEvaluate           => PairedData_CompInterpolate            
    PROCEDURE,PASS         :: Derivative                => PairedData_Derivative                 
    PROCEDURE,PASS         :: CheckGradientMonotonicity => PairedData_CheckGradientMonotonicity  
    PROCEDURE,PASS         :: WriteToFile               => PairedData_WriteToFile
    GENERIC,PUBLIC         :: New                       => PairedData_                          , &
                                                           PairedData_ReadFromFile
  END TYPE PairedDataType


  ! -------------------------------------------------------------
  ! --- MISCELLENEOUS DATA
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                   :: ModNameLen = 18
  CHARACTER(LEN=ModNameLen),PARAMETER :: ModName    ='Class_PairedData::'




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
  ! --- INSTANTIATE DATA BASED ON USER SPECIFIED VALUES
  ! -------------------------------------------------------------
  SUBROUTINE PairedData_(PairedData,NPoints,XPoint,YPoint,iStat)
    CLASS(PairedDataType),INTENT(OUT) :: PairedData
    INTEGER,INTENT(IN)                :: NPoints
    REAL(8),INTENT(IN)                :: XPoint(:),YPoint(:)
    INTEGER,INTENT(OUT)               :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+11) :: ThisProcedure = ModName // 'PairedData_'
    INTEGER                      :: ErrorCode
    CHARACTER                    :: cErrMessage*500
    
    !Initialize
    iStat = 0
    
    DEALLOCATE (PairedData%XPoint , PairedData%YPoint , STAT=ErrorCode)
    
    PairedData%NPoints = NPoints
    
    ALLOCATE (PairedData%XPoint(NPoints) , PairedData%YPoint(NPoints) , STAT=ErrorCode , ERRMSG=cErrMessage)
    IF (ErrorCode .NE. 0) THEN
        CALL SetLastMessage('Error in allocating memory for paired data!'//NEW_LINE('x')//TRIM(cErrMessage),f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    PairedData%XPoint = XPoint
    PairedData%YPoint = YPoint
    
  END SUBROUTINE PairedData_


  ! -------------------------------------------------------------
  ! --- READ DATA FROM FILE
  ! -------------------------------------------------------------
  SUBROUTINE PairedData_ReadFromFile(PairedData,InFile,iStat)
    CLASS(PairedDataType),INTENT(OUT) :: PairedData
    TYPE(GenericFileType)             :: InFile
    INTEGER,INTENT(OUT)               :: iStat
    
    !Local variables
    INTEGER :: NPoints
    
    CALL InFile%ReadData(NPoints,iStat)  ;  IF (iStat .EQ. -1) RETURN   ;  PairedData%NPoints = NPoints
    ALLOCATE (PairedData%XPoint(NPoints)  , PairedData%YPoint(NPoints))
    CALL InFile%ReadData(PairedData%XPoint,iStat)  ;  IF (iStat .EQ. -1) RETURN 
    CALL InFile%ReadData(PairedData%YPoint,iStat)  
    
  END SUBROUTINE PairedData_ReadFromFile



  
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
  ! --- KILL OBJECT
  ! -------------------------------------------------------------
  SUBROUTINE PairedData_Kill(PairedData)
    CLASS(PairedDataType)::PairedData
    
    !Local variables
    INTEGER :: ErrorCode

    PairedData%NPoints = 0
    DEALLOCATE (PairedData%XPoint ,PairedData%YPoint , STAT=ErrorCode)  

  END SUBROUTINE PairedData_Kill




! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** GETTERS
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- GET NUMBER OF DATA POINTS
  ! -------------------------------------------------------------
  PURE FUNCTION GetNPoints(PairedData) RESULT(NPoints)
    CLASS(PairedDataType),INTENT(IN) :: PairedData
    INTEGER                          :: NPoints
  
    NPoints = PairedData%NPoints
    
  END FUNCTION GetNPoints
  
  
  ! -------------------------------------------------------------
  ! --- GET Y DATA POINTS
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetYPoints(PairedData,YPoints)
    CLASS(PairedDataType),INTENT(IN) :: PairedData
    REAL(8),INTENT(OUT)              :: YPoints(:)
  
    YPoints = PairedData%YPoint
    
  END SUBROUTINE GetYPoints

  
  ! -------------------------------------------------------------
  ! --- GET X DATA POINTS
  ! -------------------------------------------------------------
  PURE SUBROUTINE GetXPoints(PairedData,XPoints)
    CLASS(PairedDataType),INTENT(IN) :: PairedData
    REAL(8),INTENT(OUT)              :: XPoints(:)
  
    XPoints = PairedData%XPoint
    
  END SUBROUTINE GetXPoints  
  
  
  
  
! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** METHODS
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- INTERPOLATE THE DATA
  ! -------------------------------------------------------------
  FUNCTION PairedData_Interpolate(Func,XP) RESULT(YP)
    CLASS(PairedDataType),INTENT(IN) :: Func
    REAL(8),INTENT(IN)               :: XP
    REAL(8)                          :: YP

    !Local variables
    INTEGER :: NPoints,IT2
    REAL(8) :: Derivative
    
    !Initialize
    NPoints = Func%NPoints
    
    ASSOCIATE (pXPoint => Func%XPoint , &
               pYPoint => Func%YPoint )
    
        !Find where XP falls in the rating table
        IF (XP .GT. pXPoint(NPoints)) THEN
          IT2 = NPoints
        ELSE
          DO IT2=1,NPoints
            IF (XP .LT. pXPoint(IT2)) THEN
              EXIT
            !If XP coincides with data point, return the Y point in the rating table
            ELSEIF (XP .EQ. pXPoint(IT2)) THEN
              YP = pYPoint(IT2)
              RETURN
            END IF
          END DO
        END IF
        
        !Interpolate 
        Derivative = Func%Derivative(XP) 
        YP         = Derivative * (XP-pXPoint(IT2)) + pYPoint(IT2)
        
    END ASSOCIATE

  END FUNCTION PairedData_Interpolate


  ! -------------------------------------------------------------
  ! --- INTERPOLATE XP GIVEN YP (COMPLEMENTARY INTERPOLATION)
  ! -------------------------------------------------------------
  FUNCTION PairedData_CompInterpolate(Func,YP) RESULT(XP)
    CLASS(PairedDataType),INTENT(IN) :: Func
    REAL(8),INTENT(IN)               :: YP
    REAL(8)                          :: XP

    !Local variables
    INTEGER :: NPoints,IT2
    REAL(8) :: Derivative
    REAL(8) :: XPoint(Func%NPoints),YPoint(Func%NPoints)
    
    !Initialize
    NPoints = Func%NPoints
    XPoint  = Func%XPoint 
    YPoint  = Func%YPoint

    !Derivative at YP
    Derivative = CompDerivative(Func,YP) 
       
    !Find where YP falls in the rating table
    IF (YP .GT. YPoint(NPoints)) THEN
      IT2 = NPoints
    ELSE
      DO IT2=1,NPoints
        IF (YP .LT. YPoint(IT2)) THEN
          EXIT
        !If YP coincides with data point, return the X point in the rating table
        ELSEIF (YP .EQ. YPoint(IT2)) THEN
          XP = XPoint(IT2)
          RETURN
        END IF
      END DO
    END IF

    !Interpolate  
    XP = Derivative * (YP-YPoint(IT2)) + XPoint(IT2)

  END FUNCTION PairedData_CompInterpolate


  ! -------------------------------------------------------------
  ! --- INTERPOLATE THE DERIVATIVE OF DATA
  ! -------------------------------------------------------------
  FUNCTION PairedData_Derivative(Func,XP) RESULT(Derivative)
    CLASS(PairedDataType),INTENT(IN) :: Func
    REAL(8),INTENT(IN)               :: XP
    REAL(8)                          :: Derivative

    !Local variables
    INTEGER :: NPoints,IT1,IT2
    REAL(8) :: XPoint(Func%NPoints),YPoint(Func%NPoints)
    
    !Initialize
    NPoints = Func%NPoints
    XPoint  = Func%XPoint 
    YPoint  = Func%YPoint 
       
    !Data needs to be exterpolated beyond the last entry
    IF (XP .GT. XPoint(NPoints)) THEN
      IT2 = NPoints
      IT1 = IT2-1

    !Data needs to be interpolated
    ELSE
      DO IT2=2,NPoints
        IF (XP .LE. XPoint(IT2)) THEN
          IT1 = IT2-1
          EXIT
        END IF
      END DO
    END IF

    !Compute derivative
    Derivative = (YPoint(IT2)-YPoint(IT1))/(XPoint(IT2)-XPoint(IT1))

  END FUNCTION PairedData_Derivative


  ! -------------------------------------------------------------
  ! --- INTERPOLATE THE DERIVATIVE OF DATA W.R.T. YP (COMPLIMENTARY DERIVATIVE)
  ! -------------------------------------------------------------
  FUNCTION CompDerivative(PairedData,YP) RESULT(Derivative)
    CLASS(PairedDataType),INTENT(IN) :: PairedData
    REAL(8),INTENT(IN)               :: YP
    REAL(8)                          :: Derivative

    !Local variables
    INTEGER :: NPoints,IT1,IT2
    REAL(8) :: XPoint(PairedData%NPoints),YPoint(PairedData%NPoints)
    
    !Initialize
    NPoints = PairedData%NPoints
    XPoint  = PairedData%XPoint 
    YPoint  = PairedData%YPoint 

    !Data needs to be exterpolated beyond the last entry
    IF (YP .GT. YPoint(NPoints)) THEN
      IT2        = NPoints
      IT1        = IT2-1

    !Data needs to be interpolated
    ELSE
      DO IT2=2,NPoints
        IF (YP .LE. YPoint(IT2)) THEN
          IT1 = IT2-1
          EXIT
        END IF
      END DO
    END IF

    !Compute derivative
    Derivative = (XPoint(IT2)-XPoint(IT1))/(YPoint(IT2)-YPoint(IT1))

  END FUNCTION CompDerivative


  ! -------------------------------------------------------------
  ! --- CHECK IF THE GRADIENT OF THE PAIRED DATA MONOTONICALLY INCREASES/DECREASES
  ! -------------------------------------------------------------
  FUNCTION PairedData_CheckGradientMonotonicity(PairedData) RESULT(IsMonotonic)
    CLASS(PairedDataType),INTENT(IN) :: PairedData
    LOGICAL                          :: IsMonotonic
    
    !Local variables
    INTEGER :: indx
    REAL(8) :: XPoint(PairedData%NPoints),XP1,XP2,g1,g2
    LOGICAL :: lIncrease 
    
    !Initialize
    IsMonotonic = .TRUE.
    XPoint      = PairedData%XPoint
    
    !If there are only two points, check if the gradient is zero
    IF (PairedData%NPoints .EQ. 2) THEN
      XP1 = (XPoint(1) + XPoint(2)) / 2d0
      g1  = PairedData_Derivative(PairedData,XP1)
      IF (g1 .EQ. 0.0) IsMonotonic = .FALSE.
      RETURN
    END IF
    
    !Check with the first two sections if the paired data is increasing or decreasing
    XP1 = (XPoint(1) + XPoint(2)) / 2d0
    g1  = PairedData_Derivative(PairedData,XP1)
    XP2 = (XPoint(2) + XPoint(3)) / 2d0
    g2  = PairedData_Derivative(PairedData,XP2)
    IF (g1*g2 .LE. 0.0) THEN    !If different signs or same gradient, function derivative is not monotonic
      IsMonotonic = .FALSE.
      RETURN
    END IF
    IF (g1 .GT. 0.0) THEN
      IF (g2 .GE. g1) THEN
        lIncrease = .TRUE.
      ELSE
        lIncrease = .FALSE.
      END IF
    ELSE
      IF (g2 .LE. g1) THEN
        lIncrease = .TRUE.
      ELSE
        lIncrease = .FALSE.
      END IF
    END IF
    
    !Checking with the remaining sections
    g1 = g2
    DO indx=3,PairedData%NPoints-1
      XP2 = (XPoint(indx) + XPoint(indx+1)) / 2d0
      g2  = PairedData_Derivative(PairedData,XP2)
      IF (g1*g2 .LE. 0.0) THEN    !If different signs or same gradient, function derivative is not monotonic
        IsMonotonic = .FALSE.
        RETURN
      END IF
      SELECT CASE (lIncrease)
        CASE (.TRUE.)
          IF (g1 .GT. 0.0) THEN
            IF (g2 .LT. g1) THEN
              IsMonotonic = .FALSE.
              RETURN
            END IF
          ELSE
            IF (g2 .GT. g1) THEN
              IsMonotonic = .FALSE.
              RETURN
            END IF
          END IF
          
        CASE (.FALSE.)
          IF (g1 .GT. 0.0) THEN
            IF (g2 .GT. g1) THEN
              IsMonotonic = .FALSE.
              RETURN
            END IF
          ELSE
            IF (g2 .LT. g1) THEN
              IsMonotonic = .FALSE.
              RETURN
            END IF
          END IF
      END SELECT              
      
      g1 = g2  
          
    END DO

  END FUNCTION PairedData_CheckGradientMonotonicity
  
  
  ! -------------------------------------------------------------
  ! --- WRITE DATA TO FILE
  ! -------------------------------------------------------------
  SUBROUTINE PairedData_WriteToFile(PairedData,OutFile)
    CLASS(PairedDataType),INTENT(IN) :: PairedData
    TYPE(GenericFileType)            :: OutFile
    
    CALL OutFile%WriteData(PairedData%NPoints)
    CALL OutFile%WriteData(PairedData%XPoint)
    CALL OutFile%WriteData(PairedData%YPoint) 
    
  END SUBROUTINE PairedData_WriteToFile


END MODULE