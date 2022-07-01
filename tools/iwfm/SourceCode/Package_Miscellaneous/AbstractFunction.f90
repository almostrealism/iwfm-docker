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
MODULE AbstractFunction
  IMPLICIT NONE
  
  
  ! -------------------------------------------------------------
  ! --- PUBLIC ENTITIES
  ! -------------------------------------------------------------
  PRIVATE
  PUBLIC :: AbstractFunctionType             

  
  ! -------------------------------------------------------------
  ! --- ABSTRACT FUNCTION TYPE
  ! -------------------------------------------------------------
  TYPE,ABSTRACT :: AbstractFunctionType
  CONTAINS
      PROCEDURE(Abstract_Evaluate),PASS,DEFERRED        :: Evaluate
      PROCEDURE(Abstract_Derivative),PASS,DEFERRED      :: Derivative
      PROCEDURE(Abstract_InverseEvaluate),PASS,DEFERRED :: InverseEvaluate
      PROCEDURE,PASS                                    :: EvaluateAndDerivative
  END TYPE AbstractFunctionType
    

  ! -------------------------------------------------------------
  ! --- ABSTRACT PROCEDURE INTERFACES
  ! -------------------------------------------------------------
  ABSTRACT INTERFACE
   
      FUNCTION Abstract_Evaluate(Func,XP) RESULT(YP)
          IMPORT                                 :: AbstractFunctionType
          CLASS(AbstractFunctionType),INTENT(IN) :: Func
          REAL(8),INTENT(IN)                     :: XP
          REAL(8)                                :: YP
      END FUNCTION Abstract_Evaluate
      
      
      FUNCTION Abstract_Derivative(Func,XP) RESULT(Derivative)
          IMPORT                                 :: AbstractFunctionType
          CLASS(AbstractFunctionType),INTENT(IN) :: Func
          REAL(8),INTENT(IN)                     :: XP
          REAL(8)                                :: Derivative
      END FUNCTION Abstract_Derivative
      
      
      FUNCTION Abstract_InverseEvaluate(Func,YP) RESULT(XP)
          IMPORT                                 :: AbstractFunctionType
          CLASS(AbstractFunctionType),INTENT(IN) :: Func
          REAL(8),INTENT(IN)                     :: YP
          REAL(8)                                :: XP
      END FUNCTION Abstract_InverseEvaluate
    
  END INTERFACE
  
  
  
CONTAINS
    
    
    
  ! -------------------------------------------------------------
  ! --- EVALUATE THE FUNCTION AND DERIVATIVE AT A POINT
  ! -------------------------------------------------------------
  SUBROUTINE EvaluateAndDerivative(Func,XP,YP,Deriv)
    CLASS(AbstractFunctionType),INTENT(IN) :: Func
    REAL(8),INTENT(IN)                     :: XP
    REAL(8),INTENT(OUT)                    :: YP,Deriv
    
    YP    = Func%Evaluate(XP)
    Deriv = Func%Derivative(XP)
    
  END SUBROUTINE EvaluateAndDerivative
  
END MODULE