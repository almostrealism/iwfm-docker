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
MODULE Class_StrmNode_v41
  USE IOInterface      , ONLY: GenericFileType
  USE Class_PairedData , ONLY: PairedDataType         
  USE Class_StrmNode   ,       StrmNode_Base_New                   => StrmNode_New                   , &
                               StrmNode_Base_WritePreprocessedData => StrmNode_WritePreprocessedData
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
  PUBLIC :: StrmNode_v41_Type               , &
            StrmNode_New                    , &
            StrmNode_WritePreprocessedData        
  
  
  ! -------------------------------------------------------------
  ! --- STREAM NODE DATA TYPE
  ! -------------------------------------------------------------
  TYPE,EXTENDS(StrmNodeType) :: StrmNode_v41_Type
    TYPE(PairedDataType) :: RatingTable_WetPerimeter    !Stage vs. wetted perimeter rating table
  END TYPE StrmNode_v41_Type 
  
  
  
  
CONTAINS




! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** DATA READERS
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- READ PREPROCESSED DATA
  ! -------------------------------------------------------------
  SUBROUTINE StrmNode_New(NNodes,InFile,Nodes,iStat)
    INTEGER,INTENT(IN)      :: NNodes
    TYPE(GenericFileType)   :: InFile
    TYPE(StrmNode_v41_Type) :: Nodes(NNodes)
    INTEGER,INTENT(OUT)     :: iStat
    
    !Local variables
    INTEGER :: indxNode
    
    !Initialize
    iStat = 0
    
    !First read the base data
    CALL StrmNode_Base_New(NNodes,InFile,Nodes%StrmNodeType,iStat)  
    IF (iStat .EQ. -1) RETURN
    
    !Then read the extended data
    DO indxNode=1,NNodes
      CALL Nodes(indxNode)%RatingTable_WetPerimeter%New(InFile,iStat)
      IF (iStat .EQ. -1) RETURN
    END DO
    
  END SUBROUTINE StrmNode_New



! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** DATA WRITERS
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- WRITE PREPROCESSED DATA
  ! -------------------------------------------------------------
  SUBROUTINE StrmNode_WritePreprocessedData(Nodes,OutFile)
    TYPE(StrmNode_v41_Type),INTENT(IN) :: Nodes(:)
    TYPE(GenericFileType)              :: OutFile
    
    !Local variables
    INTEGER :: indxNode
    
    !First write the base data
    CALL StrmNode_Base_WritePreprocessedData(Nodes%StrmNodeType,OutFile)
    
    !Then write the extended data
    DO indxNode=1,SIZE(Nodes)
      CALL Nodes(indxNode)%RatingTable_WetPerimeter%WriteToFile(Outfile)
    END DO
    
  END SUBROUTINE StrmNode_WritePreprocessedData


END MODULE