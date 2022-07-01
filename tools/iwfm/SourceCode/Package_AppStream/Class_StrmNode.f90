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
MODULE Class_StrmNode
  USE Class_PairedData
  USE IOInterface
  USE Package_Matrix     , ONLY: ConnectivityListType
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
  PUBLIC :: StrmNodeType                    , &
            StrmNode_New                    , &
            StrmNode_WritePreprocessedData        
  
  
  ! -------------------------------------------------------------
  ! --- STREAM NODE DATA TYPE
  ! -------------------------------------------------------------
  TYPE StrmNodeType
      INTEGER                    :: ID           = 0                !Stream node ID
      REAL(8)                    :: BottomElev                      !Stream bottom elevation
      TYPE(PairedDataType)       :: RatingTable                     !Stage vs. flow rating table
      TYPE(ConnectivityListType) :: Connectivity                    !List of stream nodes upstream from this node
  END TYPE StrmNodeType 
  
  
  ! -------------------------------------------------------------
  ! --- OVERLOADED METHODS
  ! -------------------------------------------------------------
  INTERFACE StrmNode_New
    MODULE PROCEDURE StrmNode_ReadPreprocessedData
  END INTERFACE StrmNode_New

  
  
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
  SUBROUTINE StrmNode_ReadPreprocessedData(NNodes,InFile,Nodes,iStat)
    INTEGER,INTENT(IN)    :: NNodes
    TYPE(GenericFileType) :: InFile
    TYPE(StrmNodeType)    :: Nodes(NNodes)
    INTEGER,INTENT(OUT)   :: iStat
    
    !Local variables
    INTEGER :: indxNode,NUpstrmNodes
    
    DO indxNode=1,NNodes
      CALL InFile%ReadData(Nodes(indxNode)%ID,iStat)          ;  IF (iStat .EQ. -1) RETURN
      CALL InFile%ReadData(Nodes(indxNode)%BottomElev,iStat)  ;  IF (iStat .EQ. -1) RETURN
      CALL Nodes(indxNode)%RatingTable%New(InFile,iStat)      ;  IF (iStat .EQ. -1) RETURN
      CALL InFile%ReadData(NUpstrmNodes,iStat)                ;  IF (iStat .EQ. -1) RETURN  ;  Nodes(indxNode)%Connectivity%nConnectedNodes = NUpstrmNodes
      ALLOCATE (Nodes(indxNode)%Connectivity%ConnectedNodes(NUpstrmNodes))
      IF (NUpstrmNodes .GT. 0) THEN
          CALL InFile%ReadData(Nodes(indxNode)%Connectivity%ConnectedNodes,iStat)  
          IF (iStat .EQ. -1) RETURN
      END IF
    END DO
    
  END SUBROUTINE StrmNode_ReadPreprocessedData



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
    TYPE(StrmNodeType),INTENT(IN) :: Nodes(:)
    TYPE(GenericFileType)         :: OutFile
    
    !Local variables
    INTEGER :: indxNode
    
    DO indxNode=1,SIZE(Nodes)
      CALL Outfile%WriteData(Nodes(indxNode)%ID)
      CALL Outfile%WriteData(Nodes(indxNode)%BottomElev)
      CALL Nodes(indxNode)%RatingTable%WriteToFile(Outfile)
      CALL Outfile%WriteData(Nodes(indxNode)%Connectivity%nConnectedNodes)
      CALL Outfile%WriteData(Nodes(indxNode)%Connectivity%ConnectedNodes)
    END DO
    
  END SUBROUTINE StrmNode_WritePreprocessedData


END MODULE