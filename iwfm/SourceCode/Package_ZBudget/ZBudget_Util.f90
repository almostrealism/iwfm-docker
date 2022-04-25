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
MODULE ZBudget_Util
  USE IOInterface        , ONLY: GenericFileType       , &
                                 iGetFileType_FromName , &
                                 f_iHDF
  USE ZBudget_Parameters , ONLY: f_cAttributesDir
  IMPLICIT NONE

  ! -------------------------------------------------------------
  ! --- PUBLIC ENTITIES
  ! -------------------------------------------------------------
  PRIVATE
  PUBLIC :: IsZBudgetFile
  
  
  ! -------------------------------------------------------------
  ! --- OVERLOADED METHODS
  ! -------------------------------------------------------------
  INTERFACE IsZBudgetFile
      MODULE PROCEDURE IsZBudgetFile_FromFilename
      MODULE PROCEDURE IsZBudgetFile_FromOpenFile
  END INTERFACE IsZBudgetFile
    
  
  
CONTAINS
    
    
  
    
  ! -------------------------------------------------------------
  ! --- CHECK IF A FILE IS Z-BUDGET FILE FROM FILENAME
  ! -------------------------------------------------------------
  FUNCTION IsZBudgetFile_FromFileName(cFileName) RESULT(IsTrue)
    CHARACTER(LEN=*),INTENT(IN) :: cFileName
    LOGICAL                     :: IsTrue
    
    !Local variables
    TYPE(GenericFileType) :: CheckFile
    INTEGER               :: iStat
    
    !Initialize
    IsTrue = .FALSE.
    
    !Make sure that file is an HDF5 file
    IF (iGetFileType_FromName(cFileName) .NE. f_iHDF) RETURN
    
    !Open file
    CALL CheckFile%New(FileName=cFileName,InputFile=.TRUE.,iStat=iStat)
    IF (iStat .EQ. -1) RETURN
    
    !Check that this is indeed Z-Budget data file by checking if an object that Budget file doesn't have exist
    IF (CheckFile%DoesHDFObjectExist(f_cAttributesDir//'/DataHDFPaths')) IsTrue = .TRUE.

    !Close file
    CALL CheckFile%Kill()
    
  END FUNCTION IsZBudgetFile_FromFileName

  
  ! -------------------------------------------------------------
  ! --- CHECK IF AN OPEN FILE IS Z-BUDGET FILE
  ! -------------------------------------------------------------
  FUNCTION IsZBudgetFile_FromOpenFile(CheckFile) RESULT(IsTrue)
    TYPE(GenericFileType),INTENT(IN) :: CheckFile
    LOGICAL                          :: IsTrue
        
    !Initialize
    IsTrue = .FALSE.
    
    !Make sure that file is an HDF5 file
    IF (CheckFile%iGetFileType() .NE. f_iHDF) RETURN
    
    !Check that this is indeed Z-Budget data file by checking if an object that Budget file doesn't have exist
    IF (CheckFile%DoesHDFObjectExist(f_cAttributesDir//'/DataHDFPaths')) IsTrue = .TRUE.

  END FUNCTION IsZBudgetFile_FromOpenFile

  
END MODULE ZBudget_Util
