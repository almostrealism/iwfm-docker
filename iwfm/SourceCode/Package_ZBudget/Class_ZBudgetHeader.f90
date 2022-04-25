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
MODULE Class_ZBudgetHeader
  USE MessageLogger       , ONLY: SetLastMessage    , &
                                  f_iFatal
  USE GeneralUtilities    , ONLY: IntToText
  USE TimeSeriesUtilities , ONLY: TimeStepType
  USE IOInterface         , ONLY: GenericFileType   , &
                                  f_iGroup
  USE ZBudget_Parameters  , ONLY: f_iMaxDataNameLen  , &
                                  f_cAttributesDir
  USE Class_SystemData    , ONLY: SystemDataType
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
  PUBLIC :: ZBudgetHeaderType    
  
  
  ! -------------------------------------------------------------
  ! --- DATA FOR ASCII OUTPUT
  ! -------------------------------------------------------------
  TYPE ZBudgetASCIIOutputType
      INTEGER                     :: iLenTitles       = 160   !Length of persistent titles
      INTEGER                     :: iNTitles         = 3     !Number of column title lines (must be minimum 3)
      INTEGER                     :: iLenColumnTitles = 0     !Actual length of the cColumnTitles
      CHARACTER(3000),ALLOCATABLE :: cColumnTitles(:)         !Column title lines
      CHARACTER(LEN=300)          :: cNumberFormat    = ''    !Format of the numbers to be output (excluding zone exchange flows, mass balance error and storage columns)
  END TYPE ZBudgetASCIIOutputType
  
  
  ! -------------------------------------------------------------
  ! --- Z-BUDGET HEADER DATA TYPE
  ! -------------------------------------------------------------
  TYPE ZBudgetHeaderType
      CHARACTER(LEN=100)                           :: cSoftwareVersion         = ''        !Version of the software that generated the Z-Budget raw data
      CHARACTER(LEN=100)                           :: cDescriptor              = ''        !Descriptor for the Z-Budget file
      LOGICAL                                      :: lVertFlows_DefinedAtNode = .TRUE.    !Are vertical flows, if defined, defined at nodes or elements?
      LOGICAL                                      :: lFaceFlows_Defined       = .FALSE.   !Are the flows through each element face saved?
      LOGICAL                                      :: lStorages_Defined        = .FALSE.   !Are the storages at each element saved?
      LOGICAL                                      :: lComputeError            = .FALSE.   !Will a mass balance error be computed and printed for each zone?
      INTEGER                                      :: iNData                   = 0         !Number of data columns (fixed columns; dynamic zone exchange data names are derived during post-processing)
      INTEGER,ALLOCATABLE                          :: iDataTypes(:)                        !Data type of each (data) column
      CHARACTER(LEN=f_iMaxDataNameLen),ALLOCATABLE :: cFullDataNames(:)                    !Full name of each (data)
      CHARACTER(LEN=200),ALLOCATABLE               :: cDataHDFPaths(:)                     !Pathnames of each (data) in the HDF file under each layer
      INTEGER,ALLOCATABLE                          :: iNDataElems(:,:)                     !Number of elements associated for each dataset given for (data,layer) combination
      INTEGER,ALLOCATABLE                          :: iElemDataColumns(:,:,:)              !Column number in a specific dataset that stores the data for an element in a layer for each (element,data,layer) combination
      INTEGER,ALLOCATABLE                          :: iErrorInCols(:)                      !If mass blance error will be computed, this stores the data column numbers for inflows
      INTEGER,ALLOCATABLE                          :: iErrorOutCols(:)                     !If mass blance error will be computed, this stores the data column numbers for outflows
      TYPE(ZBudgetASCIIOutputType)                 :: ASCIIOutput                          !Information for ASCII output
      CHARACTER(LEN=80),ALLOCATABLE                :: cDSSFParts(:)                        !F parts of DSS pathnames (excluding zone exchange flows, mass balance error and storage columns)
  CONTAINS
      PROCEDURE,PASS :: Kill
      PROCEDURE,PASS :: ReadFromFile
      PROCEDURE,PASS :: WriteToFile
      GENERIC        :: New         => ReadFromFile
  END TYPE ZBudgetHeaderType
  
  
  ! -------------------------------------------------------------
  ! --- MISC. ENTITIES
  ! -------------------------------------------------------------
  INTEGER,PARAMETER                   :: ModNameLen = 21
  CHARACTER(LEN=ModNameLen),PARAMETER :: ModName    = 'Class_ZBudgetHeader::'
  
  
  
  
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
  ! --- READ ZBUDGET HEADER FROM FILE
  ! -------------------------------------------------------------
  SUBROUTINE ReadFromFile(Header,SystemData,InFile,iStat)
    CLASS(ZBudgetHeaderType)        :: Header
    TYPE(SystemDataType),INTENT(IN) :: SystemData
    TYPE(GenericFileType)           :: InFile
    INTEGER,INTENT(OUT)             :: iStat
    
    !Local variables
    CHARACTER(LEN=ModNameLen+12),PARAMETER :: ThisProcedure = ModNAme // 'ReadFromFile'
    INTEGER                                :: indxData,indxLayer,NErrorInCols,NErrorOutCols
    CHARACTER(:),ALLOCATABLE               :: cFileName
    
    !Check that this is indeed Z-Budget data file by checking if an object that Budget file doesn't have exist
    IF (.NOT. InFile%DoesHDFObjectExist(f_cAttributesDir//'/DataHDFPaths')) THEN
        CALL InFile%GetName(cFileName)
        CALL SetLastMessage('File '//TRIM(cFileName)//' is not a Z-Budget file type!',f_iFatal,ThisProcedure)
        iStat = -1
        RETURN
    END IF
    
    !Software version that created the raw Z-Budget data
    CALL InFile%ReadData(f_cAttributesDir,'Software_Version',ScalarAttrData=Header%cSoftwareVersion,iStat=iStat)  
    IF (iStat .EQ. -1) RETURN
    
    !Z-Budget file descriptor
    CALL InFile%ReadData(f_cAttributesDir,'Descriptor',ScalarAttrData=Header%cDescriptor,iStat=iStat)  
    IF (iStat .EQ. -1) RETURN
    
    !Flags
    CALL InFile%ReadData(f_cAttributesDir,'lVertFlows_DefinedAtNode',ScalarAttrData=Header%lVertFlows_DefinedAtNode,iStat=iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(f_cAttributesDir,'lFaceFlows_Defined',ScalarAttrData=Header%lFaceFlows_Defined,iStat=iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(f_cAttributesDir,'lStorages_Defined',ScalarAttrData=Header%lStorages_Defined,iStat=iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(f_cAttributesDir,'lComputeError',ScalarAttrData=Header%lComputeError,iStat=iStat)  ;  IF (iStat .EQ. -1) RETURN
    
    !Column numbers for mass balance error computation
    IF (Header%lComputeError) THEN
        CALL InFile%ReadData(f_cAttributesDir,'NErrorInCols',ScalarAttrData=NErrorInCols,iStat=iStat)  ;  IF (iStat .EQ. -1) RETURN
        CALL InFile%ReadData(f_cAttributesDir,'NErrorOutCols',ScalarAttrData=NErrorOutCols,iStat=iStat)  ;  IF (iStat .EQ. -1) RETURN
        ALLOCATE (Header%iErrorInCols(NErrorInCols) , Header%iErrorOutCols(NErrorOutCols))
        CALL InFile%ReadData(f_cAttributesDir//'/ErrorInCols',Header%iErrorInCols,iStat=iStat)  ;  IF (iStat .EQ. -1) RETURN
        CALL InFile%ReadData(f_cAttributesDir//'/ErrorOutCols',Header%iErrorOutCols,iStat=iStat)  ;  IF (iStat .EQ. -1) RETURN
    END IF
    
    !ZBudget data columns related information
    CALL InFile%ReadData(f_cAttributesDir,'NData',ScalarAttrData=Header%iNData,iStat=iStat)  ;  IF (iStat .EQ. -1) RETURN
    ALLOCATE (Header%iDataTypes(Header%iNData)                                               , &
              Header%cFullDataNames(Header%iNData)                                           , &
              Header%cDataHDFPaths(Header%iNData)                                            , &
              Header%iNDataElems(Header%iNData,SystemData%NLayers)                           , &
              Header%iElemDataColumns(SystemData%NElements,Header%iNData,SystemData%NLayers) )
    CALL InFile%ReadData(f_cAttributesDir//'/DataTypes',Header%iDataTypes,iStat=iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(f_cAttributesDir//'/FullDataNames',Header%cFullDataNames,iStat=iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(f_cAttributesDir//'/DataHDFPaths',Header%cDataHDFPaths,iStat=iStat)  ;  IF (iStat .EQ. -1) RETURN
    DO indxLayer=1,SystemData%NLayers
        CALL InFile%ReadData(f_cAttributesDir//'/Layer'//TRIM(IntToText(indxLayer))//'_ElemDataColumns',Header%iElemDataColumns(:,:,indxLayer),iStat=iStat)  ;  IF (iStat .EQ. -1) RETURN
        DO indxData=1,Header%iNData
            Header%iNDataElems(indxData,indxLayer) = COUNT(Header%iElemDataColumns(:,indxData,indxLayer) .NE. 0)
        END DO
    END DO
        
    !ASCII output data
    CALL InFile%ReadData(f_cAttributesDir,'ASCIIOutput%iLenTitles',ScalarAttrData=Header%ASCIIOutput%iLenTitles,iStat=iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(f_cAttributesDir,'ASCIIOutput%NTitles',ScalarAttrData=Header%ASCIIOutput%iNTitles,iStat=iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(f_cAttributesDir,'ASCIIOutput%LenColumnTitles',ScalarAttrData=Header%ASCIIOutput%iLenColumnTitles,iStat=iStat)  ;  IF (iStat .EQ. -1) RETURN
    ALLOCATE (Header%ASCIIOutput%cColumnTitles(Header%ASCIIOutput%iNTitles))
    CALL InFile%ReadData(f_cAttributesDir,'ASCIIOutput%ColumnTitles',ArrayAttrData=Header%ASCIIOutput%cColumnTitles,iStat=iStat)  ;  IF (iStat .EQ. -1) RETURN
    CALL InFile%ReadData(f_cAttributesDir,'ASCIIOutput%NumberFormat',ScalarAttrData=Header%ASCIIOutput%cNumberFormat,iStat=iStat)  ;  IF (iStat .EQ. -1) RETURN
    
    !DSS pathname F parts
    ALLOCATE (Header%cDSSFParts(Header%iNData))
    CALL InFile%ReadData(f_cAttributesDir,'DSSFParts',ArrayAttrData=Header%cDSSFParts,iStat=iStat)  
    
  END SUBROUTINE ReadFromFile
  
  
  
  
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
  ! --- KILL ZBUDGET HEADER DATA
  ! -------------------------------------------------------------
  SUBROUTINE Kill(Header)
    CLASS(ZBudgetHeaderType) :: Header
    
    !Local variables
    INTEGER                 :: ErrorCode
    TYPE(ZBudgetHeaderType) :: DummyHeader
    
    !Deallocate arrays
    DEALLOCATE (Header%iDataTypes                 , &
                Header%cFullDataNames             , &
                Header%cDataHDFPaths              , &
                Header%inDataElems                , &
                Header%iElemDataColumns           , &
                Header%iErrorInCols               , &
                Header%iErrorOutCols              , &
                Header%ASCIIOutput%cColumnTitles  , &
                Header%cDSSFParts                 , &
                STAT = ErrorCode                  )
    
    !Reset attributes their default values
    SELECT TYPE (Header)
        TYPE IS (ZBudgetHeaderType)
            Header = DummyHeader
    END SELECT
    
  END SUBROUTINE Kill
  
  
  
  
! ******************************************************************
! ******************************************************************
! ******************************************************************
! ***
! *** MISC. METHODS
! ***
! ******************************************************************
! ******************************************************************
! ******************************************************************

  ! -------------------------------------------------------------
  ! --- WRITE ZBUDGET HEADER TO FILE
  ! -------------------------------------------------------------
  SUBROUTINE WriteToFile(Header,SystemData,OutFile)
    CLASS(ZBudgetHeaderType),INTENT(IN) :: Header
    TYPE(SystemDataType),INTENT(IN)     :: SystemData
    TYPE(GenericFileType)               :: OutFile
    
    !Local variables
    INTEGER :: indxLayer
    
    !Create Header directory
    CALL OutFile%CreateHDFGroup(f_cAttributesDir)
    
    !Software version that created the raw Z-Budget data
    CALL OutFile%WriteData(f_iGroup,f_cAttributesDir,'Software_Version',ScalarAttrData=Header%cSoftwareVersion)
    
    !Z-Budget file descriptor
    CALL OutFile%WriteData(f_iGroup,f_cAttributesDir,'Descriptor',ScalarAttrData=Header%cDescriptor)
    
    !Flags
    CALL OutFile%WriteData(f_iGroup,f_cAttributesDir,'lVertFlows_DefinedAtNode',ScalarAttrData=Header%lVertFlows_DefinedAtNode)
    CALL OutFile%WriteData(f_iGroup,f_cAttributesDir,'lFaceFlows_Defined',ScalarAttrData=Header%lFaceFlows_Defined)
    CALL OutFile%WriteData(f_iGroup,f_cAttributesDir,'lStorages_Defined',ScalarAttrData=Header%lStorages_Defined)
    CALL OutFile%WriteData(f_iGroup,f_cAttributesDir,'lComputeError',ScalarAttrData=Header%lComputeError)
    
    !Column numbers for mass balance error computation
    IF (Header%lComputeError) THEN
        CALL OutFile%WriteData(f_iGroup,f_cAttributesDir,'NErrorInCols',ScalarAttrData=SIZE(Header%iErrorInCols))
        CALL OutFile%WriteData(f_iGroup,f_cAttributesDir,'NErrorOutCols',ScalarAttrData=SIZE(Header%iErrorOutCols))
        CALL OutFile%WriteData(Header%iErrorInCols,cHDFPath=f_cAttributesDir//'/ErrorInCols')
        CALL OutFile%WriteData(Header%iErrorOutCols,cHDFPath=f_cAttributesDir//'/ErrorOutCols')
    END IF
    
    !ZBudget data columns related information
    CALL OutFile%WriteData(f_iGroup,f_cAttributesDir,'NData',ScalarAttrData=Header%iNData)
    CALL OutFile%WriteData(Header%iDataTypes,cHDFPath=f_cAttributesDir//'/DataTypes')
    CALL OutFile%WriteData(Header%cFullDataNames,cHDFPath=f_cAttributesDir//'/FullDataNames')
    CALL OutFile%WriteData(Header%cDataHDFPaths,cHDFPath=f_cAttributesDir//'/DataHDFPaths')
    DO indxLayer=1,SystemData%NLayers
        CALL OutFile%WriteData(Header%iElemDataColumns(:,:,indxLayer),cHDFPath=f_cAttributesDir//'/Layer'//TRIM(IntToText(indxLayer))//'_ElemDataColumns')
    END DO
    !CALL OutFile%WriteData(Header%iNDataElems,cHDFPath=cAttributesDir//'/NDataElems')  !Do not write this information; it will be compiled from Header%iElemDataColumns array during reading
    
    !ASCII output data
    CALL OutFile%WriteData(f_iGroup,f_cAttributesDir,'ASCIIOutput%iLenTitles',ScalarAttrData=Header%ASCIIOutput%iLenTitles)
    CALL OutFile%WriteData(f_iGroup,f_cAttributesDir,'ASCIIOutput%NTitles',ScalarAttrData=Header%ASCIIOutput%iNTitles)
    CALL OutFile%WriteData(f_iGroup,f_cAttributesDir,'ASCIIOutput%LenColumnTitles',ScalarAttrData=Header%ASCIIOutput%iLenColumnTitles)
    CALL OutFile%WriteData(f_iGroup,f_cAttributesDir,'ASCIIOutput%ColumnTitles',ArrayAttrData=Header%ASCIIOutput%cColumnTitles)
    CALL OutFile%WriteData(f_iGroup,f_cAttributesDir,'ASCIIOutput%NumberFormat',ScalarAttrData=Header%ASCIIOutput%cNumberFormat)
    
    !DSS pathname F parts
    CALL OutFile%WriteData(f_iGroup,f_cAttributesDir,'DSSFParts',ArrayAttrData=Header%cDSSFParts)
    
  END SUBROUTINE WriteToFile

END MODULE