cmake_minimum_required (VERSION 3.10.0)
enable_language(Fortran)
set(TOP_DIR ".")
set(THIRD_PARTY_DIR "${TOP_DIR}/third_party")
# FFLAGS depend on the compiler
get_filename_component (Fortran_COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME)
# include the common macros for compiler flags and third party libraries
include(${TOP_DIR}/CmakeLibraryMacro.cmake)

#-------        SET UP COMPILER FLAGS --------------#
set_compiler_flags()

#-------        LIBRARIES  -------------------------#
find_libs()

#----------------- EXECUTABLES --------------------------------------------
if(MSVC)
  set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} /NODEFAULTLIB:msvcrt /MANIFEST:NO")
  #set(CMAKE_SHARED_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} /NODEFAULTLIB:libifcoremd.lib /MANIFEST:NO")
  #set(VS_STARTUP_PROJECT Budget)
else(MSVC)
endif()

#------------ INCLUDE DIRS ---------------------#
include_directories("${CMAKE_Fortran_MODULE_DIRECTORY}" "${HDF5_DIR}/include/static")
set(SOURCE_DIR "${TOP_DIR}/SourceCode")

if (YES)
  set(DSS "")
  message(STATUS "Building with DSS")
  set(DSSCLASSFILESRC ${SOURCE_DIR}/IWFM_Util/IOInterface/Class_DSSFileType.f90)
else (YES)
  set(DSS "_NoDSS")
  set(DSSCLASSFILESRC "")
endif (YES)

#----------- ADD LIBRARIES FIRST ---------------#
set(IWFM_Core_Version
  ${SOURCE_DIR}/IWFM_Core_Version/IWFM_Core_Version.f90
  ${SOURCE_DIR}/IWFM_Core_Version/Revision.fi)
source_group("IWFM_Core_Version" FILES ${IWFM_Core_Version})
add_library(IWFM_Core_Version STATIC ${IWFM_Core_Version})

set(IWFM_Util
  ${DSSCLASSFILESRC}
  ${SOURCE_DIR}/IWFM_Util/IOInterface${DSS}/Class_AsciiFileType.f90
  ${SOURCE_DIR}/IWFM_Util/IOInterface${DSS}/Class_BaseFileType.f90
  ${SOURCE_DIR}/IWFM_Util/IOInterface${DSS}/Class_FortBinaryFileType.f90
  ${SOURCE_DIR}/IWFM_Util/IOInterface${DSS}/Class_HDF5FileType.f90
  ${SOURCE_DIR}/IWFM_Util/IOInterface${DSS}/IOInterface${DSS}.f90
  ${SOURCE_DIR}/IWFM_Util/Utilities${DSS}/Class_BinaryTree.f90
  ${SOURCE_DIR}/IWFM_Util/Utilities${DSS}/Class_LinkedListNode.f90
  ${SOURCE_DIR}/IWFM_Util/Utilities${DSS}/Class_Version.f90
  ${SOURCE_DIR}/IWFM_Util/Utilities${DSS}/GeneralUtilities.f90
  ${SOURCE_DIR}/IWFM_Util/Utilities${DSS}/GenericLinkedList.f90
  ${SOURCE_DIR}/IWFM_Util/Utilities${DSS}/IWFM_Util${DSS}_VersionF.f90
  ${SOURCE_DIR}/IWFM_Util/Utilities${DSS}/IWFM_Util_Revision.fi
  ${SOURCE_DIR}/IWFM_Util/Utilities${DSS}/MessageLogger.f90
  ${SOURCE_DIR}/IWFM_Util/Utilities${DSS}/ProgramTimer.f90
  ${SOURCE_DIR}/IWFM_Util/Utilities${DSS}/TimeSeriesUtilities.f90
  )
source_group("IWFM_Util" FILES ${IWFM_Util})
add_library(IWFM_Util STATIC ${IWFM_Util})

set(Package_AppGW
  ${SOURCE_DIR}/Package_AppGW/BC/Class_AppBC.f90
  ${SOURCE_DIR}/Package_AppGW/BC/Class_LayerBC.f90
  ${SOURCE_DIR}/Package_AppGW/BC/Class_TSBCDataFile.f90
  ${SOURCE_DIR}/Package_AppGW/Class_AppGW.f90
  ${SOURCE_DIR}/Package_AppGW/Class_GWState.f90
  ${SOURCE_DIR}/Package_AppGW/GWHydrograph.f90
  ${SOURCE_DIR}/Package_AppGW/Package_AppGW.f90
  ${SOURCE_DIR}/Package_AppGW/VerticalFlow.f90
  )
source_group("Package_AppGW" FILES ${Package_AppGW})
add_library(Package_AppGW STATIC ${Package_AppGW})

set(Package_AppLake
  ${SOURCE_DIR}/Package_AppLake/Class_BaseAppLake.f90
  ${SOURCE_DIR}/Package_AppLake/Class_Lake.f90
  ${SOURCE_DIR}/Package_AppLake/Package_AppLake.f90
  ${SOURCE_DIR}/Package_AppLake/Package_AppLake_Revision.fi
  ${SOURCE_DIR}/Package_AppLake/VERSION_4.0/AppLake_v40_Revision.fi
  ${SOURCE_DIR}/Package_AppLake/VERSION_4.0/Class_AppLake_v40.f90
  ${SOURCE_DIR}/Package_AppLake/VERSION_4.0/Class_MaxLakeElevFile.f90
  ${SOURCE_DIR}/Package_AppLake/VERSION_5.0/AppLake_v50_Revision.fi
  ${SOURCE_DIR}/Package_AppLake/VERSION_5.0/Class_AppLake_v50.f90
  )
source_group("Package_AppLake" FILES ${Package_AppLake})
add_library(Package_AppLake STATIC ${Package_AppLake})
  
set(Package_AppPumping
  ${SOURCE_DIR}/Package_AppPumping/Class_ElementPumping.f90
  ${SOURCE_DIR}/Package_AppPumping/Class_Pumping.f90
  ${SOURCE_DIR}/Package_AppPumping/Class_PumpsAtElem.f90
  ${SOURCE_DIR}/Package_AppPumping/Class_Well.f90
  ${SOURCE_DIR}/Package_AppPumping/Package_AppPumping.f90
  )
source_group("Package_AppPumping" FILES ${Package_AppPumping})
add_library(Package_AppPumping STATIC ${Package_AppPumping})

set(Package_AppSmallWatershed
  ${SOURCE_DIR}/Package_AppSmallWatershed/Package_AppSmallWatershed.f90
  )
source_group("Package_AppSmallWatershed" FILES ${Package_AppSmallWatershed})
add_library(Package_AppSmallWatershed STATIC ${Package_AppSmallWatershed})

set(Package_AppStream
  ${SOURCE_DIR}/Package_AppStream/Class_AppDiverBypass.f90
  ${SOURCE_DIR}/Package_AppStream/Class_BaseAppStream.f90
  ${SOURCE_DIR}/Package_AppStream/Class_Bypass.f90
  ${SOURCE_DIR}/Package_AppStream/Class_Diversion.f90
  ${SOURCE_DIR}/Package_AppStream/Class_ElemToRecvLoss.f90
  ${SOURCE_DIR}/Package_AppStream/Class_RechargeZone.f90
  ${SOURCE_DIR}/Package_AppStream/Class_StrmInflow.f90
  ${SOURCE_DIR}/Package_AppStream/Class_StrmNode.f90
  ${SOURCE_DIR}/Package_AppStream/Class_StrmNodeBudget.f90
  ${SOURCE_DIR}/Package_AppStream/Class_StrmReach.f90
  ${SOURCE_DIR}/Package_AppStream/Class_StrmState.f90
  ${SOURCE_DIR}/Package_AppStream/Package_AppStream.f90
  ${SOURCE_DIR}/Package_AppStream/Package_AppStream_Revision.fi
  ${SOURCE_DIR}/Package_AppStream/StrmHydrograph.f90
  ${SOURCE_DIR}/Package_AppStream/VERSION_4.0/AppStream_v40_Revision.fi
  ${SOURCE_DIR}/Package_AppStream/VERSION_4.0/Class_AppStream_v40.f90
  ${SOURCE_DIR}/Package_AppStream/VERSION_4.1/AppStream_v41_Revision.fi
  ${SOURCE_DIR}/Package_AppStream/VERSION_4.1/Class_AppStream_v41.f90
  ${SOURCE_DIR}/Package_AppStream/VERSION_4.1/Class_StrmNode_v41.f90
  ${SOURCE_DIR}/Package_AppStream/VERSION_4.2/AppStream_v42_Revision.fi
  ${SOURCE_DIR}/Package_AppStream/VERSION_4.2/Class_AppStream_v42.f90
  ${SOURCE_DIR}/Package_AppStream/VERSION_4.21/Class_AppStream_v421.f90
  ${SOURCE_DIR}/Package_AppStream/VERSION_4.21/AppStream_v421_Revision.fi
  ${SOURCE_DIR}/Package_AppStream/VERSION_5.0/AppStream_v50_Revision.fi
  ${SOURCE_DIR}/Package_AppStream/VERSION_5.0/Class_AppStream_v50.f90
  ${SOURCE_DIR}/Package_AppStream/VERSION_5.0/Class_StrmNode_v50.f90
  )
source_group("Package_AppStream" FILES ${Package_AppStream})
add_library(Package_AppStream STATIC ${Package_AppStream})
  
set(Package_AppSubsidence
  ${SOURCE_DIR}/Package_AppSubsidence/VERSION_4.0/Class_AppSubsidence_v40.f90
  ${SOURCE_DIR}/Package_AppSubsidence/VERSION_4.0/AppSubsidence_v40_Revision.fi
  ${SOURCE_DIR}/Package_AppSubsidence/Package_AppSubsidence_Revision.fi
  ${SOURCE_DIR}/Package_AppSubsidence/Package_AppSubsidence.f90
  ${SOURCE_DIR}/Package_AppSubsidence/VERSION_5.0/AppSubsidence_v50_Revision.fi
  ${SOURCE_DIR}/Package_AppSubsidence/VERSION_5.0/Class_AppSubsidence_v50.f90
  ${SOURCE_DIR}/Package_AppSubsidence/Class_BaseAppSubsidence.f90
  )
source_group("Package_AppSubsidence" FILES ${Package_AppSubsidence})
add_library(Package_AppSubsidence STATIC ${Package_AppSubsidence})
  
set(Package_AppTileDrain
  ${SOURCE_DIR}/Package_AppTileDrain/AppTileDrain_Parameters.f90
  ${SOURCE_DIR}/Package_AppTileDrain/Class_BaseTileDrain.f90
  ${SOURCE_DIR}/Package_AppTileDrain/Package_AppTileDrain.f90
  ${SOURCE_DIR}/Package_AppTileDrain/TileDrainHydrograph.f90
  )
source_group("Package_AppTileDrain" FILES ${Package_AppTileDrain})
add_library(Package_AppTileDrain STATIC ${Package_AppTileDrain})
  
set(Package_AppUnsatZone
  ${SOURCE_DIR}/Package_AppUnsatZone/Package_AppUnsatZone.f90
  )
source_group("Package_AppUnsatZone" FILES ${Package_AppUnsatZone})
add_library(Package_AppUnsatZone STATIC ${Package_AppUnsatZone})
  
set(Package_Budget
  ${SOURCE_DIR}/Package_Budget/Budget_Parameters.f90
  ${SOURCE_DIR}/Package_Budget/Class_Budget.f90
  ${SOURCE_DIR}/Package_Budget/Class_BudgetInputFile.f90
  ${SOURCE_DIR}/Package_Budget/Package_Budget.f90
  ${SOURCE_DIR}/Package_Budget/Package_Budget_Revision.fi
  )
source_group("Package_Budget" FILES ${Package_Budget})
add_library(Package_Budget STATIC ${Package_Budget})
  
set(Package_ComponentConnectors
  ${SOURCE_DIR}/Package_ComponentConnectors/LakeGWConnector.f90
  ${SOURCE_DIR}/Package_ComponentConnectors/Package_ComponentConnectors.f90
  ${SOURCE_DIR}/Package_ComponentConnectors/Package_ComponentConnectors_Revision.fi
  ${SOURCE_DIR}/Package_ComponentConnectors/StrmGWConnector/Class_BaseStrmGWConnector.f90
  ${SOURCE_DIR}/Package_ComponentConnectors/StrmGWConnector/Class_StrmGWConnector.f90
  ${SOURCE_DIR}/Package_ComponentConnectors/StrmGWConnector/Class_StrmGWConnector_v40.f90
  ${SOURCE_DIR}/Package_ComponentConnectors/StrmGWConnector/Class_StrmGWConnector_v41.f90
  ${SOURCE_DIR}/Package_ComponentConnectors/StrmGWConnector/Class_StrmGWConnector_v42.f90
  ${SOURCE_DIR}/Package_ComponentConnectors/StrmGWConnector/Class_StrmGWConnector_v50.f90
  ${SOURCE_DIR}/Package_ComponentConnectors/StrmGWConnector/Class_StrmGWConnector_v421.f90
  ${SOURCE_DIR}/Package_ComponentConnectors/StrmLakeConnector.f90
  ${SOURCE_DIR}/Package_ComponentConnectors/SupplyDestinationConnector.f90
  )
source_group("Package_ComponentConnectors" FILES ${Package_ComponentConnectors})
add_library(Package_ComponentConnectors STATIC ${Package_ComponentConnectors})
  
set(Package_Discretization
  ${SOURCE_DIR}/Package_Discretization/Class_AppFace.f90
  ${SOURCE_DIR}/Package_Discretization/Class_AppGrid.f90
  ${SOURCE_DIR}/Package_Discretization/Class_Grid.f90
  ${SOURCE_DIR}/Package_Discretization/Class_Stratigraphy.f90
  ${SOURCE_DIR}/Package_Discretization/Package_Discretization.f90
  ${SOURCE_DIR}/Package_Discretization/Package_Discretization_Revision.fi
  ${SOURCE_DIR}/Package_Discretization/ParametricGrid.f90
  )
source_group("Package_Discretization" FILES ${Package_Discretization})
add_library(Package_Discretization STATIC ${Package_Discretization})
  
set(Package_GWZBudget
  ${SOURCE_DIR}/Package_GWZBudget/Class_GWZBudget.f90
  ${SOURCE_DIR}/Package_GWZBudget/GWZBudget_Parameters.f90
  ${SOURCE_DIR}/Package_GWZBudget/Package_GWZBudget.f90
  )
source_group("Package_GWZBudget" FILES ${Package_GWZBudget})
add_library(Package_GWZBudget STATIC ${Package_GWZBudget})
  
set(Package_Matrix
  ${SOURCE_DIR}/Package_Matrix/Lubksb.f90
  ${SOURCE_DIR}/Package_Matrix/Ludcmp.f90
  ${SOURCE_DIR}/Package_Matrix/Package_Matrix.f90
  ${SOURCE_DIR}/Package_Matrix/Package_Matrix_Revision.fi
  ${SOURCE_DIR}/Package_Matrix/pgmres.f
  )
source_group("Package_Matrix" FILES ${Package_Matrix})
add_library(Package_Matrix STATIC ${Package_Matrix})
  
set(Package_Miscellaneous
  ${SOURCE_DIR}/Package_Miscellaneous/AbstractFunction.f90
  ${SOURCE_DIR}/Package_Miscellaneous/Class_BaseHydrograph.f90
  ${SOURCE_DIR}/Package_Miscellaneous/Class_GeneralHeadBoundary.f90
  ${SOURCE_DIR}/Package_Miscellaneous/Class_PairedData.f90
  ${SOURCE_DIR}/Package_Miscellaneous/Class_SolverData.f90
  ${SOURCE_DIR}/Package_Miscellaneous/Class_TecplotOutput.f90
  ${SOURCE_DIR}/Package_Miscellaneous/Opening_screen_LINUX.f90
  ${SOURCE_DIR}/Package_Miscellaneous/Package_Misc.f90
  ${SOURCE_DIR}/Package_Miscellaneous/Package_Misc_Revision.fi
  ${SOURCE_DIR}/Package_Miscellaneous/TSDFileHandler.f90
  )
source_group("Package_Miscellaneous" FILES ${Package_Miscellaneous})
add_library(Package_Miscellaneous STATIC ${Package_Miscellaneous})
  
set(Package_Model
  ${SOURCE_DIR}/Package_Model/Class_Model_ForInquiry.f90
  ${SOURCE_DIR}/Package_Model/Package_Model.f90
  )
source_group("Package_Model" FILES ${Package_Model})
add_library(Package_Model STATIC ${Package_Model})
  
set(Package_PrecipitationET
  ${SOURCE_DIR}/Package_PrecipitationET/Class_AtmosphericData.f90
  ${SOURCE_DIR}/Package_PrecipitationET/Package_PrecipET_Revision.fi
  ${SOURCE_DIR}/Package_PrecipitationET/Package_PrecipitationET.f90
  )
source_group("Package_PrecipitationET" FILES ${Package_PrecipitationET})
add_library(Package_PrecipitationET STATIC ${Package_PrecipitationET})
  
set(Package_RootZone
  ${SOURCE_DIR}/Package_RootZone/Class_BaseRootZone.f90
  ${SOURCE_DIR}/Package_RootZone/Class_GenericLandUse.f90
  ${SOURCE_DIR}/Package_RootZone/Class_GenericMoistureData.f90
  ${SOURCE_DIR}/Package_RootZone/Class_LandUseDataFile.f90
  ${SOURCE_DIR}/Package_RootZone/Package_RootZone.f90
  ${SOURCE_DIR}/Package_RootZone/Package_RootZone_Revision.fi
  ${SOURCE_DIR}/Package_RootZone/Util_Package_RootZone.f90
  ${SOURCE_DIR}/Package_RootZone/VERSION_4.0/Class_NativeRiparianLandUse.f90
  ${SOURCE_DIR}/Package_RootZone/VERSION_4.0/Class_NonPondedAgLandUse.f90
  ${SOURCE_DIR}/Package_RootZone/VERSION_4.0/Class_PondedAgLandUse.f90
  ${SOURCE_DIR}/Package_RootZone/VERSION_4.0/Class_RootDepthFracDataFile.f90
  ${SOURCE_DIR}/Package_RootZone/VERSION_4.0/Class_UrbanLandUse.f90
  ${SOURCE_DIR}/Package_RootZone/VERSION_4.0/RootZone_v40.f90
  ${SOURCE_DIR}/Package_RootZone/VERSION_4.0/RootZone_v40_Revision.fi
  ${SOURCE_DIR}/Package_RootZone/VERSION_4.0/Util_RootZone_v40.f90
  ${SOURCE_DIR}/Package_RootZone/VERSION_4.01/RootZone_v401.f90
  ${SOURCE_DIR}/Package_RootZone/VERSION_4.01/RootZone_v401_Revision.fi
  ${SOURCE_DIR}/Package_RootZone/VERSION_4.1/Class_GenericLandUseGW.f90
  ${SOURCE_DIR}/Package_RootZone/VERSION_4.1/Class_NativeRiparianLandUseGW.f90
  ${SOURCE_DIR}/Package_RootZone/VERSION_4.1/Class_NonPondedAgLandUseGW.f90
  ${SOURCE_DIR}/Package_RootZone/VERSION_4.1/Class_PondedAgLandUseGW.f90
  ${SOURCE_DIR}/Package_RootZone/VERSION_4.1/Class_RVETFromStrm.f90
  ${SOURCE_DIR}/Package_RootZone/VERSION_4.1/Class_UrbanLandUseGW.f90
  ${SOURCE_DIR}/Package_RootZone/VERSION_4.1/RootZone_v41.f90
  ${SOURCE_DIR}/Package_RootZone/VERSION_4.1/RootZone_v41_Revision.fi
  ${SOURCE_DIR}/Package_RootZone/VERSION_4.1/Util_RootZone_v41.f90
  ${SOURCE_DIR}/Package_RootZone/VERSION_4.11/RootZone_v411.f90
  ${SOURCE_DIR}/Package_RootZone/VERSION_4.11/RootZone_v411_Revision.fi
  ${SOURCE_DIR}/Package_RootZone/VERSION_5.0/Class_AgLandUse_v50.f90
  ${SOURCE_DIR}/Package_RootZone/VERSION_5.0/Class_NativeRiparianLandUse_v50.f90
  ${SOURCE_DIR}/Package_RootZone/VERSION_5.0/Class_UrbanLandUse_v50.f90
  ${SOURCE_DIR}/Package_RootZone/VERSION_5.0/RootZone_v50.f90
  ${SOURCE_DIR}/Package_RootZone/VERSION_5.0/RootZone_v50_Revision.fi
  )
source_group("Package_RootZone" FILES ${Package_RootZone})
add_library(Package_RootZone STATIC ${Package_RootZone})
  
set(Package_Supply
  ${SOURCE_DIR}/Package_Supply/Class_IrigFracFile.f90
  ${SOURCE_DIR}/Package_Supply/Package_Supply.f90
  ${SOURCE_DIR}/Package_Supply/SupplyAdjustment.f90
  )
source_group("Package_Supply" FILES ${Package_Supply})
add_library(Package_Supply STATIC ${Package_Supply})
  
set(Package_UnsatZone
  ${SOURCE_DIR}/Package_UnsatZone/Class_Soil.f90
  ${SOURCE_DIR}/Package_UnsatZone/Package_UnsatZone.f90
  ${SOURCE_DIR}/Package_UnsatZone/Package_UnsatZone_Revision.fi
  ${SOURCE_DIR}/Package_UnsatZone/RainfallRunoff.f90
  ${SOURCE_DIR}/Package_UnsatZone/UnsatZoneOps.f90
  )
source_group("Package_UnsatZone" FILES ${Package_UnsatZone})
add_library(Package_UnsatZone STATIC ${Package_UnsatZone})
  
set(Package_ZBudget
  ${SOURCE_DIR}/Package_ZBudget/Class_SystemData.f90
  ${SOURCE_DIR}/Package_ZBudget/Class_ZBudget.f90
  ${SOURCE_DIR}/Package_ZBudget/Class_ZBudgetHeader.f90
  ${SOURCE_DIR}/Package_ZBudget/Class_ZoneList.f90
  ${SOURCE_DIR}/Package_ZBudget/Package_ZBudget.f90
  ${SOURCE_DIR}/Package_ZBudget/Package_ZBudget_Revision.fi
  ${SOURCE_DIR}/Package_ZBudget/ZBudget_Parameters.f90
  ${SOURCE_DIR}/Package_ZBudget/ZBudget_Util.f90
  )
source_group("Package_ZBudget" FILES ${Package_ZBudget})
add_library(Package_ZBudget STATIC ${Package_ZBudget})
  
set(IWFMLib SHARED
  ${SOURCE_DIR}/IWFM_Util/IOInterface${DSS}/IOInterface${DSS}.f90
  ${SOURCE_DIR}/SharedLibrary/IWFM_Budget_Exports_C.f90
  ${SOURCE_DIR}/SharedLibrary/IWFM_Misc_Exports_C.f90
  ${SOURCE_DIR}/SharedLibrary/IWFM_Model_Exports_C.f90
  ${SOURCE_DIR}/SharedLibrary/IWFM_ZBudget_Exports_C.f90
  )
source_group("IWFMLib" FILES ${IWFMLib})
  
set(ALL_FILES
	${IWFMLib}
    ${IWFM_Util}
    ${IWFM_Core_Version}
    ${Package_AppGW}
    ${Package_AppLake}
    ${Package_AppPumping}
    ${Package_AppSmallWatershed}
    ${Package_AppStream}
    ${Package_AppSubsidence}
    ${Package_AppTileDrain}
    ${Package_AppUnsatZone}
    ${Package_Budget}
    ${Package_ComponentConnectors}
    ${Package_Discretization}
    ${Package_GWZBudget}
    ${Package_Matrix}
    ${Package_Miscellaneous}
    ${Package_Model}
    ${Package_PrecipitationET}
    ${Package_RootZone}
    ${Package_Supply}
    ${Package_UnsatZone}
    ${Package_ZBudget}
)

add_library(IWFMLib SHARED ${ALL_FILES})

#---------- DEFINE DEPENDENCIES -------------#
# Talk with Can Dogrul about correcting these dependencies. Too much fluff
add_dependencies(IWFM_Core_Version IWFM_Util)
add_dependencies(Package_AppGW IWFM_Core_Version Package_ComponentConnectors Package_AppTileDrain Package_AppSubsidence Package_AppPumping Package_Budget Package_Matrix Package_Miscellaneous Package_Discretization)
add_dependencies(Package_AppLake IWFM_Core_Version Package_PrecipitationET Package_Matrix Package_Miscellaneous Package_Discretization Package_ComponentConnectors)
add_dependencies(Package_AppStream IWFM_Core_Version Package_Matrix Package_Miscellaneous Package_Discretization Package_ComponentConnectors)
add_dependencies(Package_AppSmallWatershed IWFM_Core_Version Package_UnsatZone Package_Matrix Package_Miscellaneous Package_Discretization)
add_dependencies(Package_AppSubsidence Package_Matrix)
add_dependencies(Package_AppPumping IWFM_Core_Version Package_Miscellaneous Package_ComponentConnectors Package_Discretization)
add_dependencies(Package_AppTileDrain IWFM_Core_Version Package_Miscellaneous Package_Discretization Package_Matrix)
add_dependencies(Package_AppUnsatZone IWFM_Core_Version Package_Discretization Package_Miscellaneous Package_Budget Package_ZBudget Package_UnsatZone)
add_dependencies(Package_Budget IWFM_Core_Version Package_Miscellaneous)
add_dependencies(Package_ComponentConnectors IWFM_Core_Version Package_Matrix Package_Discretization Package_Miscellaneous)
add_dependencies(Package_Discretization IWFM_Core_Version)
add_dependencies(Package_GWZBudget IWFM_Core_Version Package_AppStream Package_AppGW Package_AppStream Package_AppSmallWatershed)
add_dependencies(Package_Matrix IWFM_Core_Version Package_Miscellaneous)
add_dependencies(Package_Miscellaneous IWFM_Core_Version Package_Discretization)
add_dependencies(Package_Model IWFM_Core_Version Package_Discretization Package_AppGW Package_GWZBudget Package_AppStream Package_AppLake Package_AppUnsatZone Package_RootZone Package_Supply)
add_dependencies(Package_PrecipitationET IWFM_Core_Version Package_Miscellaneous)
add_dependencies(Package_RootZone IWFM_Core_Version Package_Budget Package_UnsatZone)
add_dependencies(Package_Supply IWFM_Core_Version Package_AppStream Package_AppGW Package_RootZone)
add_dependencies(Package_UnsatZone Package_AppStream IWFM_Core_Version)
add_dependencies(Package_ZBudget IWFM_Core_Version Package_Budget)
add_dependencies(IWFMLib Package_Model Package_ZBudget)

#---------- LINUX LINKER GROUPING ------------#
if (WIN32)
  set(LINKER_RPATH "")
  set(LINKER_START_GROUP "")
  set(LINKER_END_GROUP "")
else (WIN32)
  set(LINKER_RPATH "-Wl,-rpath='$ORIGIN/lib'")
  set(LINKER_START_GROUP "-Wl,--start-group")
  set(LINKER_END_GROUP "-Wl,--end-group")
endif (WIN32)

#---------- ADD EXECUTABLES --------#
add_executable(Budget ${SOURCE_DIR}/Budget/Budget.f90 ${SOURCE_DIR}/Budget/BudgetControls.f90)
target_include_directories(Budget PUBLIC "${CMAKE_Fortran_MODULE_DIRECTORY}" "${HDF5_DIR}/include/static")
target_link_libraries(Budget ${LINKER_START_GROUP} IWFM_Core_Version IWFM_Util Package_Budget Package_Miscellaneous Package_ZBudget ${LINKER_END_GROUP})
target_link_libraries(Budget ${LINKER_RPATH} ${LINKER_START_GROUP} ${HDF5} ${HDF5_HL} ${HDF5_FORTRAN} ${HDF5_F90CSTUB} ${HDF5_HL_FORTRAN} ${HDF5_HL_F90CSTUB} ${SZLIB} ${ZLIB} ${LINKER_END_GROUP})
target_link_libraries(Budget ${HECLIB_RELEASE})
#
add_executable(PreProcessor ${SOURCE_DIR}/PreProcessor/Iwfm_f1.f90)
target_link_libraries(PreProcessor ${LINKER_START_GROUP} IWFM_Core_Version IWFM_Util 
  Package_AppGW Package_AppLake Package_AppPumping Package_AppSmallWatershed
  Package_AppStream Package_AppSubsidence Package_AppTileDrain Package_AppUnsatZone
  Package_Budget Package_ComponentConnectors Package_Discretization Package_GWZBudget
  Package_Matrix Package_Miscellaneous Package_Model Package_PrecipitationET Package_RootZone
  Package_Supply Package_UnsatZone Package_ZBudget ${LINKER_END_GROUP})
target_link_libraries(PreProcessor ${LINKER_RPATH} ${LINKER_START_GROUP} ${HDF5} ${HDF5_HL} ${HDF5_FORTRAN} ${HDF5_F90CSTUB} ${HDF5_HL_FORTRAN} ${HDF5_HL_F90CSTUB} ${SZLIB} ${ZLIB} ${LINKER_END_GROUP})
target_link_libraries(PreProcessor ${HECLIB_RELEASE})
target_include_directories(PreProcessor PUBLIC "${CMAKE_Fortran_MODULE_DIRECTORY}" "${HDF5_DIR}/include/static")
#
add_executable(Simulation ${SOURCE_DIR}/Simulation/Iwfm_f2.f90)
target_link_libraries(Simulation ${LINKER_START_GROUP} IWFM_Core_Version IWFM_Util 
    Package_AppGW Package_AppLake Package_AppPumping Package_AppSmallWatershed
    Package_AppStream Package_AppSubsidence Package_AppTileDrain Package_AppUnsatZone
    Package_Budget Package_ComponentConnectors Package_Discretization Package_GWZBudget
    Package_Matrix Package_Miscellaneous Package_Model Package_PrecipitationET Package_RootZone
    Package_Supply Package_UnsatZone Package_ZBudget ${LINKER_END_GROUP})
target_link_libraries(Simulation ${LINKER_RPATH} ${LINKER_START_GROUP} ${HDF5} ${HDF5_HL} ${HDF5_FORTRAN} ${HDF5_F90CSTUB} ${HDF5_HL_FORTRAN} ${HDF5_HL_F90CSTUB} ${SZLIB} ${ZLIB} ${LINKER_END_GROUP})
target_link_libraries(Simulation ${HECLIB_RELEASE})
target_include_directories(Simulation PUBLIC "${CMAKE_Fortran_MODULE_DIRECTORY}" "${HDF5_DIR}/include/static")
#
add_executable(Z-Budget ${SOURCE_DIR}/ZBudget/ZBudgetControls.f90 ${SOURCE_DIR}/ZBudget/ZBudget_Main.f90)
if (WIN32)
  set_target_properties(Z-Budget PROPERTIES LINK_FLAGS "/STACK:128000000")
endif()
target_link_libraries(Z-Budget ${LINKER_START_GROUP} IWFM_Core_Version IWFM_Util Package_Budget Package_Discretization Package_Miscellaneous Package_ZBudget ${LINKER_END_GROUP})
target_link_libraries(Z-Budget ${LINKER_RPATH} ${LINKER_START_GROUP} ${HDF5} ${HDF5_HL} ${HDF5_FORTRAN} ${HDF5_F90CSTUB} ${HDF5_HL_FORTRAN} ${HDF5_HL_F90CSTUB} ${SZLIB} ${ZLIB} ${LINKER_END_GROUP})
target_link_libraries(Z-Budget ${HECLIB_RELEASE})
target_include_directories(Z-Budget PUBLIC "${CMAKE_Fortran_MODULE_DIRECTORY}" "${HDF5_DIR}/include/static")
#
target_link_libraries(IWFMLib ${LINKER_START_GROUP} IWFM_Core_Version IWFM_Util Package_Budget Package_Discretization Package_Miscellaneous Package_ZBudget ${LINKER_END_GROUP})
target_link_libraries(IWFMLib ${LINKER_RPATH} ${LINKER_START_GROUP} ${HDF5} ${HDF5_HL} ${HDF5_FORTRAN} ${HDF5_F90CSTUB} ${HDF5_HL_FORTRAN} ${HDF5_HL_F90CSTUB} ${SZLIB} ${ZLIB} ${LINKER_END_GROUP})
target_link_libraries(IWFMLib ${HECLIB_RELEASE})
target_include_directories(IWFMLib PUBLIC "${CMAKE_Fortran_MODULE_DIRECTORY}" "${HDF5_DIR}/include/static")
#------------- install for package ----------------#
install(TARGETS Budget PreProcessor Simulation Z-Budget RUNTIME DESTINATION "bin")
set(CPACK_PACKAGE_NAME "IWFMInstaller")
set(CPACK_PACKAGE_VENDOR "Can Dogrul")
set(CPACK_PACKAGE_DESCRIPTION_SUMMARY "IWFM")
set(CPACK_PACKAGE_VERSION_MAJOR "2015")
set(CPACK_PACKAGE_VERSION_MINOR "0")
set(CPACK_PACKAGE_VERSION_PATCH "706")
set(CPACK_PACKAGE_VERSION "${CPACK_PACKAGE_VERSION_MAJOR}.${CPACK_PACKAGE_VERSION_MINOR}.${CPACK_PACKAGE_VERSION_PATCH}")
set(CPACK_PACKAGE_INSTALL_DIRECTORY "CPack")
include(CPack)
