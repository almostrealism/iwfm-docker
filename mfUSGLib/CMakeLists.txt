cmake_minimum_required(VERSION 3.16.0 FATAL_ERROR)

project(mfUSGLib Fortran)

################################################################################
# Set target arch type if empty. Visual studio solution generator provides it.
################################################################################
if(NOT CMAKE_VS_PLATFORM_NAME)
    set(CMAKE_VS_PLATFORM_NAME "x64")
endif()
message("${CMAKE_VS_PLATFORM_NAME} architecture in use")

if(NOT ("${CMAKE_VS_PLATFORM_NAME}" STREQUAL "Win32"
     OR "${CMAKE_VS_PLATFORM_NAME}" STREQUAL "x64"))
    message(FATAL_ERROR "${CMAKE_VS_PLATFORM_NAME} arch is not supported!")
endif()

################################################################################
# Global configuration types
################################################################################
set(CMAKE_CONFIGURATION_TYPES
    "Debug"
    "Release"
    CACHE STRING "" FORCE
)

################################################################################
# Global compiler options
################################################################################
if(MSVC)
    # remove default flags provided with CMake for MSVC
endif()

if(${CMAKE_Fortran_COMPILER_ID} STREQUAL "Intel")
    # remove default flags provided with CMake for ifort
    set(CMAKE_Fortran_FLAGS "")
    set(CMAKE_Fortran_FLAGS_DEBUG "")
    set(CMAKE_Fortran_FLAGS_RELEASE "")
endif()

################################################################################
# Global linker options
################################################################################
if(MSVC)
    # remove default flags provided with CMake for MSVC
    set(CMAKE_EXE_LINKER_FLAGS "")
    set(CMAKE_MODULE_LINKER_FLAGS "")
    set(CMAKE_SHARED_LINKER_FLAGS "")
    set(CMAKE_STATIC_LINKER_FLAGS "")
    set(CMAKE_EXE_LINKER_FLAGS_DEBUG "${CMAKE_EXE_LINKER_FLAGS}")
    set(CMAKE_MODULE_LINKER_FLAGS_DEBUG "${CMAKE_MODULE_LINKER_FLAGS}")
    set(CMAKE_SHARED_LINKER_FLAGS_DEBUG "${CMAKE_SHARED_LINKER_FLAGS}")
    set(CMAKE_STATIC_LINKER_FLAGS_DEBUG "${CMAKE_STATIC_LINKER_FLAGS}")
    set(CMAKE_EXE_LINKER_FLAGS_RELEASE "${CMAKE_EXE_LINKER_FLAGS}")
    set(CMAKE_MODULE_LINKER_FLAGS_RELEASE "${CMAKE_MODULE_LINKER_FLAGS}")
    set(CMAKE_SHARED_LINKER_FLAGS_RELEASE "${CMAKE_SHARED_LINKER_FLAGS}")
    set(CMAKE_STATIC_LINKER_FLAGS_RELEASE "${CMAKE_STATIC_LINKER_FLAGS}")
endif()

################################################################################
# Nuget packages function stub.
################################################################################
function(use_package TARGET PACKAGE VERSION)
    message(WARNING "No implementation of use_package. Create yours. "
                    "Package \"${PACKAGE}\" with version \"${VERSION}\" "
                    "for target \"${TARGET}\" is ignored!")
endfunction()

################################################################################
# Common utils
################################################################################
include(CMake/Utils.cmake)

################################################################################
# Additional Global Settings(add specific info there)
################################################################################
include(CMake/GlobalSettingsInclude.cmake OPTIONAL)

################################################################################
# Use solution folders feature
################################################################################
set_property(GLOBAL PROPERTY USE_FOLDERS ON)

################################################################################
# Sub-projects
################################################################################



























set(PROJECT_NAME mfUSGLib)

################################################################################
# Source groups
################################################################################
set(Source_Files
    "mf_t2p.f90"
)
source_group("Source Files" FILES ${Source_Files})

set(mfsource
    "Jan_23_2018/cln2basu1.f"
    "Jan_23_2018/cln2props1.f"
    "Jan_23_2018/density.f"
    "Jan_23_2018/disu2gncb1.f"
    "Jan_23_2018/disu2gncn1.f"
    "Jan_23_2018/glo2basu1.f"
    "Jan_23_2018/glo2btnu1.f"
    "Jan_23_2018/glo2p3du1.f"
    "Jan_23_2018/glo2sgbu1.f"
    "Jan_23_2018/glo2sms-u1.f"
    "Jan_23_2018/gmodules.f"
    "Jan_23_2018/gwf2basu1.f"
    "Jan_23_2018/gwf2bcf-lpf-u1.f"
    "Jan_23_2018/gwf2chd7u1.f"
    "Jan_23_2018/gwf2dpf1u1.f"
    "Jan_23_2018/gwf2drn7u1.f"
    "Jan_23_2018/gwf2drt8u.f"
    "Jan_23_2018/gwf2ets8u1.f"
    "Jan_23_2018/gwf2evt8u1.f"
    "Jan_23_2018/gwf2fhb7u1.f"
    "Jan_23_2018/gwf2gag7u1.f"
    "Jan_23_2018/gwf2ghb7u1.f"
    "Jan_23_2018/gwf2hfb7u1.f"
    "Jan_23_2018/gwf2lak7u1.f"
    "Jan_23_2018/gwf2mnw17.f"
    "Jan_23_2018/gwf2mnw27.f"
    "Jan_23_2018/gwf2QRT8u.f"
    "Jan_23_2018/gwf2rch8u1.f"
    "Jan_23_2018/gwf2riv7u1.f"
    "Jan_23_2018/gwf2sfr7u1.f"
    "Jan_23_2018/gwf2str7u1.f"
    "Jan_23_2018/gwf2sub7u1.f"
    "Jan_23_2018/gwf2wel7u1.f"
    "Jan_23_2018/gwt2bndsu1.f"
    "Jan_23_2018/gwt2dptu1.f"
    "Jan_23_2018/lak_gag_sfr_modules.f"
    "Jan_23_2018/openspec.inc"
    "Jan_23_2018/parutl7.f"
    "Jan_23_2018/pcgu7.f"
    "Jan_23_2018/pmmodule_usg.f90"
    "Jan_23_2018/ppcgu1.f"
    "Jan_23_2018/sparse.f"
    "Jan_23_2018/tvmu1.f"
    "Jan_23_2018/tvmu2.f"
    "Jan_23_2018/utl7u1.f"
    "Jan_23_2018/xmd.f"
    "Jan_23_2018/xmdlib_2.f"
)
source_group("mfsource" FILES ${mfsource})

set(ALL_FILES
    ${Source_Files}
    ${mfsource}
)

################################################################################
# Target
################################################################################
add_library(${PROJECT_NAME} STATIC ${ALL_FILES})

use_props(${PROJECT_NAME} "${CMAKE_CONFIGURATION_TYPES}" "${DEFAULT_Fortran_PROPS}")
################################################################################
# Include directories
################################################################################
if("${CMAKE_VS_PLATFORM_NAME}" STREQUAL "Win32")
    target_include_directories(${PROJECT_NAME} PUBLIC
        "${CMAKE_CURRENT_SOURCE_DIR}/"
    )
elseif("${CMAKE_VS_PLATFORM_NAME}" STREQUAL "x64")
    target_include_directories(${PROJECT_NAME} PUBLIC
        "${CMAKE_CURRENT_SOURCE_DIR}/"
    )
endif()

################################################################################
# Compile definitions
################################################################################

################################################################################
# Compile and link options
################################################################################
if(${CMAKE_Fortran_COMPILER_ID} STREQUAL "Intel" AND WIN32)
    if("${CMAKE_VS_PLATFORM_NAME}" STREQUAL "Win32")
        target_compile_options(${PROJECT_NAME} PRIVATE
            $<$<CONFIG:Debug>:
                -debug:full;
                -Od;
                -traceback;
                -libs:static;-threads;-dbglibs;
                -check:bounds,stack;
                -warn:interfaces
            >
            -nologo
        )
    elseif("${CMAKE_VS_PLATFORM_NAME}" STREQUAL "x64")
        target_compile_options(${PROJECT_NAME} PRIVATE
            $<$<CONFIG:Debug>:
                -debug:full;
                -Od;
                -traceback;
                -libs:static;-threads;-dbglibs;
                -check:bounds,stack;
                -warn:interfaces
            >
            -nologo
        )
    endif()
endif()

if(${CMAKE_Fortran_COMPILER_ID} STREQUAL "Intel" AND UNIX)
    if("${CMAKE_VS_PLATFORM_NAME}" STREQUAL "Win32")
        target_compile_options(${PROJECT_NAME} PRIVATE
            $<$<CONFIG:Debug>:
                -debug full;
                -O0;
                -traceback;
                -threads;
                -check bounds,stack;
                -warn interfaces
            >
        )
    elseif("${CMAKE_VS_PLATFORM_NAME}" STREQUAL "x64")
        target_compile_options(${PROJECT_NAME} PRIVATE
            $<$<CONFIG:Debug>:
                -debug full;
                -O0;
                -traceback;
                -threads;
                -check bounds,stack;
                -warn interfaces
            >
        )
    endif()
endif()

