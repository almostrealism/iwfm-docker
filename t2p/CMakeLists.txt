cmake_minimum_required(VERSION 3.16.0 FATAL_ERROR)

project(Texture2Par Fortran)

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



























set(PROJECT_NAME Texture2Par)

################################################################################
# Source groups
################################################################################
set(Libraries64_release
    "mfUSGLib/x64/Release/mfUSGLib.lib"
)
source_group("Libraries64_release" FILES ${Libraries64_release})

set(Libraries_release
    "mfUSGLib/Release/mfUSGLib.lib"
)
source_group("Libraries_release" FILES ${Libraries_release})

set(Resource_Files
    "T2P.ico"
    "T2P.rc"
)
source_group("Resource Files" FILES ${Resource_Files})

set(Source_Files
    "fpath.f90"
    "KDTree.f90"
    "MakePar.f90"
    "mfsupport.f90"
    "readIWFM.f90"
    "spkrige.f90"
    "Texture2Par.f90"
    "Tools.f90"
    "writeLPF.f90"
)
source_group("Source Files" FILES ${Source_Files})

set(ALL_FILES
    ${Libraries64_release}
    ${Libraries_release}
    ${Resource_Files}
    ${Source_Files}
)

################################################################################
# Target
################################################################################
add_executable(${PROJECT_NAME} ${ALL_FILES})

use_props(${PROJECT_NAME} "${CMAKE_CONFIGURATION_TYPES}" "${DEFAULT_Fortran_PROPS}")
################################################################################
# Include directories
################################################################################
if("${CMAKE_VS_PLATFORM_NAME}" STREQUAL "Win32")
    target_include_directories(${PROJECT_NAME} PUBLIC
        "${CMAKE_CURRENT_SOURCE_DIR}/;"
        "${CMAKE_CURRENT_SOURCE_DIR}/mfUSGLib/Release"
    )
elseif("${CMAKE_VS_PLATFORM_NAME}" STREQUAL "x64")
    target_include_directories(${PROJECT_NAME} PUBLIC
        "${CMAKE_CURRENT_SOURCE_DIR}/;"
        "${CMAKE_CURRENT_SOURCE_DIR}/mfUSGLib/x64/Release"
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
                -check:bounds;
                -warn:interfaces
            >
            -nologo;
            -real-size:64
        )
    elseif("${CMAKE_VS_PLATFORM_NAME}" STREQUAL "x64")
        target_compile_options(${PROJECT_NAME} PRIVATE
            $<$<CONFIG:Debug>:
                -debug:full;
                -Od;
                -traceback;
                -warn:interfaces
            >
            -nologo;
            -real-size:64
        )
    endif()
    if("${CMAKE_VS_PLATFORM_NAME}" STREQUAL "Win32")
        target_link_options(${PROJECT_NAME} PRIVATE
            $<$<CONFIG:Debug>:
                /DEBUG
            >
            /INCREMENTAL:NO;
            /NOLOGO;
            /SUBSYSTEM:CONSOLE
        )
    elseif("${CMAKE_VS_PLATFORM_NAME}" STREQUAL "x64")
        target_link_options(${PROJECT_NAME} PRIVATE
            $<$<CONFIG:Debug>:
                /DEBUG
            >
            /INCREMENTAL:NO;
            /NOLOGO;
            /SUBSYSTEM:CONSOLE
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
                -check bounds;
                -warn interfaces
            >
            -real-size 64
        )
    elseif("${CMAKE_VS_PLATFORM_NAME}" STREQUAL "x64")
        target_compile_options(${PROJECT_NAME} PRIVATE
            $<$<CONFIG:Debug>:
                -debug full;
                -O0;
                -traceback;
                -warn interfaces
            >
            -real-size 64
        )
    endif()
endif()

