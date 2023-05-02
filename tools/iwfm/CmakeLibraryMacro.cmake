macro (SET_COMPILER_FLAGS)
  set(OLD_LINUX "NO")
  # make sure that the default is a RELEASE
  if( NOT CMAKE_BUILD_TYPE )
    set( CMAKE_BUILD_TYPE Release CACHE STRING
         "Choose the type of build, options are: None Debug Release RelWithDebInfo MinSizeRel."
         FORCE )
  endif()
  if(WIN32)
    #- To ensure static linking for C++ code
    set(CMAKE_CXX_FLAGS_RELEASE "/MT")
    set(CMAKE_CXX_FLAGS_DEBUG "/MTd")
    #- To ensure static linking for C code
    set(CMAKE_C_FLAGS_RELEASE "/MT")
    set(CMAKE_C_FLAGS_DEBUG "/MTd")
  else (WIN32)
    set(CMAKE_C_FLAGS_RELEASE "-O3 -xHost -no-prec-div -fPIC")
  endif (WIN32)
  # Fortran flags
  set(CMAKE_Fortran_MODULE_DIRECTORY ${PROJECT_BINARY_DIR}/modules)
  #--- Fortran compiler options
  if(WIN32)
    message(STATUS "Fortran Compiler Name: ${Fortran_COMPILER_NAME}")
  else(WIN32) #Bug in linux version of cmake. cannot identify fortran compiler
    get_filename_component(Fortran_COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME)
    message(STATUS "Fortran Compiler Name: ${Fortran_COMPILER_NAME}")
  endif(WIN32)  
  # set compiler specific options
  if (Fortran_COMPILER_NAME MATCHES "gfortran.*")
    # gfortran
    set (CMAKE_Fortran_FLAGS_RELEASE "-funroll-all-loops -fno-f2c -O3")
    set (CMAKE_Fortran_FLAGS_DEBUG   "-fno-f2c -O0 -g")
  elseif (Fortran_COMPILER_NAME MATCHES "ifort.*")
    # ifort
    if (WIN32)
      message(STATUS "Matched Intel Fortran Compiler on Windows")
      set (CMAKE_Fortran_FLAGS "/threads /libs:static")  # /standard-semantics. not needed?
      set (CMAKE_Fortran_FLAGS_RELEASE "/O3")
      set (CMAKE_Fortran_FLAGS_DEBUG   "/Qdiag-disable:8290,10212 /debug:full /Od /traceback")
    else (WIN32)
      message(STATUS "Matched Intel Fortran Compiler on Linux")
      set (CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fPIC") #removing -standard-semantics as it is causing _MP_ instead of _mp_ for module linking  
      set (CMAKE_Fortran_FLAGS_RELEASE "-heap-arrays -f77rtl -O3 -xHost -no-prec-div")
      set (CMAKE_Fortran_FLAGS_DEBUG   "-heap-arrays -O0 -check bounds,stack -traceback -threads -g -debug full -debug-parameters all -gen-interfaces -warn interfaces -fpp -nologo -threads")
    endif (WIN32)
  elseif (Fortran_COMPILER_NAME MATCHES "g77")
    # g77
    set (CMAKE_Fortran_FLAGS_RELEASE "-funroll-all-loops -fno-f2c -O3 -m32")
    set (CMAKE_Fortran_FLAGS_DEBUG   "-fno-f2c -O0 -g -m32")
  else (Fortran_COMPILER_NAME MATCHES "gfortran.*")
    message ("CMAKE_Fortran_COMPILER full path: " ${CMAKE_Fortran_COMPILER})
    message ("Fortran compiler: " ${Fortran_COMPILER_NAME})
    message ("No optimized Fortran compiler flags are known, we just try -O2...")
    set (CMAKE_Fortran_FLAGS_RELEASE "-O2")
    set (CMAKE_Fortran_FLAGS_DEBUG   "-O0 -g")
  endif (Fortran_COMPILER_NAME MATCHES "gfortran.*")
  #-------------add linker flags and excluded and included libraries
endmacro(SET_COMPILER_FLAGS)
###############################################################################################################
macro (FIND_LIBS)
  #----------------- FIND THE LIBRARIES -------------------
  if (WIN32)   
    set(STATIC_LIB_SUFFIX "lib")
  else (WIN32)
    set(STATIC_LIB_SUFFIX "so") # This is normally "a", but we've switched to using shared libraries
  endif (WIN32)
  # ---------------- HDF5 setup -------------- hdf5.lib hdf5_hl_fortran.lib hdf5_fortran.lib hdf5_f90cstub.lib hdf5_hl.lib 
  set(HDF5_LIB_PREFIX "lib") # lib prefix if static compiled HDF5
  set(HDF5_LIB_PATH "${HDF5_DIR}/lib")
  set(HDF5_FORTRAN_INCLUDE_PATH "${HDF5_DIR}/include/static")
  include_directories("${HDF5_DIR}/include" "${HDF5_DIR}/include/static")
  find_library(HDF5 NAMES ${HDF5_LIB_PREFIX}hdf5.${STATIC_LIB_SUFFIX} PATHS ${HDF5_LIB_PATH} NO_DEFAULT_PATH)
  find_library(HDF5_HL NAMES ${HDF5_LIB_PREFIX}hdf5_hl.${STATIC_LIB_SUFFIX} PATHS ${HDF5_LIB_PATH} NO_DEFAULT_PATH)
  find_library(HDF5_CPP NAMES ${HDF5_LIB_PREFIX}hdf5_cpp.${STATIC_LIB_SUFFIX} PATHS ${HDF5_LIB_PATH} NO_DEFAULT_PATH)
  find_library(HDF5_HL_CPP NAMES ${HDF5_LIB_PREFIX}hdf5_hl_cpp.${STATIC_LIB_SUFFIX} PATHS ${HDF5_LIB_PATH} NO_DEFAULT_PATH)
  find_library(HDF5_FORTRAN NAMES ${HDF5_LIB_PREFIX}hdf5_fortran.${STATIC_LIB_SUFFIX} PATHS ${HDF5_LIB_PATH} NO_DEFAULT_PATH)
  find_library(HDF5_HL_FORTRAN NAMES ${HDF5_LIB_PREFIX}hdf5_hl_fortran.${STATIC_LIB_SUFFIX} PATHS ${HDF5_LIB_PATH} NO_DEFAULT_PATH)
  find_library(HDF5_F90CSTUB NAMES ${HDF5_LIB_PREFIX}hdf5_f90cstub.${STATIC_LIB_SUFFIX} PATHS ${HDF5_LIB_PATH} NO_DEFAULT_PATH)
  find_library(HDF5_HL_F90CSTUB NAMES ${HDF5_LIB_PREFIX}hdf5_hl_f90cstub.${STATIC_LIB_SUFFIX} PATHS ${HDF5_LIB_PATH} NO_DEFAULT_PATH)
  #szlib.lib zlib.lib 
  find_library(SZLIB NAMES ${HDF5_LIB_PREFIX}sz.${STATIC_LIB_SUFFIX} PATHS "${HDF5_LIB_PATH}" NO_DEFAULT_PATH)
  if (WIN32)
    find_library(ZLIB NAMES ${HDF5_LIB_PREFIX}zlib.${STATIC_LIB_SUFFIX} PATHS "${HDF5_LIB_PATH}" NO_DEFAULT_PATH)
  else (WIN32)
    find_library(ZLIB NAMES ${HDF5_LIB_PREFIX}z.${STATIC_LIB_SUFFIX} PATHS "${HDF5_LIB_PATH}" NO_DEFAULT_PATH)
  endif (WIN32)
  if (WIN32)
    set(LIB_PREFIX "")
    set(TOOLSET "_vs14")
    set(STATIC_COMPILE "_mt")
    set(DEBUG_COMPILE "d")
  else (WIN32)
    set(LIB_PREFIX "lib")
    set(TOOLSET "_intel")
    set(STATIC_COMPILE "")
    set(DEBUG_COMPILE "")
  endif (WIN32)
  set(COMPILE_INDICATOR ${STATIC_COMPILE})
  find_library(HECLIB_DEBUG NAMES ${LIB_PREFIX}heclib.${STATIC_LIB_SUFFIX} PATHS "${HDF5_LIB_PATH}" NO_DEFAULT_PATH)
  find_library(HECLIB_RELEASE NAMES ${LIB_PREFIX}heclib.${STATIC_LIB_SUFFIX} PATHS "${HDF5_LIB_PATH}" NO_DEFAULT_PATH)
endmacro(FIND_LIBS)
