cmake_minimum_required (VERSION 3.2.2)
project(heclib C CXX Fortran)
enable_language(C CXX Fortran)

#####################
### GENERAL FLAGS ###
#####################
set(CMAKE_CXX_FLAGS "-fPIC")
set(CMAKE_C_FLAGS "-fPIC")

# Fortran flags
set(CMAKE_Fortran_MODULE_DIRECTORY ${PROJECT_BINARY_DIR}/modules)
#--- Fortran compiler options
#if (Fortran_COMPILER_NAME MATCHES "gfortran.*")
#  # gfortran
#  set (CMAKE_Fortran_FLAGS_RELEASE "-funroll-all-loops -fno-f2c -O3")
#  set (CMAKE_Fortran_FLAGS_DEBUG   "-fno-f2c -O0 -g")
#elseif (Fortran_COMPILER_NAME MATCHES "ifort.*")
#  # ifort (untested)
#  if (WIN32)
#     set (CMAKE_Fortran_FLAGS_RELEASE "/nologo /MT /O2 /debug:none")
#     set (CMAKE_Fortran_FLAGS_DEBUG   "/nologo /MT /debug:full /Qfp-stack-check /dbglibs /check:all /traceback /warn:unused")
#  else (WIN32)
#      # free format
#      set (CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -extend_source 132 -m32 -fPIC")  
#      set (CMAKE_Fortran_FLAGS_RELEASE "-f77rtl -O3")
#      set (CMAKE_Fortran_FLAGS_DEBUG   "-f77rtl -O0 -fp-stack-check -fstack-protector-all -check bounds -check uninit -traceback -g -debug full -debug-parameters all")
#  endif (WIN32)
#elseif (Fortran_COMPILER_NAME MATCHES "g77")
#  # g77
#  set (CMAKE_Fortran_FLAGS_RELEASE "-funroll-all-loops -fno-f2c -O3 -m32")
#  set (CMAKE_Fortran_FLAGS_DEBUG   "-fno-f2c -O0 -g -m32")
#else (Fortran_COMPILER_NAME MATCHES "gfortran.*")
#  message ("CMAKE_Fortran_COMPILER full path: " ${CMAKE_Fortran_COMPILER})
#  message ("Fortran compiler: " ${Fortran_COMPILER_NAME})
#  message ("No optimized Fortran compiler flags are known, we just try -O2...")
#  set (CMAKE_Fortran_FLAGS_RELEASE "-O2")
#  set (CMAKE_Fortran_FLAGS_DEBUG   "-O0 -g")
#endif (Fortran_COMPILER_NAME MATCHES "gfortran.*")

set (CMAKE_Fortran_FLAGS "-fPIC")
# set (CMAKE_Fortran_FLAGS_RELEASE "-m32 -fPIC -f77rtl -O3")
# set (CMAKE_Fortran_FLAGS_DEBUG   "-m32 -fPIC -f77rtl -O0 -fp-stack-check -fstack-protector-all -check bounds -check uninit -traceback -g -debug full -debug-parameters all")

#-------------add linker flags and excluded and included libraries
## add pre build command
#add_custom_command(OUTPUT version.fi PRE_BUILD COMMAND python version_generate.py WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})
# search by file pattern and include in library
file(GLOB sources dc/*.f dss/*.f gen/*.f pread/*.f system/linux/*.f)
file(GLOB c_sources system/*.c system/linux/*.c)
file(GLOB cpp_sources system/*.cpp)
set_source_files_properties(gen/fortranwrite.f PROPERTIES COMPILE_FLAGS "-names lowercase")
set_source_files_properties(system/linux/intel_f_interfaces.f PROPERTIES COMPILE_FLAGS "-names as_is")
set_source_files_properties(extra_intel64.f PROPERTIES COMPILE_FLAGS "-extend_source 132")

add_library(heclib_f OBJECT ${sources})
target_include_directories(heclib_f PUBLIC fortran_include)
add_library(heclib_c OBJECT ${c_sources})
add_library(heclib_cpp OBJECT ${cpp_sources})

# add_library(heclib STATIC "$<TARGET_OBJECTS:heclib_f>" "$<TARGET_OBJECTS:heclib_c>" "$<TARGET_OBJECTS:heclib_cpp>")
add_library(heclib SHARED "$<TARGET_OBJECTS:heclib_f>" "$<TARGET_OBJECTS:heclib_c>" "$<TARGET_OBJECTS:heclib_cpp>")
