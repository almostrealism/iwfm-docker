#!/bin/sh
CC=cc
CXX=c++
CF=ifort

cmake -S h5fortran/scripts -B build/hdf5 -DCMAKE_INSTALL_PREFIX=$(pwd)/libs
cmake -S heclib -B build/heclib
cmake --build build/hdf5
cmake --build build/heclib

cp build/heclib/libheclib.a libs/lib/libheclib.a

CC=icc
CXX=icpc
F9X=ifort

cmake -S iwfm -B build/iwfm -DHDF5_DIR=$(pwd)/libs
cmake --build build/iwfm