#!/bin/sh

CC=cc
CXX=c++
FC=ifort

mkdir libs
mkdir libs/lib

cd szip
./configure
make
make install
cd ..
cp szip/szip/lib/* libs/lib/

cmake -S h5fortran/scripts -B build/hdf5 -DCMAKE_INSTALL_PREFIX=$(pwd)/libs -DBUILD_SHARED_LIBS=true
cmake -S heclib -B build/heclib
cmake --build build/hdf5
cmake --build build/heclib

ls -la libs/lib

#cp build/heclib/libheclib.a libs/lib/libheclib.a
cp build/heclib/libheclib.so libs/lib/libheclib.so

CC=icc
CXX=icpc
FC=ifort

echo "Building IWFM..."
cmake -S iwfm -B build/iwfm -DHDF5_DIR=$(pwd)/libs
cmake --build build/iwfm

cp build/iwfm/libIWFMLib.so libs/lib/libIWFMLib.so

cmake -S iwfm2obs -B build/iwfm2obs
cmake --build build/iwfm2obs

cmake -S srs -B build/srs
cmake --build build/srs

cmake -S mlt -B build/mlt
cmake --build build/mlt

cmake -S mfUSGLib -B build/mfUSGLib
cmake --build build/mfUSGLib

cp build/mfUSGLib/libmfUSGLib.a libs/lib/libmfUSGLib.a
cp build/mfUSGLib/mfUSGLib.Modules.dir/* libs/include/

cmake -S t2p -B build/t2p -DMFUSG_DIR=$(pwd)/libs
cmake --build build/t2p

FC=ifort

cmake -S cth -B build/cth
cmake --build build/cth