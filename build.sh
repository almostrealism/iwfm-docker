#!/bin/sh
CC=cc
CXX=c++
CF=ifort

mkdir libs
mkdir libs/lib

cd szip
./configure
make
make install
cd ..
cp szip/szip/lib/* libs/lib/

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

cmake -S iwfm2obs -B build/iwfm2obs
cmake --build build/iwfm2obs

cmake -S mlt -B build/mlt
cmake --build build/mlt

cmake -S mfUSGLib -B build/mfUSGLib
cmake --build build/mfUSGLib

cp build/mfUSGLib/libmfUSGLib.a libs/lib/libmfUSGLib.a
cp build/mfUSGLib/mfUSGLib.Modules.dir/* libs/include/

cmake -S t2p -B build/t2p -DMFUSG_DIR=$(pwd)/libs
cmake --build build/t2p