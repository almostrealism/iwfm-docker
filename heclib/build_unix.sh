
icc --version
ifort --version
FC=ifort
CC=icc
CXX=icpc
export FC
export CC
export CXX
cmake -E remove_directory BUILD
cmake -E make_directory BUILD
cd BUILD
cmake -G "Unix Makefiles" -DCMAKE_BUILD_TYPE=Release ../
#cmake --build . --target ALL_BUILD --config Debug
#cmake --build . --target ALL_BUILD --config Release
cmake --build .
