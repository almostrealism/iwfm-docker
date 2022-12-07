#!/bin/sh
PATH=$PATH:/build/iwfm
LD_LIBRARY_PATH=/opt/intel/oneapi/compiler/2022.0.2/linux/compiler/lib/intel64_lin
WORKING_PATH=$1

echo "WORKING_PATH = ${WORKING_PATH}"
cd ${WORKING_PATH}

cd Preprocessor
dos2unix *.dat
PreProcessor C2VSimFG_Preprocessor.in

cp C2VSimFG_Preprocessor.in ../Simulation/..\\Preprocessor\\C2VSimFG_Preprocessor.in
cp C2VSimFG_Elements.dat ../Simulation/..\\Preprocessor\\C2VSimFG_Elements.dat
cp C2VSimFG_Nodes.dat ../Simulation/..\\Preprocessor\\C2VSimFG_Nodes.dat
cp C2VSimFG_Stratigraphy.dat ../Simulation/..\\Preprocessor\\C2VSimFG_Stratigraphy.dat
cp C2VSimFG_StreamsSpec.dat ../Simulation/..\\Preprocessor\\C2VSimFG_StreamsSpec.dat

mv ..\\Simulation\\C2VSimFG_PreprocessorOut.bin ../Simulation/C2VSimFG_PreprocessorOut.bin

cd ${WORKING_PATH}
/run_model.sh