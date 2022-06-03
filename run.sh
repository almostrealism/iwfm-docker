#!/bin/sh
PATH=$PATH:/build/iwfm

ln -s / /var/www/html/files

wget -O model.zip $IWFM_MODEL
unzip model.zip

cd Preprocessor
dos2unix *.dat
PreProcessor C2VSimFG_Preprocessor.in
ls -la

cp C2VSimFG_Preprocessor.in ../Simulation/..\\Preprocessor\\C2VSimFG_Preprocessor.in
cp C2VSimFG_Elements.dat ../Simulation/..\\Preprocessor\\C2VSimFG_Elements.dat
cp C2VSimFG_Nodes.dat ../Simulation/..\\Preprocessor\\C2VSimFG_Nodes.dat
cp C2VSimFG_Stratigraphy.dat ../Simulation/..\\Preprocessor\\C2VSimFG_Stratigraphy.dat
cp C2VSimFG_StreamsSpec.dat ../Simulation/..\\Preprocessor\\C2VSimFG_StreamsSpec.dat

mv ..\\Simulation\\C2VSimFG_PreprocessorOut.bin ../Simulation/C2VSimFG_PreprocessorOut.bin

echo "Running initial simulation..."
cd /
/run_simulation.sh

cd /
echo "Running pest..."
/pestbin/pestpp-glm C2VSimFG_01.pst