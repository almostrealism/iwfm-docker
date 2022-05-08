#!/bin/sh
cd Simulation
cp ../Preprocessor/C2VSimFG_Preprocessor.in ..\\Preprocessor\\C2VSimFG_Preprocessor.in
cp ../Preprocessor/C2VSimFG_Elements.dat ..\\Preprocessor\\C2VSimFG_Elements.dat
cp ../Preprocessor/C2VSimFG_Nodes.dat ..\\Preprocessor\\C2VSimFG_Nodes.dat
cp ../Preprocessor/C2VSimFG_Stratigraphy.dat ..\\Preprocessor\\C2VSimFG_Stratigraphy.dat
cp ../Preprocessor/C2VSimFG_StreamsSpec.dat ..\\Preprocessor\\C2VSimFG_StreamsSpec.dat
# /build/iwfm2obs/iwfm2obs_2017 <iwfm2obs_2015.in
/build/mlt/MultiLayerTarget MultiLayerTarget.in
ls -la
