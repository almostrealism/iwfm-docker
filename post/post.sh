#!/bin/sh
cd Simulation
/build/iwfm2obs/iwfm2obs_2017 <iwfm2obs_2015.in
/build/mlt/MultiLayerTarget MultiLayerTarget.in
ls -la