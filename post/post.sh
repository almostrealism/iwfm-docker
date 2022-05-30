#!/bin/sh

echo "creating directories"
mkdir C2VSimFG
mkdir C2VSimFG/simulation
echo "moving files"
mv /Simulation/* C2VSimFG/simulation/
echo "running pest"
/pestbin/pestpp-glm C2VSimFG_01.pst