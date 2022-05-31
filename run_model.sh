#!/bin/sh
rm /Simulation/GW_MultiLayer.out
/run_simulation.sh

echo "Moving files..."
mv /Simulation/* C2VSimFG/simulation/