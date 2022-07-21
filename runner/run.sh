#!/bin/sh
PATH=$PATH:/build/iwfm

/init.sh

cd /
echo "Running pest..."
/pestbin/pestpp-$PEST_CMD C2VSimFG_01.pst

echo "Running postprocessing..."
python3 /scripts/plot_rz_budget.py /Simulation/..\\Results\\C2VSimFG_RZ_Budget.hdf