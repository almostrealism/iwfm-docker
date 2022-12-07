#!/bin/sh
PATH=$PATH:/build/iwfm

echo "WORKING_PATH = ${WORKING_PATH}"
cd ${WORKING_PATH}

rm Simulation/GW_MultiLayer.out
/run_simulation.sh

# Process output
cd ${WORKING_PATH}/Simulation ; python3 /scripts/budget_hdf_to_text.py "..\Results\C2VSimFG_GW_Budget.hdf"
cd ${WORKING_PATH}/Simulation ; python3 /scripts/budget_hdf_to_text.py "..\Results\C2VSimFG_L&WU_Budget.hdf"
cd ${WORKING_PATH}/Simulation ; python3 /scripts/budget_hdf_to_text.py "..\Results\C2VSimFG_RZ_Budget.hdf"
cd ${WORKING_PATH}