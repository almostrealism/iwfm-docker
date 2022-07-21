#!/bin/sh
rm /Simulation/GW_MultiLayer.out
/run_simulation.sh

# Process output
python3 /scripts/budget_hdf_to_text.py /Simulation/..\\Results\\C2VSimFG_GW_Budget.hdf
python3 /scripts/budget_hdf_to_text.py /Simulation/..\\Results\\C2VSimFG_LWU_Budget.hdf
python3 /scripts/budget_hdf_to_text.py /Simulation/..\\Results\\C2VSimFG_RZ_Budget.hdf