#!/bin/sh
rm /Simulation/GW_MultiLayer.out
/run_simulation.sh

# Process output
cd /Simulation ; python3 /scripts/budget_hdf_to_text.py ..\\Results\\C2VSimFG_GW_Budget.hdf
cd /Simulation ; python3 /scripts/budget_hdf_to_text.py ..\\Results\\C2VSimFG_LWU_Budget.hdf
cd /Simulation ; python3 /scripts/budget_hdf_to_text.py ..\\Results\\C2VSimFG_RZ_Budget.hdf
cd /