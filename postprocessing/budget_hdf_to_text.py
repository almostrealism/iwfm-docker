

import os
import sys
import numpy as np
import pandas as pd

from pywfm import IWFMBudget

def read_from_commandline(args):
    if len(args) == 2:
        file_name = args[-1]
        if not os.path.exists(file_name):
            raise FileNotFoundError("file name {} does not exist.".format(file_name))
        return file_name
    else:
        raise ValueError("Too few or too many command line arguments provided")

if __name__ == "__main__":
    hdf_file = read_from_commandline(sys.argv)
    
    with IWFMBudget(hdf_file) as bud:
        # generate output file name
        out_name = "{}.out".format(os.path.splitext(os.path.basename(bud.budget_file_name))[0])
        
        # get location names
        locations = bud.get_location_names()
        

        # retrieve budgets
        budgets = []
        for i, location in enumerate(locations, start=1):
            
            # determine budget type from title
            budget_type = bud.get_title_lines(i)[1].split()[0]
            if budget_type == "GROUNDWATER":
                prefix = 'gwb'
            elif budget_type == "ROOT":
                prefix = 'rzb'
            elif budget_type == 'LAND':
                prefix = 'lwb'
            
            budget = bud.get_values(i)
            budget['location'] = location
            budget.set_index(['location', 'Time'], inplace=True)

            stacked_budget = budget.stack().reset_index()
            stacked_budget['pest_name'] = ["{}{:02d}_{:06d}".format(prefix, i, val+1) for val in range(len(stacked_budget))]
            
            budgets.append(stacked_budget[["pest_name", "location", "level_2", 0]])

        out_bud = pd.concat(budgets)
        out_bud.rename(columns={"level_2": "value_title", 0:"value"}, inplace=True)

        # write to output file
        with open(out_name, 'w') as f:
            out_bud.to_string(
                f,
                index=False,
                formatters={
                    "pest_name": "{:>12s}".format,
                    "location": "{:>20s}".format,
                    "value": "{:>25.7f}".format
                }
            )