import sys
import os
import math

import numpy as np
import pandas as pd
import shapefile as shp
import matplotlib.pyplot as plt
import matplotlib.patches as mpatches

from matplotlib.backends.backend_pdf import PdfPages
from descartes import PolygonPatch

import sspa_tools

# For plotting style
try:
    plt.style.use('plotadopulos-white')
except IOError:
    try:
        plt.style.use('r:/software/SSP&A Open-Source Code/'
                      'Python Plotadopulos Plot Style/plotadopulos-white.mplstyle')
    except IOError:
        print("Warning: plotadopulos MPL style is not found. Plots will not look as intended.")
        print("See `style.available` for list of available styles.")

def read_from_command_line(args):
    ''' returns a list of inputs provided in a text file for running the program '''
    if len(args) == 2:
        with open(args[-1], 'r') as f:
            input_data = f.read()
    elif len(args) == 1:
        file_name = input("Please specify the name of the input file:\n")
        with open(file_name, 'r') as f:
            input_data = f.read()
    else:
        raise TypeError("Too many arguments were provided.")

    # parse values from file to list
    input_list = input_data.split('\n')

    clean_list = [item for item in input_list if len(item) != 0 and item[0] != '#']
    
    return clean_list

if __name__ == "__main__":

    inputs_list = read_from_command_line(sys.argv)

    pest_folder = inputs_list[0]
    sim_folder = inputs_list[1]
    obs_file = inputs_list[2]
    modweighted_file = inputs_list[3]
    wellinfo_file = inputs_list[4]
    pdf_name = inputs_list[5]
    well_layer_fraction = inputs_list[6]

    # Filtering
    subregion_groups = [[int(val) for val in grp.split(',')] for grp in inputs_list[7].split(';')]
    sr_group_names = inputs_list[8].split(',')
    
    if '*' in inputs_list[9]:
        date_breaks = []
    else:
        date_breaks = pd.to_datetime(inputs_list[9].split(',')) # Needs start/end dates (e.g. before dataset, after dataset)

    # read observation well info file
    wellinfo = sspa_tools.format_well_info(wellinfo_file)

    # read well observation file
    wlobs = sspa_tools.format_water_level_observations(sim_folder, obs_file)
    
    # read well simulated head file - output from MultiLayerTarget.exe
    wlweight = sspa_tools.format_simulated_head_data(sim_folder, modweighted_file)
    
    # Read RES weights
    res = sspa_tools.format_pest_res_data(pest_folder, 'wlt')
    
    # Read in MultiLayer_LayFrac
    layer_fractions = sspa_tools.format_layer_fractions(sim_folder, well_layer_fraction)
    
    layfrac1 = layer_fractions[layer_fractions['F1'] >= 0.5]
    layfrac2 = layer_fractions[layer_fractions['F2'] >= 0.5]
    layfrac3 = layer_fractions[layer_fractions['F3'] >= 0.5]

    # Add RES weights to observed data
    wlobs = sspa_tools.add_res_weights_to_data(wlobs, 'ObsWL', res, 'Measured')

    # Sameify
    wlweight, wlobs, zeroweight = sspa_tools.prep_well_data(wlweight, wlobs, return_zero_weights=True)

    well_subs = [layfrac1, layfrac2, layfrac3]
    sub_names = ['Layer 1', 'Layer 2', 'Layer 3']
    
    # Create PDF(s) for adding plots to, use massplot to create plots
    with PdfPages(pdf_name) as pp:
        plt.ioff()

        sspa_tools.do_plots(wlobs, wlweight, zeroweight, wellinfo, 
                            well_subs, sub_names, date_breaks, 
                            subregion_groups, sr_group_names, pp)

        plt.close('all')
        plt.ion()