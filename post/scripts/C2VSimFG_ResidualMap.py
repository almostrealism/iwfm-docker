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

# Font
plt.rcParams["font.family"] = "sans-serif"
plt.rcParams['font.sans-serif'] = 'Arial, sans-serif'

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
    shape_dir = inputs_list[5]
    model_outline = inputs_list[6]
    pdf_name = inputs_list[7]
    well_layer_fraction = inputs_list[8]
  
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

    # Add RES weights to observed head data
    wlobs = sspa_tools.add_res_weights_to_data(wlobs, 'ObsWL', res, 'Measured')
    
    # Sameify
    wlweight, wlobs = sspa_tools.prep_well_data(wlweight, wlobs)

    # Calculate stats
    wellstats = sspa_tools.calculate_mean_residuals(wellinfo, wlweight, wlobs)

    colors = {'default': '#45f226', 
                      5: '#ffcc00', 
                     15: '#ff6a00', 
                     30: '#e00000', 
                    100: '#A00000', 
                     -5: '#00c7ff', 
                    -15: '#008cff', 
                    -30: '#0c00ff', 
                   -100: '#4B0082'}

    wellstats = sspa_tools.add_plot_formatting(wellstats, color_dict=colors)
  
    # output table
    sspa_tools.calculate_residuals(wellinfo, wlweight, wlobs).to_csv('residual_table.csv')
    wellstats.to_csv('mean_residual_table.csv')

    # Read in shapefiles
    river_shp = shp.Reader(shape_dir + 'C2VSimFG_StreamReaches')
    model_shp = shp.Reader(shape_dir + model_outline)
    shapelist = [river_shp, model_shp]
    shapecolors = ['#1F77B4', '#666666']
    line_widths = [0.9, 1.5]
    zorders = [-1, 1]

    # Create PDF(s) for adding plots to, use massplot to create plots
    with PdfPages(pdf_name) as pp:
        plt.ioff()

        print('Plotting All Layer Residuals')
        sspa_tools.plot_residual_map(wellstats, 
                                     "Average Residuals - All Layers", 
                                     pp, shapelist, shapecolors, 
                                     line_widths, zorders)

        # Plot by Layer
        print('Plotting Layer 1 Residuals')
        sspa_tools.plot_residual_map(wellstats[wellstats.index.isin(layfrac1['Name'].to_numpy())],
                    "Average Residuals - Layer 1",
                    pp, shapelist, shapecolors,
                    line_widths, zorders)
        print('Plotting Layer 2 Residuals')
        sspa_tools.plot_residual_map(wellstats[wellstats.index.isin(layfrac2['Name'].to_numpy())],
                    "Average Residuals - Layer 2",
                    pp, shapelist, shapecolors,
                    line_widths, zorders)
        print('Plotting Layer 3 Residuals')
        sspa_tools.plot_residual_map(wellstats[wellstats.index.isin(layfrac3['Name'].to_numpy())],
                    "Average Residuals - Layer 3",
                    pp, shapelist, shapecolors,
                    line_widths, zorders)

        plt.close('all')
        plt.ion()
    
    

    
