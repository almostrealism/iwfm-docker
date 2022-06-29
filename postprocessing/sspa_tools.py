# -*- coding: utf-8 -*-
"""
functions by Leland Scantlebury
updates and docstrings by Tyler Hatch
"""
import os
import sys
import numpy as np
import pandas as pd
import shapefile as shp
import matplotlib.pyplot as plt
import matplotlib.patches as mpatches

from matplotlib.backends.backend_pdf import PdfPages
from descartes import PolygonPatch

import massplot as mp

#------------------------------------------------------------------------------
# FUNCTIONS
#------------------------------------------------------------------------------
def format_well_info(wellinfo_file):
    ''' reads and removed missing data from well info file 
    Parameters
    ----------
    wellinfo_file: str
        file path and name for well info file
        
    Returns
    -------
    pd.DataFrame
        pandas DataFrame of well info data
    '''
    # read observation well info file
    wellinfo = pd.read_csv(wellinfo_file)

    # Remove non-FG Wells, does nothing if no missing values
    wellinfo.dropna(axis=0, how='any', subset=['FG_Elem'], inplace=True)

    return wellinfo

def format_water_level_observations(path, obs_file):
    ''' reads and formats water level observation data.
    Parameters
    ----------
    path : str
        file path to observation data
        
    obs_file : str
        name of file containing observation well data used as water level targets
        for calibration
        
    Returns
    -------
    pd.DataFrame
        pandas DataFrame containing formatted observed head data'''
    # read water level observation file
    wlobs = pd.read_csv(path + obs_file, header=None,
                            names=['NameLayer','Date','Time','ObsWL'],
                            parse_dates = ['Date'], infer_datetime_format=True,
                            delim_whitespace=True)
    
    # Separate NameLayer field into three columns Model, Name, and Layer
    wlobs['Model'] = wlobs['NameLayer'].apply(lambda x: x.split('_')[0])
    wlobs['Name'] = wlobs['NameLayer'].apply(lambda x: x.split('_')[1].split('%')[0])
    wlobs['Layer'] = wlobs['NameLayer'].apply(lambda x: int(x.split('%')[1]))

    # filter observed for only layer 1, same obs data used for all 4 layers
    wlobs = wlobs.loc[wlobs['Layer'] == 1][['Name', 'Date', 'ObsWL']]

    return wlobs

def format_simulated_head_data(path, sim_file):
    ''' reads and formats simulated head data. This data is transmissivity
    weighted heads based on wells that span multiple model layers
    
    Paramters
    ---------
    path : str
        file path where simulated heads file is located
        
    sim_file : str
        name of file containing transmissivity weighted simulated heads
    
    Returns
    -------
    pd.DataFrame
        pandas DataFrame containing simulated head data with columns
        converted to correct data type and Name column split into two
        columns Model and Name '''
    # read well simulated head file - output from MultiLayerTarget.exe
    wlweight = pd.read_csv(path + sim_file,
                        parse_dates=['Date'], infer_datetime_format=True,
                        delim_whitespace=True)

    # check columns after Name, Date, and Time are type float
    if not all([wlweight[col].dtype == np.float for col in wlweight.columns[3:]]):
        wlweight.iloc[:, 3:] = wlweight.iloc[:, 3:].astype(float)

    # Separate Name into two columns Model and Name
    wlweight['Model'] = wlweight['Name'].apply(lambda x: x.split('_')[0])
    wlweight['Name'] = wlweight['Name'].apply(lambda x: x.split('_')[1])

    return wlweight

def format_pest_res_data(path, filter_string):
    ''' gets the latest file of .res format and reads it to a dataframe,
    then filters based on a provided string 
    Parameters
    ----------
    path : str
        file path to search for res file
        
    filter_string : str
        prefix used to filter a subset of res data
    
    Returns
    -------
    pd.DataFrame
        pandas DataFrame containing filtered PEST RES data
    '''
    # Read RES weights
    res_file = get_latest_file_bytype(path, '.res')
    res = pd.read_csv(res_file, delim_whitespace=True)
    res = res.loc[res['Name'].str.startswith(filter_string)]

    return res

def format_layer_fractions(path, layer_fraction_file):
    ''' reads layer fractions data from file and formats Model and Name columns based
    on a column called obsname 
    Parameters
    ----------
    path : str
        file path to layer fraction file
        
    layer_fraction_file : str
        file containing layer fraction data for wells
        
    Returns
    -------
    pd.DataFrame
        pandas dataframe containing layer fractions and obsname column split to create model and name column 
    '''
    layer_fractions = pd.read_csv(path + layer_fraction_file, delim_whitespace=True, low_memory=False)
    
    layer_fractions[['Model', 'Name']] = layer_fractions['obsname'].str.split('_', expand=True)

    return layer_fractions

def add_res_weights_to_data(data, data_column, res_data, res_column):
    ''' adds PEST RES weights to data based on matching order of 
    data_column and res_column 
    Parameters
    ----------
    data : pd.DataFrame
        pandas dataframe containing dataset to add RES weights
        
    data_column : str
        name of column in data used to check order
    
    res_data : pd.DataFrame
        pandas DataFrame containing RES data
    
    res_column: str
        name of column in RES file used to check order
        
    Returns
    pd.DataFrame
        data dataframe with RES weights added
    '''
    check = np.allclose(data[data_column].to_numpy(), res_data[res_column].to_numpy(), rtol=0, atol=0.001)
    if check:
        data['Weight'] = res_data['Weight'].to_numpy()
    else:
        raise ValueError('ERROR: Well Observation and PEST RES file are not in the same order.')
    
    return data

def prep_well_data(df_sim, df_obs, filter_zero_weights=True, return_zero_weights=False):
    ''' cleans simulated and observed data to match dates and well names

    Parameters
    ----------
    df_sim : pd.DataFrame
        dataframe object containing simulated heads at well locations

    df_obs : pd.DataFrame
        dataframe object containing observed heads at well locations

    filter_zero_weights : bool, default=True
        flag to filter out zero-weighted observation wells

    return_zero_weights : bool, default=False
        flag to return a third DataFrame of zero-weighted values. 
        Only returned as a separate when filter_zero_weights is True.
        Otherwise, values are included in df_sim and df_obs

    Returns
    -------
    tuple of pd.DataFrame
        cleaned df_sim, df_obs, and optionally zero_weights
    '''
    # check df_sim has columns 'Date', 'Name', and 'Simulated'
    if not {'Date', 'Name', 'Simulated'}.issubset(df_sim.columns):
        raise ValueError('df_sim must have columns "Date", "Name", and "Simulated"')

    # check df_obs has columns 'Date', 'Name', 'ObsWL', 'Weight'
    if not {'Date', 'Name', 'ObsWL', 'Weight'}.issubset(df_obs.columns):
        raise ValueError('df_obs must have columns "Date", "Name", "ObsWL", and "Weight"')

    # keeps matching dates and names only
    wl_combined = pd.merge(df_obs, df_sim, on=['Name', 'Date'])

    if filter_zero_weights and return_zero_weights:
        df_sim = wl_combined[wl_combined['Weight'] > 0][['Name', 'Date', 'Simulated']]
        df_obs = wl_combined[wl_combined['Weight'] > 0][['Name', 'Date', 'ObsWL']]
        zero_weights = wl_combined[wl_combined['Weight'] <= 0][['Name', 'Date', 'ObsWL', 'Simulated']]

        return df_sim, df_obs, zero_weights

    elif filter_zero_weights:
        df_sim = wl_combined[wl_combined['Weight'] > 0][['Name', 'Date', 'Simulated']]
        df_obs = wl_combined[wl_combined['Weight'] > 0][['Name', 'Date', 'ObsWL']]
        
        return df_sim, df_obs

    else:
        # include weights if 0-weights are not filtered so they can be reviewed later
        df_sim = wl_combined[['Name', 'Date', 'Simulated', 'Weight']]
        df_obs = wl_combined[['Name', 'Date', 'ObsWL', 'Weight']]

        return df_sim, df_obs

def get_latest_file_bytype(path, extension):
    ''' returns the latest file with the provided file extension
    
    Parameters
    ----------
    path : str
        path to directory where file exists
        
    extension : str
        file extension for file
        
    Returns
    -------
    str
        path and name of latest file in folder with provided extension
    '''
    files = [os.path.join(path, f) for f in os.listdir(path) if f.endswith(extension)]

    if len(files) == 0:
        raise FileNotFoundError('No file with extension {} was found in {}'.format(extension, path))

    elif len(files) == 1:
        return files[0]

    else:
        time_modified = np.array([os.path.getmtime(f) for f in files])
        return files[np.argmax(time_modified)] 

def gen_MultiLayerTargetHeader(file_name):
    ''' returns number of layers and header list from multilayer target
    file assuming header line, first row, is incorrect.

    Parameters
    ----------
    file_name : str
        path and name to simulated transmissivity weighted head file 
        for multilayer wells

    Returns
    -------
    tuple
        int number of layers based on column count and list of column names

    '''
    # read first data line, 2nd line
    with open(file_name) as f:
        f.readline()  # Incorrect Header
        cols = len(f.readline().split())
    
    # calculate number of layers from number of data columns
    layers = cols - 6
    
    # generate column name list
    header = ['Name', 'Date', 'Time', 'Simulated']
    header += ['T{}'.format(i) for i in range(1,layers+1)]
    header += ['NewTOS','NewBOS']
    
    return layers, header

def blankify_plot(axis_object):
    ''' resets tick parameters and tick labels for a matplotlib Axes object '''
    axis_object.tick_params(axis = 'x',          # changes apply to the x-axis
        which = 'both',       # both major and minor ticks are affected
        bottom = False,       # ticks along the bottom edge are off
        top = False,          # ticks along the top edge are off
        labeltop = False,
        labelbottom = False)  # labels along the bottom edge are off
    
    axis_object.tick_params(axis = 'y',          # changes apply to the y-axis
        which = 'both',      # both major and minor ticks are affected
        left = False,      # ticks along the bottom edge are off
        right = False,         # ticks along the top edge are off
        labelright = False,
        labelleft = False)  # labels along the bottom edge are off
    
    axis_object.xaxis.set_ticklabels([])
    axis_object.yaxis.set_ticklabels([])


def get_shape_type(shape_object):
    ''' returns the shapefile type based on the integer value provided on
    pg. 4 of the ESRI shapefile specification.
    https://www.esri.com/library/whitepapers/pdfs/shapefile.pdf

    Parameters
    ----------
    shape_object : shapefile object from pyShp shp.Reader()
        shapefile object 

    returns
    -------
    str
        shape type as a string
    '''
    if shape_object.shapeType in [1,11,21]:
        return 'point'
    
    if shape_object.shapeType in [3,13,23]:
        return 'line'
    
    if shape_object.shapeType in [5,15,25]:
        return 'polygon'
    
    if shape_object.shapeType in [8,18,28]:
        return 'multipoint'
    
    if shape_object.shapeType in [31]:
        return 'multipatch'


def add_shapefiles(axis_object, shapelist, shapecolors, linewidths=None, zorders=None):
    ''' draws line or polygon shapefiles on a matplotlib axes object
    
    Parameters
    ----------
    axis_object : matplotlib.axes._subplots.AxesSubplot
        Axes object instantiated by fig, ax = plt.subplots() or ax = fig.add_subplot()
        
    shapelist : list
        list of shapefiles to be added to axis_object
        
    shapecolors : list
        list of Hexadecimal colors for shapefiles. shapecolors must be the 
        same length as shapelist
        
    linewidths : list, NoneType, default=None
        list of linewidths for line shapefiles. linewidths should be the 
        same length as shapelist

    zorders : list, NoneType, default=None
        zorder for  matplotlib artist. lower zorder values are drawn first.
        zorder should be the same length as shapelist.
     '''
    if linewidths is None:
        linewidths = [1] * len(shapelist)
    
    if zorders is None:
        zorders = [1] * len(shapelist)
    
    for i, item in enumerate(shapelist):
        item_type = get_shape_type(item)
        
        if item_type == 'polygon':
            for shape in item.iterShapes():
                axis_object.add_patch(PolygonPatch(shape,
                                    fc=shapecolors[i], ec=shapecolors[i], zorder=zorders[i]))
        if item_type == 'line':
            for shape in item.iterShapes():
                x = [j[0] for j in shape.points[:]]
                y = [j[1] for j in shape.points[:]]
                axis_object.plot(x,y, color=shapecolors[i], linewidth=linewidths[i],
                                 zorder=zorders[i])


def calculate_residuals(well_info, df_sim, df_obs):
    ''' calculates the residuals for simulated heads at observation well locations.

    Parameters
    ----------
    well_info : pd.DataFrame
        DataFrame containing well information including well name, location information, GSE,
        top of screen (TOS), bottom of screen (BOS)

    df_sim : pd.DataFrame
        DataFrame containing model simulated groundwater heads by well name and date

    df_obs : pd.DataFrame
        DataFrame containing observed groundwater heads by well name and date

    Returns
    -------
    pd.DataFrame
        DataFrame containing mean residuals (Observed - Simulated) by well name and x,y-location
    '''
    # check well_info has 'Name' and 'X', 'Y'
    if not {'Name', 'X', 'Y'}.issubset(well_info.columns):
        raise ValueError('well_info must have columns "Name", "X", and "Y"')

    # check df_sim has columns 'Date', 'Name', and 'Simulated'
    if not {'Date', 'Name', 'Simulated'}.issubset(df_sim.columns):
        raise ValueError('df_sim must have columns "Date", "Name", and "Simulated": has {}'.format(df_sim.columns))

    # check df_obs has columns 'Date', 'Name', 'ObsWL'
    if not {'Date', 'Name', 'ObsWL'}.issubset(df_obs.columns):
        raise ValueError('df_obs must have columns "Date", "Name", and "ObsWL"')

    # combine observed and simulated data
    df_residuals = pd.merge(df_obs, df_sim, on=['Name', 'Date'])
    
    # add well info to residuals DataFrame
    df_residuals = pd.merge(df_residuals, well_info[['Name', 'X', 'Y']], on='Name')

    # rename Simulated column to SimWL
    df_residuals.rename(columns={'Simulated': 'SimWL'}, inplace=True)

    # calculate residuals as Observed WL - Simulated WL
    df_residuals['Residual'] = df_residuals['ObsWL'] - df_residuals['SimWL']

    return df_residuals[['Name', 'X', 'Y', 'Date', 'ObsWL', 'SimWL', 'Residual']]

def calculate_mean_residuals(well_info, df_sim, df_obs):
    ''' calculates the mean residuals from all residuals'''
    df_residuals = calculate_residuals(well_info, df_sim, df_obs)
    return df_residuals[['Name', 'X', 'Y', 'ObsWL', 'SimWL', 'Residual']].groupby('Name').mean()

def calculate_max_residuals(well_info, df_sim, df_obs):
    ''' calculates the mean residuals from all residuals'''
    df_residuals = calculate_residuals(well_info, df_sim, df_obs)
    return df_residuals[['Name', 'X', 'Y', 'ObsWL', 'SimWL', 'Residual']].groupby('Name').max()

def add_plot_formatting(df_residuals, color_dict, default_color=None):
    ''' adds pt size and colors to residuals for plotting
    
    Parameters
    ----------
    df_residuals : pd.DataFrame
        pandas DataFrame object containing model residuals at observation well locations
        
    color_dict : dict
        dictionary for mapping colors based on threshold values for residuals
        negative values use less than or equal to value and positive values use 
        greater than or equal to value.
        e.g. {threshold_value1: color1, threshold_value2: color2, ...}. if 
        default_color is not specified, must include a threshold_value of 'default'

    default_color : str
        hexadecimal color for default assignment to dataframe
    '''
    if not {'Residual'}.issubset(df_residuals.columns):
        raise ValueError('DataFrame provided must have a column titled "Residual"')

    # create a local copy for modification
    df_residuals = df_residuals.copy()

    # add 'pt_size' column to df_residuals
    df_residuals['pt_size'] = 1 + 1 * np.sqrt(np.abs(df_residuals['Residual']))

    # add 'color' column to df_residuals
    if default_color is None:
        df_residuals['color'] = color_dict.pop('default')
    else:
        df_residuals['color'] = default_color

    for key, val in color_dict.items():
        if key > 0:
            df_residuals.loc[df_residuals['Residual'] >= key, 'color'] = val
        else:
            df_residuals.loc[df_residuals['Residual'] <= key, 'color'] = val

    df_residuals.sort_values('pt_size', ascending=False, inplace=True)

    return df_residuals

def round_to_nearest(value, base):
    '''rounds value to nearest increment of base '''
    return base * round(value/base, 0)

def plot_residual_map(headtargs, plt_title, pp, shapelist, shapecolors, linewidths, zorders):
    ''' plots residuals on a map and writes to a pdf
    Parameters
    ----------
    headtargs : pd.DataFrame
        pandas DataFrame object containing simulated and observed data with
        x,y-locations for plotting
        
    plt_title : str
        title of the figure
        
    pp : PdfPages object
        pdf to save the residual maps
        
    shapelist : list
        list of shapefile objects. see add_shapefiles function.
        
    shapecolors : list
        list of hexadecimal shape colors. see add_shapefiles function.
        
    linewidths : list
        list of linewidths for line shapefiles. see add_shapefiles function
        
    zorders : list
        list of zorder integer values. see add_shapefiles function.
        
    Returns
    -------
    None
        writes figure to pdf
    '''
    fig, ax = plt.subplots(figsize=(6.8, 12))
    ax.set_aspect('equal')
    ax.set_title(plt_title)

    # Remove junk, add shapefiles
    blankify_plot(ax)
    add_shapefiles(ax, shapelist, shapecolors, linewidths, zorders)

    # plot mean residuals
    ax.scatter(headtargs['X'], headtargs['Y'], marker='o',
               c = headtargs['color'], s = headtargs['pt_size'], zorder=4)

    # Legend
    label_df = headtargs.groupby('color').agg({'Residual': [min, max]})
    label_df = label_df['Residual'].sort_values(by='min', ascending=False).reset_index()
    label_df['label'] = label_df.apply(lambda row: '{} - {} ft'.format(round_to_nearest(row['min'], 5), 
                                                                       round_to_nearest(row['max'], 5)), axis=1)
    
    patches = [mpatches.Patch(color=color, label=label) for color, label in label_df[['color', 'label']].to_numpy()]
    
    fig.legend(handles = patches, labels = [lab.get_label() for lab in patches],
            loc = 'upper right', bbox_to_anchor=(0.90, 0.96))

    fig.tight_layout()
    pp.savefig()
    fig.clear()

def calc_r2(obs, sim):
    ''' returns r2 (rsquared) '''
    if len(obs) == 0 or len(sim) == 0:
        return "N/A"
    # calculate the difference from the mean observed
    omeanres = np.subtract(obs, np.mean(obs))
    
    # calculate the difference from the mean simulated
    smeanres = np.subtract(sim, np.mean(sim))

    # calculate the r2
    rsquared = np.dot(omeanres, smeanres) / (
                np.sqrt(np.sum(np.square(omeanres))) * np.sqrt(np.sum(np.square(smeanres))))
    return np.round(rsquared, 3)

# Index of Agreement (Willmott, 1981)
def calc_d(obs, sim):
    ''' returns the index of agreement from Willmott, 1981 '''
    if len(obs) == 0 or len(sim) == 0:
        return "N/A"
    # calculate the difference from the mean observed
    omeanres = np.subtract(obs, np.mean(obs))
    smeanres = np.subtract(sim, np.mean(obs))  # uses obs mean

    # calculate the sum of square residuals
    sum_sq_res = np.sum(np.square(np.subtract(obs, sim)))

    # calculate the potential error
    potential_error = np.sum(np.square(np.add(smeanres, omeanres)))
    
    # calculate the index of agreement
    d = 1.0 - (sum_sq_res / potential_error)
    return np.round(d, 3)

# Coefficient of Efficiency (NSE)
def calc_E(obs, sim):
    ''' returns the coefficient of efficiency (NSE) '''
    if len(obs) == 0 or len(sim) == 0:
        return "N/A"
    # calculate the residuals
    res = np.subtract(obs, sim)
    
    # calculate the mean observed value
    mean = np.mean(obs)
    
    # calculate the sum of squared residuals
    rss = np.sum(np.square(res))

    # calculate the sum of squared difference from mean observed value
    tss = np.sum(np.square(np.subtract(sim, mean)))
    
    # handle case where tss is zero
    try:
        rsquared = 1 - rss/tss
    except ZeroDivisionError:
        rsquared = 0.0
    
    return np.round(rsquared, 3)

def do_plots(wlobs, wlweight, zeroweight, wellinfo, well_subs, sub_names, 
             date_breaks, subregion_groups, sr_group_names, pp, include_zero_weights=True):
    ''' Creates and writes scatter plots and cumulative distribution of residuals plots for
    entire model, subregion groups, each subregion, each layer, well screen availability, 
    and date ranges. 
    
    Parameters
    ----------
    wlobs : pd.DataFrame
        pandas DataFrame of observation heads
        
    wlweight : pd.DataFrame
        pandas DataFrame of simulated heads
        
    zeroweight : pd.DataFrame
        pandas DataFrame of zero-weighted simulated and observed heads
        
    wellinfo : pd.DataFrame
        pandas DataFrame of well info for selection of plotting groups
        
    well_subs : list of pd.DataFrame
        list of pandas DataFrames containing subset of layer fraction information
        
    sub_names : list of str
        names for each of the well_subs. should be same length as well_subs
        
    date_breaks : list of dates
        list containing date ranges for plotting
        
    subregion_groups : list of lists of integers
        list containing lists of integer subregion ids for grouping wells
        
    sr_group_names : list of str
        list of names for the subregion groups. should be same length as subregion_groups
        
    pp : PdfPages object
        PdfPages object to write the plots
        
    include_zero_weights : bool, default=True
        flag to determine if plots include zero-weighted values for reference
    
    Returns
    -------
    None
        Writes a PDF containing plots
    '''
    # Colors and symbols:
    clrs = ['#AEC7E8','#FF7F0E','#FFBB78','#2CA02C','#98DF8A','#D62728','#FF9896','#9467BD',
            '#C5B0D5','#8C564B','#C49C94','#E377C2','#F7B6D2','#7F7F7F','#C7C7C7','#BCBD22',
            '#DBDB8D','#17BECF','#9EDAE5','blue']
    marks = ['^','v','s','p','D','*','X','<','+','>','*','H','s','^','v','s','p','D','*','X']

    # Axis limits
    pmin = -300 
    pmax = np.ceil(max(wlobs['ObsWL'].max(), wlweight['Simulated'].max())) + 25.0
    lims = (pmin, pmax)

    # Create massplot object
    scatter = mp.create(lims, lims, 'Observed', 'Simulated', 'linear', 'linear', 8.5, 8.5)
    scatter.set_title('Scatterplot - All Wells')

    # 1:1 line
    scatter.add_feature('-', color='#999999', label='1:1 line', rasterized=True)
    scatter.update_feature(0, lims, lims)

    # Zero weighted data
    scatter.add_feature('o', color='#BEBEBE', empty=True, rasterized=True)
    
    if include_zero_weights:
        scatter.update_feature(1, zeroweight['ObsWL'], zeroweight['Simulated'], 'Not In Calibration')

    # Add data
    scatter.add_feature('o', empty=True, rasterized=True)
    scatter.update_feature(2, wlobs['ObsWL'], wlweight['Simulated'], 'Values')
    scatter.create_legend(4, 8, 2)

    # R2
    r2 = calc_r2(wlobs['ObsWL'].to_numpy(), wlweight['Simulated'].to_numpy())
    d = calc_d(wlobs['ObsWL'].to_numpy(), wlweight['Simulated'].to_numpy())
    
    scatter.add_text(x=0.04, y=0.95, text='$R^2={}$\n$d={}$'.format(r2, d),
                              color='#333333', fontsize=10)
    scatter.add_to_pdf(pp, dpi=200)

    # Add features for max number of regions in groups
    max_regions = max([len(reg) for reg in subregion_groups])
    
    # add one less than the max regions because one of the features previously created will be reused.
    for i in range(1, max_regions):
        scatter.add_feature(marks[i], color=clrs[i], empty=True, rasterized=True)
    
    # Hide features to reuse later
    scatter.mask_feature(list(range(2, max_regions + 2)))

    # Subregions by color/shape
    regions = int(wellinfo['Subregion'].max())
    nregion_groups = len(subregion_groups)
    
    for i in range(0, nregion_groups):
        subregionwells = wellinfo.loc[wellinfo['Subregion'].isin(subregion_groups[i]),'Name']
        subobs = wlobs[wlobs['Name'].isin(subregionwells)]
        subsim = wlweight[wlweight['Name'].isin(subregionwells)]
        scatter.update_feature(2+i,subobs['ObsWL'],subsim['Simulated'],label=sr_group_names[i])
    
    scatter.update_legend()
    scatter.set_title('Scatterplot - Subregions')
    scatter.add_to_pdf(pp, dpi=200)

    # Feature Cleanup
    scatter.mask_feature(list(range(2, max_regions+2))) # mask_feature in massplot requires list or int

    # Loop over Subregions creating group scatterplots
    for i in range(0, nregion_groups):
        regiongroupwells = wellinfo.loc[wellinfo['Subregion'].isin(subregion_groups[i]),'Name']
        subzero = zeroweight[zeroweight['Name'].isin(regiongroupwells)]
        
        if include_zero_weights:
            scatter.update_feature(1, subzero['ObsWL'], subzero['Simulated'])
        
        # Loop over regions in group
        for j in range(0, len(subregion_groups[i])):
            reg = subregion_groups[i][j]
            subregionwells = wellinfo.loc[wellinfo['Subregion'] == reg,'Name']
            subobs = wlobs[wlobs['Name'].isin(subregionwells)]
            subsim = wlweight[wlweight['Name'].isin(subregionwells)]
            scatter.update_feature(2+j, subobs['ObsWL'], subsim['Simulated'], 'Subregion {}'.format(reg))
        
        # Update text
        subobs = wlobs[wlobs['Name'].isin(regiongroupwells)]
        subsim = wlweight[wlweight['Name'].isin(regiongroupwells)]
        scatter.set_title('Scatterplot - ' + sr_group_names[i])
        
        r2 = calc_r2(subobs['ObsWL'].to_numpy(), subsim['Simulated'].to_numpy())
        d = calc_d(subobs['ObsWL'].to_numpy(), subsim['Simulated'].to_numpy())
        
        scatter.update_text(0, text='$R^2={}$\n$d={}$'.format(r2, d))
        scatter.update_legend()
        scatter.add_to_pdf(pp, dpi=200)
        scatter.mask_feature(list(range(2, max_regions+2))) # mask_feature in massplot requires list or int

    # Loop over subregions plotting each one
    for i in range(0, regions):
        subregionwells = wellinfo.loc[wellinfo['Subregion'] == i+1, 'Name']
        subzero = zeroweight[zeroweight['Name'].isin(subregionwells)]
        subobs = wlobs[wlobs['Name'].isin(subregionwells)]
        subsim = wlweight[wlweight['Name'].isin(subregionwells)]

        if include_zero_weights:
            scatter.update_feature(1, subzero['ObsWL'], subzero['Simulated'])
        
        scatter.update_feature(2, subobs['ObsWL'], subsim['Simulated'], 'Values')
        scatter.set_title('Scatterplot - Subregion ' + str(i + 1))
        
        r2 = calc_r2(subobs['ObsWL'].to_numpy(), subsim['Simulated'].to_numpy())
        d = calc_d(subobs['ObsWL'].to_numpy(), subsim['Simulated'].to_numpy())
        
        scatter.update_text(0, text='$R^2={}$\n$d={}$'.format(r2, d))
        scatter.update_legend()
        scatter.add_to_pdf(pp, dpi=200)

    # Target Group by color/shape
    grpnames = ['Known Screens', 'Interpolated TOS', 'Interpolated TOS and BOS']
    
    for i in range(3, 0, -1):
        groupwells = wellinfo.loc[wellinfo['KrigeType'] == 4-i,'Name']
        subobs = wlobs[wlobs['Name'].isin(groupwells)]
        subsim = wlweight[wlweight['Name'].isin(groupwells)]
        scatter.update_feature(i+1,subobs['ObsWL'],subsim['Simulated'],label=grpnames[3-i], inlegend=True)
    
    if include_zero_weights:
        scatter.update_feature(1, zeroweight['ObsWL'], zeroweight['Simulated'], 'Not In Calibration')
    
    scatter.update_legend()
    scatter.set_title('Scatterplot - Target Groups')
    
    r2 = calc_r2(wlobs['ObsWL'].to_numpy(), wlweight['Simulated'].to_numpy())
    d = calc_d(wlobs['ObsWL'].to_numpy(), wlweight['Simulated'].to_numpy())
    
    scatter.update_text(0, text='$R^2={}$\n$d={}$'.format(r2, d))
    scatter.add_to_pdf(pp, dpi=200)

    # Feature Cleanup
    scatter.mask_feature(list(range(2, nregion_groups+2))) # mask_feature in massplot requires list or int

    # Loop over wl target groups (relates to screen interpolation) creating individual scatterplots
    for i in range(1,4):
        groupwells = wellinfo.loc[wellinfo['KrigeType'] == i,'Name']
        subzero = zeroweight[zeroweight['Name'].isin(groupwells)]
        subobs = wlobs[wlobs['Name'].isin(groupwells)]
        subsim = wlweight[wlweight['Name'].isin(groupwells)]
        
        if include_zero_weights:
            scatter.update_feature(1, subzero['ObsWL'], subzero['Simulated'])
        
        scatter.update_feature(2, subobs['ObsWL'], subsim['Simulated'],label=grpnames[i-1])
        scatter.set_title('Scatterplot - ' + grpnames[i-1])
        
        r2 = calc_r2(subobs['ObsWL'].to_numpy(), subsim['Simulated'].to_numpy())
        d = calc_d(subobs['ObsWL'].to_numpy(), subsim['Simulated'].to_numpy())
        
        scatter.update_text(0, text='$R^2={}$\n$d={}$'.format(r2, d))
        scatter.update_legend()
        scatter.add_to_pdf(pp, dpi=200)

    # Loop over well subset list
    for i in range(0,len(well_subs)):
        groupwells = well_subs[i].loc[:,'Name']
        subzero = zeroweight[zeroweight['Name'].isin(groupwells)]
        subobs = wlobs[wlobs['Name'].isin(groupwells)]
        subsim = wlweight[wlweight['Name'].isin(groupwells)]
        
        if include_zero_weights:
            scatter.update_feature(1, subzero['ObsWL'], subzero['Simulated'])
        
        scatter.update_feature(2, subobs['ObsWL'], subsim['Simulated'],label='Values')
        scatter.set_title('Scatterplot - ' + sub_names[i])
        
        r2 = calc_r2(subobs['ObsWL'].to_numpy(), subsim['Simulated'].to_numpy())
        d = calc_d(subobs['ObsWL'].to_numpy(), subsim['Simulated'].to_numpy())
        
        scatter.update_text(0, text='$R^2={}$\n$d={}$'.format(r2, d))
        scatter.update_legend()
        scatter.add_to_pdf(pp, dpi=200)

    # Loop over dates, does nothing if date_breaks not provided i.e. len(date_breaks) == 0
    for i in range(1, len(date_breaks)):
        subzero = zeroweight[(zeroweight['Date'] >= date_breaks[i-1]) & (zeroweight['Date'] < date_breaks[i])]
        subobs = wlobs[(wlobs['Date'] >= date_breaks[i-1]) & (wlobs['Date'] < date_breaks[i])]
        subsim = wlweight[(wlweight['Date'] >= date_breaks[i-1]) & (wlweight['Date'] < date_breaks[i])]
        
        if include_zero_weights:
            scatter.update_feature(1, subzero['ObsWL'], subzero['Simulated'])
        
        scatter.update_feature(2, subobs['ObsWL'], subsim['Simulated'],label='Values')
        scatter.set_title('Scatterplot - from ' + date_breaks[i-1].strftime('%Y-%m-%d') + ' to ' + (date_breaks[i] - pd.to_timedelta(1,'d')).strftime('%Y-%m-%d'))
        
        r2 = calc_r2(subobs['ObsWL'].to_numpy(), subsim['Simulated'].to_numpy())
        d = calc_d(subobs['ObsWL'].to_numpy(), subsim['Simulated'].to_numpy())
        
        scatter.update_text(0, text='$R^2={}$\n$d={}$'.format(r2, d))
        scatter.update_legend()
        scatter.add_to_pdf(pp, dpi=200)

    scatter.mask_feature(1)  # Zero-weight

    # Residual percentiles
    res = np.sort(np.subtract(wlobs['ObsWL'].to_numpy(), wlweight['Simulated'].to_numpy()))
    rank = np.arange(1, len(res) + 1)/len(res)
    
    scatter.ax.set_ylim(-0.05, 1.05)
    scatter.ax.set_xlim(-350,350)
    scatter.update_feature(0, [1e-10,1e-9], [-1.5,1.5], 'Zero line')
    scatter.update_feature(2, res, rank, 'Values')
    scatter.set_title('Residual Cumulative Frequency - All Wells')
    scatter.set_xlabel('Residual')
    scatter.set_ylabel('Percentile')
    
    stats = 'Min: {} - Max: {}'.format(round(min(res),1), round(max(res),1))
    stats += '\nAvg: {} - Abs.Avg: {}'.format(round(np.average(res),1),
                                               round(np.average(np.abs(res)),1))
    scatter.update_text(0, text=stats)
    scatter.update_legend()
    scatter.add_to_pdf(pp, dpi=200)

    # Group CDP
    for i in range(0, nregion_groups):
        subregionwells = wellinfo.loc[wellinfo['Subregion'].isin(subregion_groups[i]),'Name']
        subobs = wlobs[wlobs['Name'].isin(subregionwells)]
        subsim = wlweight[wlweight['Name'].isin(subregionwells)]
        
        res = np.sort(np.subtract(subobs['ObsWL'].to_numpy(), subsim['Simulated'].to_numpy()))
        rank = np.arange(1, len(res) + 1)/len(res)
        
        scatter.ax.set_ylim(-0.05, 1.05)
        scatter.ax.set_xlim(-350, 350)
        scatter.update_feature(i+2, res, rank, sr_group_names[i])
    
    scatter.set_title('Residual Cumulative Frequency - By Subregion Group')
    scatter.set_xlabel('Residual')
    scatter.set_ylabel('Percentile')
    scatter.update_text(0, text='')
    scatter.update_legend()
    scatter.add_to_pdf(pp, dpi=200)
    
def do_massplot(gaugeinfo, strobs, strsim, pp1, pp2, shapelist, shapecolors ,linewidths, zorders):

    # Axis limits
    xmin = strsim['Date'].min()
    xmax = strsim['Date'].max()
    ymin = 10
    ymax = 1.0e07
    xlims = (xmin, xmax)
    ylims = (ymin, ymax)

    plots = mp.MultiPlot(3,3,figwidth=11, figheight=8.5,
                         xlabel='Date', ylabel='Flow (Acre-Feet / Month)',
                         xlabelsize=8, ylabelsize=8, ticklabelsize=8, xlims=xlims, yscale='log')
    plots.set_fig_area([0.0,0.0,0.95,0.95], w_pad=4, h_pad=3)

    # Set up plots
    for i in range(0,9):
        # Add features
        #colors = ['#87bdd8','#588c7e','#f2e394','#f2ae72','#d96459','#FF1196','#B011FF','#00D98A']
        plots[i].add_feature('o', label='Observed Streamflow', rasterized=False, ms=1.5)
        plots[i].add_feature('--', label='Simulated Streamflow', rasterized=False, lw=1)
        #plots[i].feature_list[1].set_lw(2.5)
        #[plots[i].feature_list[j].set_lw(1.5) for j in range(2,layers+2)]

        # Add Legend
        plots[i].create_legend((0.0,0.99), size=7, ncol=2)
        #wlplt.create_legend((0.05,1.0), 12, 3)

        # Add Minimap
        plots[i].create_minimap(i*0.1, 0.70, .19, .29,
                                gaugeinfo['X'],
                                gaugeinfo['Y'],
                                16000, 16000,
                                'none', 1,
                                shapelist, shapecolors, linewidths, zorders, rasterized=True)
        plt_pos = plots[i].ax.get_position()
        map_pos = [plt_pos.x0 + 0.229, plt_pos.y0 + 0.127, plt_pos.width / 4.6, plt_pos.height / 2.0]
        plots[i].axinset.set_position(map_pos)

    strnum = gaugeinfo['Name'].unique().shape[0]
    npages = int(pd.np.ceil(strnum / 9.0))
    n = 0
    iplot = 0
    remaining = strnum

    # Ready for plotting - loop over wells, make sure there's data (at least 2 observation points)
    for ind, pstr in gaugeinfo.iterrows():
        if strobs.loc[strobs['Reach'] == pstr['Name']].shape[0] <= -2:
            remaining -= 1
        if strobs.loc[strobs['Reach'] == pstr['Name']].shape[0] > -2:

            if iplot >= 9:
                n += 1
                print("Plotting Page {:4d}/{:4d}".format(n, npages))
                iplot = 0
                plots.add_to_pdf(pp1, dpi=200)
                plots.autoscale_plots('y', buffer=1, min_diff=10)
                plots.add_to_pdf(pp2, dpi=200)
                plots.set_all_lim(xlims=xlims)
            strplot = plots[iplot]

            # print "Plotting Well {:4d}/{:4d} - Name:".format(n, wellnum), well['Name']

            # Reset y-axis
            strplot.ax.set_ylim(ylims)

            pltobs = strobs.loc[strobs['Reach'] == pstr['Name']].sort_values('Date')
            pltmod = strsim.loc[strsim['Reach'] == pstr['Name']].sort_values('Date')

            strplot.update_feature(0, pltobs['Date'], pltobs['Value'])
            strplot.update_feature(1, pltmod['Date'], pltmod['Value'])

            new_title = 'Reach: ' + pstr['Name'] + '\n'
            strplot.set_title(new_title, fontsize=10)

            strplot.minimap_current_loc(pstr['X'], pstr['Y'], 'red', 3)

            iplot += 1
            remaining -= 1

            if remaining <= 0:
                for j in range(9 - 1, iplot - 1, -1):
                    plots[j].ax.remove()
                    plots[j].axinset.remove()
                n += 1
                plots.nplots = iplot
                print("Plotting Page {:4d}/{:4d} (>9 plots)".format(n, npages))
                plots.add_to_pdf(pp1, dpi=200)
                plots.autoscale_plots('y', buffer=1, min_diff=10)
                plots.add_to_pdf(pp2, dpi=200)
                plots.set_all_lim(xlims=xlims)
