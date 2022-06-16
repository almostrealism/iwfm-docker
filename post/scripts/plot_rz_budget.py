import matplotlib
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

matplotlib.use('Agg')

from pywfm import IWFMBudget

BUDGET_FILE = '/home/michael/postprocess/Simulation/..\Results\C2VSimFG_RZ_Budget.hdf'

def date_to_water_year(month, year):
    if month > 9:
        return int(year + 1)
    else:
        return int(year)

if __name__ == '__main__':

    rz_budget_file = BUDGET_FILE # '../Results/C2VSimFG_RZ_Budget.hdf'
    
    with IWFMBudget(rz_budget_file) as bud:
        locations = bud.get_location_names()
        for i, l in enumerate(locations, start=1):
            
            rz_annual = bud.get_values(
                i,
                output_interval='1YEAR',
                area_conversion_factor=1/43560,
                area_units='Acres',
                volume_conversion_factor=1/43560,
                volume_units='AF'
            )
            
            # Generate new columns to separate fields where positive and negative values are allowed
            rz_annual['Ag. Net Loss from Land Reduction (-)'] = np.where(rz_annual['Ag. Net Gain from Land Expansion (+)'].to_numpy() < 0, -1*rz_annual['Ag. Net Gain from Land Expansion (+)'].to_numpy(), 0)
            rz_annual['Ag. Net Gain from Land Expansion (+)'] = np.where(rz_annual['Ag. Net Gain from Land Expansion (+)'].to_numpy() >= 0, rz_annual['Ag. Net Gain from Land Expansion (+)'].to_numpy(), 0)
            rz_annual['Urban Net Loss from Land Reduction (-)'] = np.where(rz_annual['Urban Net Gain from Land Expansion (+)'].to_numpy() < 0, -1*rz_annual['Urban Net Gain from Land Expansion (+)'].to_numpy(), 0)
            rz_annual['Urban Net Gain from Land Expansion (+)'] = np.where(rz_annual['Urban Net Gain from Land Expansion (+)'].to_numpy() >= 0, rz_annual['Urban Net Gain from Land Expansion (+)'].to_numpy(), 0)
            rz_annual['Native&Riparian Veg. Net Loss from Land Reduction (-)'] = np.where(rz_annual['Native&Riparian Veg. Net Gain from Land Expansion (+)'].to_numpy() < 0, -1*rz_annual['Native&Riparian Veg. Net Gain from Land Expansion (+)'].to_numpy(), 0)
            rz_annual['Native&Riparian Veg. Net Gain from Land Expansion (+)'] = np.where(rz_annual['Native&Riparian Veg. Net Gain from Land Expansion (+)'].to_numpy() >= 0, rz_annual['Native&Riparian Veg. Net Gain from Land Expansion (+)'].to_numpy(), 0)
            
            width = 0.35

            # plot agricultural land surface runoff and infiltration of precipitation and applied water
            fig, ax = plt.subplots(figsize=(20,6))
            ax.bar(
                rz_annual.index - width/2, 
                rz_annual['Ag. Infiltration (+)'],
                width,
                color='#C35ABC',
                label='Infiltration'
            )
            
            ax.bar(
                rz_annual.index - width/2, 
                rz_annual['Ag. Runoff'],
                width,
                bottom=rz_annual['Ag. Infiltration (+)'],
                color='#61B74F',
                label='Runoff'
            )
            
            ax.bar(
                rz_annual.index - width/2, 
                rz_annual['Ag. Net Return Flow'],
                width,
                bottom=rz_annual['Ag. Infiltration (+)'] + rz_annual['Ag. Runoff'],
                color='#6367A9',
                label='Return Flow'
            )
            
            ax.bar(
                rz_annual.index + width/2,
                rz_annual['Ag. Precipitation'],
                width,
                color='#7561CF',
                label='Precipitation'
            )
            
            ax.bar(
                rz_annual.index + width/2,
                rz_annual['Ag. Prime Applied Water'],
                width,
                bottom=rz_annual['Ag. Precipitation'],
                color='#C8D0F6',
                label='Prime Applied Water'
            )
            
            ax2 = ax.twinx()
            
            ax2.plot(
                rz_annual.index,
                rz_annual['Ag. Area (Acres)'],
                'k'
            )
            
            ax.set_xticks(rz_annual.index, rz_annual['Time'].dt.year)
            for label in ax.get_xticklabels():
                label.set_rotation(90)
                
            ax.grid()
            box = ax.get_position()
            ax.set_position([box.x0, box.y0 + 0.25 * box.height, box.width, box.height * 0.75])
            
            # Put a legend below the current axis
            ax.legend(loc='lower center', ncol=3, fontsize=8, bbox_to_anchor=(0.5, -0.35), frameon=False)
            
            ax.set_ylabel('Annual Volume (AF)')
            ax2.set_ylabel('Area (Acres)')
            ax.set_xlabel('Water Year')
            ax.set_title('Root Zone Budget - Ag\nRunoff and Infiltration for Subregion {}'.format(1))
            plt.savefig('{}_Ag_RZ.png'.format(l))
            plt.close()

            # plot urban land surface runoff and infiltration of precipitation and applied water
            fig, ax = plt.subplots(figsize=(20,6))
            ax.bar(
                rz_annual.index - width/2, 
                rz_annual['Urban Infiltration (+)'],
                width,
                color='#C35ABC',
                label='Infiltration'
            )
            
            ax.bar(
                rz_annual.index - width/2, 
                rz_annual['Urban Runoff'],
                width,
                bottom=rz_annual['Urban Infiltration (+)'],
                color='#61B74F',
                label='Runoff'
            )
            
            ax.bar(
                rz_annual.index - width/2, 
                rz_annual['Urban Net Return Flow'],
                width,
                bottom=rz_annual['Urban Infiltration (+)'] + rz_annual['Urban Runoff'],
                color='#6367A9',
                label='Return Flow'
            )
            
            ax.bar(
                rz_annual.index + width/2,
                rz_annual['Urban Precipitation'],
                width,
                color='#7561CF',
                label='Precipitation'
            )
            
            ax.bar(
                rz_annual.index + width/2,
                rz_annual['Urban Prime Applied Water'],
                width,
                bottom=rz_annual['Urban Precipitation'],
                color='#C8D0F6',
                label='Prime Applied Water'
            )
            
            ax2 = ax.twinx()
            
            ax2.plot(
                rz_annual.index,
                rz_annual['Urban Area (Acres)'],
                'k'
            )
            
            ax.set_xticks(rz_annual.index, rz_annual['Time'].dt.year)
            for label in ax.get_xticklabels():
                label.set_rotation(90)
                
            ax.grid()
            box = ax.get_position()
            ax.set_position([box.x0, box.y0 + 0.25 * box.height, box.width, box.height * 0.75])
            
            # Put a legend below the current axis
            ax.legend(loc='lower center', ncol=3, fontsize=8, bbox_to_anchor=(0.5, -0.35), frameon=False)
            
            ax.set_ylabel('Annual Volume (AF)')
            ax2.set_ylabel('Area (Acres)')
            ax.set_xlabel('Water Year')
            ax.set_title('Root Zone Budget - Urban\nRunoff and Infiltration for Subregion {}'.format(1))
            plt.savefig('{}_Urban_RZ.png'.format(l))
            plt.close()

            # plot native and riparian land surface runoff and infiltration of precipitation
            fig, ax = plt.subplots(figsize=(20,6))
            ax.bar(
                rz_annual.index - width/2, 
                rz_annual['Native&Riparian Veg. Infiltration (+)'],
                width,
                color='#C35ABC',
                label='Infiltration'
            )
            
            ax.bar(
                rz_annual.index - width/2, 
                rz_annual['Native&Riparian Veg. Runoff'],
                width,
                bottom=rz_annual['Native&Riparian Veg. Infiltration (+)'],
                color='#61B74F',
                label='Runoff'
            )
            
            ax.bar(
                rz_annual.index + width/2,
                rz_annual['Native&Riparian Veg. Precipitation'],
                width,
                color='#7561CF',
                label='Precipitation'
            )
            
            ax2 = ax.twinx()
            
            ax2.plot(
                rz_annual.index,
                rz_annual['Native&Riparian Veg. Area (Acres)'],
                'k'
            )
            
            ax.set_xticks(rz_annual.index, rz_annual['Time'].dt.year)
            for label in ax.get_xticklabels():
                label.set_rotation(90)
                
            ax.grid()
            box = ax.get_position()
            ax.set_position([box.x0, box.y0 + 0.25 * box.height, box.width, box.height * 0.75])
            
            # Put a legend below the current axis
            ax.legend(loc='lower center', ncol=3, fontsize=8, bbox_to_anchor=(0.5, -0.35), frameon=False)
            
            ax.set_ylabel('Annual Volume (AF)')
            ax2.set_ylabel('Area (Acres)')
            ax.set_xlabel('Water Year')
            ax.set_title('Root Zone Budget - Native and Riparian\nRunoff and Infiltration for Subregion {}'.format(i))
            plt.savefig('{}_NR_RZ.png'.format(l))
            plt.close()

            # plot root zone budget for ag
            fig, ax = plt.subplots(figsize=(20,6))
            ax.bar(
                rz_annual.index - width/2, 
                rz_annual['Ag. Infiltration (+)'],
                width, 
                color='#C35ABC', 
                label='Infiltration (Inflow)'
            )
            
            ax.bar(
                rz_annual.index - width/2, 
                rz_annual['Ag. Groundwater Inflow (+)'],
                width,
                bottom=rz_annual['Ag. Infiltration (+)'],
                color='#61B74F', 
                label='Groundwater Inflow (Inflow)'
            )
            
            ax.bar(
                rz_annual.index - width/2, 
                rz_annual['Ag. Other Inflow (+)'],
                width,
                bottom=rz_annual['Ag. Infiltration (+)'] + rz_annual['Ag. Groundwater Inflow (+)'],
                color='#B4B445', 
                label='Other Inflow (Inflow)'
            )
            
            ax.bar(
                rz_annual.index - width/2, 
                rz_annual['Ag. Net Gain from Land Expansion (+)'],
                width,
                bottom=rz_annual['Ag. Infiltration (+)'] + rz_annual['Ag. Groundwater Inflow (+)'] + rz_annual['Ag. Other Inflow (+)'],
                color='#66796D', 
                label='Gain from Land Expansion (Inflow)'
            )
            
            ax2 = ax.twinx()
            ax2.plot(
                rz_annual.index, 
                (rz_annual['Ag. Ending Storage (-)'] - rz_annual['Ag. Beginning Storage (+)']).cumsum(),
                'r--', 
                label='Change in Storage'
            )
            ax2.set_ylabel("Change in Storage (AF)")
            
            ax.bar(
                rz_annual.index + width/2, 
                rz_annual['Ag. Actual ET (-)'],
                width, 
                color='#787EC7', 
                label='Actual ET (Outflow)'
            )
            
            ax.bar(
                rz_annual.index + width/2, 
                rz_annual['Ag. Percolation (-)'],
                width,
                bottom=rz_annual['Ag. Actual ET (-)'],
                color='#D68B31', 
                label='Percolation (Outflow)'
            )
            
            ax.bar(
                rz_annual.index + width/2, 
                rz_annual['Ag. Net Loss from Land Reduction (-)'],
                width,
                bottom=rz_annual['Ag. Actual ET (-)'] + rz_annual['Ag. Percolation (-)'],
                color='#4FACD8', 
                label='Loss from Land Reduction (Outflow)'
            )
            
            ax.set_xticks(rz_annual.index, rz_annual['Time'].dt.year)
            for label in ax.get_xticklabels():
                label.set_rotation(90)
                
            ax.grid()
            box = ax.get_position()
            ax.set_position([box.x0, box.y0 + 0.2 * box.height, box.width, box.height * 0.8])
            
            # Put a legend below the current axis
            ax.legend(loc='lower center', ncol=4, fontsize=8, bbox_to_anchor=(0.5, -0.35), frameon=False)
            
            ax.set_ylabel('Annual Volume (AF)')
            ax.set_xlabel('Water Year')
            ax.set_title("Agricultural Root Zone Budget\nfor Subregion {}".format(i))
            plt.savefig('{}_AG_RZ_Budget.png'.format(l))
            plt.close()
            
            # plot root zone budget for urban
            fig, ax = plt.subplots(figsize=(20,6))
            ax.bar(
                rz_annual.index - width/2, 
                rz_annual['Urban Infiltration (+)'],
                width, 
                color='#C35ABC', 
                label='Infiltration (Inflow)'
            )
            
            ax.bar(
                rz_annual.index - width/2, 
                rz_annual['Urban Groundwater Inflow (+)'],
                width,
                bottom=rz_annual['Urban Infiltration (+)'],
                color='#61B74F', 
                label='Groundwater Inflow (Inflow)'
            )
            
            ax.bar(
                rz_annual.index - width/2, 
                rz_annual['Urban Other Inflow (+)'],
                width,
                bottom=rz_annual['Urban Infiltration (+)'] + rz_annual['Urban Groundwater Inflow (+)'],
                color='#B4B445', 
                label='Other Inflow (Inflow)'
            )
            
            ax.bar(
                rz_annual.index - width/2, 
                rz_annual['Urban Net Gain from Land Expansion (+)'],
                width,
                bottom=rz_annual['Urban Infiltration (+)'] + rz_annual['Urban Groundwater Inflow (+)'] + rz_annual['Urban Other Inflow (+)'],
                color='#66796D', 
                label='Gain from Land Expansion (Inflow)'
            )
            
            ax2 = ax.twinx()
            ax2.plot(
                rz_annual.index, 
                (rz_annual['Urban Ending Storage (-)'] - rz_annual['Urban Beginning Storage (+)']).cumsum(),
                'r--', 
                label='Change in Storage'
            )
            ax2.set_ylabel("Change in Storage (AF)")
            
            ax.bar(
                rz_annual.index + width/2, 
                rz_annual['Urban Actual ET (-)'],
                width, 
                color='#787EC7', 
                label='Actual ET (Outflow)'
            )
            
            ax.bar(
                rz_annual.index + width/2, 
                rz_annual['Urban Percolation (-)'],
                width,
                bottom=rz_annual['Urban Actual ET (-)'],
                color='#D68B31', 
                label='Percolation (Outflow)'
            )
            
            ax.bar(
                rz_annual.index + width/2, 
                rz_annual['Urban Net Loss from Land Reduction (-)'],
                width,
                bottom=rz_annual['Urban Actual ET (-)'] + rz_annual['Urban Percolation (-)'],
                color='#4FACD8', 
                label='Loss from Land Reduction (Outflow)'
            )
            
            ax.set_xticks(rz_annual.index, rz_annual['Time'].dt.year)
            for label in ax.get_xticklabels():
                label.set_rotation(90)
                
            ax.grid()
            box = ax.get_position()
            ax.set_position([box.x0, box.y0 + 0.2 * box.height, box.width, box.height * 0.8])
            
            # Put a legend below the current axis
            ax.legend(loc='lower center', ncol=4, fontsize=8, bbox_to_anchor=(0.5, -0.35), frameon=False)
            
            ax.set_ylabel('Annual Volume (AF)')
            ax.set_xlabel('Water Year')
            ax.set_title("Urban Root Zone Budget\nfor Subregion {}".format(i))
            plt.savefig('{}_UR_RZ_Budget.png'.format(l))
            plt.close()
            
            # plot root zone budget for native and riparian vegetation
            fig, ax = plt.subplots(figsize=(20,6))
            ax.bar(
                rz_annual.index - width/2, 
                rz_annual['Native&Riparian Veg. Infiltration (+)'],
                width, 
                color='#C35ABC', 
                label='Infiltration (Inflow)'
            )
            
            ax.bar(
                rz_annual.index - width/2, 
                rz_annual['Native&Riparian Veg. Groundwater Inflow (+)'],
                width,
                bottom=rz_annual['Native&Riparian Veg. Infiltration (+)'],
                color='#61B74F', 
                label='Groundwater Inflow (Inflow)'
            )
            
            ax.bar(
                rz_annual.index - width/2, 
                rz_annual['Native&Riparian Veg. Stream Inflow for ET (+)'],
                width,
                bottom=rz_annual['Native&Riparian Veg. Infiltration (+)'] + rz_annual['Native&Riparian Veg. Groundwater Inflow (+)'],
                color='#7561CF', 
                label='Stream Inflow for ET (Inflow)'
            )
            
            ax.bar(
                rz_annual.index - width/2, 
                rz_annual['Native&Riparian Veg. Other Inflow (+)'],
                width,
                bottom=rz_annual['Native&Riparian Veg. Infiltration (+)'] + rz_annual['Native&Riparian Veg. Groundwater Inflow (+)'] + rz_annual['Native&Riparian Veg. Stream Inflow for ET (+)'],
                color='#B4B445', 
                label='Other Inflow (Inflow)'
            )
            
            ax2 = ax.twinx()
            ax2.plot(
                rz_annual.index, 
                (rz_annual['Native&Riparian Veg. Ending Storage (-)'] - rz_annual['Native&Riparian Veg. Beginning Storage (+)']).cumsum(),
                'r--', 
                label='Change in Storage'
            )
            ax2.set_ylabel("Change in Storage (AF)")
            
            ax.bar(
                rz_annual.index + width/2, 
                rz_annual['Native&Riparian Veg. Actual ET (-)'],
                width, 
                color='#787EC7', 
                label='Actual ET (Outflow)'
            )
            
            ax.bar(
                rz_annual.index + width/2, 
                rz_annual['Native&Riparian Veg. Percolation (-)'],
                width,
                bottom=rz_annual['Native&Riparian Veg. Actual ET (-)'],
                color='#D68B31', 
                label='Percolation (Outflow)'
            )
            
            ax.bar(
                rz_annual.index + width/2, 
                rz_annual['Native&Riparian Veg. Net Loss from Land Reduction (-)'],
                width,
                bottom=rz_annual['Native&Riparian Veg. Actual ET (-)'] + rz_annual['Native&Riparian Veg. Percolation (-)'],
                color='#4FACD8', 
                label='Loss from Land Reduction (Outflow)'
            )
            
            ax.set_xticks(rz_annual.index, rz_annual['Time'].dt.year)
            for label in ax.get_xticklabels():
                label.set_rotation(90)
                
            ax.grid()
            box = ax.get_position()
            ax.set_position([box.x0, box.y0 + 0.2 * box.height, box.width, box.height * 0.8])
            
            # Put a legend below the current axis
            ax.legend(loc='lower center', ncol=4, fontsize=8, bbox_to_anchor=(0.5, -0.35), frameon=False)
            
            ax.set_ylabel('Annual Volume (AF)')
            ax.set_xlabel('Water Year')
            ax.set_title("Native and Riparian Vegetation Root Zone Budget\nfor Subregion {}".format(i))
            plt.savefig('{}_NR_RZ_Budget.png'.format(l))
            plt.close()