import sys
import os

import numpy as np
from pywfm import IWFMBudget
import awswrangler as wr
import boto3

db_bucket = "iwfm-athena-1"
db_name = "iwfm_db"
table_name = "budgets"

access_key = os.getenv("AWS_ACCESS_KEY_ID")
secret_key = os.getenv("AWS_SECRET_ACCESS_KEY")


def read_filename_from_commandline(args):
    """ Read the budget hdf file name from the commandline
    """
    if len(args) == 1:
        input("Provide name of budget HDF file: ")

    elif len(args) > 2:
        raise ValueError("Too many values provided on command line")

    else:
        file_name = args[1]
        if not os.path.exists(file_name):
            raise FileNotFoundError("File provided {} was not found".format(file_name))

        if not file_name.endswith('hdf'):
            raise ValueError("Budget files must be HDF format")

        return file_name


def date_to_water_year(month, year):
    if month > 9:
        return int(year + 1)
    else:
        return int(year)


if __name__ == '__main__':

    rz_budget_file = read_filename_from_commandline(sys.argv)

    data = None

    with IWFMBudget(rz_budget_file) as bud:
        locations = bud.get_location_names()

        for i, l in enumerate(locations, start=1):

            rz_annual = bud.get_values(
                i,
                output_interval='1YEAR',
                area_conversion_factor=1 / 43560,
                area_units='Acres',
                volume_conversion_factor=1 / 43560,
                volume_units='AF'
            )

            # Generate new columns to separate fields where positive and negative values are allowed
            rz_annual['Ag. Net Loss from Land Reduction (-)'] = np.where(rz_annual['Ag. Net Gain from Land Expansion (+)'].to_numpy() < 0, -1*rz_annual['Ag. Net Gain from Land Expansion (+)'].to_numpy(), 0)
            rz_annual['Ag. Net Gain from Land Expansion (+)'] = np.where(rz_annual['Ag. Net Gain from Land Expansion (+)'].to_numpy() >= 0, rz_annual['Ag. Net Gain from Land Expansion (+)'].to_numpy(), 0)
            rz_annual['Urban Net Loss from Land Reduction (-)'] = np.where(rz_annual['Urban Net Gain from Land Expansion (+)'].to_numpy() < 0, -1*rz_annual['Urban Net Gain from Land Expansion (+)'].to_numpy(), 0)
            rz_annual['Urban Net Gain from Land Expansion (+)'] = np.where(rz_annual['Urban Net Gain from Land Expansion (+)'].to_numpy() >= 0, rz_annual['Urban Net Gain from Land Expansion (+)'].to_numpy(), 0)
            rz_annual['Native&Riparian Veg. Net Loss from Land Reduction (-)'] = np.where(rz_annual['Native&Riparian Veg. Net Gain from Land Expansion (+)'].to_numpy() < 0, -1*rz_annual['Native&Riparian Veg. Net Gain from Land Expansion (+)'].to_numpy(), 0)
            rz_annual['Native&Riparian Veg. Net Gain from Land Expansion (+)'] = np.where(rz_annual['Native&Riparian Veg. Net Gain from Land Expansion (+)'].to_numpy() >= 0, rz_annual['Native&Riparian Veg. Net Gain from Land Expansion (+)'].to_numpy(), 0)
            rz_annual['location_id'] = i
            rz_annual['location_name'] = l

            # print("Adding {} rows".format(len(rz_annual)))

            if data is None:
                data = rz_annual.copy(deep=True)
            else:
                data = data.append(rz_annual)

    print("Total rows: {}".format(len(data)))

    session = boto3.Session(
        aws_access_key_id=access_key,
        aws_secret_access_key=secret_key
    )

    # Storing the data and metadata to Data Lake
    wr.s3.to_parquet(
        df=data,
        database=db_name,
        table=table_name,
        path=f's3://{db_bucket}/budgets',
        partition_cols=["location_id"],
        dataset=True,
        boto3_session=session
    )
