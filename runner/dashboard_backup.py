import sys
import os
import time
import subprocess

import awswrangler as wr

dashboard_bucket = os.getenv("DASHBOARDS_BUCKET")

if __name__ == '__main__':
    while (True):
        time.sleep(60 * 60)

        print("Exporting dashboard data from superset...")
        subprocess.call(["superset", "export-dashboards", "-f", "/backups/dashboards.zip"])

        print("Uploading dashboard data to S3...")
        with open("/backups/dashboards.zip", "rb") as local_f:
            wr.s3.upload(local_file=local_f, path=f's3://{dashboard_bucket}/dashboards.zip')
