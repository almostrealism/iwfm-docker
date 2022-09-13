import os
import awswrangler as wr

dashboard_bucket = os.getenv("DASHBOARDS_BUCKET")

if __name__ == '__main__':
    print("Downloading dashboards from S3...")
    wr.s3.download(path=f's3://{dashboard_bucket}/dashboards.zip', local_file='/dashboards/dashboards.zip')
