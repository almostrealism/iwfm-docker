import os
import awswrangler as wr

dashboard_bucket = os.getenv("DASHBOARDS_BUCKET")
analytics_bucket = os.getenv("DB_BUCKET")
# geojson_file = os.getenv("GEOJSON_FILENAME")

if __name__ == '__main__':
    print("Downloading dashboards from S3...")
    wr.s3.download(path=f's3://{dashboard_bucket}/dashboards.zip', local_file='/dashboards/dashboards.zip')

    # print("Downloading geojson from S3...")
    # wr.s3.download(path=f's3://{analytics_bucket}/{geojson_file}', local_file='/dashboards/data.geojson')