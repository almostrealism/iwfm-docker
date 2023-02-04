import os
import awswrangler as wr

dashboard_bucket = os.getenv("DASHBOARDS_BUCKET")
geojson_s3_url = os.getenv("GEOJSON_S3_URL")

# if __name__ == '__main__':
    # print("Downloading geojson from S3...")
    # wr.s3.download(path=geojson_s3_url, local_file='/backups/data.geojson')

    # print("Downloading dashboards from S3...")
    # wr.s3.download(path=f's3://{dashboard_bucket}/dashboards.zip', local_file='/backups/dashboards.zip')