import os
import awswrangler as wr

dashboard_bucket = os.getenv("DASHBOARDS_BUCKET")

if __name__ == '__main__':
    print("Uploading dashboard data to S3...")
    with open("/dashboards/dashboards.zip", "rb") as local_f:
        wr.s3.upload(local_file=local_f, path=f's3://{dashboard_bucket}/dashboards.zip')

    print("Uploading geojson data to S3...")
    with open("/dashboards/data.geojson", "rb") as local_f:
        wr.s3.upload(local_file=local_f, path=f's3://{dashboard_bucket}/data.geojson')