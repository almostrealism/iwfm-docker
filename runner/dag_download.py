import os
import awswrangler as wr

resource_bucket = os.getenv("RESOURCE_BUCKET")

if __name__ == '__main__':
    print("Downloading DAG from S3...")
    wr.s3.download(path=f's3://{resource_bucket}/control.py', local_file='/opt/airflow/dags/control.py')