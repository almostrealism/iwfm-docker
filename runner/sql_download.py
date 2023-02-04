import os
import awswrangler as wr

sql_s3_url = os.getenv("SQL_S3_URL")

if __name__ == '__main__':
    print("Downloading SQL from S3...")
    wr.s3.download(path=sql_s3_url, local_file='/backups/data.sql')
