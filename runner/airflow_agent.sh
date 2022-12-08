#!/bin/sh
python3 /dag_download.py
echo $IWFM_MODEL > /tmp/model_url
/download_model.sh
airflow celery worker --celery-hostname `tr -dc A-Za-z </dev/urandom | head -c 13`