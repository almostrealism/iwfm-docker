#!/bin/sh
echo "RESOURCE_BUCKET = ${RESOURCE_BUCKET}"
echo $IWFM_MODEL > /tmp/model_url
/download_model.sh
airflow celery worker --celery-hostname `tr -dc A-Za-z </dev/urandom | head -c 13`