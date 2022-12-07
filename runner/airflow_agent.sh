#!/bin/sh
echo "CLOUDWATCH_LOGS = ${CLOUDWATCH_LOGS}"
cat /opt/airflow/airflow.cfg | grep remote_base_log_folder

echo $IWFM_MODEL > /tmp/model_url
/download_model.sh
airflow celery worker --celery-hostname `tr -dc A-Za-z </dev/urandom | head -c 13`