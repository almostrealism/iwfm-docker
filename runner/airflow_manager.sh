#!/bin/sh
sudo service postgresql start
sudo service redis-server start
sudo /bin/sh /dbinit.sh

echo "RESOURCE_BUCKET = ${RESOURCE_BUCKET}"
# python3 /dag_download.py
echo $IWFM_MODEL > /tmp/model_url
/download_model.sh

airflow db init

airflow users create \
    --email admin@example.org --firstname admin \
    --lastname admin --password $AIRFLOW_PASSWORD \
    --role Admin --username $AIRFLOW_USERNAME
airflow scheduler &
airflow webserver -p 8080
