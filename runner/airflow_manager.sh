#!/bin/sh
sudo service postgresql start
sudo service redis-server start
sudo /bin/sh /dbinit.sh

python3 /dag_download.py

airflow db init

airflow users create \
    --email admin@example.org --firstname admin \
    --lastname admin --password $AIRFLOW_PASSWORD \
    --role Admin --username $AIRFLOW_USERNAME
airflow scheduler &
airflow webserver -p 8080
