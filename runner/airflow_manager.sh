#!/bin/sh
sudo service postgresql start
sudo /bin/sh /dbinit.sh
#psql -U airflow -d airflow -f /postgres_airflow.sql

airflow db init

#airflow users create \
#    --email admin@example.org --firstname admin \
#    --lastname admin --password $AIRFLOW_PASSWORD \
#    --role Admin --username $AIRFLOW_USERNAME
#airflow scheduler &
#airflow webserver -p 8080
