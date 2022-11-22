#!/bin/sh

airflow db init
airflow users create \
    --email test@example.org --firstname admin \
    --lastname admin --password airflow \
    --role Admin --username airflow
airflow scheduler &
airflow webserver -p 8080
