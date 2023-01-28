#!/bin/sh
cd /
wget -O ${WORKING_PATH}/model.zip `cat /tmp/model_url`

cd ${WORKING_PATH}
unzip model.zip

echo "Moving model DAGs to /opt/airflow/dags"
mv dags/* /opt/airflow/dags/