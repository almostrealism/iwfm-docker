#!/bin/sh

echo "Building container images..."
docker build . -t tjhatch/iwfm-base:latest --network=host
docker build . -f Dockerfile.airflow -t tjhatch/iwfm-airflow:latest --network=host
docker build . -f Dockerfile.afmgr -t tjhatch/iwfm-airflow-manager:latest --network=host
docker build . -f Dockerfile.afagt -t tjhatch/iwfm-airflow-agent:latest
docker build . -f Dockerfile.manager -t tjhatch/iwfm-manager:latest
docker build . -f Dockerfile.agent -t tjhatch/iwfm-agent:latest
docker build . -f Dockerfile.analysis -t tjhatch/iwfm-analysis:latest --network=host

echo "Pushing container images..."
docker push tjhatch/iwfm-base:latest
docker push tjhatch/iwfm-airflow:latest
docker push tjhatch/iwfm-airflow-manager:latest
docker push tjhatch/iwfm-airflow-agent:latest
docker push tjhatch/iwfm-manager:latest
docker push tjhatch/iwfm-agent:latest
docker push tjhatch/iwfm-analysis:latest