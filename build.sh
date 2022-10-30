#!/bin/sh

echo "Building container images..."
docker build . -t ashesfall/iwfm-base:1403-php7.4-apache --network=host
docker build . -f Dockerfile.manager -t ashesfall/iwfm-manager:1403-php7.4-apache
docker build . -f Dockerfile.agent -t ashesfall/iwfm-agent:1403-php7.4-apache
docker build . -f Dockerfile.pmgr -t ashesfall/iwfm-parallel-mgr:1403-php7.4-apache
docker build . -f Dockerfile.pagt -t ashesfall/iwfm-parallel-agt:1403-php7.4-apache

echo "Pushing container images..."
docker push ashesfall/iwfm-base:1403-php7.4-apache
docker push ashesfall/iwfm-manager:1403-php7.4-apache
docker push ashesfall/iwfm-agent:1403-php7.4-apache
docker push ashesfall/iwfm-parallel-mgr:1403-php7.4-apache
docker push ashesfall/iwfm-parallel-agt:1403-php7.4-apache
