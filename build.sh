#!/bin/sh

echo "Building container images..."
docker build . -t ashesfall/iwfm-base:1403-ubuntu20.04 --network=host
docker build . -f Dockerfile.manager -t ashesfall/iwfm-manager:1403-ubuntu20.04
docker build . -f Dockerfile.agent -t ashesfall/iwfm-agent:1403-ubuntu20.04
docker build . -f Dockerfile.pmgr -t ashesfall/iwfm-parallel-mgr:1403-ubuntu20.04
docker build . -f Dockerfile.pagt -t ashesfall/iwfm-parallel-agt:1403-ubuntu20.04

echo "Pushing container images..."
docker push ashesfall/iwfm-base:1403-ubuntu20.04
docker push ashesfall/iwfm-manager:1403-ubuntu20.04
docker push ashesfall/iwfm-agent:1403-ubuntu20.04
docker push ashesfall/iwfm-parallel-mgr:1403-ubuntu20.04
docker push ashesfall/iwfm-parallel-agt:1403-ubuntu20.04
