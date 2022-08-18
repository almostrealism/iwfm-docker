#!/bin/sh

echo "Building container images..."
docker build . -t ashesfall/iwfm-base:1273 --network=host
docker build . -f Dockerfile.manager -t ashesfall/iwfm-manager:1273
docker build . -f Dockerfile.agent -t ashesfall/iwfm-agent:1273
docker build . -f Dockerfile.pmgr -t ashesfall/iwfm-parallel-mgr:1273
docker build . -f Dockerfile.pagt -t ashesfall/iwfm-parallel-agt:1273

echo "Pushing container images..."
docker push ashesfall/iwfm-base:1273
docker push ashesfall/iwfm-manager:1273
docker push ashesfall/iwfm-agent:1273
docker push ashesfall/iwfm-parallel-mgr:1273
docker push ashesfall/iwfm-parallel-agt:1273
