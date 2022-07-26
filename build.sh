#!/bin/sh

echo "Building container images..."
docker build . -t ashesfall/iwfm-base:1403 --network=host
docker build . -f Dockerfile.manager -t ashesfall/iwfm-manager:1403
docker build . -f Dockerfile.agent -t ashesfall/iwfm-agent:1403

echo "Pushing container images..."
docker push ashesfall/iwfm-base:1403
docker push ashesfall/iwfm-manager:1403
docker push ashesfall/iwfm-agent:1403