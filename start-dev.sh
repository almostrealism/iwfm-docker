#!/bin/sh

docker run -it --rm --mount type=bind,src=/home/thatch,target=/data tjhatch/iwfm-local:latest bash