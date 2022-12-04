#!/bin/sh
cd /
wget -O ${WORKING_PATH}/model.zip `cat /tmp/model_url`

cd ${WORKING_PATH}
unzip model.zip