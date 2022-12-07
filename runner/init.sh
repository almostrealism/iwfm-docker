#!/bin/sh
PATH=$PATH:/build/iwfm

echo "WORKING_PATH = ${WORKING_PATH}"
cd ${WORKING_PATH}

wget -O model.zip $IWFM_MODEL
unzip model.zip

echo "Running initial simulation..."
/run_model_with_prep.sh ${WORKING_PATH}