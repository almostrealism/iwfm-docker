#!/bin/sh
PATH=$PATH:/build/iwfm

mkdir /dashboards

python3 /scripts/dashboard_download.py
unzip /dashboards/dashboards.zip -d /dashboards
rm /dashboards/dashboards.zip

cd /dashboards
mv dashboard_* dashboard
python3 /scripts/dashboard_sync.py

zip -r dashboards.zip dashboard
python3 /scripts/dashboard_upload.py