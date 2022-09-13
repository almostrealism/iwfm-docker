#!/bin/sh
PATH=$PATH:/build/iwfm

python3 /scripts/dashboards_download.py
unzip /dashboards/dashboards.zip -d /dashboards
rm /dashboards/dashboards.zip

cd /dashboards
mv dashboard_* dashboard
python3 /scripts/dashboards_sync.py

zip -r dashboards.zip dashboard
python3 /scripts/dashboards_upload.py