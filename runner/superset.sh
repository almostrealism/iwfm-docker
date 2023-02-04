#!/bin/sh
export SUP_WEBSERVER_WORKERS=5
export SUP_WEBSERVER_PORT=8088
export SUP_WEBSERVER_TIMEOUT=300
export SUP_WEBSERVER_LOG_LEVEL=info

sudo service postgresql start
sudo /bin/sh /dbinit.sh

python3 /dashboard_download.py
echo "Importing database dump..."
sudo /bin/sh /dbimport.sh

superset fab create-admin \
                  --username admin \
                  --firstname Superset \
                  --lastname Admin \
                  --email "$ADMIN_EMAIL" \
                  --password "$ADMIN_PASSWORD"
superset db upgrade
superset init

# superset import-dashboards --path /backups/dashboards.zip

gunicorn \
      -w ${SUP_WEBSERVER_WORKERS} \
      --timeout ${SUP_WEBSERVER_TIMEOUT} \
      -b  0.0.0.0:${SUP_WEBSERVER_PORT} \
      --log-level ${SUP_WEBSERVER_LOG_LEVEL} \
      "superset.app:create_app()"