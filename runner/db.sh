#!/bin/sh
sudo service postgresql start
sudo /bin/sh /dbinit.sh

python3 /sql_download.py

echo "Importing database dump..."
sudo /bin/sh /dbimport.sh

echo "Keeping container alive for 24h..."
sleep 24h