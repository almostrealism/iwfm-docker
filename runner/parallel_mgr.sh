#!/bin/sh
PATH=$PATH:/build/iwfm

echo "Running dashboards sync..."
sh /dashboards.sh
echo "Done with dashboards sync"

ln -s / /var/www/html/files

wget -O model.zip $IWFM_MODEL
unzip model.zip

echo "Starting flowtree..."
cd /
/opt/jdk-17.0.2/bin/java -cp flowtree-shaded.jar io.flowtree.Manager