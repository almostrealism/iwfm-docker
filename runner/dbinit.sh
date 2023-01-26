#!/bin/sh
runuser -l postgres -c 'psql -U postgres -f /postgres_init.sql'
runuser -l postgres -c 'psql -d analysis -c "CREATE EXTENSION postgis;"'