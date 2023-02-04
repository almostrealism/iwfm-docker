#!/bin/sh
runuser -l postgres -c 'psql -U postgres -f /backups/data.sql'