#!/bin/sh
runuser -l postgres -c 'pg_restore -d analysis -f /backups/data.sql'