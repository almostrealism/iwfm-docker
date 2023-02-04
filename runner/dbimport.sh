#!/bin/sh
runuser -l postgres -c 'pg_restore -f /backups/data.sql'