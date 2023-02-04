#!/bin/sh
runuser -l postgres -c 'dropdb analysis'
runuser -l postgres -c 'pg_restore -C -d postgres /backups/data.sql'