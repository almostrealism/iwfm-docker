#!/bin/sh
runuser -l postgres -c 'dropdb analysis'
runuser -l postgres -c 'pg_restore --verbose -C -d postgres /backups/data.sql'