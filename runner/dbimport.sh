#!/bin/sh
runuser -l postgres -c 'pg_restore -C -d postgres /backups/data.sql'