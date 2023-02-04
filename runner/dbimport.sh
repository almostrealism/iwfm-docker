#!/bin/sh
runuser -l postgres -c 'pg_restore /backups/data.sql'