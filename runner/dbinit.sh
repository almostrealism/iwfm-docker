#!/bin/sh
runuser -l postgres -c 'psql -U postgres -f /postgres_init.sql'