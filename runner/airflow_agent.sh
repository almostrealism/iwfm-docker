#!/bin/sh

airflow celery worker -n `tr -dc A-Za-z </dev/urandom | head -c 13`