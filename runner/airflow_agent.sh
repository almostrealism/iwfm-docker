#!/bin/sh

airflow celery worker --celery-hostname `tr -dc A-Za-z </dev/urandom | head -c 13`