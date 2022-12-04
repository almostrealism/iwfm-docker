from __future__ import annotations

import datetime

import pendulum

from airflow import DAG
from airflow.operators.bash import BashOperator
from airflow.operators.empty import EmptyOperator

with DAG(
        dag_id="test",
        schedule="0 0 * * *",
        start_date=pendulum.datetime(2021, 1, 1, tz="UTC"),
        catchup=False,
        dagrun_timeout=datetime.timedelta(minutes=60),
        tags=["iwfm"],
) as dag:
    run_this_last = EmptyOperator(
        task_id="run_this_last",
    )

    run_this = BashOperator(
        task_id="echo",
        bash_command="echo 1",
    )

    run_this >> run_this_last

if __name__ == "__main__":
    dag.test()