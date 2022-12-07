from __future__ import annotations

import datetime

import pendulum

from airflow import DAG
from airflow.operators.bash import BashOperator
from airflow.operators.empty import EmptyOperator

with DAG(
        dag_id="run_model",
        schedule="0 0 * * *",
        start_date=pendulum.datetime(2021, 1, 1, tz="UTC"),
        catchup=False,
        dagrun_timeout=datetime.timedelta(minutes=60),
        tags=["iwfm"],
) as dag:
    run_this_last = EmptyOperator(
        task_id="run_this_last",
    )

    for i in range(25):
        run_this = BashOperator(
            task_id=f"run_{i}",
            bash_command="/run_model.sh /data ",
        )

        run_this >> run_this_last

if __name__ == "__main__":
    dag.test()