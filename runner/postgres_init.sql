CREATE USER airflow PASSWORD 'airflow';
CREATE DATABASE airflow;
GRANT CONNECT ON DATABASE airflow TO airflow;
GRANT CREATE ON DATABASE airflow TO airflow;