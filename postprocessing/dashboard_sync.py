import os
import io
import yaml

access_key_id = os.getenv("USER_ACCESS_KEY_ID")
secret_access_key = os.getenv("USER_SECRET_ACCESS_KEY")
region = os.getenv("AWS_DEFAULT_REGION")
database = os.getenv("DB_NAME")
workgroup = os.getenv("WORKGROUP_NAME")

if __name__ == '__main__':
    data = {}

    with open("/dashboards/dashboard/databases/Amazon_Athena.yaml", "r") as stream:
        try:
            data = yaml.safe_load(stream)
        except yaml.YAMLError as exc:
            print(exc)

    data["sqlalchemy_uri"] = f"awsathena+rest://{access_key_id}:{secret_access_key}@athena.{region}.amazonaws.com/{database}?work_group={workgroup}"

    with io.open("/dashboards/dashboard/databases/Amazon_Athena.yaml", "w", encoding="utf8") as outfile:
        yaml.dump(data, outfile, default_flow_style=False, allow_unicode=True)

    for file in os.listdir("/dashboards/dashboard/datasets/Amazon_Athena"):
        if file.endswith(".yaml"):
            with open(f"/dashboards/dashboard/datasets/Amazon_Athena/{file}", "r") as stream:
                try:
                    data = yaml.safe_load(stream)
                except yaml.YAMLError as exc:
                    print(exc)

            data["schema"] = database

            with io.open(f"/dashboards/dashboard/datasets/Amazon_Athena/{file}", "w", encoding="utf8") as outfile:
                yaml.dump(data, outfile, default_flow_style=False, allow_unicode=True)

    print(f"Synced datasets and database to {database}")