provider "aws" {
  version = "4.12.1"
  region = var.region
  access_key = var.aws_access_key
  secret_key = var.aws_secret_key
}

resource "aws_cloudwatch_log_group" "main" {
  name = "${var.prefix}-logs"
}

resource "aws_ecs_task_definition" "definition" {
  family                   = "${var.prefix}-task"
  task_role_arn            = aws_iam_role.ecs_task_role.arn
  execution_role_arn       = aws_iam_role.ecs_task_execution_role.arn
  network_mode             = "host"
  cpu                      = "2048"
  memory                   = "3860"

  container_definitions = <<DEFINITION
  [
    {
      "image": "${var.image}:${var.tag}",
      "name": "${var.prefix}-container",
      "portMappings": [
        {
          "containerPort": 8088,
          "hostPort": 8088
        }
      ],
      "logConfiguration": {
                  "logDriver": "awslogs",
                  "options": {
                      "awslogs-region" : "${var.region}",
                      "awslogs-group" : "${var.prefix}-logs",
                      "awslogs-stream-prefix" : "${var.prefix}"
                  }
              },
      "environment": [
              {
                  "name": "ADMIN_EMAIL",
                  "value": "${var.admin_email}"
              },
              {
                  "name": "ADMIN_PASSWORD",
                  "value": "${var.admin_password}"
              },
              {
                  "name": "DASHBOARDS_BUCKET",
                  "value": "${var.dashboards_bucket}"
              },
              {
                  "name": "MAPBOX_API_KEY",
                  "value": "${var.mapbox_api_key}"
              },
              {
                  "name": "AWS_DEFAULT_REGION",
                  "value": "${var.region}"
              }
          ]
      }
  ]
  DEFINITION
}

resource "aws_ecs_service" "main" {
  name            = "${var.prefix}-service"
  cluster         = aws_ecs_cluster.cluster.id
  task_definition = aws_ecs_task_definition.definition.arn
  desired_count   = 1
  launch_type     = "EC2"
  depends_on = [aws_cloudwatch_log_group.main]
}