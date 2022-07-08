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

#  requires_compatibilities = ["FARGATE"]
#  ephemeral_storage {
#    size_in_gib = 100
#  }

  container_definitions = <<DEFINITION
  [
    {
      "image": "${var.image}",
      "name": "${var.prefix}-container",
      "portMappings": [
        {
          "containerPort": 80,
          "hostPort": 80
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
                  "name": "IWFM_MODEL",
                  "value": "http://${aws_s3_bucket_website_configuration.www.website_endpoint}/model.zip"
              },
              {
                  "name": "DB_BUCKET",
                  "value": "${aws_s3_bucket.db.bucket}"
              },
              {
                  "name": "DB_NAME",
                  "value": "${var.prefix}_database"
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
  depends_on = [aws_cloudwatch_log_group.main, aws_s3_object.model]

#  network_configuration {
#    subnets = [aws_subnet.public.id]
#    security_groups = [aws_security_group.ecs_tasks.id]
#    # assign_public_ip = true
#  }
}