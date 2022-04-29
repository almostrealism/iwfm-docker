provider "aws" {
  // version = ">= 1.58.0, <= 2.0.0"
  region = var.region
  access_key = var.aws_access_key
  secret_key = var.aws_secret_key
}

resource "aws_ecs_task_definition" "definition" {
  family                   = "${var.prefix}-task"
  task_role_arn            = aws_iam_role.ecs_task_role.arn
  execution_role_arn       = aws_iam_role.ecs_task_execution_role.arn
  network_mode             = "awsvpc"
  cpu                      = "1024"
  memory                   = "2048"
  requires_compatibilities = ["FARGATE"]

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
                      "awslogs-group" : "stream-to-log-fluentd",
                      "awslogs-stream-prefix" : "${var.prefix}"
                  }
              },
      "environment": [
              {
                  "name": "IWFM_MODEL",
                  "value": "${var.iwfm_model}"
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
  launch_type     = "FARGATE"

  network_configuration {
    subnets = [aws_subnet.public.id]
    security_groups = [aws_security_group.ecs_tasks.id]
    assign_public_ip = true
  }
}