resource "aws_cloudwatch_log_group" "db" {
  name = "${var.prefix}-db-logs"
}

resource "aws_ecs_task_definition" "db" {
  family                   = "${var.prefix}-task"
  task_role_arn            = aws_iam_role.ecs_task_role.arn
  execution_role_arn       = aws_iam_role.ecs_task_execution_role.arn
  network_mode             = "host"
  cpu                      = "2048"
  memory                   = "3860"

  container_definitions = <<DEFINITION
  [
    {
      "image": "${var.db_image}:${var.db_tag}",
      "name": "${var.prefix}-db-container",
      "portMappings": [
        {
          "containerPort": 5432,
          "hostPort": 5432
        }
      ],
      "logConfiguration": {
                  "logDriver": "awslogs",
                  "options": {
                      "awslogs-region" : "${var.region}",
                      "awslogs-group" : "${var.prefix}-db-logs",
                      "awslogs-stream-prefix" : "${var.prefix}-db"
                  }
              },
      "environment": [
              {
                  "name": "SQL_S3_URL",
                  "value": "${var.sql_s3_url}"
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

resource "aws_ecs_service" "db" {
  name            = "${var.prefix}-db-service"
  cluster         = aws_ecs_cluster.cluster.id
  task_definition = aws_ecs_task_definition.db.arn
  desired_count   = 1
  launch_type     = "EC2"
  depends_on = [aws_cloudwatch_log_group.db]
}