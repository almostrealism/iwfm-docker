resource "aws_ecs_task_definition" "agent" {
  family                   = "${var.prefix}-agent-task"
  task_role_arn            = aws_iam_role.ecs_task_role.arn
  execution_role_arn       = aws_iam_role.ecs_task_execution_role.arn
  network_mode             = "host"
  cpu                      = "1584"
  memory                   = "3072"

  container_definitions = <<DEFINITION
  [
    {
      "image": "${var.agent_image}:${var.tag}",
      "name": "${var.prefix}-agent",
      "logConfiguration": {
                  "logDriver": "awslogs",
                  "options": {
                      "awslogs-region" : "${var.region}",
                      "awslogs-group" : "${var.prefix}-agents",
                      "awslogs-stream-prefix" : "${var.prefix}-agent"
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
                  "value": "${var.prefix}_${var.analytics_title}_db"
              },
              {
                  "name": "SQS_ARN",
                  "value": "${aws_sqs_queue.main.arn}"
              },
              {
                  "name": "BROKER_URL",
                  "value": "sqs://${aws_sqs_queue.main.id}"
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

resource "aws_ecs_service" "agent" {
  name            = "${var.prefix}-agents"
  cluster         = aws_ecs_cluster.cluster.id
  task_definition = aws_ecs_task_definition.agent.arn
  desired_count   = var.agent_count
  launch_type     = "EC2"
  depends_on = [aws_cloudwatch_log_group.agents, aws_s3_object.model]

#  network_configuration {
#    subnets = [aws_subnet.public.id]
#    security_groups = [aws_security_group.ecs_tasks.id]
#    # assign_public_ip = true
#  }
}