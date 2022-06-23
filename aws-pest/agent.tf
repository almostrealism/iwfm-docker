resource "aws_ecs_task_definition" "agent" {
  family                   = "${var.prefix}-agent-task"
  task_role_arn            = aws_iam_role.ecs_task_role.arn
  execution_role_arn       = aws_iam_role.ecs_task_execution_role.arn
  network_mode             = "host"
  cpu                      = "1024"
  memory                   = "2048"

  container_definitions = <<DEFINITION
  [
    {
      "image": "${var.agent_image}",
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
                  "name": "PEST_HOST",
                  "value": "127.0.0.1:4000"
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