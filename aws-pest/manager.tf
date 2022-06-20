
resource "aws_ecs_task_definition" "manager" {
  family                   = "${var.prefix}-manager-task"
  task_role_arn            = aws_iam_role.ecs_task_role.arn
  execution_role_arn       = aws_iam_role.ecs_task_execution_role.arn
  network_mode             = "host"
  cpu                      = "2048"
  memory                   = "3860"

  container_definitions = <<DEFINITION
  [
    {
      "image": "${var.manager_image}",
      "name": "${var.prefix}-manager",
      "portMappings": [
        {
          "containerPort": 80,
          "hostPort": 80
        },
        {
          "containerPort": 4000,
          "hostPort": 4000
        }
      ],
      "logConfiguration": {
                  "logDriver": "awslogs",
                  "options": {
                      "awslogs-region" : "${var.region}",
                      "awslogs-group" : "${var.prefix}-logs",
                      "awslogs-stream-prefix" : "${var.prefix}-manager"
                  }
              },
      "environment": [
              {
                  "name": "IWFM_MODEL",
                  "value": "http://${aws_s3_bucket_website_configuration.www.website_endpoint}/model.zip"
              }
          ]
      }
  ]
  DEFINITION
}

resource "aws_ecs_service" "manager" {
  name            = "${var.prefix}-management"
  cluster         = aws_ecs_cluster.cluster.id
  task_definition = aws_ecs_task_definition.manager.arn
  desired_count   = 1
  launch_type     = "EC2"
  depends_on = [aws_cloudwatch_log_group.main, aws_s3_object.model]

#  network_configuration {
#    subnets = [aws_subnet.public.id]
#    security_groups = [aws_security_group.ecs_tasks.id]
#    # assign_public_ip = true
#  }
}