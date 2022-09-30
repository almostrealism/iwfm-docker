
resource "aws_ecs_task_definition" "manager" {
  family                   = "${var.prefix}-manager-task"
  task_role_arn            = aws_iam_role.ecs_task_role.arn
  execution_role_arn       = aws_iam_role.ecs_task_execution_role.arn
  network_mode             = "host"
  cpu                      = "12288"
  memory                   = "18432"

  container_definitions = <<DEFINITION
  [
    {
      "image": "${var.manager_image}:${var.tag}",
      "name": "${var.prefix}-manager",
      "portMappings": [
        {
          "containerPort": 80,
          "hostPort": 80
        },
        {
          "containerPort": 7766,
          "hostPort": 7766
        }
      ],
      "logConfiguration": {
                  "logDriver": "awslogs",
                  "options": {
                      "awslogs-region" : "${var.region}",
                      "awslogs-group" : "${var.prefix}-management",
                      "awslogs-stream-prefix" : "${var.prefix}-manager"
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
                  "name": "DASHBOARDS_BUCKET",
                  "value": "${aws_s3_bucket.dashboards.bucket}"
              },
              {
                  "name": "USER_ACCESS_KEY_ID",
                  "value": "${var.aws_access_key}"
              },
              {
                  "name": "USER_SECRET_ACCESS_KEY",
                  "value": "${var.aws_secret_key}"
              },
              {
                  "name": "WORKGROUP_NAME",
                  "value": "${var.prefix}-${var.analytics_title}"
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

resource "aws_ecs_service" "manager" {
  name            = "${var.prefix}-management"
  cluster         = aws_ecs_cluster.cluster.id
  task_definition = aws_ecs_task_definition.manager.arn
  desired_count   = 1
  launch_type     = "EC2"
  depends_on = [aws_cloudwatch_log_group.management, aws_s3_object.model]

#  network_configuration {
#    subnets = [aws_subnet.public.id]
#    security_groups = [aws_security_group.ecs_tasks.id]
#    # assign_public_ip = true
#  }
}