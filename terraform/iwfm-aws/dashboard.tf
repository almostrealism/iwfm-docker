resource "aws_cloudwatch_dashboard" "main" {
  dashboard_name = "${var.prefix}-dashboard"
  depends_on = [aws_ecs_service.main, aws_cloudwatch_log_group.main]

  dashboard_body = <<EOF
  {
    "widgets": [
        {
            "height": 6,
            "width": 11,
            "y": 4,
            "x": 0,
            "type": "metric",
            "properties": {
                "view": "timeSeries",
                "stacked": false,
                "metrics": [
                    [ "AWS/ECS", "CPUUtilization", "ServiceName", "${var.prefix}-service", "ClusterName", "${var.prefix}-cluster" ]
                ],
                "region": "${var.region}"
            }
        },
        {
            "height": 6,
            "width": 11,
            "y": 4,
            "x": 11,
            "type": "metric",
            "properties": {
                "view": "timeSeries",
                "stacked": false,
                "metrics": [
                    [ "AWS/ECS", "MemoryUtilization", "ServiceName", "${var.prefix}-service", "ClusterName", "${var.prefix}-cluster" ]
                ],
                "region": "${var.region}"
            }
        },
        {
            "height": 6,
            "width": 22,
            "y": 10,
            "x": 0,
            "type": "log",
            "properties": {
                "query": "SOURCE '${var.prefix}-logs' | fields @timestamp, @message\n| sort @timestamp desc\n| limit 200",
                "region": "${var.region}",
                "stacked": false,
                "view": "table"
            }
        },
        {
            "height": 4,
            "width": 4,
            "y": 0,
            "x": 18,
            "type": "metric",
            "properties": {
                "view": "pie",
                "metrics": [
                    [ "AWS/Usage", "ResourceCount", "Type", "Resource", "Resource", "OnDemand", "Service", "Fargate", "Class", "None" ],
                    [ "...", "Spot", ".", ".", ".", "." ]
                ],
                "region": "${var.region}"
            }
        },
        {
            "height": 4,
            "width": 9,
            "y": 0,
            "x": 0,
            "type": "metric",
            "properties": {
                "view": "singleValue",
                "metrics": [
                    [ "AWS/Usage", "ResourceCount", "Type", "Resource", "Resource", "OnDemand", "Service", "Fargate", "Class", "None" ],
                    [ "...", "Spot", ".", ".", ".", "." ]
                ],
                "region": "${var.region}"
            }
        },
        {
            "height": 4,
            "width": 9,
            "y": 0,
            "x": 9,
            "type": "metric",
            "properties": {
                "view": "singleValue",
                "metrics": [
                    [ "AWS/Logs", "IncomingLogEvents", "LogGroupName", "${var.prefix}-logs" ],
                    [ ".", "IncomingBytes", ".", "." ]
                ],
                "region": "${var.region}"
            }
        }
    ]
}
EOF
}