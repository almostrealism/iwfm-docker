resource "aws_cloudwatch_dashboard" "main" {
  dashboard_name = "${var.prefix}-dashboard"
  depends_on = [aws_ecs_service.manager, aws_ecs_service.agent, aws_cloudwatch_log_group.main]

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
                "metrics": [
                    [ "AWS/ECS", "CPUUtilization", "ServiceName", "${var.prefix}-service", "ClusterName", "${var.prefix}-cluster", { "visible": false } ],
                    [ "...", "${var.prefix}-management", ".", "." ],
                    [ "...", "${var.prefix}-agents", ".", "." ]
                ],
                "view": "timeSeries",
                "stacked": false,
                "region": "${var.region}",
                "period": 300,
                "stat": "Average"
            }
        },
        {
            "height": 6,
            "width": 11,
            "y": 4,
            "x": 11,
            "type": "metric",
            "properties": {
                "metrics": [
                    [ "AWS/ECS", "MemoryUtilization", "ServiceName", "${var.prefix}-service", "ClusterName", "${var.prefix}-cluster", { "visible": false } ],
                    [ "...", "${var.prefix}-management", ".", "." ],
                    [ "...", "${var.prefix}-agents", ".", "." ]
                ],
                "view": "timeSeries",
                "stacked": false,
                "region": "${var.region}",
                "period": 300,
                "stat": "Average"
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
            "width": 10,
            "y": 0,
            "x": 0,
            "type": "metric",
            "properties": {
                "metrics": [
                    [ "AWS/Usage", "ResourceCount", "Type", "Resource", "Resource", "OnDemand", "Service", "Fargate", "Class", "None", { "visible": false } ],
                    [ "...", "Spot", ".", ".", ".", ".", { "visible": false } ],
                    [ "AWS/ECS/ManagedScaling", "CapacityProviderReservation", "ClusterName", "${var.prefix}-cluster", "CapacityProviderName", "${var.prefix}-provider" ],
                    [ "AWS/EC2", "StatusCheckFailed", "AutoScalingGroupName", "${var.prefix}-scale-group" ]
                ],
                "view": "singleValue",
                "region": "${var.region}",
                "period": 300,
                "stat": "Average"
            }
        },
        {
            "height": 4,
            "width": 12,
            "y": 0,
            "x": 10,
            "type": "metric",
            "properties": {
                "metrics": [
                    [ "AWS/Logs", "IncomingLogEvents", "LogGroupName", "${var.prefix}-logs", { "visible": false } ],
                    [ ".", "IncomingBytes", ".", ".", { "visible": false } ],
                    [ "AWS/EC2", "EBSReadBytes", "AutoScalingGroupName", "${var.prefix}-scale-group" ],
                    [ ".", "EBSWriteBytes", ".", "." ],
                    [ ".", "EBSByteBalance%", ".", "." ]
                ],
                "view": "singleValue",
                "region": "${var.region}",
                "period": 300,
                "stat": "Average"
            }
        }
    ]
}
EOF
}