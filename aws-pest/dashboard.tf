resource "aws_cloudwatch_dashboard" "main" {
  dashboard_name = "${var.prefix}-dashboard"
  depends_on = [data.aws_instance.capacity-provider, aws_ecs_service.manager, aws_ecs_service.agent, aws_cloudwatch_log_group.management, aws_cloudwatch_log_group.agents]

  dashboard_body = <<EOF
  {
    "widgets": [
        {
            "height": 5,
            "width": 11,
            "y": 7,
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
            "height": 5,
            "width": 11,
            "y": 7,
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
            "y": 20,
            "x": 0,
            "type": "log",
            "properties": {
                "query": "SOURCE '${var.prefix}-management' | fields @timestamp, @message\n| sort @timestamp desc\n| limit 200",
                "region": "${var.region}",
                "stacked": false,
                "view": "table",
                "title": "Manager"
            }
        },
        {
            "height": 3,
            "width": 7,
            "y": 0,
            "x": 0,
            "type": "metric",
            "properties": {
                "metrics": [
                    [ "AWS/ECS/ManagedScaling", "CapacityProviderReservation", "ClusterName", "${var.prefix}-cluster", "CapacityProviderName", "${var.prefix}-provider", { "label": "Reserved Capacity" } ],
                    [ "AWS/EC2", "StatusCheckFailed", "AutoScalingGroupName", "${var.prefix}-scale-group", { "label": "Failed Status Checks" } ]
                ],
                "view": "singleValue",
                "region": "${var.region}",
                "period": 300,
                "stat": "Average"
            }
        },
        {
            "height": 3,
            "width": 15,
            "y": 0,
            "x": 7,
            "type": "metric",
            "properties": {
                "metrics": [
                    [ "AWS/EC2", "EBSReadBytes", "AutoScalingGroupName", "${var.prefix}-scale-group", { "label": "Bytes Read" } ],
                    [ ".", "EBSWriteBytes", ".", ".", { "label": "Bytes Written" } ],
                    [ "AWS/EBS", "VolumeQueueLength", "VolumeId", "${tolist(data.aws_instance.capacity-provider.root_block_device)[0].volume_id}", { "label": "IO Queue Length" } ],
                    [ ".", "VolumeThroughputPercentage", ".", ".", { "label": "IO Throughput %" } ]
                ],
                "view": "singleValue",
                "region": "${var.region}",
                "period": 300,
                "stat": "Average"
            }
        },
        {
            "height": 4,
            "width": 5,
            "y": 3,
            "x": 0,
            "type": "metric",
            "properties": {
                "view": "timeSeries",
                "stat": "Sum",
                "period": 300,
                "stacked": false,
                "yAxis": {
                    "left": {
                        "min": 0
                    }
                },
                "region": "${var.region}",
                "metrics": [
                    [ { "expression": "m1_0 / PERIOD(m1_0) / 1024", "label": "${tolist(data.aws_instance.capacity-provider.root_block_device)[0].volume_id}" } ],
                    [ "AWS/EBS", "VolumeReadBytes", "VolumeId", "${tolist(data.aws_instance.capacity-provider.root_block_device)[0].volume_id}", { "label": "${tolist(data.aws_instance.capacity-provider.root_block_device)[0].volume_id}", "id": "m1_0", "visible": false } ]
                ],
                "title": "Read bandwidth (KiB/s)"
            }
        },
        {
            "height": 4,
            "width": 6,
            "y": 3,
            "x": 5,
            "type": "metric",
            "properties": {
                "view": "timeSeries",
                "stat": "Sum",
                "period": 300,
                "stacked": false,
                "yAxis": {
                    "left": {
                        "min": 0
                    }
                },
                "region": "${var.region}",
                "metrics": [
                    [ { "expression": "m1_0 / PERIOD(m1_0) / 1024", "label": "${tolist(data.aws_instance.capacity-provider.root_block_device)[0].volume_id}" } ],
                    [ "AWS/EBS", "VolumeWriteBytes", "VolumeId", "${tolist(data.aws_instance.capacity-provider.root_block_device)[0].volume_id}", { "label": "${tolist(data.aws_instance.capacity-provider.root_block_device)[0].volume_id}", "id": "m1_0", "visible": false } ]
                ],
                "title": "Write bandwidth (KiB/s)"
            }
        },
        {
            "height": 4,
            "width": 5,
            "y": 3,
            "x": 11,
            "type": "metric",
            "properties": {
                "view": "timeSeries",
                "stat": "Sum",
                "period": 300,
                "stacked": false,
                "yAxis": {
                    "left": {
                        "min": 0
                    }
                },
                "region": "${var.region}",
                "metrics": [
                    [ { "expression": "m1_0 / PERIOD(m1_0)", "label": "${tolist(data.aws_instance.capacity-provider.root_block_device)[0].volume_id}" } ],
                    [ "AWS/EBS", "VolumeReadOps", "VolumeId", "${tolist(data.aws_instance.capacity-provider.root_block_device)[0].volume_id}", { "label": "${tolist(data.aws_instance.capacity-provider.root_block_device)[0].volume_id}", "id": "m1_0", "visible": false } ]
                ],
                "title": "Read throughput (Ops/s)"
            }
        },
        {
            "height": 4,
            "width": 6,
            "y": 3,
            "x": 16,
            "type": "metric",
            "properties": {
                "view": "timeSeries",
                "stat": "Sum",
                "period": 300,
                "stacked": false,
                "yAxis": {
                    "left": {
                        "min": 0
                    }
                },
                "region": "${var.region}",
                "metrics": [
                    [ { "expression": "m1_0 / PERIOD(m1_0)", "label": "${tolist(data.aws_instance.capacity-provider.root_block_device)[0].volume_id}" } ],
                    [ "AWS/EBS", "VolumeWriteOps", "VolumeId", "${tolist(data.aws_instance.capacity-provider.root_block_device)[0].volume_id}", { "label": "${tolist(data.aws_instance.capacity-provider.root_block_device)[0].volume_id}", "id": "m1_0", "visible": false } ]
                ],
                "title": "Write throughput (Ops/s)"
            }
        },
        {
            "height": 4,
            "width": 5,
            "y": 12,
            "x": 0,
            "type": "metric",
            "properties": {
                "view": "timeSeries",
                "stat": "Average",
                "period": 300,
                "stacked": false,
                "yAxis": {
                    "left": {
                        "min": 0
                    }
                },
                "region": "${var.region}",
                "metrics": [
                    [ "AWS/EBS", "VolumeQueueLength", "VolumeId", "${tolist(data.aws_instance.capacity-provider.root_block_device)[0].volume_id}", { "label": "${tolist(data.aws_instance.capacity-provider.root_block_device)[0].volume_id}", "id": "m1_0", "visible": true } ]
                ],
                "title": "Average queue length (Operations)"
            }
        },
        {
            "height": 4,
            "width": 6,
            "y": 12,
            "x": 5,
            "type": "metric",
            "properties": {
                "view": "timeSeries",
                "stat": "Sum",
                "period": 300,
                "stacked": false,
                "yAxis": {
                    "left": {
                        "min": 0
                    }
                },
                "region": "${var.region}",
                "metrics": [
                    [ { "expression": "m1_0 / PERIOD(m1_0) * 100", "label": "${tolist(data.aws_instance.capacity-provider.root_block_device)[0].volume_id}" } ],
                    [ "AWS/EBS", "VolumeIdleTime", "VolumeId", "${tolist(data.aws_instance.capacity-provider.root_block_device)[0].volume_id}", { "label": "${tolist(data.aws_instance.capacity-provider.root_block_device)[0].volume_id}", "id": "m1_0", "visible": false } ]
                ],
                "title": "Time spent idle (%)"
            }
        },
        {
            "height": 4,
            "width": 5,
            "y": 12,
            "x": 11,
            "type": "metric",
            "properties": {
                "view": "timeSeries",
                "stat": "Sum",
                "period": 300,
                "stacked": false,
                "yAxis": {
                    "left": {
                        "min": 0
                    }
                },
                "region": "${var.region}",
                "metrics": [
                    [ { "expression": "IF(m2_0 != 0, (m1_0 / m2_0) / 1024, 0)", "label": "${tolist(data.aws_instance.capacity-provider.root_block_device)[0].volume_id}" } ],
                    [ "AWS/EBS", "VolumeReadBytes", "VolumeId", "${tolist(data.aws_instance.capacity-provider.root_block_device)[0].volume_id}", { "label": "${tolist(data.aws_instance.capacity-provider.root_block_device)[0].volume_id}", "id": "m1_0", "visible": false } ],
                    [ ".", "VolumeReadOps", ".", ".", { "label": "${tolist(data.aws_instance.capacity-provider.root_block_device)[0].volume_id}", "id": "m2_0", "visible": false } ]
                ],
                "title": "Average read size (KiB/op)"
            }
        },
        {
            "height": 4,
            "width": 6,
            "y": 12,
            "x": 16,
            "type": "metric",
            "properties": {
                "view": "timeSeries",
                "stat": "Sum",
                "period": 300,
                "stacked": false,
                "yAxis": {
                    "left": {
                        "min": 0
                    }
                },
                "region": "${var.region}",
                "metrics": [
                    [ { "expression": "IF(m2_0 != 0, (m1_0 / m2_0) / 1024, 0)", "label": "${tolist(data.aws_instance.capacity-provider.root_block_device)[0].volume_id}" } ],
                    [ "AWS/EBS", "VolumeWriteBytes", "VolumeId", "${tolist(data.aws_instance.capacity-provider.root_block_device)[0].volume_id}", { "label": "${tolist(data.aws_instance.capacity-provider.root_block_device)[0].volume_id}", "id": "m1_0", "visible": false } ],
                    [ ".", "VolumeWriteOps", ".", ".", { "label": "${tolist(data.aws_instance.capacity-provider.root_block_device)[0].volume_id}", "id": "m2_0", "visible": false } ]
                ],
                "title": "Average write size (KiB/op)"
            }
        },
        {
            "height": 4,
            "width": 11,
            "y": 16,
            "x": 0,
            "type": "metric",
            "properties": {
                "view": "timeSeries",
                "stat": "Sum",
                "period": 300,
                "stacked": false,
                "yAxis": {
                    "left": {
                        "min": 0
                    }
                },
                "region": "${var.region}",
                "metrics": [
                    [ { "expression": "IF(m2_0 !=0, (m1_0 / m2_0) * 1000, 0)", "label": "${tolist(data.aws_instance.capacity-provider.root_block_device)[0].volume_id}" } ],
                    [ "AWS/EBS", "VolumeTotalReadTime", "VolumeId", "${tolist(data.aws_instance.capacity-provider.root_block_device)[0].volume_id}", { "label": "${tolist(data.aws_instance.capacity-provider.root_block_device)[0].volume_id}", "id": "m1_0", "visible": false } ],
                    [ ".", "VolumeReadOps", ".", ".", { "label": "${tolist(data.aws_instance.capacity-provider.root_block_device)[0].volume_id}", "id": "m2_0", "visible": false } ]
                ],
                "title": "Average read latency (ms/op)"
            }
        },
        {
            "height": 4,
            "width": 11,
            "y": 16,
            "x": 11,
            "type": "metric",
            "properties": {
                "view": "timeSeries",
                "stat": "Sum",
                "period": 300,
                "stacked": false,
                "yAxis": {
                    "left": {
                        "min": 0
                    }
                },
                "region": "${var.region}",
                "metrics": [
                    [ { "expression": "IF(m2_0 !=0, (m1_0 / m2_0) * 1000, 0)", "label": "${tolist(data.aws_instance.capacity-provider.root_block_device)[0].volume_id}" } ],
                    [ "AWS/EBS", "VolumeTotalWriteTime", "VolumeId", "${tolist(data.aws_instance.capacity-provider.root_block_device)[0].volume_id}", { "label": "${tolist(data.aws_instance.capacity-provider.root_block_device)[0].volume_id}", "id": "m1_0", "visible": false } ],
                    [ ".", "VolumeWriteOps", ".", ".", { "label": "${tolist(data.aws_instance.capacity-provider.root_block_device)[0].volume_id}", "id": "m2_0", "visible": false } ]
                ],
                "title": "Average write latency (ms/op)"
            }
        },
        {
            "height": 6,
            "width": 22,
            "y": 26,
            "x": 0,
            "type": "log",
            "properties": {
                "query": "SOURCE '${var.prefix}-agents' | fields @timestamp, @message\n| sort @timestamp desc\n| limit 20",
                "region": "${var.region}",
                "stacked": false,
                "view": "table",
                "title": "All Agents"
            }
        }
    ]
}
EOF
}