resource "aws_ecs_cluster" "cluster" {
  name = "${var.prefix}-cluster"
}

resource "aws_cloudwatch_log_group" "instance" {
  name = "${var.prefix}-instance"
}

resource "aws_security_group_rule" "instance_out_all" {
  type              = "egress"
  from_port         = 0
  to_port           = 65535
  protocol          = "tcp"
  cidr_blocks       = ["0.0.0.0/0"]
  security_group_id = "${aws_security_group.ecs_tasks.id}"
}

data "aws_ami" "ecs" {
  most_recent = true

  filter {
    name   = "name"
    values = ["amzn2-ami-ecs-hvm-2.0.20220520-x86_64-ebs"]
  }

  filter {
    name   = "virtualization-type"
    values = ["hvm"]
  }

  owners = ["amazon"]
}

#resource "aws_key_pair" "user" {
#  key_name   = "${var.prefix}-key"
#  public_key = "${file("~/.ssh/id_rsa.pub")}"
#}

resource "aws_launch_configuration" "instance" {
  name_prefix          = "${var.prefix}-lc"
  image_id             = "${data.aws_ami.ecs.id}"
  instance_type        = "${var.instance_type}"
  iam_instance_profile = "${aws_iam_instance_profile.instance.name}"
  security_groups      = ["${aws_security_group.ecs_tasks.id}"]
#  key_name             = "${aws_key_pair.user.key_name}"


  user_data = <<EOF
#!/bin/bash
echo ECS_CLUSTER=${aws_ecs_cluster.cluster.name} >> /etc/ecs/ecs.config
EOF

  root_block_device {
    volume_size = "${var.instance_root_volume_size}"
    volume_type = "gp2"
  }

  ebs_block_device {
    device_name = "/dev/xvdcz"
    volume_size = "${var.instance_docker_volume_size}"
    volume_type = "gp2"
  }

  lifecycle {
    create_before_destroy = false
  }
}

resource "aws_autoscaling_group" "asg" {
  name = "${var.prefix}-scale-group"

  launch_configuration = "${aws_launch_configuration.instance.name}"
  vpc_zone_identifier  = ["${aws_subnet.public.id}"]
  max_size             = "1"
  min_size             = "1"

  health_check_grace_period = 300
  health_check_type         = "EC2"

  tag {
    key                 = "activity"
    value               = "${var.prefix}"
    propagate_at_launch = true
  }

  lifecycle {
    create_before_destroy = false
  }
}

resource "aws_ecs_capacity_provider" "main" {
  name = "${var.prefix}-provider"

  auto_scaling_group_provider {
    auto_scaling_group_arn = aws_autoscaling_group.asg.arn

    managed_scaling {
      status                    = "ENABLED"
      target_capacity           = 1
      maximum_scaling_step_size = 1
    }
  }
}

resource "aws_ecs_cluster_capacity_providers" "main" {
  cluster_name = aws_ecs_cluster.cluster.name

  capacity_providers = [aws_ecs_capacity_provider.main.name]

  default_capacity_provider_strategy {
    base              = 1
    weight            = 100
    capacity_provider = aws_ecs_capacity_provider.main.name
  }
}