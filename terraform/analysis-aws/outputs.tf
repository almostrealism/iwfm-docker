data "aws_instance" "capacity-provider" {
  depends_on = [aws_autoscaling_group.asg]

  filter {
    name          = "tag:activity"
    values        = ["${var.prefix}"]
  }

  filter {
    name = "instance-state-name"
    values = ["pending", "running"]
  }
}

output "url" {
  value = "http://${data.aws_instance.capacity-provider.public_ip}:8088"
}