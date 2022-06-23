data "aws_instance" "capacity-provider" {
  depends_on = [aws_autoscaling_group.asg]

  filter {
    name          = "tag:iwfm-activity"
    values        = ["${var.prefix}"]
  }
}

output "instance_ip" {
  value = data.aws_instance.capacity-provider.public_ip
}

output "instance_volume" {
  value = tolist(data.aws_instance.capacity-provider.root_block_device)[0].volume_id
}