data "aws_instance" "capacity-provider" {
  instance_tags {
    iwfm-activity = "${var.prefix}"
  }
}

output "instance_ip" {
  value = data.aws_instance.capacity-provider.public_ip
}

output "instance_volume" {
  value = data.aws_instance.capacity-provider.root_block_device
}