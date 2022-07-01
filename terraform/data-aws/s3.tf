resource "aws_s3_bucket" "resources" {
  bucket = var.resource_bucket

  tags = {
    Name = "${var.prefix}-resources"
  }
}

