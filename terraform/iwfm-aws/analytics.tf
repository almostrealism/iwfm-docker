resource "aws_s3_bucket" "db" {
  bucket = var.resource_bucket

  tags = {
    Name = "${var.prefix}-db"
  }
}

resource "aws_athena_workgroup" "main" {
  name = "${var.prefix}-workgroup"

  configuration {
    enforce_workgroup_configuration    = true
    publish_cloudwatch_metrics_enabled = true

    result_configuration {
      output_location = "s3://${aws_s3_bucket.resources.bucket}/output/"
    }
  }
}

resource "aws_athena_database" "main" {
  name   = "${var.prefix}_db"
  bucket = aws_s3_bucket.resources.bucket
}