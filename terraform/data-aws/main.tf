provider "aws" {
  version = "4.12.1"
  region = var.region
  access_key = var.aws_access_key
  secret_key = var.aws_secret_key
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

