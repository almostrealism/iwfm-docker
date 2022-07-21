resource "aws_s3_bucket" "db" {
  bucket = var.analytics_bucket

  tags = {
    Name = "${var.prefix}-db"
  }
}

resource "aws_athena_workgroup" "main" {
  name = "${var.prefix}-${var.analytics_workspace_name}"

  configuration {
    enforce_workgroup_configuration    = true
    publish_cloudwatch_metrics_enabled = true

    result_configuration {
      output_location = "s3://${aws_s3_bucket.db.bucket}/output/"
    }
  }
}

resource "aws_athena_database" "main" {
  name   = "${var.prefix}_database"
  bucket = aws_s3_bucket.resources.bucket
}