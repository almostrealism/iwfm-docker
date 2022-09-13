resource "aws_s3_bucket" "db" {
  bucket = var.analytics_bucket

  tags = {
    Name = "${var.prefix}-db"
  }

  force_destroy = true
}

resource "aws_athena_workgroup" "main" {
  name = "${var.prefix}-${var.analytics_title}"

  configuration {
    enforce_workgroup_configuration    = true
    publish_cloudwatch_metrics_enabled = true

    result_configuration {
      output_location = "s3://${aws_s3_bucket.db.bucket}/output/"
    }
  }
}

resource "aws_athena_database" "main" {
  name   = "${var.prefix}_${var.analytics_title}_db"
  bucket = aws_s3_bucket.resources.bucket
}