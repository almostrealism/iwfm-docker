resource "aws_s3_bucket" "resources" {
  bucket = var.resource_bucket

  tags = {
    Name = "${var.prefix}-resources"
  }

  force_destroy = true
}

resource aws_s3_bucket_website_configuration "www" {
  bucket = aws_s3_bucket.resources.id

  index_document {
    suffix = "model.zip"
  }
}

resource "aws_s3_object" "model" {
  bucket = aws_s3_bucket.resources.id
  key    = "model.zip"
  acl    = "public-read"
  source = var.iwfm_model
  etag = filemd5(var.iwfm_model)
}

resource "aws_s3_object" "control" {
  bucket = aws_s3_bucket.resources.id
  key    = "control.py"
  source = var.control_file
  etag = filemd5(var.control_file)
}

resource "aws_s3_bucket" "dashboards" {
  bucket = var.dashboard_bucket

  tags = {
    Name = "${var.prefix}-dashboards"
  }

  force_destroy = true
}

resource "aws_s3_object" "dashboards" {
  bucket = aws_s3_bucket.dashboards.id
  key    = "dashboards.zip"
  source = var.dashboards_zip
  etag = filemd5(var.dashboards_zip)
}