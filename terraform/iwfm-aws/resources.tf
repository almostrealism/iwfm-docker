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