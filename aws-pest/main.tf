provider "aws" {
  version = "4.12.1"
  region = var.region
  access_key = var.aws_access_key
  secret_key = var.aws_secret_key
}

resource "aws_cloudwatch_log_group" "main" {
  name = "${var.prefix}-logs"
}