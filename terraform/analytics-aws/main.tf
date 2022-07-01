provider "aws" {
  version = "4.12.1"
  region = "us-east-1"
  access_key = var.aws_access_key
  secret_key = var.aws_secret_key
}