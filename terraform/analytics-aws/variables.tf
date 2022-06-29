variable "prefix" {
  description = "The prefix used for all resources"
  default = "iwfm"
}

variable "region" {
  description = "The AWS region where all resources should be created"
  default = "us-east-2"
}

variable "aws_access_key" {
  description = "AWS Access Key"
}

variable "aws_secret_key" {
  description = "AWS Secret Key"
}

variable "resource_bucket" {
  description = "A unique name for an S3 bucket to store the resources for the process"
}