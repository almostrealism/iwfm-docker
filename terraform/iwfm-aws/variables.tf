variable "prefix" {
  description = "The prefix used for all resources"
  default = "iwfm"
}

variable "region" {
  description = "The AWS region where all resources should be created"
  default = "us-east-2"
}

variable "instance_type" {
  description = "The type of AWS EC2 instance to use for the cluster"
  default = "c6i.large"
}

variable "instance_root_volume_size" {
  description = "The size of the root volume for cluster instances"
  default = "750"
}

variable "instance_docker_volume_size" {
  description = "The size of the volume for docker images on the cluster instances"
  default = "150"
}

variable "aws_access_key" {
  description = "AWS Access Key"
}

variable "aws_secret_key" {
  description = "AWS Secret Key"
}

variable "image" {
  description = "Docker image"
  default = "ashesfall/iwfm-base:latest"
}

variable "resource_bucket" {
  description = "A unique name for an S3 bucket to store the resources for the process"
}

variable "analytics_bucket" {
  description = "A unique name for an S3 bucket to store the parquet files for analytics"
}

variable "analytics_workspace_name" {
  description = "A name for the analytics workspace"
}

variable "iwfm_model" {
  description = "The path to the IWFM model to run"
}