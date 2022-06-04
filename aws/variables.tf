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
  default = "m5zn.large"
}

variable "instance_root_volume_size" {
  description = "The size of the root volume for cluster instances"
  default = "75"
}

variable "instance_docker_volume_size" {
  description = "The size of the root volume for cluster instances"
  default = "300"
}

variable "aws_access_key" {
  description = "AWS Access Key"
}

variable "aws_secret_key" {
  description = "AWS Secret Key"
}

variable "image" {
  description = "Docker image"
  default = "ashesfall/iwfm-pest:latest"
}

variable "iwfm_model" {
  description = "The URL to obtain the IWFM model to run"
  default = "https://data.cnra.ca.gov/dataset/31f3ddf8-752a-4b04-99e0-2a9f9139817a/resource/bc00cfa5-86ac-4e95-acda-6df1f3d85a73/download/c2vsimfg_version1.01.zip"
}