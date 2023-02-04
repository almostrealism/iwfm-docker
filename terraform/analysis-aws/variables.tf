variable "prefix" {
  description = "The prefix used for all resources"
  default = "analysis"
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
  default = "250"
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

variable "admin_email" {
  description = "Email address for admin user"
}

variable "admin_password" {
  description = "Password for admin user"
}

variable "image" {
  description = "Docker image"
  default = "tjhatch/iwfm-analysis"
}

variable "tag" {
  description = "Docker image"
  default = "latest"
}

variable "dashboards_bucket" {
  description = "A unique name where the dashboards are located"
  default = ""
}

variable "sql_s3_url" {
  description = "The URL of the SQL scripts to run"
}

variable "mapbox_api_key" {
  description = "The API key to use for MapBox"
}