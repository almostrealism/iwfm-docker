variable "prefix" {
  description = "The prefix used for all resources"
  default = "iwfm"
}

variable "location" {
  description = "The Azure location where all resources should be created"
  default = "Central US"
}

variable "subscription_id" {
  description = "Azure Subscription ID to be used for billing"
}

variable "docker_image" {
  description = "Docker image name"
  default = "ashesfall/iwfm-base"
}

variable "docker_image_tag" {
  description = "Docker image tag"
  default = "latest"
}

variable "iwfm_model" {
  description = "The URL to obtain the IWFM model to run"
  default = "https://data.cnra.ca.gov/dataset/31f3ddf8-752a-4b04-99e0-2a9f9139817a/resource/bc00cfa5-86ac-4e95-acda-6df1f3d85a73/download/c2vsimfg_version1.01.zip"
}