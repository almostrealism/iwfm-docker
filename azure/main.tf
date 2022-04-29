provider "azurerm" {
  features { }
  subscription_id = "${var.subscription_id}"
}

resource "azurerm_resource_group" "main" {
  name     = "${var.prefix}-resources"
  location = "${var.location}"
}

# This creates the plan that the service use
resource "azurerm_app_service_plan" "main" {
  name                = "${var.prefix}-asp"
  location            = "${azurerm_resource_group.main.location}"
  resource_group_name = "${azurerm_resource_group.main.name}"
  kind                = "Linux"
  reserved            = true

  sku {
    tier = "Standard"
    size = "S1"
  }
}

# This creates the service definition
resource "azurerm_app_service" "main" {
  name                = "${var.prefix}-appservice"
  location            = "${azurerm_resource_group.main.location}"
  resource_group_name = "${azurerm_resource_group.main.name}"
  app_service_plan_id = "${azurerm_app_service_plan.main.id}"

  site_config {
    app_command_line = ""
    linux_fx_version = "DOCKER|${var.docker_image}:${var.docker_image_tag}"
    always_on        = true
  }

  app_settings = {
    "WEBSITES_ENABLE_APP_SERVICE_STORAGE" = "false"
    "DOCKER_REGISTRY_SERVER_URL" = "https://index.docker.io"
    "IWFM_MODEL" = "${var.iwfm_model}"
  }
}

output "app_service_name" {
  value = "${azurerm_app_service.main.name}"
}

output "app_service_default_hostname" {
  value = "https://${azurerm_app_service.main.default_site_hostname}"
}