resource "aws_quicksight_group" "main" {
  group_name = "${var.prefix}-group"
}

resource "aws_quicksight_data_source" "athena" {
  data_source_id = "${var.prefix}-db-source"
  name           = "Athena Data"
  type           = "ATHENA"

  parameters {
    athena {
      work_group = "${var.prefix}-workgroup"
    }
  }
}