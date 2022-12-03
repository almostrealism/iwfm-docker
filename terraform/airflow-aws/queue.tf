resource "aws_sqs_queue" "deadletter" {
  name = "${var.prefix}-deadletter-queue"
#  redrive_allow_policy = jsonencode({
#    redrivePermission = "byQueue",
#    sourceQueueArns   = [aws_sqs_queue.main.arn]
#  })
}

resource "aws_sqs_queue" "main" {
  name                      = "${var.prefix}-queue"
  delay_seconds             = 90
  max_message_size          = 2048
  message_retention_seconds = 86400
  receive_wait_time_seconds = 10
  redrive_policy = jsonencode({
    deadLetterTargetArn = aws_sqs_queue.deadletter.arn
    maxReceiveCount     = 4
  })
}