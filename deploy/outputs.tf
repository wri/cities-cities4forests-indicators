output "app_server_ip" {
    value = aws_eip.app_server_ip.public_ip
}