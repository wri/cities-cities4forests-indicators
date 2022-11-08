terraform {
    required_providers {
        aws = {
            source = "hashicorp/aws"
            version = "~> 4.16"
        }
    }

    required_version = ">= 1.2.0"
}

provider "aws" {
    region = "us-east-1"
}

data "aws_ssm_parameter" "aws_ami" {
    name = "/aws/service/ami-amazon-linux-latest/amzn2-ami-hvm-x86_64-gp2"
}

## Instance Profile and Role for AWS Session Manager Access
resource "aws_iam_role" "app_server_iam_role" {
    name = "C4FIndicatorsAppServerIAMRole"
    description = "The role for the C4FIndicators App Server"
    assume_role_policy = <<EOF
    {
        "Version": "2012-10-17",
        "Statement": {
            "Effect": "Allow",
            "Principal": {
                "Service": "ec2.amazonaws.com"
            },
            "Action": "sts:AssumeRole"
        }
    }
    EOF

    tags = {
        Application = "C4FIndicators"
        ProjectCode = "01012.00000.000.00"
        Project = "Cities4Forests Indicators"
        Program = "Cities"
    }
}

resource "aws_iam_role_policy_attachment" "app_server_ssm_policy" {
    role = aws_iam_role.app_server_iam_role.name
    policy_arn = "arn:aws:iam::aws:policy/AmazonSSMManagedInstanceCore"
}

resource "aws_iam_instance_profile" "app_server_instance_profile" {
    name = "C4FIndicatorsAppServerInstanceProfile"
    role = aws_iam_role.app_server_iam_role.name

    tags = {
        Application = "C4FIndicators"
        ProjectCode = "01012.00000.000.00"
        Project = "Cities4Forests Indicators"
        Program = "Cities"
    }
}

## App Server
resource "aws_security_group" "app_server_sg" {
    name = "C4FIndicatorsAppServerSecurityGroup"
    vpc_id = "vpc-0c97c96a7d7efc7e0"
    description = "Allows access to Shiny Server port."

    # Allow HTTP inbound
    ingress {
        from_port = 80
        to_port = 80
        protocol = "tcp"
        cidr_blocks = ["0.0.0.0/0"]
    }

    # Allow Shiny Server Inbound
    ingress {
        from_port = 3838
        to_port = 3838
        protocol = "tcp"
        cidr_blocks = ["0.0.0.0/0"]
    }

    # Allow All Outbound
    egress {
        from_port = 0
        to_port = 0
        protocol = "-1"
        cidr_blocks = ["0.0.0.0/0"]
    }

    tags = {
        Application = "C4FIndicators"
        ProjectCode = "02566.00000.000.00"
        Project = "Cities4Forests Indicators"
        Program = "Cities"
    }
}

resource "aws_instance" "app_server" {
    ami = data.aws_ssm_parameter.aws_ami.value
    instance_type = "m4.xlarge"
    subnet_id = "subnet-04284d52a74e95147" # aws-controltower-PublicSubnet1
    iam_instance_profile = aws_iam_instance_profile.app_server_instance_profile.name
    vpc_security_group_ids = [aws_security_group.app_server_sg.id]

    user_data = <<-EOF
      #!/bin/bash
      set -ex
      sudo yum update -y
      sudo yum install docker -y
      sudo systemctl enable docker
      sudo systemctl start docker
      sudo usermod -a -G docker ssm-user
      sudo usermod -a -G docker ec2-user
      sudo yum install git -y
    EOF

    tags = {
        Name = "C4FIndicatorsAppServerInstance"
        Application = "C4FIndicators"
        ProjectCode = "02566.00000.000.00"
        Project = "Cities4Forests Indicators"
        Program = "Cities"
    }
}

## Public IP
resource "aws_eip" "app_server_ip" {
    instance = aws_instance.app_server.id
    vpc = true
    tags = {
	    Name = "c4findicators_alb_eip"
        Application = "C4FIndicators"
        ProjectCode = "01012.00000.000.00"
        Project = "Cities4Forests Indicators"
        Program = "Cities"
    }
}



