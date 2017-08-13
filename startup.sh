#!/bin/bash
# Startup script to download neccessary stuff
sudo echo "deb https://mirrors.tuna.tsinghua.edu.cn/CRAN/bin/linux/ubuntu xenial/" | sudo tee -a /etc/apt/sources.list

apt-get update
apt-get -y install r-base r-base-dev

nohup Rscript Run_v0-3_serverrun.R &