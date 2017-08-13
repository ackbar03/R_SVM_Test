#!/bin/bash
# Startup script to download neccessary stuff

echo "deb https://mirrors.tuna.tsinghua.edu.cn/CRAN/bin/linux/ubuntu xenial/" | tee -a /etc/apt/sources.list 

sudo apt-get update

sudo apt-get -y install r-base

sudo apt-get -y install r-base-dev

sudo nohup Rscript Run_v0-3_serverrun.R &
