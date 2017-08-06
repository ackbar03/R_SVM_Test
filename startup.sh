#!/bin/bash
# Startup script to download neccessary stuff

sudo echo "deb https://mirrors.tuna.tsinghua.edu.cn/CRAN/bin/linux/ubuntu xenial/" | sudo tee -a /etc/apt/sources.list

gpg --keyserver keyserver.ubuntu.com --recv-key E084DAB9

gpg -a --export E084DAB9 | sudo apt-key add -

sudo apt-get update
sudo apt-get install r-base r-base-dev

nohup Rscript Run_v0-3_serverrun.R &