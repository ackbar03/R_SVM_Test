#!/bin/bash
# Startup script to download neccessary stuff

apt-get update

apt-get -y install r-base r-base-dev

nohup Rscript Run_v0-3_serverrun.R &