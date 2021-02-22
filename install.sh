#!/usr/bin/env bash

if [ $(lsb_release -si) = "Debian" ]; then
	sudo apt-get -y install ansible git curl wget
else
	echo "os not supported"
	exit
fi

ansible-playbook install.yml