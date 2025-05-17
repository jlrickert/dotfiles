# Use the latest Ubuntu image as the base.  This provides a stable and up-to-date environment.
FROM ubuntu:latest

# Set noninteractive mode to avoid prompts during package installation.
ARG DEBIAN_FRONTEND=noninteractive

# Update the package lists and install necessary dependencies. See
# https://github.com/nvim-lua/kickstart.nvim?tab=readme-ov-file#Install-Recipes
# for ubuntu
RUN apt-get update && \
	apt-get install -y --no-install-recommends \
	build-essential \
	cmake \
	curl \
	fd-find \
	fontconfig \
	gcc \
	git \
	locales \
	luarocks \
	make \ 
	pkg-config \
	python3 \
	python3-pip \
	ripgrep \
	software-properties-common \
	unzip \
	wget \
	xclip \
	zsh

RUN rm -rf /var/lib/apt/lists/* && \
	localedef -i en_US -c -f UTF-8 -A /usr/share/locale/locale.alias en_US.UTF-8

ENV LANG=en_US.utf8

# Install stylua
RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs > ./rustup.sh && \
	bash ./rustup.sh -y && \
	rm ./rustup.sh

RUN . /root/.cargo/env && cargo install stylua --features luajit

# Add the Neovim PPA to get the latest version.  This is the recommended way to install Neovim on Ubuntu.
RUN add-apt-repository ppa:neovim-ppa/unstable && \
	apt-get update

# Install Neovim.
RUN apt-get install -y neovim

# Install Node.js and npm (optional, but often needed for plugins).  Use the
# NodeSource repository for a more up-to-date version.
# Probably used for TypeScript
RUN curl -sL https://deb.nodesource.com/setup_lts.x | bash - && \
	apt-get install -y nodejs

# Install Ruby and gem (optional, but often needed for plugins).
# No clue if I use this at all
# RUN apt-get install -y ruby ruby-dev

# Copy over the neovim configuration
COPY . /root/.config/dotfiles

# Define the entry point command.  This is the command that will be executed
# when the container starts.
ENTRYPOINT ["zsh"]
