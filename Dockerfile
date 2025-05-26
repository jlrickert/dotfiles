# Use the latest Ubuntu image as the base.  This provides a stable and up-to-date environment.
FROM ubuntu:latest

# Set noninteractive mode to avoid prompts during package installation.
ARG DEBIAN_FRONTEND=noninteractive

# Update the package lists and install necessary dependencies. See
# https://github.com/nvim-lua/kickstart.nvim?tab=readme-ov-file#Install-Recipes
# for ubuntu
RUN apt-get update && \
	apt-get install -y --no-install-recommends \
	apt-utils \
	build-essential \
	cmake \
	curl \
	fd-find \
	fish \
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

WORKDIR /root

# Used to mark that this is a docker container
RUN touch /.dockerenv

WORKDIR /root/.config/dotfiles

# Copy over everything needed to boot strap
COPY ./bin /root/.config/dotfiles/bin
COPY ./env /root/.config/dotfiles/env
RUN ./bin/dotsh bootstrap

COPY ./pkg/editor /root/.config/dotfiles/pkg/editor
RUN ./bin/dotsh install editor

COPY ./pkg/go /root/.config/dotfiles/pkg/go
RUN ./bin/dotsh install go

COPY ./pkg/deno /root/.config/dotfiles/pkg/deno
RUN ./bin/dotsh install deno

COPY ./pkg/rust /root/.config/dotfiles/pkg/rust
RUN ./bin/dotsh install rust

COPY ./pkg/shell /root/.config/dotfiles/pkg/shell
RUN ./bin/dotsh install shell

# Copy the rest over. Doing this last for faster incremental builds
COPY . /root/.config/dotfiles

WORKDIR /root

# Define the entry point command.  This is the command that will be executed
# when the container starts.
ENTRYPOINT ["zsh"]
