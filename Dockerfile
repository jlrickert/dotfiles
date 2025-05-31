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
	sudo \
	unzip \
	wget \
	xclip \
	zsh

RUN rm -rf /var/lib/apt/lists/* && \
	localedef -i en_US -c -f UTF-8 -A /usr/share/locale/locale.alias en_US.UTF-8

ARG APP_USER=jlrickert
ARG APP_GROUP=jlrickert
ARG APP_HOME=/home/${APP_USER}
ARG APP_UID=1000
ARG APP_GID=1000

RUN groupadd -r ${APP_GROUP} && useradd -r -g ${APP_GROUP} ${APP_USER} \
	&& echo "${APP_USER} ALL=(ALL) NOPASSWD: ALL" > /etc/sudoers.d/${APP_USER}-nopasswd \
	&& chmod 0440 /etc/sudoers.d/${APP_USER}-nopasswd

# RUN groupadd -r -g ${APP_GID} ${APP_GROUP} && useradd -r -u ${APP_UID} -g ${APP_GROUP} ${APP_USER} \
#     && echo "${APP_USER} ALL=(ALL) NOPASSWD: ALL" > /etc/sudoers.d/${APP_USER}-nopasswd \
#     && chmod 0440 /etc/sudoers.d/${APP_USER}-nopasswd

ENV LANG=en_US.utf8

# Used to mark that this is a docker container
RUN touch /.dockerenv

USER ${APP_USER}
WORKDIR ${APP_HOME}/dotfiles

# Copy over everything needed to boot strap
# COPY --chown=${APP_USER}:${APP_GROUP} . ${APP_HOME}/dotfiles
COPY --chown=${APP_USER}:${APP_GROUP} . ${APP_HOME}/dotfiles
RUN ./bin/dotsh bootstrap

RUN ./bin/dotsh install shell
RUN ./bin/dotsh install editor
RUN ./bin/dotsh install go
RUN ./bin/dotsh install deno
RUN ./bin/dotsh install rust
RUN ./bin/dotsh install knut

# Copy the rest over. Doing this last for faster incremental builds
COPY --chown=${APP_USER}:${APP_GROUP} . ${APP_HOME}/dotfiles

# Define the entry point command.  This is the command that will be executed
# when the container starts.
ENTRYPOINT ["zsh"]
