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
	bat \
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

RUN add-apt-repository ppa:neovim-ppa/unstable -y && apt-get update
RUN apt-get install -y neovim

RUN curl -sS https://starship.rs/install.sh | sh -s -- -y

ARG APP_USER=jlrickert
ARG APP_GROUP=jlrickert
ARG APP_HOME=/home/${APP_USER}
ARG APP_UID=1000
ARG APP_GID=1000

# Allow user to use sudo without a password
RUN groupadd -r ${APP_GROUP} && useradd -r -g ${APP_GROUP} ${APP_USER} \
	&& echo "${APP_USER} ALL=(ALL) NOPASSWD: ALL" > /etc/sudoers.d/${APP_USER}-nopasswd \
	&& chmod 0440 /etc/sudoers.d/${APP_USER}-nopasswd

ENV LANG=en_US.utf8

# Used to mark that this is a docker container
RUN touch /.dockerenv

COPY ./lib/entrypoint.sh /usr/local/bin/entrypoint.sh
RUN chmod +x /usr/local/bin/entrypoint.sh

USER ${APP_USER}
WORKDIR ${APP_HOME}

COPY --chown=${APP_USER}:${APP_GROUP} . ${APP_HOME}/dotfiles

RUN ./dotfiles/bin/dotsh bootstrap

ENTRYPOINT ["entrypoint.sh"]
