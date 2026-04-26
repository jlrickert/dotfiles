ARG DOTS_VERSION=0.4.0

FROM ubuntu:24.04

ARG DOTS_VERSION
# TARGETARCH is auto-injected by BuildKit (amd64, arm64, ...). Leave it
# undefaulted here so the auto value isn't shadowed; fall back to dpkg below.
ARG TARGETARCH

ENV DEBIAN_FRONTEND=noninteractive \
    TZ=America/Chicago

# tzdata reads $TZ during postinst under noninteractive frontend, so it
# configures /etc/localtime and /etc/timezone without dpkg-reconfigure.
RUN apt-get update && apt-get install -y --no-install-recommends \
        bash \
        build-essential \
        ca-certificates \
        curl \
        git \
        locales \
        sudo \
        tzdata \
        zsh \
    && rm -rf /var/lib/apt/lists/* \
    && locale-gen en_US.UTF-8

ENV LANG=en_US.UTF-8 \
    LC_ALL=en_US.UTF-8

RUN set -eux; \
    arch="${TARGETARCH:-$(dpkg --print-architecture)}"; \
    tmp="$(mktemp -d)"; \
    curl -fsSL "https://github.com/jlrickert/dots/releases/download/v${DOTS_VERSION}/dots_${DOTS_VERSION}_linux_${arch}.tar.gz" -o "${tmp}/dots.tgz"; \
    tar -xzf "${tmp}/dots.tgz" -C "${tmp}"; \
    install -m 0755 "${tmp}/dots" /usr/local/bin/dots; \
    rm -rf "${tmp}"; \
    dots --version

RUN useradd --create-home --shell /usr/bin/zsh jlrickert \
    && echo 'jlrickert ALL=(ALL) NOPASSWD:ALL' > /etc/sudoers.d/jlrickert \
    && chmod 0440 /etc/sudoers.d/jlrickert

COPY --chown=jlrickert:jlrickert . /opt/dotfiles-src

USER jlrickert
WORKDIR /home/jlrickert
# dots invokes hook scripts via $SHELL (else /bin/sh). On Ubuntu /bin/sh is
# dash, which chokes on the bash-isms in this repo's install hooks. Pick bash
# explicitly for the install step.
ENV SHELL=/bin/bash

# `dots init --from` registers the source as a tap (named via --name) and
# installs the package given by --path (here: dots-config). `requires:` is
# parsed but not auto-resolved by dots 0.4.0, so the rest install in
# dependency order.
RUN dots init --from "file:///opt/dotfiles-src" --path dots-config --name jlrickert \
    && dots install jlrickert/common-shell \
    && dots install jlrickert/bash \
    && dots install jlrickert/zsh

ENTRYPOINT ["zsh", "-l"]
