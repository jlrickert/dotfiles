FROM ubuntu:22.04

# Set the default username
ENV USERNAME jlrickert
ARG USER_UID=1000
ARG USER_GID=1000

RUN apt-get update -y && apt-get install -y \
        apt-transport-https \
        apt-utils \
        atool \
        bash-completion \
        bc \
        bind9-dnsutils \
        btop \
        build-essential \
        ca-certificates \
        cifs-utils \
        curl \
        dialog \
        direnv \
        ed \
        fd-find \
        figlet \
        fzf \
        gh \
        git \
        htop \
        httpie \
        iputils-ping \
        jq \
        less \
        libcurses-perl \
        libncurses5 \
        lolcat \
        lynx \
        make \
        man-db \
        ncal \
        net-tools \
        nmap \
        perl \
        pgformatter \
        python3 \
        python3-argcomplete \
        python3-pip \
        ranger \
        ripgrep \
        rsync \
        shellcheck \
        shfmt \
        silversearcher-ag \
        smbclient \
        software-properties-common \
        ssh \
        sshfs \
        sshpass \
        stow \
        sudo \
        tig \
        tmux \
        tree \
        unzip \
        vim \
        wget \
        xclip \
        zip

# Create a non-root user with the specified UID and GID
RUN groupadd --gid "${USER_GID}" "${USERNAME}" \
    && useradd --uid "${USER_UID}" --gid "${USER_GID}" -m -s /bin/bash "${USERNAME}" \
    && echo "$USERNAME ALL=(ALL) NOPASSWD:ALL" >> /etc/sudoers

# Switch to the non-root user
USER "${USERNAME}"

COPY . "/home/${USERNAME}/.local/share/chezmoi"

RUN sh -c "$(curl -fsLS get.chezmoi.io)" -- -b $HOME/.local/bin
RUN sh $HOME/.local/bin/chezmoid apply

# Set the working directory
WORKDIR "/home/${USERNAME}"

# Set any additional instructions or configurations for your application
# ...

# Define the default command or entrypoint
CMD ["/bin/bash"]
