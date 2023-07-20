FROM ubuntu:22.04

RUN apt-get update -y && apt-get install -y curl git sudo

# Set the default username
ENV USERNAME jlrickert
ARG USER_UID=1000
ARG USER_GID=1000
ARG BRANCH=main

# Create a non-root user with the specified UID and GID
RUN groupadd --gid "${USER_GID}" "${USERNAME}" \
    && useradd --uid "${USER_UID}" --gid "${USER_GID}" -m -s /bin/zsh "${USERNAME}" \
    && echo "$USERNAME ALL=(ALL) NOPASSWD:ALL" >> /etc/sudoers

# Switch to the non-root user
COPY --chown=${USERNAME} . "/home/${USERNAME}/.local/share/chezmoi"
COPY entrypoint.sh /usr/local/bin/entrypoint.sh

# Set the working directory
WORKDIR "/home/${USERNAME}"

# RUN "/home/${USERNAME}/.local/share/chezmoi/bootstrap"
USER "${USERNAME}"
RUN mkdir -p ~/.config/chezmoi

# Define the default command or entrypoint
ENTRYPOINT ["/usr/local/bin/entrypoint.sh"]
CMD ["/bin/bash"]
