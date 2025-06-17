#!/usr/bin/env bash

# Script to build the Neovim Docker image.

set -o errexit  # exit if non-zero status code is returned
set -o nounset  # exit if undefined variable is used
set -o pipefail # exit if no-zero status code is returned in a pipeline

declare NOCOLOR=
[[ -t 1 ]] || NOCOLOR=y
declare RED='[38;2;255;0;0m'
declare GREEN='[38;2;0;255;0m'
declare RESET='[0m'

if [[ -n "$NOCOLOR" ]]; then
	RED=
	GREEN=
	RESET=
fi

SCRIPT_DIR="$(dirname "$(readlink -f "$0")")"
cd "${SCRIPT_DIR}"

# Define the Docker image name.
IMAGE_NAME="dvim"

# Define the Dockerfile path.
DOCKERFILE_PATH="./Dockerfile"

# Check if Docker is installed.
if ! command -v docker &>/dev/null; then
	echo "${RED}Error: Docker is not installed. Please install Docker.${RESET}"
	exit 1
fi

# Check if the Dockerfile exists.
if [ ! -f "${DOCKERFILE_PATH}" ]; then
	echo "${RED}Error: Dockerfile not found at: ${DOCKERFILE_PATH}${RESET}"
	exit 1
fi

# Build the Docker image.
echo "${GREEN}Building Docker image: ${IMAGE_NAME}${RESET}"
if ! docker build -t "${IMAGE_NAME}" -f "${DOCKERFILE_PATH}" .; then
	:
fi

echo "${GREEN}Docker image built successfully: ${IMAGE_NAME}${RESET}"
