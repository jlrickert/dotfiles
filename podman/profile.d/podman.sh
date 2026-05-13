# Podman Desktop's .dmg installer drops the full bundled toolchain
# (podman, krunkit, gvproxy, vfkit, podman-mac-helper) at /opt/podman/bin.
# Front-of-PATH prepend so the bundle wins over any stray brew install.
# No-op when the directory is absent (linux test runners, hosts without
# the .dmg installed).
if [ -d "/opt/podman/bin" ]; then
    case ":$PATH:" in
        *":/opt/podman/bin:"*) ;;
        *) PATH="/opt/podman/bin:$PATH" ;;
    esac
    export PATH
fi
