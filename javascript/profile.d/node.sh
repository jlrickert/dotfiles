if [ -d "$HOME/.local/share/fnm" ]; then
    case ":$PATH:" in
        *":$HOME/.local/share/fnm:"*) ;;
        *) PATH="$HOME/.local/share/fnm:$PATH" ;;
    esac
    export PATH
    if command -v fnm >/dev/null 2>&1; then
        eval "$(fnm env --use-on-cd 2>/dev/null)"
    fi
fi
