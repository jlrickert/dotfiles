if [ -d "$HOME/.bun/bin" ]; then
    case ":$PATH:" in
        *":$HOME/.bun/bin:"*) ;;
        *) PATH="$HOME/.bun/bin:$PATH" ;;
    esac
    export PATH
    export BUN_INSTALL="$HOME/.bun"
fi
