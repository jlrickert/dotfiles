if [ -d "$HOME/.deno/bin" ]; then
    case ":$PATH:" in
        *":$HOME/.deno/bin:"*) ;;
        *) PATH="$HOME/.deno/bin:$PATH" ;;
    esac
    export PATH
    export DENO_INSTALL="$HOME/.deno"
fi
