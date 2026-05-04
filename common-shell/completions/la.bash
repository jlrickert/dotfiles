if declare -F _longopt >/dev/null 2>&1; then
    complete -F _longopt la
elif declare -F _filedir >/dev/null 2>&1; then
    _la_fallback() { local cur=${COMP_WORDS[COMP_CWORD]}; _filedir; }
    complete -F _la_fallback la
else
    complete -f la
fi
