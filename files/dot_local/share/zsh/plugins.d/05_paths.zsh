# Ensure path arrays do not contain duplicates.
typeset -gU cdpath fpath mailpath path

path=(
  $HOME/.local/bin
  $path[@]
)

# Set the list were completions are stored
fpath=(
  $fpath[@]
)
