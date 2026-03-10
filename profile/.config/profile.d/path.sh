# Shared PATH setup belongs here.

case ":$PATH:" in
  *":$HOME/projects/others/doom/bin:"*)
    ;;
  *)
    if [ -d "$HOME/projects/others/doom/bin" ]; then
      PATH="${PATH:+$PATH:}$HOME/projects/others/doom/bin"
    fi
    ;;
esac

case ":$PATH:" in
  *":$HOME/bin:"*)
    ;;
  *)
    if [ -d "$HOME/bin" ]; then
      PATH="$HOME/bin${PATH:+:$PATH}"
    fi
    ;;
esac

export PATH
