# Linux-specific shared shell configuration.

SDK_DIR="$HOME/Android/Sdk"
if [ -d "$SDK_DIR" ]; then
  export SDK_DIR

  case ":$PATH:" in
    *":$SDK_DIR/platform-tools:"*)
      ;;
    *)
      if [ -d "$SDK_DIR/platform-tools" ]; then
        PATH="${PATH:+$PATH:}$SDK_DIR/platform-tools"
      fi
      ;;
  esac

  case ":$PATH:" in
    *":$SDK_DIR/tools:"*)
      ;;
    *)
      if [ -d "$SDK_DIR/tools" ]; then
        PATH="${PATH:+$PATH:}$SDK_DIR/tools"
      fi
      ;;
  esac
fi

export PATH
