# macOS-specific shared shell configuration.

homebrew_prefix=""
for candidate in /opt/homebrew /usr/local; do
  if [ -x "$candidate/bin/brew" ]; then
    homebrew_prefix="$candidate"
    break
  fi
done

if [ -n "$homebrew_prefix" ]; then
  case ":$PATH:" in
    *":$homebrew_prefix/bin:"*)
      ;;
    *)
      if [ -d "$homebrew_prefix/bin" ]; then
        PATH="$homebrew_prefix/bin${PATH:+:$PATH}"
      fi
      ;;
  esac

  case ":$PATH:" in
    *":$homebrew_prefix/sbin:"*)
      ;;
    *)
      if [ -d "$homebrew_prefix/sbin" ]; then
        PATH="${PATH:+$PATH:}$homebrew_prefix/sbin"
      fi
      ;;
  esac
fi

SDK_DIR="$HOME/Library/Android/sdk"
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

android_studio_java="/Applications/Android Studio.app/Contents/jre/Contents/Home"
if [ -d "$android_studio_java" ]; then
  export JAVA_HOME="$android_studio_java"
fi

export PATH
unset homebrew_prefix
unset candidate
unset android_studio_java
