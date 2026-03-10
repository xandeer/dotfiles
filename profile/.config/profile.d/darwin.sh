# macOS-specific shared shell configuration.

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
unset android_studio_java
