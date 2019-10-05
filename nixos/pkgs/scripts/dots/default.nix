{ stdenv, writeShellScriptBin }:

writeShellScriptBin "dots" ''
  #! ${stdenv.shell}
  case "$1" in
      install)
          make -C $DOTS_DIR install
          ;;
      update)
          make -C $DOTS_DIR update
          ;;
      *)
          echo "== ! dots: missing or invalid argument ! =="
          echo "Try again with: install | update"
          exit 2
  esac

  exit 0
''
