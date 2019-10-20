{ stdenv }:

stdenv.mkDerivation {
  name = "doom-icons";
  src = ./doom-icons.tar.gz;
  installPhase = ''
    tar -xzf $src
    mkdir -p $out/share/fonts
    install -m644 *.ttf *.otf $out/share/fonts/
  '';
}
