{ stdenv }:

stdenv.mkDerivation {
  name = "inconsolata";
  src = ./inconsolata.tar.gz;
  installPhase = ''
    tar -xzf $src
    mkdir -p $out/share/fonts
    install -m644 *.ttf *.otf $out/share/fonts/
  '';
}
