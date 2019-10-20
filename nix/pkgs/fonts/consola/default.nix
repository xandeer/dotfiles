{ stdenv }:

stdenv.mkDerivation {
  name = "consola";
  src = ./consola.tar.gz;
  installPhase = ''
    tar -xzf $src
    mkdir -p $out/share/fonts
    install -m644 *.ttf *.otf $out/share/fonts/
  '';
}
