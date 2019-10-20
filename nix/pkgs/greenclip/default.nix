{ stdenv, fetchurl }:

stdenv.mkDerivation rec {
  name = "greenclip";
  version = "3.2";

  src = fetchurl {
    url = "https://github.com/erebe/${name}/releases/download/${version}/${name}";
    sha256 = "0hy491v75l8w8a24x9485vdlzz404ydfa1r5ljxgw2pvq6b7wi0q";
  };

  phases = [ "installPhase" ];
  installPhase = ''
    mkdir -p $out/bin
    cp $src $out/bin/$name
    chmod +x $out/bin/$name
  '';
}
