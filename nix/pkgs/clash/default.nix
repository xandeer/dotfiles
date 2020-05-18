{ stdenv, fetchurl }:

stdenv.mkDerivation rec {
  name = "clash";
  version = "0.20.0";

  # src = fetchurl {
  #   url = "https://github.com/Dreamacro/clash/releases/download/premium/clash-linux-amd64-2020.05.08.gz";
  #   sha256 = "0di6dcxyihjcj0p71hjchlxdfx5fp716j0sbjk73r0vi5k08717f";
  # };
  src = ./clash;

  phases = [ "installPhase" ];
  installPhase = ''
    mkdir -p $out/bin
    cp $src $out/bin/$name
    chmod +x $out/bin/$name
  '';

  meta = {
    description = "clash";
    homepage = http://github.com/Dreamacro/clash;
  };
}
