{ stdenv, fetchurl, unzip }:

stdenv.mkDerivation rec {
  name = "v2ray";
  version = "4.20.0";

  src = fetchurl {
    url = "https://github.com/v2ray/v2ray-core/releases/download/v${version}/${name}-linux-64.zip";
    sha256= "ef2a6748dad7aaa8dba68589f3860f336b7ac63deac73214970855657c8d9e86";
  };

  phases = [ "unpackPhase" "installPhase" ];
  nativeBuildInputs = [ unzip ];
  unpackCmd = "unzip ${src} -d dsit";
  installPhase = ''
    mkdir -p $out/bin $out/etc
    cp v2ctl v2ray geoip.dat geosite.dat $out/bin
    cp config.json $out/etc
  '';

  meta = {
    description = "v2ray";
    homepage = http://gihub.com/v2ray/v2ray-core;
    license = stdenv.lib.licenses.mit;
  };
}
