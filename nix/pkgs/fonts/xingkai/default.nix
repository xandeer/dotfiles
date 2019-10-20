{ stdenv }:

stdenv.mkDerivation {
  name = "xingkai";
  src = ./xingkai.tar.gz; # CloudKaiXingGBK
  installPhase = ''
    tar -xzf $src
    mkdir -p $out/share/fonts
    install -m644 *.ttf *.otf $out/share/fonts/
  '';

  meta = {
    description = "A chinese font: 锐字云字库行楷体GBK";
    homepage = http://www.fonts.net.cn/font-32435571934.html;
    fontName = "CloudKaiXingGBK";
  };
}
