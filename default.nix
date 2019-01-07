{ mkDerivation, array, base, bifunctors, cairo, comonad
, contravariant, free, gtk3, JuicyPixels, ListZipper, mtl, pango
, stdenv, text, vector
}:
mkDerivation {
  pname = "disguise";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    array base bifunctors cairo comonad contravariant free gtk3
    JuicyPixels ListZipper mtl pango text vector
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base ];
  homepage = "https://github.com/talanis85/disguise#readme";
  description = "Compositional UI on top of Gtk";
  license = stdenv.lib.licenses.bsd3;
}
