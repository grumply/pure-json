{ mkDerivation, aeson, aeson-pretty, base, pure-txt, stdenv, text
, unordered-containers, vector
}:
mkDerivation {
  pname = "pure-json";
  version = "0.7.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson aeson-pretty base pure-txt text unordered-containers vector
  ];
  homepage = "github.com/grumply/pure-json";
  license = stdenv.lib.licenses.bsd3;
}
