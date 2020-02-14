{ mkDerivation, aeson, aeson-pretty, base, pure-txt, stdenv, text
, unordered-containers, vector, bytestring
}:
mkDerivation {
  pname = "pure-json";
  version = "0.8.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson aeson-pretty base pure-txt text unordered-containers vector bytestring
  ];
  homepage = "github.com/grumply/pure-json";
  license = stdenv.lib.licenses.bsd3;
}
