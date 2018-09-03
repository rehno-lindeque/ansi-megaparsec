{ mkDerivation, base, containers, fetchgit, hspec
, hspec-expectations, megaparsec, stdenv
}:
mkDerivation {
  pname = "hspec-megaparsec";
  version = "2.0.0";
  src = fetchgit {
    url = "https://github.com/mrkkrp/hspec-megaparsec.git";
    sha256 = "1g3hk77vx539nlzqcnp61v3hfaizbpcb1jzv51j1vy8ajzh7kajr";
    rev = "11d26764e6233f271a3a82145b7f6b3b6a329c05";
  };
  prePatch = "sed -i 's|1\.1\.0|2.0.0|' hspec-megaparsec.cabal";
  libraryHaskellDepends = [
    base containers hspec-expectations megaparsec
  ];
  testHaskellDepends = [ base hspec hspec-expectations megaparsec ];
  homepage = "https://github.com/mrkkrp/hspec-megaparsec";
  description = "Utility functions for testing Megaparsec parsers with Hspec";
  license = stdenv.lib.licenses.bsd3;
}
