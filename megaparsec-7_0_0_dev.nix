{ mkDerivation, base, bytestring, case-insensitive, containers
, criterion, deepseq, fetchgit, hspec, hspec-expectations, mtl
, parser-combinators, QuickCheck, scientific, stdenv, text
, transformers, weigh
}:
mkDerivation {
  pname = "megaparsec";
  version = "7.0.0-dev";
  src = fetchgit {
    url = "https://github.com/mrkkrp/megaparsec.git";
    sha256 = "1y3xjyyr03kl25l6d1i484hzfmdk3scwpbrd14vay5pmqxjwa925";
    rev = "31ce453837f463abdecfe27c272675a851358826";
  };
  prePatch = "sed -i 's|6\.3\.0|7.0.0|' megaparsec.cabal";
  libraryHaskellDepends = [
    base bytestring case-insensitive containers deepseq mtl
    parser-combinators scientific text transformers
  ];
  testHaskellDepends = [
    base bytestring case-insensitive containers hspec
    hspec-expectations mtl parser-combinators QuickCheck scientific
    text transformers
  ];
  benchmarkHaskellDepends = [
    base containers criterion deepseq text weigh
  ];
  homepage = "https://github.com/mrkkrp/megaparsec";
  description = "Monadic parser combinators";
  license = stdenv.lib.licenses.bsd2;
}
