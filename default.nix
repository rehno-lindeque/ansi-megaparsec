{ mkDerivation, base, megaparsec, stdenv, text }:
mkDerivation {
  pname = "ansi-megaparsec";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base megaparsec text ];
  homepage = "https://github.com/rehno-lindeque/megaparsec-ansi-code";
  description = "Parsers for ANSI escape sequences";
  license = stdenv.lib.licenses.bsd3;
}
