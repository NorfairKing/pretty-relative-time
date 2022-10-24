{ mkDerivation, base, genvalidity, genvalidity-hspec
, genvalidity-time, hspec, lib, QuickCheck, time, validity
, validity-time
}:
mkDerivation {
  pname = "pretty-relative-time";
  version = "0.3.0.0";
  src = ./.;
  libraryHaskellDepends = [ base time validity validity-time ];
  testHaskellDepends = [
    base genvalidity genvalidity-hspec genvalidity-time hspec
    QuickCheck time validity validity-time
  ];
  homepage = "https://github.com/NorfairKing/pretty-relative-time#readme";
  description = "Pretty relative time";
  license = lib.licenses.mit;
}
