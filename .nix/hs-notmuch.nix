{ mkDerivation, base, bytestring, c2hs, deepseq, mtl, notmuch
  , profunctors, stdenv, tagged, talloc, text, time, fetchFromGitHub
}:
mkDerivation {
  pname = "notmuch";
  version = "0.2.0.0";
  src = fetchFromGitHub {
    owner = "purebred-mua";
    repo = "hs-notmuch";
    rev = "95b78ed74deb244098e2b623ab1f958307826a08";
    sha256 = "12459gdqq822m392r6aq44pjn3ihzjfwp704mnch6aw9skqll2b6";
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring deepseq mtl profunctors tagged text time
  ];
  librarySystemDepends = [ notmuch talloc ];
  libraryToolDepends = [ c2hs ];
  homepage = "https://github.com/purebred-mua/hs-notmuch";
  description = "Haskell binding to Notmuch, the mail indexer";
  license = stdenv.lib.licenses.gpl3;
}
