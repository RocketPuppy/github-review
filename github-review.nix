{ mkDerivation, base, bytestring, cmdargs, data-default, Diff
, exceptions, filestore, github, stdenv, text, vty, vty-ui
, vector
, buildTools ? [],
}:
mkDerivation {
  pname = "github-review";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base bytestring cmdargs data-default Diff exceptions filestore
    github text vty vty-ui vector
  ];
  buildTools = buildTools;
  description = "Command line tool for reviewing Github pull requests";
  license = stdenv.lib.licenses.unfree;
}
