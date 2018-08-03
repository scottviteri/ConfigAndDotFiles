with import <nixpkgs> {};
stdenv.mkDerivation rec {
  name = "env";
  env = buildEnv { name = name; paths = buildInputs; };
  buildInputs = [
    python
    python27Packages.virtualenv
    python27Packages.pip
    go_1_4
    lua5_3
  ];
}

