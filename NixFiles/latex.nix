with import <nixpkgs> {};
stdenv.mkDerivation rec {
  name = "env";
  env = buildEnv { name = name; paths = buildInputs; };
  buildInputs = [
    python36
    dblatex
    vimPlugins.latex-box
    vimPlugins.latex-live-preview
    texlive.combined.scheme-basic
  ];
}

