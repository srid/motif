with (import <nixpkgs> {});
let 
  motif = callPackage ./default.nix {};
in
derivation {
  name = "motif";
  builder = "${bash}/bin/bash";
  args = [ ./build.sh ];
  buildInputs = [ motif.ghc.backend motif.ghcjs.frontend ];
  coreutils = coreutils;
  frontend = motif.ghcjs.frontend;
  backend = motif.ghc.backend;
  system = builtins.currentSystem;
}

