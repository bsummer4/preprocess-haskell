with import <nixpkgs> {};
let haskellPackages = pkgs.haskellPackages.override {
      extension = self: super: {
        preprocessHaskell = self.callPackage ./. {};
      };
    };
 in lib.overrideDerivation haskellPackages.preprocessHaskell (attrs: {
      buildInputs = [ haskellPackages.cabalInstall ] ++ attrs.buildInputs;
    })
