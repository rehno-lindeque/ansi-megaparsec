{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = import ./default.nix;

  haskellOverrides =
    { overrides = self: super: {
        megaparsec = self.callPackage ./megaparsec-7_0_0_dev.nix {};
        hspec-megaparsec = self.callPackage ./hspec-megaparsec-2_0_0_dev.nix {};
        parser-combinators = self.callPackage ./parser-combinators-1_0_0.nix {};
      };
    };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant ((haskellPackages.override haskellOverrides).callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
