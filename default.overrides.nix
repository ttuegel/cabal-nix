self: super:
let lib = self.haskell.lib; in
{
  haskellPackages = super.haskellPackages.override (args: {
    overrides = self: super:
      (args.overrides or (self: super: super)) self super // {
        hnix =
          lib.dontCheck  # Tests do not run in Nix builder
          (self.callPackage ./hnix.nix {});
        these =
          lib.dontCheck  # Speculative upper bound on `tasty'
          super.these;
        ref-tf =
          lib.doJailbreak  # Speculative upper bound on `stm'
          super.ref-tf;
      };
  });
}
