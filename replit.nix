{pkgs}: let
  myHaskellPackages = pkgs.haskellPackages.override {
    overrides = self: super:
      with pkgs.haskell.lib; {
#       script-monad = dontCheck super.script-monad;
#       webdriver-w3c = dontCheck super.webdriver-w3c;
      };
  };
in {
  deps = with pkgs; [
    alejandra
    fish
    less
    neovim
    nodejs
    sqlite
    stack
    unzip
    zip
    starship
    lsd
    bat
    # Haskell
    cabal-install
    haskell-language-server
    # (haskellPackages.ghcWithPackages (pkgs:
    (myHaskellPackages.ghcWithPackages (pkgs:
      with pkgs; [
        # Custom packages
        extra
        formatting
        fourmolu
        lens
        optparse-applicative
        optparse-simple
        non-empty-text
        pathtype
        pretty-simple
        raw-strings-qq
        rawstring-qm
        regex-tdfa
        rio
        string-conversions
        text-builder
      ]))
  ];
}
