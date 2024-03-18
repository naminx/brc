{
  allowBroken = true;
  allowUnfree = true;
  time.timeZone = "Asia/Bangkok";
  settings.experimental-features = [ "nix-command" "flakes" "read-only-local-store" "local-overlay-store" ];
  packageOverrides = pkgs: {
    neovim =
      pkgs.neovim.override
      {
        viAlias = true;
        configure = {
          customRC = pkgs.lib.strings.fileContents ./init.vim;
          packages.myPlugins = with pkgs.vimPlugins; {
            start = [
              coc-json
              coc-nvim
              coc-prettier
              gruvbox
              haskell-vim
              rainbow
              vim-airline
              vim-fish
              vim-lastplace
              vim-nix
              vim-ormolu
            ];
            opt = [];
          };
        };
      };
  };
}
