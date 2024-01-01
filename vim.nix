{pkgs}:
with pkgs; let
  fish-vim = vimUtils.buildVimPluginFrom2Nix {
    pname = "fish-vim";
    version = "2023-02-04";
    src = fetchFromGitHub {
      owner = "nickeb96";
      repo = "fish.vim";
      rev = "363a03469d2b774e50089ed38efaf4d3a346b561";
      sha256 = "1lvxr7zm7qkp80r07q59vqnsksp4qzi7cs1bsvrswc0livbf30yf";
    };
  };
  deferred-clipboard-x = vimUtils.buildVimPluginFrom2Nix {
    pname = "deferred-clipboard";
    version = "2023-03-15";
    src = fetchFromGitHub {
      owner = "naminx";
      repo = "deferred-clipboard.nvim";
      rev = "eaad79a6a381048bdbebcd63ec3b1df2f6503f92";
      sha256 = "0jfx84kcvzq7c1265j83xd1732hbckdp37d6x809armfmnqmxnzx";
    };
  };
  myVimPlugins = with vimPlugins; [
    coc-nvim # for haskell language server
    coc-python
    coc-prettier
    coc-json
    fish-vim # fish syntax highlighting
    gruvbox # color scheme close to "Groovy Lambda"
    haskell-vim # haskell syntax highlighting
    rainbow # color parenthesis
    vim-airline # customized status line
    vim-lastplace # remember last position
    vim-nix # nix source file highlight
    vim-ormolu # format haskell source file when saving
    vim-prettier
    # deferred-clipboard
  ];
in
  neovim.override {
    viAlias = true;
    configure = {
      customRC = lib.strings.fileContents ./init.vim;
      packages.myPlugins = with vimPlugins; {
        start = myVimPlugins;
        opt = [];
      };
    };
  }
