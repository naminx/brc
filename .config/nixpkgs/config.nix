{
  allowBroken = true;
  allowUnfree = true;
  time.timeZone = "Asia/Bangkok";
  packageOverrides = pkgs: {
    neovim =
      pkgs.neovim.override
      {
        viAlias = true;
        configure = {
          customRC = ''
            set tabstop=2 softtabstop=2 shiftwidth=2 expandtab
            set noautoindent nocindent nosmartindent indentexpr=
            set number colorcolumn=80
            highlight ExtraWhitespace ctermbg=red guibg=red
            match ExtraWhitespace /\s\+$/
            autocmd BufWinEnter * match ExtraWhitespace /\s\+$/
            autocmd InsertEnter * match ExtraWhitespace /\s\+\%#\@<!$/
            autocmd InsertLeave * match ExtraWhitespace /\s\+$/
            set termguicolors
            colorscheme gruvbox
            let g:ormolu_command="fourmolu"
            let g:ormolu_suppress_stderr=1
            let g:ormolu_options=["--no-cabal"]
            let g:rainbow_active=1
            nnoremap <silent><nowait><expr> <C-f> coc#float#has_scroll() ? coc#float#scroll(1,1) : "\<C-f>"
            nnoremap <silent><nowait><expr> <C-b> coc#float#has_scroll() ? coc#float#scroll(0,1) : "\<C-b>"
            inoremap <silent><nowait><expr> <C-f> coc#float#has_scroll() ? "\<c-r>=coc#float#scroll(1,1)\<cr>" : "\<Right>"
            inoremap <silent><nowait><expr> <C-b> coc#float#has_scroll() ? "\<c-r>=coc#float#scroll(0,1)\<cr>" : "\<Left>"
            vnoremap <silent><nowait><expr> <C-f> coc#float#has_scroll() ? coc#float#scroll(1,1) : "\<C-f>"
            vnoremap <silent><nowait><expr> <C-b> coc#float#has_scroll() ? coc#float#scroll(0,1) : "\<C-b>"
          '';
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