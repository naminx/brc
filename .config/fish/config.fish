if not status is-interactive
    return
end

# Commands to run in interactive sessions can go here

starship init fish | source
zoxide init fish | source

alias cat='bat'
alias cd='z'
alias copy='command cp -i'
alias cp='command cp -i'
alias del='command rm -i'
alias dir='eza -l'
alias fe='vi (fzf --ansi --color=16)'
alias fzf='fzf --ansi --color=16'
alias ls='eza'
alias md='mkdir'
alias move='command mv -i'
alias mv='command mv -i'
alias rd='rmdir'
alias ren='command mv -i'
alias rm='command rm -i'
alias rmdir='rmdir -i'

set -x CACHIX_AUTH_TOKEN "eyJhbGciOiJIUzI1NiJ9.eyJqdGkiOiI1MGJlYzI3Ni1lNmY2LTQyOTMtYmM0MC01Yzk2NzMzZDllNzAiLCJzY29wZXMiOiJ0eCJ9.mJzOYgW1h0MERQQwH1-RKWMKYdD5tGZxp7Lm-L--fN0"
fish_add_path {$REPL_HOME}/bin