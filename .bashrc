# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

# Commands to run in interactive sessions can go here
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

eval "$(starship init bash)"
eval "$(zoxide init bash)"
