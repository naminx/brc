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
alias dir='lsd -lF --icon=never'
alias fe='vi (fzf --ansi --color=16)'
alias fzf='fzf --ansi --color=16'
alias ls='command ls -F --color=auto'
alias md='mkdir'
alias move='command mv -i'
alias mv='command mv -i'
alias rd='rmdir'
alias ren='command mv -i'
alias rm='command rm -i'
alias rmdir='rmdir -i'

set PATH $REPL_HOME/bin:$PATH

