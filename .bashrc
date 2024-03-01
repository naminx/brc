# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

# Commands to run in interactive sessions can go here
alias cat='bat'
alias cd=z
alias cp=copy
alias del='rip'
alias dir='lsd -lF --color=auto --icon=never'
alias fe='vi (fzf --ansi --color=16)'
alias find=fd
alias fzf='fzf --ansi --color=16'
alias grep=rg
alias ls='lsd -F --color=auto --icon=never'
alias md='mkdir'
alias mv=move
alias rd='rip'
alias ren=move
alias rm='rip'
alias rmdir='rip'

copy () {
    if [ -f "${@: -1}" -o -L "${@: -1}" ]; then
        rip "${@: -1}"
    fi
    command cp "$@"
}

move () {
    if [ -f "${@: -1}" -o -L "${@: -1}" ]; then
        rip "${@: -1}"
    fi
    command mv "$@"
}

eval "$(starship init bash)"
eval "$(zoxide init bash)"
