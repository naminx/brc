if status is-interactive

    # Commands to run in interactive sessions can go here
    alias cat='bat'
    alias copy='cp -i'
    alias cp='cp -i'
    alias del='rm -i'
    alias dir='lsd -lF --color=auto --icon=never --group-directories-first'
    alias egrep='egrep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias grep='grep --color=auto'
    alias ls='lsd -F --color=auto --icon=never --group-directories-first'
    alias md='mkdir'
    alias move='mv -i'
    alias mv='mv -i'
    alias rd='rmdir'
    alias ren='mv -i'
    alias rm='rm -i'

    starship init fish | source
end
