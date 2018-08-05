# set 256 color terminal
if [ -n "$DISPLAY" -a "$TERM" == "xterm" ]; then
    export TERM=xterm-256color
fi

# custom prompt line
PS1="${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h:\[\033[01;34m\]\w\n\[\033[01;34m\]\$\[\033[00m\] "

# history options
shopt -s histappend
export HISTCONTROL="erasedups"
export HISTFILESIZE=
export HISTSIZE=
export HISTTIMEFORMAT="[%F %T] "

# enable reverse history search (ctrl+s)
stty -ixon

# aliases
alias ll="ls -l -h --color"
alias l="ls -l -h --color"
alias g="grep -inH --color"
alias h="head"
alias t="tail"
alias vi="vim"
alias diff="diff -u"

calc() {  awk "BEGIN{ print $* }" ;}
