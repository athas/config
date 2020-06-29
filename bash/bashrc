[ -f /etc/bashrc ] && source /etc/bashrc

export red=$'\e[0;31m'
export RED=$'\e[1;31m'
export green=$'\e[0;32m'
export GREEN=$'\e[1;32m'
export blue=$'\e[0;34m'
export BLUE=$'\e[1;34m'
export purple=$'\e[0;35m'
export PURPLE=$'\e[1;35m'
export cyan=$'\e[0;36m'
export CYAN=$'\e[1;36m'
export white=$'\e[0;37m'
export WHITE=$'\e[1;37m'
export yellow=$'\e[0;33m'
export YELLOW=$'\e[01;33m'
export NC=$'\e[0m'

# Work around old terminfo data.
if [ "$TERM" = alacritty ]; then
    TERM=xterm-256color
fi

if [ "$TERM" ]; then
    reset=$(tput sgr0 2>/dev/null)
fi

PS1='$(exit_code=$?; [[ $exit_code -eq 0 ]] || printf $exit_code)[\[\033[00;31m\]\h\[\033[00m\]]\[\033[00;32m\]\W\[\033[00m\]>\[\033[01;33m\]\$ \[\033[00m\]'

#Aliases...

if [ $(uname) = Darwin ]; then
    alias e="emacsclient -n --"
else
    alias e="TMPDIR=/tmp emacsclient -n --"
fi
alias playmidi="aplaymidi -p 73:0"
alias halt="sudo /sbin/halt"
alias p3="play123"
alias nano="nano -w"
alias clim="/usr/bin/sbcl --eval \"(run-clim-desktop)\""
alias murder="killall -9"
if ls --version 2>&1 | grep -q GNU; then
   alias ls="ls --color=auto"
fi

if [ $(uname) = Darwin ]; then
  alias lldb='PATH=/usr/bin:$PATH lldb'
  PATH="/Users/athas/perl5/bin${PATH:+:${PATH}}"; export PATH;
  PERL5LIB="/Users/athas/perl5/lib/perl5${PERL5LIB:+:${PERL5LIB}}"; export PERL5LIB;
  PERL_LOCAL_LIB_ROOT="/Users/athas/perl5${PERL_LOCAL_LIB_ROOT:+:${PERL_LOCAL_LIB_ROOT}}"; export PERL_LOCAL_LIB_ROOT;
  PERL_MB_OPT="--install_base \"/Users/athas/perl5\""; export PERL_MB_OPT;
  PERL_MM_OPT="INSTALL_BASE=/Users/athas/perl5"; export PERL_MM_OPT;
fi

export CLICOLOR=1

#Other stuff

shopt -s histappend
shopt -s checkwinsize
export PROMPT_COMMAND="history -a; history -c; history -r; $PROMPT_COMMAND"

# Disable programmable completion
# complete -r

umask 077
export EDITOR=emacsclient

# No limit on history size
export HISTSIZE=
export HISTFILESIZE=

export HISTCONTROL=ignoredups

alias ..='cd ..'
alias ...='.. && ..'
alias ....='... && ..'
alias .....='.... && ..'
alias ......='..... && ..'
alias .......='...... && ..'
alias alfa='systemctl --user $(systemctl --user is-active alfa.service >/dev/null && echo stop || echo start) alfa.service'

PROMPT_COMMAND='echo -ne "\033]0;$(pwd|sed s!^$HOME!~!)\007"'

# HACK
MONO_PATH=/usr/lib/mono/fsharp/api/.NETFramework/v4.0/4.4.0.0/:$MONO_PATH

# added by travis gem
[ -f /home/athas/.travis/travis.sh ] && source /home/athas/.travis/travis.sh
