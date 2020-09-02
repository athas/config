# This file is evaluated for all login shells.

if [ ! "$SSH_AGENT_PID" -a ! "$SSH_AUTH_SOCK" ]; then
    eval `ssh-agent -s`
fi

umask 077

# if which dmenu_path_watcher 2>/dev/null; then
#     dmenu_path_watcher &
# fi

export MANPATH="$HOME/.cabal/share/man/:$HOME/.local/share/man/:"

export HOSTTYPE="$(uname -m)"
export COLORTERM=yes
export CC=gcc
export CXX=g++
export MANPAGER=less
export PAGER=less
export EDITOR=emacsclient
export LC_ALL=en_US.UTF-8
export DARCS_EMAIL="athas@sigkill.dk"
export DARCS_EDITOR="emacsclient"
export PREFIX=$HOME/.local

# XDG is useful, but not very many things respect it.
export XDG_CONFIG_HOME=$HOME/.config
export XDG_DATA_HOME=$HOME/.local/share

if [ -d /usr/local/cuda ]; then
    export CPATH=$HOME/.local/include/:/usr/local/cuda/include:$CPATH
    export LIBRARY_PATH=$HOME/.local/lib:/usr/local/cuda/lib64:$LIBRARY_PATH
    export LD_LIBRARY_PATH=$HOME/.local/lib:/usr/local/cuda/lib64/$LD_LIBRARY_PATH
fi

export PATH="$HOME/.cargo/bin:$PATH"

if [ $(hostname) = uhyret ]; then
  export LIBRARY_PATH=/opt/amdgpu-pro/lib64/:$LIBRARY_PATH
  export PATH=$HOME/.texlive2017/bin/x86_64-linux/:$PATH
  export LIBRARY_PATH=/opt/amdgpu-pro/lib/x86_64-linux-gnu/
  export LD_LIBRARY_PATH=/opt/amdgpu-pro/lib/x86_64-linux-gnu/
fi

if [ $(uname) = Darwin ]; then
    PATH=/Users/athas/Library/Python/3.7/bin:$PATH
fi

test -e "${HOME}/.iterm2_shell_integration.bash" && source "${HOME}/.iterm2_shell_integration.bash"

if [ "$BASH_VERSION" ] && [ -f $HOME/.bashrc ]; then
  source $HOME/.bashrc
fi

# Linuxbrew setup.
if [ -d /home/linuxbrew/.linuxbrew ]; then
    LINUXBREW=/home/linuxbrew/.linuxbrew
else
    LINUXBREW=$HOME/.linuxbrew
fi

export PATH="$LINUXBREW/bin:$PATH"
export MANPATH="$LINUXBREW/share/man:$MANPATH"
export INFOPATH="$LINUXBREW/share/info:$INFOPATH"

export PATH=$HOME/go/bin:$PATH

if [ $(hostname) = uhyret ]; then
  export MOZ_ENABLE_WAYLAND=1
fi

# Nix setup.
if [ -e $HOME/.nix-profile/etc/profile.d/nix.sh ]; then . $HOME/.nix-profile/etc/profile.d/nix.sh; fi

# I prefer my own scripts and programs, then those installed by cabal,
# then system commands.
export PATH=$HOME/.smackage/bin:$HOME/.local/bin:$HOME/.cabal/bin:$HOME/andets/:$HOME/scripts/:/usr/local/cuda/bin:/usr/sbin:/sbin:/usr/games:$PATH
if [ -e /Users/athas/.nix-profile/etc/profile.d/nix.sh ]; then . /Users/athas/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer
