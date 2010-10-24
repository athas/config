#!/bin/sh
if [ "$HOME" != "$(pwd)" ]; then
echo Run the script from your home directory.
exit 1
fi
dir=${XDG_CONFIG_HOME:-~/.config}
ln -s $dir/emacs/'emacs' '.emacs'
ln -s $dir/bash/'bashrc' '.bashrc'
ln -s $dir/zsh/'zshenv' '.zshenv'
ln -s $dir/zsh/'zshrc' '.zshrc'
ln -s $dir/zsh/'zprofile' '.zprofile'
ln -s $dir/x11/'Xdefaults' '.Xdefaults'
ln -s $dir/x11/'xinitrc' '.xinitrc'
ln -s $dir/xmobar/'xmobar' '.xmobarrc'
ln -s $dir/xmonad/'xmonad.hs' '.xmonad/xmonad.hs'
