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
ln -s $dir/x11/'xsession' '.xsession'
ln -s $dir/xmobar/'xmobar' '.xmobarrc'
ln -s $dir/xmonad/'xmonad.hs' '.xmonad/xmonad.hs'
ln -s $dir/abcde/'abcde.conf' '.abcde.conf'
ln -s $dir/tinyfugue/'tfaardwolf' '.tfaardwolf'
ln -s $dir/tinyfugue/'tfcybersphere' '.tfcybersphere'
ln -s $dir/tinyfugue/'tfdiscworld' '.tfdiscworld'
ln -s $dir/tinyfugue/'tfgodwars2' '.tfgodwars2'
ln -s $dir/tinyfugue/'tfhellmoo' '.tfhellmoo'
ln -s $dir/tinyfugue/'tflegendsofthejedi' '.tflegendsofthejedi'
ln -s $dir/tinyfugue/'tfrc' '.tfrc'
ln -s $dir/tinyfugue/'tfshadowsofisildur' '.tfshadowsofisildur'
ln -s $dir/beirc/'beirc.lisp' '.beirc.lisp'
ln -s $dir/mercurial/'hgrc' '.hgrc'
ln -s $dir/lisp/'init.lisp' '.init.lisp'
ln -s $dir/sbcl/'sbclrc' '.sbclrc'
ln -s $dir/clisp/'clisprc' '.clisprc'
ln -s $dir/cmucl/'cmucl-init' '.cmucl-init'
ln -s $dir/bash/'bash_logout' '.bash_logout'
ln -s $dir/git/'gitconfig' '.gitconfig'
ln -s $dir/mksh/'mkshrc' '.mkshrc'
ln -s $dir/mksh/'profile' '.profile'
