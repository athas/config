# Bash is annoying and only reads .bashrc from a shell that is
# interactive and non-login.
#
# This is the bash_profile, read by all login shells.  It will read my
# normal .profile, and then .bashrc.

if [ -e /home/athas/.nix-profile/etc/profile.d/nix.sh ]; then . /home/athas/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer
source $HOME/.profile
source $HOME/.bashrc

# opam configuration
test -r /home/athas/.opam/opam-init/init.sh && . /home/athas/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true
