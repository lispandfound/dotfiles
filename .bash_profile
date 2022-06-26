# /etc/skel/.bash_profile

# This file is sourced by bash for login shells.  The following line
# runs your .bashrc and is recommended by the bash info pages.
if [[ -f ~/.bashrc ]] ; then
	. ~/.bashrc
fi
export PATH=~/.local/nyxt/usr/local/bin:~/.local/bin:$PATH
export XDG_CONFIG_HOME=$HOME/.config
