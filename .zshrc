# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
setopt autocd extendedglob notify
bindkey -e
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename '/home/jake/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall
source <(/usr/bin/starship init zsh --print-full-init)
source ~/.zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
export PATH=~/.ghcup/bin:~/.emacs.d/bin/:~/.local/bin:$PATH
export EDITOR="/usr/bin/emacsclient -c"
