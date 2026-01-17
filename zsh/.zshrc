# Initialize zoxide
# completion

autoload -U +X compinit && compinit
autoload -U +X bashcompinit && bashcompinit

zstyle ':completion::complete:*' use-cache on
zstyle ':completion::complete:*' cache-path ~/.zsh/cache

# menu and group completion
zstyle ':completion:*' menu select
zstyle ':completion:*' group-name ''

# show descriptions for options and arguments
zstyle ':completion:*:descriptions' format '%b%d%b'

# ignore case
zstyle ':completion:*' matcher-list 'm:{a-z}={a-z}' 'r:|=*' 'l:|=* r:|=*'

# complete even if prefix doesn't fully match
zstyle ':completion:*' completer _extensions _complete _approximate

# list directories first
zstyle ':completion:*' list-dirs-first true

# the following lines were added by compinstall
zstyle :compinstall filename '/home/jake/.zshrc'

eval "$(zoxide init zsh --cmd cd)"
export BAT_THEME="Catppuccin Latte"
mkdir -p ~/.zshrc.d

# initialize fzf
if command -v fzf &> /dev/null; then
    eval "$(fzf --zsh)"
    export FZF_DEFAULT_COMMAND="fd --type f"
    export FZF_CTRL_T_COMMAND="fd"
    export FZF_ALT_C_COMMAND="fd --type d"
    export FZF_CTRL_T_OPTS="
      --walker-skip .git,node_modules,target
      --preview 'bat -n --color=always {}'
      --bind 'ctrl-/:change-preview-window(down|hidden|)'"
    export FZF_ALT_C_OPTS="--preview 'eza -l --icons=auto --colour=always {}'"
    if [ ! -f ~/.zshrc.d/fzf-git.sh ]; then
	wget https://raw.githubusercontent.com/junegunn/fzf-git.sh/refs/heads/main/fzf-git.sh -O ~/.zshrc.d/fzf-git.sh
    fi

fi

if [ -d ~/.zshrc.d ]; then
    for rc in ~/.zshrc.d/*; do
	if [ -f "$rc" ]; then
	    . "$rc"
	fi
    done
fi

# Shell aliases
alias cat="bat"
alias grep="rg"
alias less="bat --paging=always"

if [ $TERM = "xterm-kitty" ]
then alias ssh="kitten ssh"
fi


# Enable eza integration
if command -v eza &> /dev/null; then
    alias ls="eza --icons=auto"
    alias ll="eza --icons=auto -l"
    alias la="eza --icons=auto -la"
    alias lt="eza --icons=auto --tree"
fi


prompt default # required to turn off grml prompt
eval "$(starship init zsh)"

alias hx=helix


HISTFILE=~/.zsh_history
HISTSIZE=1000
SAVEHIST=1000
setopt SHARE_HISTORY
