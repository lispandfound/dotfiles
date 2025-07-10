=# Initialize zoxide
eval "$(zoxide init zsh --cmd cd)"

# Initialize fzf
if command -v fzf &> /dev/null; then
    eval "$(fzf --zsh)"
    export FZF_DEFAULT_OPTS=" \
	--color=bg+:#CCD0DA,bg:#EFF1F5,spinner:#DC8A78,hl:#D20F39 \
	--color=fg:#4C4F69,header:#D20F39,info:#8839EF,pointer:#DC8A78 \
	--color=marker:#7287FD,fg+:#4C4F69,prompt:#8839EF,hl+:#D20F39 \
	--color=selected-bg:#BCC0CC \
	--color=border:#CCD0DA,label:#4C4F69"
fi


# Shell aliases
alias cat="bat"
alias less="bat --paging=always"

# Enable eza integration
if command -v eza &> /dev/null; then
    alias ls="eza --icons=auto"
    alias ll="eza --icons=auto -l"
    alias la="eza --icons=auto -la"
    alias lt="eza --icons=auto --tree"
fi


eval "$(starship init zsh)"

# Completion

autoload -Uz compinit
compinit

zstyle ':completion::complete:*' use-cache on
zstyle ':completion::complete:*' cache-path ~/.zsh/cache

# menu and group completion
zstyle ':completion:*' menu select
zstyle ':completion:*' group-name ''

# show descriptions for options and arguments
zstyle ':completion:*:descriptions' format '%B%d%b'

# ignore case
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}' 'r:|=*' 'l:|=* r:|=*'

# complete even if prefix doesn't fully match
zstyle ':completion:*' completer _extensions _complete _approximate

# list directories first
zstyle ':completion:*' list-dirs-first true
