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

# initialize tv
if command -v tv &> /dev/null; then
    eval "$(tv init zsh)"
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
