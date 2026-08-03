export PATH="/opt/gmt/bin:$HOME/.local/bin:$HOME/.cargo/bin:$HOME/.ghcup/bin:$PATH"
export SSH_AUTH_SOCK="${XDG_RUNTIME_DIR}/ssh-agent.socket"
export EDITOR=helix
. "$HOME/.cargo/env"
alias scipy="uv run --project ~/src/scratchpad/ run ipython"
