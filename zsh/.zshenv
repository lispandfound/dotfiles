# Prepend user tool dirs only when they are missing.  Every `zsh -c' reads this
# file -- Emacs's compile/shell-command, just recipes, git hooks -- and an
# unconditional prepend would put ~/.local/bin (uv's own python, uv) and
# ~/.cargo/bin back ahead of a direnv/nix develop toolchain or a .venv that the
# inherited PATH already ranks first.  Fresh logins get the same order as before.
for _d in "$HOME/.ghcup/bin" "$HOME/.cargo/bin" "$HOME/.local/bin" /opt/gmt/bin; do
  case ":$PATH:" in
    *":$_d:"*) ;;
    *) PATH="$_d:$PATH" ;;
  esac
done
unset _d
export PATH
export SSH_AUTH_SOCK="${XDG_RUNTIME_DIR}/ssh-agent.socket"
export EDITOR=helix
. "$HOME/.cargo/env"
alias scipy="uv run --project ~/src/scratchpad/ run ipython"
