#!/bin/bash

# Arch Linux Setup Script
# Converts Nix home-manager configuration to Arch Linux setup

set -eou pipefail

echo "Setting up development environment on Arch Linux..."

# Update system
sudo pacman -Syu --noconfirm

# Install packages from official repositories
echo "Installing packages from official repositories..."
sudo pacman -S --needed --noconfirm \
    atool \
    bat \
    bottom \
    cargo \
    git \
    git-lfs \
    git-delta \
    eza \
    fd \
    file \
    zsh \
    fzf \
    television \
    gawk \
    gcc \
    github-cli \
    gnupg \
    sed \
    tar \
    jq \
    nodejs \
    p7zip \
    pandoc \
    ripgrep \
    rust \
    stow \
    tldr \
    tree \
    unzip \
    watchexec \
    which \
    xan \
    xz \
    yazi \
    zip \
    zoxide \
    zstd \
    zenith

echo "Installing AUR packages..."
if command -v yay &> /dev/null; then
    AUR_HELPER="yay"
elif command -v paru &> /dev/null; then
    AUR_HELPER="paru"
else
    echo "No AUR helper found. Installing paru..."
    sudo pacman -S --needed --noconfirm base-devel git
    git clone https://aur.archlinux.org/paru.git /tmp/paru
    cd /tmp/paru
    makepkg -si --noconfirm
    cd - > /dev/null
    AUR_HELPER="paru"
fi

$AUR_HELPER -S --needed --noconfirm dtach-ng

# Set up zsh as default shell
echo "Setting up zsh..."
if [[ "$SHELL" != *"zsh"* ]]; then
    echo "Changing default shell to zsh..."
    chsh -s $(which zsh)
fi

# Install grml-zsh-config
echo "Installing grml-zsh-config..."
if [[ ! -f /etc/zsh/zshrc.grml ]]; then
    sudo pacman -S --needed --noconfirm grml-zsh-config
fi

# Create .zshrc configuration
echo "Setting up .zshrc configuration..."
cat > ~/.zshrc << 'EOF'
# Load grml config first
if [[ -f /etc/zsh/zshrc.grml ]]; then
    source /etc/zsh/zshrc.grml
fi

# Initialize zoxide
eval "$(zoxide init zsh --cmd cd)"

# Initialize tv
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
EOF

# Git configuration
git config --global github.user "lispandfound"
git config --global push.default "current"
git config --global pull.rebase false

# Delta configuration
git config --global core.pager delta
git config --global interactive.diffFilter "delta --color-only"
git config --global delta.navigate true
git config --global delta.side-by-side true
