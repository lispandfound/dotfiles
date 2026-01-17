#!/bin/bash

# Arch Linux Setup Script
# Idempotent setup for dotfiles and programs

set -eou pipefail

echo "Setting up development environment on Arch Linux..."

# Update system
echo "Updating system packages..."
sudo pacman -Syu --noconfirm

# Install packages from official repositories
echo "Installing packages from official repositories..."
sudo pacman -S --needed --noconfirm \
    atool \
    bat \
    bottom \
    cargo \
    emacs \
    eza \
    fd \
    file \
    fzf \
    gawk \
    gcc \
    git \
    git-delta \
    git-lfs \
    github-cli \
    gnupg \
    helix \
    imagemagick \
    jq \
    nodejs \
    openssh \
    p7zip \
    pandoc \
    python \
    python-pip \
    ripgrep \
    rsync \
    sed \
    starship \
    stow \
    tar \
    tectonic \
    television \
    tldr \
    tree \
    unzip \
    watchexec \
    wget \
    which \
    xan \
    xz \
    yazi \
    zenith \
    zip \
    zoxide \
    zsh \
    zstd

# Install AUR packages
echo "Installing AUR packages..."
if command -v yay &> /dev/null; then
    AUR_HELPER="yay"
elif command -v paru &> /dev/null; then
    AUR_HELPER="paru"
else
    echo "No AUR helper found. Installing paru..."
    sudo pacman -S --needed --noconfirm base-devel
    git clone https://aur.archlinux.org/paru.git /tmp/paru
    cd /tmp/paru
    makepkg -si --noconfirm
    cd - > /dev/null
    AUR_HELPER="paru"
fi

# Install AUR packages if AUR helper is available
$AUR_HELPER -S --needed --noconfirm \
    dtach-ng

# Install Python tools via cargo/pip
echo "Installing Python package manager (uv)..."
if ! command -v uv &> /dev/null; then
    # Ensure cargo is available
    if command -v cargo &> /dev/null; then
        cargo install uv
    else
        echo "Warning: cargo not found, skipping uv installation. Please install cargo first."
    fi
fi

# Set up zsh as default shell
echo "Setting up zsh..."
if [[ "$SHELL" != *"zsh"* ]]; then
    echo "Changing default shell to zsh..."
    chsh -s "$(which zsh)"
fi

# Install grml-zsh-config if not present
echo "Checking for grml-zsh-config..."
if [[ ! -f /etc/zsh/zshrc.grml ]]; then
    sudo pacman -S --needed --noconfirm grml-zsh-config
fi

# Use stow to symlink dotfiles
echo "Staging dotfiles with stow..."
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Stow zsh configuration
if [[ -d "$SCRIPT_DIR/zsh" ]]; then
    echo "Staging zsh configuration..."
    stow -d "$SCRIPT_DIR" -t "$HOME" zsh
fi

# Stow config directory (.config)
if [[ -d "$SCRIPT_DIR/config" ]]; then
    echo "Staging config files..."
    # Stow each config subdirectory individually to ~/.config
    for config_dir in "$SCRIPT_DIR/config"/*; do
        if [[ -d "$config_dir" ]]; then
            config_name=$(basename "$config_dir")
            echo "  - Staging $config_name..."
            stow -d "$SCRIPT_DIR/config" -t "$HOME/.config" "$config_name"
        fi
    done
fi

# Git configuration
echo "Configuring git..."
git config --global github.user "lispandfound"
git config --global push.default "current"
git config --global pull.rebase false

# Delta configuration
git config --global core.pager delta
git config --global interactive.diffFilter "delta --color-only"
git config --global delta.navigate true
git config --global delta.side-by-side true

echo ""
echo "============================================"
echo "Setup complete!"
echo "============================================"
echo ""
echo "Dotfiles have been staged with stow."
echo "Please restart your shell or run: exec zsh"
echo ""
