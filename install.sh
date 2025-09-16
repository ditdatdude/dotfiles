#!/bin/bash
# Simple dotfiles installer

# Backup existing files
mkdir -p ~/.dotfiles_backup

# Create symlinks for each dotfile
for file in .bashrc .vimrc .gitconfig .zshrc; do
    if [ -f "$HOME/dotfiles/$file" ]; then
        # Backup original if it exists
        if [ -f "$HOME/$file" ]; then
            mv "$HOME/$file" "$HOME/.dotfiles_backup/"
        fi
        # Create symlink
        ln -sf "$HOME/dotfiles/$file" "$HOME/$file"
        echo "Linked $file"
    fi
done
