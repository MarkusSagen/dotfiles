#!/bin/bash

# Copy files from local changes and push to Github
cp ~/.zshrc Arch
cp ~/.vimrc Arch
cp ~/.tmux.conf Arch
cp ~/install.sh Arch
cp ~/.gitignore Arch
cp ~/.gitconfig Arch


git status
git add -A
git commit -m "Updated dotfiles"
git push


