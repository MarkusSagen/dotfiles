#!/bin/bash

# Copy files from local changes and push to Github
cp ~/.zshrc Arch/.zshrc
cp ~/.vimrc Arch/.vimrc
cp ~/.tmux.conf Arch/.tmux.conf
cp ~/install.sh Arch/install.sh
cp ~/.gitignore Arch/.gitignore
cp ~/.gitconfig Arch/.gitconfig

# To commit the files type for instance:
# > cap "updated files"
#
# cap is the alias for
# -  Commiting
# -  Adding
# -  Pushing



