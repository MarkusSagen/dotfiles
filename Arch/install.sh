#!/bin/bash

packman -Sy		# Initialize Packman
Yes | pacman -S vim   # vim
Yes | pacman -S git   # git
Yes | pacman -S tmux  # tmux
Yes | pacman -S code  # VS cpde
Yes | pacman -S zsh   # Zsh shell

echo ''
# Install zsh syntax highlighting and auto complete
git clone https://github.com/zsh-users/zsh-syntax-highlighting.git ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-syntax-highlighting

git clone https://github.com/zsh-users/zsh-autosuggestions ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-autosuggestions

# Set zsh theme
git clone https://github.com/denysdovhan/spaceship-prompt.git "$ZSH_CUSTOM/themes/spaceship-prompt"

ln -s "$ZSH_CUSTOM/themes/spaceship-prompt/spaceship.zsh-theme" "$ZSH_CUSTOM/themes/spaceship.zsh-theme"

# Set zsh as the default shell script
chsh -s /bin/zsh root
chsh -s /bin/zsh sagen
chsh -s $(which zsh)

# Set up so github wont ask for username and password
git config --global user.name "MarkusSagen"
git config --global user.email "markus.john.sagen@gmail.com"
git config --global credential.helper store


# End of commands
echo ''

