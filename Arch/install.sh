#!/bin/bash

packman -Sy		# Initialize Packman
pacman -S vim --noconfirm  # vim
pacman -S git --noconfirm  # git
pacman -S tmux --noconfirm # tmux
pacman -S code --noconfirm # VS cpde
pacman -S zsh  --noconfirm # Zsh shell
pacman -S discord --noconfirm

# Download Zoom.us
wget https://zoom.us/client/latest/zoom_x86_64.pkg.tar.xz

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

