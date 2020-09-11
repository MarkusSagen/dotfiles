

brew install sqlite3
echo 'export PATH="/usr/local/opt/sqlite/bin:$PATH"' >> ~/.profile
brew install fzf
brew install python3
brew install vim
brew install neovim
brew install httpie
brew install fzy 									# Other fuzzy finder
brew install tmuxinator 					# Other terminal
brew install the_silver_searcher 	# Fast search with 'ag'
brew install fzf 									# Fuzzy finder, type '**<TAB>' or just <TAB> to search
brew install ripgrep
brew install cmake
brew cask install mactex

# iTerm2
brew cask install iterm2


# Emacs and Doom emacs
brew tap d12frosted/emacs-plus
brew install emacs-plus
ln -s /usr/local/opt/emacs-plus/Emacs.app /Applications/Emacs.app
git clone --depth 1 https://github.com/hlissner/doom-emacs ~/.emacs.d
~/.emacs.d/bin/doom install


# Install zsh
brew install zsh
sudo sh -c "echo $(which zsh) >> /etc/shells" && chsh -s $(which zsh)
sudo -s 'echo /usr/local/bin/zsh >> /etc/shells' && chsh -s /usr/local/bin/esh
zsh
0
brew install zsh-completions

# oh-my-zsh
Y | sh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"
ZSH=/Users/SagenOS/.oh-my-zsh

# Syntax highlight
git clone https://github.com/zsh-users/zsh-syntax-highlighting.git ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-syntax-highlighting
echo "source ${(q-)PWD}/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh" >> ${ZDOTDIR:-$HOME}/.zshrc
source ~/.oh-my-zsh/custom/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

# autocomplete zsh
git clone https://github.com/zsh-users/zsh-autosuggestions ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-autosuggestions
git clone https://github.com/zsh-users/zsh-autosuggestions ~/.zsh/zsh-autosuggestions
source ~/.zsh/zsh-autosuggestions/zsh-autosuggestions.zsh

# Install powerline and other fonts
pip3 install powerline-status

#################################
git clone https://github.com/powerline/fonts.git --depth=1
cd fonts
./install.sh
cd ..
rm -rf fonts
#########################
# MAKE SURE TO GO IN AND CHANGE IN iTERM2:
#	       Preferences -> Profiles -> Text -> Font -> Powerline
#########################




# Install YARN
curl -o- -L https://yarnpkg.com/install.sh | bash

# Install NodeJS
curl -sL install-node.now.sh/lts | bash

# Anaconda
curl -O https://repo.anaconda.com/archive/Anaconda3-2020.02-MacOSX-x86_64.pkg
sudo installer -pkg Anaconda3-2020.02-MacOSX-x86_64.pkg -target /


################
# When done,...
# Copy .zshrc and .doom configs to home from dotfiles/

