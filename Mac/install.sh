# Install on Mac


# Install Homebrew
# /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install.sh)"

brew install git ripgrep
brew install coreutils fd
xcode-select --install
brew install python3
brew install git
brew install node
brew install vim
brew install neovim
brew install httpie
brew install fzy 									# Other fuzzy finder
brew install tmuxinator 					# Other terminal
brew install the_silver_searcher 	# Fast search with 'ag'
brew install fzf 									# Fuzzy finder, type '**<TAB>' or just <TAB> to search
brew install ripgrep
brew install cmake
brew install cask mactex

# remote access to filesystems and list filesystem (NeoTree)
brew cask install osxfuse
brew install sshfs

# Install Doom Emacs
# https://github.com/hlissner/doom-emacs/blob/develop/docs/getting_started.org#emacs--dependencies
# Pre-requisites
brew tap d12frosted/emacs-plus
brew install emacs-plus
ln -s /usr/local/opt/emacs-plus/Emacs.app /Applications/Emacs.app
# Doom Emcas
git clone --depth 1 https://github.com/hlissner/doom-emacs ~/.emacs.d
~/.emacs.d/bin/doom install


# Install neovim
curl -LO https://github.com/neovim/neovim/releases/download/nightly/nvim-macos.tar.gz
tar xzf nvim-macos.tar.gz
./nvim-osx64/bin/nvim


# Install YARN
curl -o- -L https://yarnpkg.com/install.sh | bash

# Install NodeJS
curl -sL install-node.now.sh/lts | bash


# Install zsh
brew install zsh
sudo sh -c "echo $(which zsh) >> /etc/shells" && chsh -s $(which zsh)
sudo -s 'echo /usr/local/bin/zsh >> /etc/shells' && chsh -s /usr/local/bin/esh
brew install zsh-completions


# Install oh-my-zsh
sh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"
ZSH=/Users/SagenOS/.oh-my-zsh
sh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"


# Install zsh syntax highlighting
git clone https://github.com/zsh-users/zsh-syntax-highlighting.git
git clone https://github.com/zsh-users/zsh-syntax-highlighting.git ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-syntax-highlighting
echo "source ${(q-)PWD}/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh" >> ${ZDOTDIR:-$HOME}/.zshrc
source ./zsh-syntax-highlighting/zsh-syntax-highlighting.zsh


# Install zsh autocomplete / suggestions
git clone https://github.com/zsh-users/zsh-autosuggestions ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-autosuggestions
git clone https://github.com/zsh-users/zsh-autosuggestions ~/.zsh/zsh-autosuggestions
source ~/.zsh/zsh-autosuggestions/zsh-autosuggestions.zsh


# Install The zsh theme
curl -L git.io/antigen > ~/antigen.zsh
git clone https://github.com/denysdovhan/spaceship-prompt.git "$ZSH_CUSTOM/themes/spaceship-prompt"
ln -s "$ZSH_CUSTOM/themes/spaceship-prompt/spaceship.zsh-theme" "$ZSH_CUSTOM/themes/spaceship.zsh-theme"


# Install required fonts for zsh themes
# clone
git clone https://github.com/powerline/fonts.git --depth=1
cd fonts
./install.sh
cd ..
rm -rf fonts
source ~/.zshrc
# MAKE SURE TO GO IN AND CHANGE IN iTERM2:
#	Preferences -> Profiles -> Font -> Powerline



# Install Anaconda
curl -O https://repo.anaconda.com/archive/Anaconda3-2020.02-MacOSX-x86_64.pkg
# Now install manually or by:
sudo installer -pkg Anaconda3-2020.02-MacOSX-x86_64.pkg -target /



# Install Java TODO



# Install iTerm2
curl -O https://iterm2.com/downloads/stable/iTerm2-3_2_7.zip
unzip iTerm2-3_2_7.zip
mv iTerm.app/ Applications/
rm iTerm2-3_2_7*
spctl --add /Applications/iTerm.app/
nohup open /Applications/iTerm.app/ &>/dev/null &


