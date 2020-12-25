# Install on Mac

# TODO: Update and sync between dotfiles
###################################
# When done, make sure to install 
#    python3.7 and update all packages
#    
#    yes | conda install -c anaconda python3.7
#    yes | conda update --all
#
#   Make sure to add those paths to .config/nvim/init.vim

# Install Homebrew
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install.sh)"

# search files and directories
brew install ack
brew install coreutils fd
xcode-select --install
brew install python3
brew install cmake
brew install git
brew install node
brew install vim
brew install neovim
brew install httpie
brew install fzy 					    # Other fuzzy finder
brew install tmuxinator 	            # Other terminal
brew install fzf 					    # Fuzzy finder, type '**<TAB>' or just <TAB> to search
brew install rg
brew install cmake
brew install cask mactex
brew install wkhtmltopdf
brew cask install amethyst              # window manager
brew install wget
brew install go
brew install direnv                     # Work with different shells on different projects

# remote access to filesystems and list filesystem (NeoTree)
brew cask install osxfuse

# mu4e Mail
brew install mu
# And one of the following
brew install isync  # mbsync
brew install offlineimap

# install python language server for Doom emacs
# Python
pip install pytest
pip install nose
pip install black
pip install pyflakes
pip install isort
pip install python-language-server
pip install pyright


# Doom emacs pre-requisites
brew install emacs-mac --with-modules
brew install libvterm





# Install Doom Emacs
# https://github.com/hlissner/doom-emacs/blob/develop/docs/getting_started.org#emacs--dependencies
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

# install plug
curl -fLo ~/.vim/autoload/plug.vim --create-dirs \
    https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
sh -c 'curl -fLo "${XDG_DATA_HOME:-$HOME/.local/share}"/nvim/site/autoload/plug.vim --create-dirs \
       https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'

# install pynvim
pip3 install pynvim

# Install YARN
curl -o- -L https://yarnpkg.com/install.sh | bash

# Install NodeJS
curl -sL install-node.now.sh/lts | bash

# install remove neovim
pip3 install neovim-remote


# Install zsh
brew install zsh
sudo sh -c "echo $(which zsh) >> /etc/shells" && chsh -s $(which zsh)
sudo -s 'echo /usr/local/bin/zsh >> /etc/shells' && chsh -s /usr/local/bin/esh
git clone https://github.com/zsh-users/zsh-completions ${ZSH_CUSTOM:=~/.oh-my-zsh/custom}/plugins/zsh-completions

# Install oh-my-zsh
sh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"


# Install zsh syntax highlighting
git clone https://github.com/zsh-users/zsh-syntax-highlighting.git ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-syntax-highlighting

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

# Add my configs to nvim 
cp .config/nvim/init.vim ~/.config/nvim/
cp .zshrc ~/
cp .config/alacritty/alacritty.yml ~/.config/alacritty/

# Install Toggl
# -O specify filename and path, -P specify path only, 
wget https://toggl-open-source.github.io/toggldesktop/download/macos-stable/ -P ~/Downloads
