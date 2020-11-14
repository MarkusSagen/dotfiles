



# Download and install new configured TMUX
git clone https://github.com/samoshkin/tmux-config.git ~/
~/tmux-config/install.sh

## install zsh
sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"
#zsh

## zsh witout root access
#export SHELL=bin/zsh
#exec /bin/zsh -l


# Add these settings
#echo "# For remote shell access
#if [[ "$TERM" == "dumb" ]]; then
#  unset zle_bracketed_paste
#  unset zle
#  PS1='$ '
#  return
#fi" >> .zshrc
#echo "# Completion and suggest commands when typing
#plugins=(git zsh-completions zsh-syntax-highlighting zsh-autosuggestions)" >> .zshrc
#echo "# Load shell changes
#source $ZSH/oh-my-zsh.sh" >> .zshrc

## Set zsh as default shell when logging back in
#echo "export SHELL=bin/zsh" >> .bashrc
#echo "exec /bin/zsh -l" >> .bashrc
## Reload and apply the settings
#source .zshrc
#source .bashrc





# oh-my-zsh
sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"

# zsh-autosuggestion
git clone https://github.com/zsh-users/zsh-autosuggestions ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-autosuggestions

# zsh-completion
git clone https://github.com/zsh-users/zsh-completions ${ZSH_CUSTOM:=~/.oh-my-zsh/custom}/plugins/zsh-completions

# zsh-syntax-highlight
git clone https://github.com/zsh-users/zsh-syntax-highlighting.git ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-syntax-highlighting


# nvim
curl -LO https://github.com/neovim/neovim/releases/latest/download/nvim.appimage
cmod u+x nvim.appimage
./nvim.appimage

# Doom emacs
# git clone --depth 1 https://github.com/hlissner/doom-emacs ~/.emacs.d
# ~/.emacs.d/bin/doom install
