sudo pacman -Syu
yes | sudo pacman -S neovim 
yes | sudo pacman -S xmonad 
yes | sudo pacman -S python 
yes | sudo pacman -S rust 
yes | sudo pacman -S emacs 
yes | sudo pacman -S git 
yas | sudo pacman -S tree
yes | sudo pacman -S exa 
yes | sudo pacman -S make 
yes | sudo pacman -S ripgrep
yes | sudo pacman -S fd
yes | sudo pacman -S nodejs
yes | sudo pacman -S alacritty
yes | sudo pacman -S gdb valgrind cunit gcc     # C libs 
yes | sudo pacman -S gcc-c++ 

# For the fish shell
yes | sudo pacman -S fish
chsh -s /usr/bin/fish
fish_config colors # change colors to Dracula

# python installations 
python3 -m pip install pynvim black ipython neovim-remote

# Github configs
git config --global user.name "MarkusSagen"
git config --global user.email "markus.john.sagen@gmail.com"
git config --global credential.helper store

# install yarn
curl -o- -L https://yarnpkg.com/install.sh | bash

# install fonts
git clone https://github.com/powerline/fonts.git --depth=1
cd fonts && ./install.sh && cd ..
rm -rf fonts 

# To install yay and remove error: ¨Cannot find fakeroot binary¨
5 | sudo pacman -Sy base-devel     

# Install Dropbox
git clone https://aur.archlinux.org/dropbox.git
cd dropbox && gpg --recv-keys FC918B335044912E
makepkg -s && sudo pacman -U dropbox*.pkg.tar.zst
cd .. && rm -rf dropbox


# Install yay
cd /opt && sudo git clone https://aur.archlinux.org/yay-git.git 
sudo chown -R sagenos:sagenos ./yay-git
cd yay-git && makepkg -si && cd /home/sagenos

# Install packages with yay
yay -S brave-bin



# Window managers
# cd .config
# mkdir bspwm sxhkd
# touch bspwm/bspwmrc sxhkd/sxhkdrc
yes | sudo pacman -S dmenu sxhkd rxvt-unicode bs
