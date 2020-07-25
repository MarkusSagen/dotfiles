

sudo apt update && sudo apt upgrade
sudo apt install sqlite3 fzy texlive-latex-extra
mkdir ~/.nvm
sudo apt install build-essential -y
curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.34.0/install.sh | bash


# golang
wget -c https://dl.google.com/go/go1.14.2.linux-amd64.tar.gz -O - | sudo tar -xz -C /usr/local

# Install Doom Emacs
sudo add-apt-repository ppa:kelleyk/emacs
sudo apt update
sudo apt install emacs26
git clone https://github.com/hlissner/doom-emacs ~/.emacs.d
~/.emacs.d/bin/doom install
~/.emacs.d/bin/doom sync
cp ../.doom.d/* ~/.doom.d/

# Anaconda 
wget https://repo.anaconda.com/miniconda/Miniconda3-latest-Linux-x86_64.sh
bash Miniconda3-latest-Linux-x86_64.sh
conda install -y jupyter
jupyter notebook --generate-config
jupyter notebook password
echo 'alias jn="jupyter notebook --ip=0.0.0.0 -
-port=8000 --no-browser"' >> ~/.bashrc

# Tensorflow
# conda activate /home/sagenos/miniconda3/envs/tensorflow
conda create --name tensorflow python=3.7
conda activate tensorflow
conda install -y nb_conda
conda install -y -c anaconda tensorflow 
conda install -y -c anaconda tensorflow-gpu
conda env update --file tools.yml
python -m ipykernel install --user --name tensorflow --display-name "Python 3.7 (tensorflow)"
conda deactivate

# PyTorch
conda create --name torch python=3.7
conda activate torch
conda install -y nb_conda
conda install pytorch torchvision cudatoolkit=10.1 -y -c pytorch
conda env update --file tools.yml
python -m ipykernel install --user --name torch --display-name "Python 3.7 (torch)"
conda deactivate

# Zsh and Activate oh-my-zsh
sudo apt install zsh
zsh

sh -c "$(curl -fsSL https://raw.github.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"
git clone https://github.com/zsh-users/zsh-syntax-highlighting.git
echo "source ${(q-)PWD}/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh" >> ${ZDOTDIR:-$HOME}/.zshrc
source ./zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

git clone https://github.com/zsh-users/zsh-autosuggestions ~/.oh-my-zsh/custom/plugins/zsh-autosuggestions
source ~/.oh-my-zsh/custom/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh

git clone --depth 1 https://github.com/junegunn/fzf.git ~/.fzf
~/.fzf/install

git clone https://github.com/denysdovhan/spaceship-prompt.git "$ZSH_CUSTOM/themes/spaceship-prompt"
ln -s "$ZSH_CUSTOM/themes/spaceship-prompt/spaceship.zsh-theme" "$ZSH_CUSTOM/themes/spaceship.zsh-theme"

cp .zshrc ~
export PATH=$PATH:/usr/local/go/bin
source ~/.profile
source ~/.zshrc


# Node
nvm install node

# Docker
sudo apt update
sudo apt install apt-transport-https ca-certificates curl software-properties-common
curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo apt-key add -
sudo apt-key fingerprint 0EBFCD88
sudo add-apt-repository "deb [arch=amd64] https://download.docker.com/linux/ubuntu $(lsb_release -cs) stable"
sudo apt update
sudo apt install docker-ce
sudo curl -L "https://github.com/docker/compose/releases/download/1.24.0/docker-compose-$(uname -s)-$(uname -m)" -o /usr/local/bin/docker-compose
sudo chmod +x /usr/local/bin/docker-compose


# yarn
curl -sS https://dl.yarnpkg.com/debian/pubkey.gpg | sudo apt-key add -
echo "deb https://dl.yarnpkg.com/debian/ stable main" | sudo tee /etc/apt/sources.list.d/yarn.list
sudo apt install --no-install-recommends yarn

# Java
wget https://corretto.aws/downloads/latest/amazon-corretto-11-x64-linux-jdk.deb
sudo apt-get update && sudo apt-get install java-common
sudo dpkg --install amazon-corretto-11-x64-linux-jdk.deb
sudo update-alternatives --config java
sudo update-alternatives --config javac
