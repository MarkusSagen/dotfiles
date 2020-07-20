# If you come from bash you might have to change your $PATH.
# export PATH=$HOME/bin:/usr/local/bin:$PATH

# Initialize Anaconda path
# Initialize MySQL
source ~/.bash_profile

# Install Antigen for handling plugins
source ~/antigen.zsh

# Path to your oh-my-zsh installation.
export ZSH="/Users/SagenOS/.oh-my-zsh"

# Set name of the theme to load --- if set to "random", it will
# load a random theme each time oh-my-zsh is loaded, in which case,
# to know which specific one was loaded, run: echo $RANDOM_THEME
# See https://github.com/ohmyzsh/ohmyzsh/wiki/Themes
# ZSH_THEME="robbyrussell"
ZSH_THEME="spaceship"

# Use oh-my-zsh
antigen use oh-my-zsh

# Bundles from the default repo
antigen bundle git
antigen bundle pip
antigen bundle heroku
antigen bundle command-not-found
antigen bundle sindresorhus/pretty-time-zsh # Human readable time
antigen bundle hschne/fzf-git # git checkout completion
antigen bundle RobertAudi/tsm
antigen bundle nviennot/zsh-vim-plugin 	# Open file at line-> vim file.txt:123
antigen bundle sinetoami/web-search 		# Search from terminal
antigen bundle zpm-zsh/title 	# Allow setting terminal header
export PROMPT_TITLE='$USER@$HOST:$PWD'



# Autopair: "{('
antigen bundle hlissner/zsh-autopair

# calculator
antigen bundle arzzen/calc.plugin.zsh

# Fuzzy finder
antigen bundle aperezdc/zsh-fzy

# Syntax highlighting bundle
antigen bundle zsh-users/zsh-syntax-highlighting

# Load theme
# antigen theme robbyrussell
antigen theme denysdovhan/spaceship-prompt

# Apply antigen changes
antigen apply


# Set list of themes to pick from when loading at random
# Setting this variable when ZSH_THEME=random will cause zsh to load
# a theme from this variable instead of looking in ~/.oh-my-zsh/themes/
# If set to an empty array, this variable will have no effect.
# ZSH_THEME_RANDOM_CANDIDATES=( "robbyrussell" "agnoster" )

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion.
# Case-sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to automatically update without prompting.
# DISABLE_UPDATE_PROMPT="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line if pasting URLs and other text is messed up.
# DISABLE_MAGIC_FUNCTIONS=true

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# You can set one of the optional three formats:
# "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# or set a custom format using the strftime function format specifications,
# see 'man strftime' for details.
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load?
# Standard plugins can be found in ~/.oh-my-zsh/plugins/*
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(
  archlinux
  git
  colored-man-pages
  zsh-autosuggestions
  zsh-syntax-highlighting
)

source $ZSH/oh-my-zsh.sh

# User configuration

# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"
alias c="clear"
alias la="ls -a"
alias mk3drive="cd /run/media/sagen/Mk3Drive"
alias mk3="cd /run/media/sagen/Mk3Drive"
alias vimi="vim +PlugInstall"

tlmgr() {
	sudo -S /usr/local/texlive/2020basic/bin/x86_64-darwin/tlmgr "$1"
}

# Git add, commit and push
cap() {
  git add .
  git commit -m "$1"
  git push origin
}

# Uppload to Uppmax
scp_uppmax() {
  scp "$1" sagen@rackham.uppmax.uu.se:/home/sagen/
}



# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/home/sagen/anaconda3/bin/conda' 'shell.bash' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
  eval "$__conda_setup"
else
  if [ -f "/home/sagen/anaconda3/etc/profile.d/conda.sh" ]; then
    . "/home/sagen/anaconda3/etc/profile.d/conda.sh"
  else
      export PATH="/home/sagen/anaconda3/bin:$PATH"
  fi
fi
unset __conda_setup
# <<< conda initialize <<<

# Addend conda local python installations
# PATH=$PATH:/Users/SagenOS/.local/bin
# PATH=$PATH:/Users/SagenOS/Library/Python/3.7/bin
# export PATH



# Set Spaceship ZSH as a prompt
autoload -U promptinit; promptinit
prompt spaceship


# Set corrct colors for tmux and vim
if [[ $TERM == xterm ]]; then
	    TERM=xterm-256color
fi

function vterm_printf(){
    if [ -n "$TMUX" ]; then
       printf "\ePtmux;\e\e]%s\007\e\\" "$1"
    elif [ "${TERM%%-*}" = "screen" ]; then
         printf "\eP\e]%s\007\e\\" "$1"
    else
        printf "\e]%s\e\\" "$1"
    fi
}


# If in emacs
if [[ "$INSIDE_EMACS" = 'vterm' ]]; then
   alias clear='vterm_printf "51;Evterm-clear-scrollback";tput clear'
fi

# GOLANG
export GOPATH=$HOME/go
export PATH="$HOME/.yarn/bin:$HOME/.config/yarn/global/node_modules/.bin:$PATH"
export PATH=$PATH:$(go env GOPATH)/bin
export GOPATH=$(go env GOPATH)
export GO111MODULE="auto"


# VIM and DOOM Emacs
export EDITOR='vim'
export PATH="/usr/local/opt/icu4c/bin:$PATH"
export PATH="/usr/local/opt/icu4c/sbin:$PATH"
export LDFLAGS="-L/usr/local/opt/icu4c/lib"
export CPPFLAGS="-I/usr/local/opt/icu4c/include"
export CPPFLAGS="-I/usr/local/opt/icu4c/include"
PATH="$PATH:/Users/SagenOS/.local/bin"
PATH="$PATH:/Users/SagenOS/Library/Python/3.7/bin"
PATH="$PATH:/Users/SagenOS/.emacs.d/bin"
export PATH


# Load FZF Fuzzy finder
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh



# Latex installer for Mac and ORg-mode support
#export PATH="$PATH:/Library/TeX/texbin"
export PATH="$PATH:/usr/local/texlive/2020basic/bin/x86_64-darwin"














# Install Irony-Server for C/C++
# Requires cmake and llvm
# brew install cmake llvm
# cmake -DCMAKE_INSTALL_RPATH_USE_LINK_PATH=ON -DCMAKE_INSTALL_PREFIX\=/Users/SagenOS/.emacs.d/.local/etc/irony-server/ -DLIBCLANG_LIBRARY=/usr/local/opt/llvm/lib/libclang.dylib -DLIBCLANG_INCLUDE_DIR=/usr/local/opt/llvm/include -DCMAKE_PREFIX_PATH=/usr/local/opt/llvm /Users/SagenOS/.emacs.d/.local/straight/build/irony/server && cmake --build . --use-stderr --config Release --target install
