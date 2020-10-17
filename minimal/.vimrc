


" call plug#begin('~/.vim/plugged')
"
" Plug 'neoclide/coc.nvim', {'branch': 'release'}
"
" " Initialize plugin system
" call plug#end()


syntax on " enable highlighting
" set number " enable line numbers
set number relativenumber

set backspace=indent,eol,start " let backspace delete over lines
set autoindent " enable auto indentation of lines
set smartindent " allow vim to best-effort guess the indentation
set pastetoggle=<F2> " enable paste mode (more on this below)

" set indent for 2 spaces
set tabstop=2
set shiftwidth=2
set expandtab

" enable mouse support
set mouse=a
