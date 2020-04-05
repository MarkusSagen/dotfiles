" Copy of Ben Awad's NeoVim vimrc file
" Incorporates VS code into Vim

" Some nice features to include initially
" Add indentation to 4 spaces
" Add relative line numbering
set nocompatible
"" Set auto reload when saving .vimrc
" autocmd BufWritePost .vimrc source %
" set autoread

set rnu!
syntax enable
" setlocal spell spelllang=en_us
highlight CursorLine guibg=pink "lightgrey  " ctermbg=lightgrey
set cursorline
set colorcolumn=85
set history=1000


" set autoindent of lines
set autoindent
set smartindent
set pastetoggle=<F2>

" set manual indent
set tabstop=2
set shiftwidth=2
set expandtab

" enable mouse suport
" set mouse=a

" set title
set title
" filetype plugin indent on

" Make searching smarter
set ignorecase		" Ignore case sensitive searches
set smartcase		" Unless specified by used with Uppercase
nnoremap n nnz		" Add highlighting on search word as you type
nnoremap N Nzz

" Set the number of sceenlines to keep above and below the cursor
set scrolloff=4



" Specify a directory for plugins
if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif
call plug#begin('~/.vim/plugged')

Plug 'vim-airline/vim-airline'		" Adds color to bottom navigation
Plug 'djoshea/vim-autoread' 		  " Adds autoreload
" " Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'majutsushi/tagbar'
Plug 'godlygeek/tabular'          " Makes tabs better formated
"
" " NERDTree
Plug 'scrooloose/nerdtree'
Plug 'Xuyuanp/nerdtree-git-plugin'
Plug 'tiagofumo/vim-nerdtree-syntax-highlight'

" " FuzzyFinder - NerdTree in terminal
Plug 'junegunn/fzf'
Plug 'junegunn/fzf.vim'

" Plug 'ryanoasis/vim-devicons'

" This one makes problems on UPPMAX
" Plug 'airblade/vim-gitgutter'

Plug 'ctrlpvim/ctrlp.vim' " fuzzy find files
Plug 'scrooloose/nerdcommenter'
" Plug 'prettier/vim-prettier', { 'do': 'yarn install' }
Plug 'christoomey/vim-tmux-navigator'
" Plug 'HerringtonDarkholme/yats.vim' " TS Syntax
" "
" " " Completion and syntax
" " Plug 'roxma/nvim-completion-manager'
" " Plug 'gaalcaras/ncm-R'
" Plug 'ycm-core/YouCompleteMe'
" Plug 'xavierd/clang_complete'
Plug 'tpope/vim-surround'
Plug 'garbas/vim-snipmate'

Plug 'w0rp/ale'
"
" "
" " " Themes
Plug 'kristijanhusak/vim-hybrid-material'
Plug 'sheerun/vim-polyglot'
Plug 'itchyny/lightline.vim'
Plug 'w0ng/vim-hybrid'
Plug 'dracula/vim', { 'as': 'dracula' }
Plug 'relastle/bluewery.vim'
Plug 'whatyouhide/vim-gotham'
Plug 'artanikin/vim-synthwave84'
Plug 'hzchirs/vim-material'
Plug 'jdkanani/vim-material-theme'
Plug 'joshdick/onedark.vim'
Plug 'arcticicestudio/nord-vim'
Plug 'morhetz/gruvbox'
"
" Plug 'ryanoasis/vim-devicons'
call plug#end()


" Set GUI for Vim
if (empty($TMUX))
   if (has("termguicolors")) 	" Importatint for setting terminal color correct
		set termguicolors
  endif
endif


set background=dark		" Fixes dark background in MacVim and some terminals
syntax enable
" colorscheme material-theme
" colorscheme nord
" colorscheme bluewery
colorscheme onedark
" colorscheme dracula
" colorscheme synthwave84		" Orange
" colorscheme gotham256 		" Complete darkness

let g:gruvbox_italic=1
let g:gruvbox_termcolors=256




" Toggle NERD Tree and file manager
inoremap jk <ESC>
nmap <C-n> :NERDTreeToggle<CR>
vmap ++ <plug>NERDCommenterToggle
nmap ++ <plug>NERDCommenterToggle

" TODO
"" open NERD Tree automatically
"autocmd StdinReadPre * let s:std_in=1
"autocmd VimEnter * NERDTree



" Commands for FuzzyFinder fzf
" Mapping selecting mappings
map ; :Files<CR>



" Highlight trailing white space
highlight ExtraWhitespace ctermbg=white guibg=white
au ColorScheme * highlight ExtraWhitespace guibg=lightblue
au BufEnter * match ExtraWhitespace /\s\+$/
au InsertEnter * match ExtraWhitespace /\s\+\%#\@<!$/
au InsertLeave * match ExtraWhiteSpace /\s\+$/

" Trim white space on save
fun! <SID>StripTrailingWhitespaces()
    let l = line(".")
    let c = col(".")
    %s/\s\+$//e
    call cursor(l, c)
endfun
autocmd BufWritePre * :call <SID>StripTrailingWhitespaces()



" Linters
let b:ale_linters = ['pyflakes', 'flake8', 'pylint']
let b:fix_on_save = 1



" For C/C++ files
set exrc
set secure
augroup project
  autocmd!
  autocmd BufRead,BufNewFile *.h,*.c set filetype=c.doxygen
augroup END

" Allow for seaching in headerfiles when curser is on file
" Press:  <C-W><C-F>
let &path.="src/include,/usr/include/AL,"
" Set make configure to a mapping
set makeprg=make\ -C\ ../build\ -j9
nnoremap <F4>
" Map to run project after it is compiled
nnoremap <F5> :!./project_name<cr>

" Configure clang_complete
" Set the library path directories to look for
" let g:clang_library_path='/usr/lib/llvm-3.8/lib'
" let g:clang_library_path='/usr/lib64/libclang.so.3.8'




" Add commenting block for different file structures and languages
autocmd FileType c,cpp,java,scala let b:comment_leader = '// '
autocmd FileType sh,ruby,python   let b:comment_leader = '# '
autocmd FileType conf,fstab       let b:comment_leader = '# '
autocmd FileType tex              let b:comment_leader = '% '
autocmd FileType mail             let b:comment_leader = '> '
autocmd FileType vim              let b:comment_leader = '" '
" ,cc	- Comments blocks
" ,cu	- uncomment blocks
noremap <silent> ,cc :<C-B>silent <C-E>s/^/<C-R>=escape(b:comment_leader,'\/')<CR>/<CR>:nohlsearch<CR>
noremap <silent> ,cu :<C-B>silent <C-E>s/^\V<C-R>=escape(b:comment_leader,'\/')<CR>//e<CR>:nohlsearch<CR>




" ======================================
" TagBar short commands
" ======================================
nmap <F8> :TagBarToggle<CR>     " Toggle TagBar window



