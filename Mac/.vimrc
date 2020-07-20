" Some nice features to include initially
" Add indentation to 4 spaces
" Add relative line numbering
set nocompatible
set backspace=indent,eol,start
"" Set auto reload when saving .vimrc
" autocmd BufWritePost .vimrc source %
" set autoread
set rnu!
syntax enable
" setlocal spell spelllang=en_us
highlight CursorLine guibg=pink
" lightgrey ctermbg=lightgrey
set cursorline
set history=1000

" set autoindent of lines
" set autoindent
" set noautoindent
set smartindent
set pastetoggle=<F2>

" set manual indent
set tabstop=2
set shiftwidth=2
" set expandtab

" enable mouse suport
set mouse=a

" set title
set title
" filetype plugin indent on

" Make searching smarter
set ignorecase		" Ignore case sensitive searches
set smartcase		" Unless specified by used with Uppercase""
nnoremap n nnz
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

Plug 'rizzatti/dash.vim'
Plug 'vim-airline/vim-airline'		" Adds color to bottom navigation
Plug 'djoshea/vim-autoread' 		  " Adds autoreload
Plug 'neoclide/coc.nvim', {'branch': 'release'} " Auto-complete
Plug 'ycm-core/YouCompleteMe' 									" Auto-complete TS
Plug 'mileszs/ack.vim'
Plug 'majutsushi/tagbar'
Plug 'godlygeek/tabular'          " Makes tabs better formated
Plug 'junegunn/limelight.vim' 		" Distraction free editing
Plug 'vim-utils/vim-man'
Plug 'lyuts/vim-rtags'

" Language support
Plug 'fatih/vim-go', { 'do': ':GoInstallBinaries' }
Plug 'chr4/nginx.vim'
Plug 'chrisbra/csv.vim'
Plug 'ekalinin/dockerfile.vim'
Plug 'elzr/vim-json'
Plug 'iamcco/markdown-preview.nvim', { 'do': 'cd app & yarn install' }
Plug 'iamcco/markdown-preview.vim'
Plug 'iamcco/mathjax-support-for-mkdp'
Plug 'vimwiki/vimwiki'
Plug 'vim-pandoc/vim-rmarkdown'
Plug 'vim-pandoc/vim-pandoc'
Plug 'vim-pandoc/vim-pandoc-syntax'
Plug 'sampsyo/autolink.vim'
Plug 'ying17zi/vim-live-latex-preview'
Plug 'leafgarland/typescript-vim'

" TMUX support
Plug 'MikeDacre/tmux-zsh-vim-titles'
Plug 'tmux-plugins/vim-tmux'
Plug 'christoomey/vim-tmux-navigator'

" NERDTree
Plug 'scrooloose/nerdtree'
Plug 'Xuyuanp/nerdtree-git-plugin'
Plug 'tiagofumo/vim-nerdtree-syntax-highlight'

" Fuzzy Finder fzf
" Plugin outside ~/.vim/plugged with post-update hook
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'

" Plug 'ryanoasis/vim-devicons'
Plug 'airblade/vim-gitgutter'
Plug 'ctrlpvim/ctrlp.vim' " fuzzy find files
Plug 'scrooloose/nerdcommenter'
Plug 'prettier/vim-prettier', { 'do': 'yarn install' }
Plug 'christoomey/vim-tmux-navigator'

Plug 'tpope/vim-surround'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-dispatch'
Plug 'tpope/vim-fugitive'
Plug 'mattn/emmet-vim'
Plug 'jiangmiao/auto-pairs'


" Vim Snippets
Plug 'MarcWeber/vim-addon-mw-utils'
Plug 'tomtom/tlib_vim'
Plug 'garbas/vim-snipmate'
Plug 'honza/vim-snippets'
Plug 'garbas/vim-snipmate'

Plug 'w0rp/ale'

" " Themes
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

Plug 'ryanoasis/vim-devicons'
call plug#end()


"==============================
" 	TMUX Settings
"=============================
" Set GUI for Vim
if (empty($TMUX))
   if (has("termguicolors")) 	" Importatint for setting terminal color correct
		set termguicolors
  endif
endif


set background=dark		" Fixes dark background in MacVim and some terminals
syntax enable
syntax on
filetype on

"==============================
"		THEMES
"==============================
" colorscheme material-theme
" colorscheme nord
" colorscheme bluewery
colorscheme onedark
" colorscheme dracula

let g:gruvbox_italic=1
let g:gruvbox_termcolors=16







"================================"
" 		Markdown settings
"================================
"let g:mkdp_path_to_chrome = ""
let g:mkdp_browserfunc = 'MKDP_browserfunc_default'
let g:mkdp_auto_start = 0 " Start preview when entering Markdown file
let g:mkdp_auto_open = 1  " Start preview on editing file
let g:mkdp_auto_close = 1 " Auto-close preview when leaving Markdown file
let g:mkdp_refresh_slow = 1 "0 = auto update :: 1 = update on save "
let g:mkdp_command_for_global = 0 "If preview is used for other than MD-files"
let g:mkdp_open_to_the_world = 0 "If preview is availabel to others in your network"
let g:vim_markdown_math = 1
let g:vim_markdown_fontmatter = 1
let g:vim_markdown_json_frontmatter = 1
let g:vim_markdown_strikethrough = 1 "Add strike through by: ~~striked text~~"
let g:vim_markdown_new_list_item_indent = 4 "Indent list by 4 spaces"



"======================================="
" 		Remap Leader Key									"
"======================================="
" Set leader key to 'comma'
let mapleader = ","

" Set escape characters
inoremap jk <ESC>
" Map Tab
"nnoremap <Tab><Tab> <Esc>
"vnoremap <Tab><Tab> <Esc>gV
"onoremap <Tab><Tab> <Esc>
"cnoremap <Tab><Tab> <C-C><Esc>
"inoremap <Tab><Tab> <Esc>`^
"inoremap <Leader><Tab> <Tab>
"inoremap <leader><Tab> <Tab>
" Map Caps-Lock
" Mac ---> System Preferences > Keyboard > Modifier Keys > Caps Locl = Escape
" Linux -> :
"au VimEnter * !xmodmap -e 'clear Lock' -e 'keycode 0x42 = Escape'
"au VimLeave * !xmodmap -e 'clear Lock' -e 'keycode 0x42 = Caps_Lock'


" Configure spell checking
nmap <silent> <leader>p :set spell!<CR>
set spelllang=en_us

" Send all vim registers to the mac clipboard
set clipboard=unnamed

" Default to magic mode when using substitution
cnoremap %s/ %s/\v
cnoremap \>s/ \>s/\v
" }}}

" Terminal Mode Configuration {{{
" Terminal mode mappings
tnoremap <Esc> <C-\><C-n>
" }}}
"


"======================================="
" 		Search in Files 									"
"======================================="
"Ack / Ag support - Search for instance in all files and sub folders
let g:ackprg = 'ag --nogroup --nocolor --column'

" Toggle NERD Tree and file manager
nmap <C-n> :NERDTreeToggle<CR>
vmap ++ <plug>NERDCommenterToggle
nmap ++ <plug>NERDCommenterToggle

" TODO
" open NERD Tree automatically
"autocmd StdinReadPre * let s:std_in=1
"autocmd VimEnter * NERDTree

"FZF fuzzy finder commands
map 			; :Files<CR>
map 			<C-f> <Esc><Esc>:Files!<CR>
inoremap 	<C-f> <Esc><Esc>:BLines!<CR>
" map 			<C-g> <Esc><Esc>:BCommits!<CR>
nnoremap 	<C-g> :Rg<CR>
nnoremap 	,b :ls<CR>:buffer<Space>


"======================================"
" 		Comment Files
"======================================
"Add commenting block for different file structures and languages
autocmd FileType c,cpp,java,scala let b:comment_leader = '// '
autocmd FileType js               let b:comment_leader = '// '
autocmd FileType sh,ruby,python   let b:comment_leader = '# '
autocmd FileType conf,fstab       let b:comment_leader = '# '
autocmd FileType tex              let b:comment_leader = '% '
autocmd FileType mail             let b:comment_leader = 'd> '
autocmd FileType vim              let b:comment_leader = '" '
" ,cc	- Comments blocks
" ,cu	- uncomment blocks
noremap <silent> ,cc :<C-B>silent <C-E>s/^/<C-R>=escape(b:comment_leader,'\/')<CR>/<CR>:nohlsearch<CR>
noremap <silent> ,cu :<C-B>silent <C-E>s/^\V<C-R>=escape(b:comment_leader,'\/')<CR>//e<CR>:nohlsearch<CR>


"======================================
" 	Hightligts and White-spaces
"======================================
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
" Strip whitespace on save
autocmd BufWritePre * :call <SID>StripTrailingWhitespaces()



" ======================================
" 	F1-F10 Mappgins
" ======================================
" Get current time
map <F10> :echo 'TIME: -  ' . strftime('%c')<CR>

" =======================================
"  Github short commands
" =======================================
map <F1> :! git status
map <F2> :! git diff %
map <F3> :! git add %
map <F4> :! git commit -m       "This commit was automatically created with Vim" %
map <F5> :! git log — oneline — abbrev-commit



" 	AUTOCOMPLETE - CoC and YouCompleteMe
" ======================================
" 		LANGUAGE SPECIFICS
" == ====================================
" GOLANG
let g:go_highlight_build_constraints = 1
let g:go_highlight_extra_types = 1
let g:go_highlight_fields = 1
let g:go_highlight_functions = 1
let g:go_highlight_methods = 1
let g:go_highlight_operators = 1
let g:go_highlight_structs = 1
let g:go_highlight_types = 1
let g:go_auto_sameids = 1
let g:go_fmt_command = "goimports"
let g:go_def_mode="gopls"
let g:go_info_mode="gopls"


" autocmd FileType markdown let b:sleuth_automatic=0
" autocmd FileType markdown set conceallevel=0
" autocmd FileType markdown normal zR

" TextEdit might fail if hidden is not set.
set hidden
"
" " Some servers have issues with backup files, see #649.
set nobackup
set nowritebackup
"
" " Give more space for displaying messages.
set cmdheight=2

" Having longer updatetime (default is 4000 ms = 4 s) leads to noticeable
" delays and poor user experience.
set updatetime=300
"
" " Don't pass messages to |ins-completion-menu|.
set shortmess+=c

"Always show the signcolumn, otherwise it would shift the text each time
"diagnostics appear/become resolved.
set signcolumn=yes

" Use tab for trigger completion with characters ahead and navigate.
" NOTE: Use command ':verbose imap <tab>' to make sure tab is not mapped by
" other plugin before putting this into your config.
inoremap <silent><expr> <TAB>
		 	\ pumvisible() ? "\<C-n>" :
			\ <SID>check_back_space() ? "\<TAB>" :
			\ coc#refresh()
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

function! s:check_back_space() abort
	let col = col('.') - 1
	return !col || getline('.')[col - 1]  =~# '\s'
endfunction

" Use <c-space> to trigger completion.
inoremap <silent><expr> <c-space> coc#refresh()


if exists('*complete_info')
  inoremap <expr> <cr> complete_info()["selected"] != "-1" ? "\<C-y>" : "\<C-g>u\<CR>"
  inoremap <expr> <cr> complete_info()["selected"] != "-1" ? "\<C-y>" : "\<C-g>u\<CR>"
else
  inoremap <expr> <cr> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"
endif

" Use `[g` and `]g` to navigate diagnostics
nmap <silent> [g <Plug>(coc-diagnostic-prev)
nmap <silent> ]g <Plug>(coc-diagnostic-next)

" GoTo code navigation.
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)

" Use K to show documentation in preview window.
nnoremap <silent> K :call <SID>show_documentation()<CR>
function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  else
    call CocAction('doHover')
  endif
endfunction

" Highlight the symbol and its references when holding the cursor.
autocmd CursorHold * silent call CocActionAsync('highlight')

" Symbol renaming.
nmap <leader>rn <Plug>(coc-rename)

" Formatting selected code.
xmap <leader>f <Plug>(coc-format-selected)
nmap <leader>f <Plug>(coc-format-selected)

augroup mygroup
  autocmd!
 	" Setup formatexpr specified filetype(s).
  autocmd FileType typescript,json setl formatexpr=CocAction('formatSelected')
  " Update signature help on jump placeholder.
  autocmd User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')
augroup end

" Applying codeAction to the selected region.
" Example: `<leader>aap` for current paragraph
xmap <leader>a <Plug>(coc-codeaction-selected)
nmap <leader>a <Plug>(coc-codeaction-selected)

" Remap keys for applying codeAction to the current line.
nmap <leader>ac <Plug>(coc-codeaction)
" Apply AutoFix to problem on the current line.
nmap <leader>qf <Plug>(coc-fix-current)

" Introduce function text object
" NOTE: Requires 'textDocument.documentSymbol' support from the language server.
xmap if <Plug>(coc-funcobj-i)
xmap af <Plug>(coc-funcobj-a)
omap if <Plug>(coc-funcobj-i)
omap af <Plug>(coc-funcobj-a)

" Use <TAB> for selections ranges.
" NOTE: Requires 'textDocument/selectionRange' support from the language server.
" coc-tsserver, coc-python are the examples of servers that support it.
nmap <silent> <TAB> <Plug>(coc-range-select)
xmap <silent> <TAB> <Plug>(coc-range-select)

" Add `:Format` command to format current buffer.
command! -nargs=0 Format 	:call CocAction('format')

" Add `:Fold` command to fold current buffer.
command! -nargs=? Fold 		:call CocAction('fold', <f-args>)

" Add `:OR` command for organize imports of the current buffer.
command! -nargs=0 OR 			:call CocAction('runCommand', 'editor.action.organizeImport')

" Add (Neo)Vim's native statusline support.
" NOTE: Please see `:h coc-status` for integrations with external plugins that
" provide custom statusline: lightline.vim, vim-airline.
set statusline^=%{coc#status()}%{get(b:,'coc_current_function','')}

" Mappings using CoCList:
" Show all diagnostics.
nnoremap <silent> <space>a :<C-u>CocList diagnostics<cr>

" Manage extensions.
nnoremap <silent> <space>e :<C-u>CocList extensions<cr>

" Show commands.
nnoremap <silent> <space>c :<C-u>CocList commands<cr>

" Find symbol of current document.
nnoremap <silent> <space>o :<C-u>CocList outline<cr>

" Search workspace symbols.
nnoremap <silent> <space>s :<C-u>CocList -I symbols<cr>

" Do default action for next item.
nnoremap <silent> <space>j :<C-u>CocNext<CR>

" Do default action for previous item.
nnoremap <silent> <space>k :<C-u>CocPrev<CR>

" Resume latest coc list.
nnoremap <silent> <space>p :<C-u>CocListResume<CR>





