" https://github.com/zyeri/dotfiles

call plug#begin('~/.config/nvim/plugged')
    " plugins
    Plug 'tpope/vim-repeat'
    Plug 'tpope/vim-git'
    Plug 'tpope/vim-sensible'
    Plug 'tpope/vim-fugitive'
    Plug 'tpope/vim-unimpaired'
    Plug 'tpope/vim-commentary'
    Plug 'tpope/vim-surround'
    Plug 'tpope/vim-speeddating'

    Plug 'scrooloose/nerdtree', { 'on': 'NERDTreeToggle' }
    Plug 'scrooloose/syntastic'

    Plug 'bling/vim-airline' | Plug 'bling/vim-bufferline'

    Plug 'Valloric/YouCompleteMe'
    Plug 'rdnetto/YCM-Generator', { 'branch': 'stable' }

    Plug 'mhinz/vim-startify'
    Plug 'mhinz/vim-grepper'

    Plug 'godlygeek/tabular'
    Plug 'ctrlpvim/ctrlp.vim'
    Plug 'tacahiroy/ctrlp-funky'
    Plug 'FelikZ/ctrlp-py-matcher'
    Plug 'deris/vim-shot-f'
    Plug 'mbbill/undotree'
    Plug 'majutsushi/tagbar'
    Plug 'sheerun/vim-polyglot'
    Plug 'rhysd/committia.vim'
    Plug 'neovimhaskell/haskell-vim', { 'for': 'haskell' }
    Plug 'mtth/scratch.vim'
    Plug 'Valloric/ListToggle'
    Plug 'junegunn/fzf', { 'do': 'yes \| ./install' }

    " colorschemes
    Plug 'w0ng/vim-hybrid'
    Plug 'chriskempson/base16-vim'
call plug#end()

let mapleader = ','
let maplocalleader = ','
let g:mapleader = ','
let g:maplocalleader = ','

try
    " enable 24-bit TRUE COLOR for neovim
    let $NVIM_TUI_ENABLE_TRUE_COLOR=1
    let g:hybrid_custom_term_colors=1

    set background=dark
    colorscheme hybrid
catch /:E185:/
    " silently ignore colorscheme not found error
endtry

" don't reload a file when it has been edited outside of vim
set noautoread

" never autowrite a file
set noautowrite
set noautowriteall

" don't keep a backup of any files currently being edited
set nobackup

" preserve indent on line breaks
set breakindent

" show a vertical column on the 79th column
set colorcolumn=72

" show a popup menu for insert completion matches
set completeopt=menu,menuone,longest

" copy existing indent when autoindenting a new line
set copyindent

" diff display options
set diffopt=filler,icase,iwhite,vertical

" no error bells
set noerrorbells

" use spaces instead of the tab character
set expandtab

set nofoldenable foldmethod=syntax foldcolumn=0

" use g flag for :s by default
set gdefault

" if available, use ag in place of grep
if executable('ag')
    set grepprg=ag\ --nogroup\ --nocolor
endif

" don't destroy hidden buffers
set hidden

" ignore case while searching
set ignorecase

" enable the magic
set magic

" enable line numbering
set number

" preserve insent structure
set preserveindent

" use four spaces for each autoindent
set shiftwidth=4

" jump to matching bracket when inserted
set showmatch

" intelligently ignore case
set smartcase

" autoindent on newline
set smartindent

" number of spaces to use when tabbing during editing operations
set softtabstop=4

" always split new windows to the bottom right
set splitright
set splitbelow

" disable swap file
set noswapfile

" don't highlight past 256 columns
set synmaxcol=256

" number of spaces to use for tabs
set tabstop=4

" where to store undo files
set undodir=$HOME/.config/nvim/undo

" enable persistent undo
set undofile

" keep 2500 changes for undos
set undolevels=2500

" allow cursor to be placed one character after the end of line
set virtualedit=onemore

" disable visualbells
set novisualbell

" ignore certain file patterns
set wildignore+=*./git/*,*.o,*.obj

" disable softwrapping by default; enable via autocommand or modeline
set nowrap

" searches wrap around the end of file
set wrapscan

" commands
" Create bulleted or number lists out of a range of lines
command! -nargs=0 -range=% NumberList <line1>,<line2>s/^\s*\zs/\=(line('.') - line("'<")+1).'. '
command! -nargs=0 -range=% BulletList <line1>,<line2>s/^\s*\zs/*

" bindings
" move between windows
nnoremap <C-h> <C-w><C-h>
nnoremap <C-j> <C-w><C-j>
nnoremap <C-k> <C-w><C-k>
nnoremap <C-l> <C-w><C-l>

" yank to end of line
nnoremap Y y$

" keep visual selecting when tabbing
vnoremap << <gv
vnoremap >> >gv

" toggle folds
nnoremap <silent> <Space> @=(foldlevel('.')?'za':"\<Space>")<CR>
vnoremap <Space> zf

" clear trailing whitespace (http://vi.stackexchange.com/a/2285)
nnoremap <F5> :let _s=@/<Bar>:%s/\s\+$//e<Bar>:let @/=_s<Bar><CR>

nnoremap <F1> :NERDTreeToggle<CR>
nnoremap <F2> :TagbarToggle<CR>
nnoremap <F3> :UndotreeToggle<CR>

nnoremap <silent><C-p> :<C-u>CtrlP<CR>
nnoremap <silent><C-b> :<C-U>CtrlPBuffer<CR>
nnoremap <silent><leader>t :<C-u>CtrlPTag<CR>

" insert #! for current file type
inoreabbrev <expr>#!! '#!/usr/bin/env' . (empty(&filetype) ? '' : ' '.&filetype)

" plugin configuration

" scratch.vim
let g:scratch_autohide = 1
let g:scratch_top = 0
let g:scratch_persistence_file = '/tmp/scratch.vim'

" airline
let g:bufferline_echo = 0
let g:airline_inactive_collapse=1
let g:airline_powerline_fonts=0

if !exists('g:airline_symbols')
    let g:airline_symbols = { }
endif

let g:airline_left_sep           = ""
let g:airline_right_sep          = ""
let g:airline_symbols.branch     = ''
let g:airline_symbols.readonly   = ''
let g:airline_symbols.linenr     = ''
let g:airline_symbols.whitespace = 'Ξ'

" ctrlp
if executable('ag')
    let g:ctrlp_user_command = {
                \ 'types': {
                    \ 1: ['.git', 'cd %s && git ls-files . -co --exclude-standard'],
                    \ },
                \ 'fallback': 'ag %s -l -f -S --nocolor --nogroup --hidden -g ""'
                \ }

    let g:ctrlp_custom_ignore = {
                \ 'dir':    '\.git$\|\.svn$\',
                \ 'file':   '\.so$\|'
                \ }

    let g:ctrlp_use_caching = 0
endif

let g:ctrlp_status_func = { 'main': 'CtrlPStatusFunc1', 'prog': 'CtrlPStatusFunc2' }
let g:ctrlp_match_func = { 'match': 'pymatcher#PyMatch' }
let g:ctrlp_extensions = ['tag', 'line']

let g:ctrlp_working_path_mode = 'ca'
let g:ctrlp_open_new_file = 'h'
let g:ctrlp_match_window = 'bottom,order:btt,min:1,max:10,results:10'

" tagbar
" change focus to tagbar when opening it
let g:tagbar_autofocus = 1
let g:tagbar_autclose = 1

" haskell-vim
let g:haskell_enable_quantification=1
let g:haskell_enable_recursivedo=1
let g:haskell_enable_arrowsyntax=1
let g:haskell_enable_pattern_synonyms=1
let g:haskell_enable_typeroles=1
let g:haskell_enable_static_pointers=1

" polyglot
let g:polyglot_disabled=['haskell']

" vim-plug
let g:plug_threads = 40

" syntastic
let g:syntastic_always_populate_loc_list = 1

" ListToggle
let g:lt_location_list_toggle_map = '<leader>tl'
let g:lt_quickfix_list_toggle_map = '<leader>tq'

" YouCompleteMe
let g:ycm_path_to_python_interpreter = '/usr/bin/python3'

