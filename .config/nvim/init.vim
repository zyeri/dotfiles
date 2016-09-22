" I'm in the (slow) process of refactoring this, so it's a bit of a mess.
" What I plan to do is have it detect both what platform it's running on
" (windows, *nix, etc) and which version/fork of vim is being used 
" (vim 7.x, vim 8.x, neovim) and use those to configure/disable/modify 
" features for that specific platform. I'd also like to organize it a bit,
" possibly splitting up related functionality into separate files so it
" can be a bit more modular or at the very least, somewhat organized.

" determine the operating system vim is running on
if !exists("g:platform")
    if has("win32") || has("win64")
        let g:platform='windows'
    else 
        let g:platform = tolower(substitute(system('uname'), '\n', '', ''))
    endif
endif

" plugins
call plug#begin('~/.config/nvim/plugged')
    Plug 'chriskempson/base16-vim'

    Plug 'tpope/vim-repeat'
    Plug 'tpope/vim-git'
    Plug 'tpope/vim-sensible'
    Plug 'tpope/vim-fugitive'
    Plug 'tpope/vim-unimpaired'
    Plug 'tpope/vim-commentary'
    Plug 'tpope/vim-surround'
    Plug 'tpope/vim-speeddating'

    Plug 'mattn/emmet-vim', { 'for': ['php','html','css','javascript'] }
    Plug 'godlygeek/tabular', { 'on': 'Tab' }
    Plug 'mbbill/undotree', { 'on': 'UndotreeToggle' }
    Plug 'deris/vim-shot-f'
    Plug 'sheerun/vim-polyglot'
    Plug 'ctrlpvim/ctrlp.vim'
    Plug 'tacahiroy/ctrlp-funky'
    Plug 'FelikZ/ctrlp-py-matcher'
    Plug 'editorconfig/editorconfig-vim'

    Plug 'itchyny/lightline.vim'
    Plug 'daviesjamie/vim-base16-lightline'

    if executable("tmux")
        Plug 'edkolev/tmuxline.vim'
    endif

    Plug 'junegunn/vim-github-dashboard', { 'on': ['GHDashboard', 'GHActivity'] }
    
    Plug 'SirVer/ultisnips' | Plug 'honza/vim-snippets'

    " latex
    Plug 'lervag/vimtex', { 'for': 'tex' }

    " pandoc markdown
    Plug 'vim-pandoc/vim-pandoc', { 'for': ['markdown'] }
    Plug 'vim-pandoc/vim-pandoc-syntax', { 'for': ['markdown'] }
    Plug 'junegunn/goyo.vim', { 'for': ['markdown'] }

    " haskell
    Plug 'neovimhaskell/haskell-vim', { 'for': 'haskell' }

    if g:platform ==? "linux"
        Plug 'rhysd/vim-clang-format'
    endif

    " doesn't work so well for me on Windows
    if g:platform ==? "linux"
        Plug 'Valloric/YouCompleteMe' " ,
        Plug 'shawncplus/phpcomplete.vim', { 'for': ['php','html'] }
        Plug 'rdnetto/YCM-Generator',  { 'branch': 'stable' }
    endif

    if g:platform ==? "windows"
        Plug 'ajh17/VimCompletesMe'
    endif
    
    " only use if running neovim and on Linux
    if has("nvim") && g:platform ==? "linux"
        Plug 'neomake/neomake'
    endif

    " check to see if executable exists before trying to use the plugin
    if executable('ctags')
        Plug 'majutsushi/tagbar',           { 'on': 'TagbarToggle' }
    endif
call plug#end()


if filereadable(expand("~/.vimrc_background"))
    let base16colorspace=256
    source ~/.vimrc_background
endif

if has("termguicolors")
    set termguicolors
endif

if has("gui")
    if g:platform ==? "windows"
        set guifont=Consolas:h12
    endif

    set guioptions=
    set guiheadroom=
endif

" if available, use ag in place of grep
if executable('ag')
    set grepprg=ag\ --nogroup\ --nocolor
endif

let mapleader = ','
let maplocalleader = ","

colorscheme base16-tomorrow-night

" dark terminal
set background=dark

" TODO: configure errorformat for clang only inside a C++ buffer
" for clang errors
set errorformat=%f:%l:%c:\ %t%s:\ %m
set omnifunc=syntaxcomplete#Complete
set noautoread
set noautowrite
set noautowriteall
set nobackup
set breakindent
set colorcolumn=72
set completeopt=menu,menuone,longest
set copyindent
set pumheight=5
set diffopt=filler,icase,iwhite,vertical
set noerrorbells
set expandtab
set nofoldenable
set foldmethod=syntax 
set foldcolumn=0
set gdefault
set hidden
set ignorecase
set magic
set number
set preserveindent
set shiftwidth=4
set showmatch
set smartcase
set smartindent
set softtabstop=4
set splitright
set splitbelow
set noswapfile
set virtualedit=onemore
set nowrap
set wrapscan
set novisualbell
set wildignore+=*./git/*,*.o,*.obj

set undodir=$HOME/.config/nvim/undo
set undofile
set undolevels=5000

" commands
" Create bulleted or number lists out of a range of lines
command! -nargs=0 -range=% NumberList <line1>,<line2>s/^\s*\zs/\=(line('.') - line("'<")+1).'. '
command! -nargs=0 -range=% BulletList <line1>,<line2>s/^\s*\zs/*

" bindings
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

nnoremap <F2> :TagbarToggle<CR>
nnoremap <F3> :UndotreeToggle<CR>

nnoremap <silent><C-p> :<C-u>CtrlP<CR>
nnoremap <silent><C-b> :<C-U>CtrlPBuffer<CR>
nnoremap <silent><leader>t :<C-u>CtrlPTag<CR>

nnoremap <silent><leader>qco :copen<cr>
nnoremap <silent><leader>qcc :cclose<cr>
nnoremap <silent><leader>lco :lopen<cr>
nnoremap <silent><leader>lcc :lclose<cr>

" autocommands
" C++ stuff
augroup cpp
    autocmd!
    autocmd FileType c,cpp nnoremap <buffer><leader>cf :<C-u>ClangFormat<CR>
    autocmd FileType c,cpp vnoremap <buffer><leader>cf :ClangFormat<CR>
    autocmd FileType c,cpp setlocal textwidth=72
augroup END

augroup tex
    autocmd!
    autocmd FileType tex setlocal textwidth=72
augroup END

" insert #! for current file type
inoreabbrev <expr>#!! '#!/usr/bin/env' . (empty(&filetype) ? '' : ' '.&filetype)

" plugin configuration
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
" dont bother configuring if the plugin wasn't loaded/installed
if has("executable")
    " change focus to tagbar when opening it
    let g:tagbar_autofocus = 1
    let g:tagbar_autclose = 1
endif

" haskell-vim
let g:haskell_enable_quantification=1
let g:haskell_enable_recursivedo=1
let g:haskell_enable_arrowsyntax=1
let g:haskell_enable_pattern_synonyms=1
let g:haskell_enable_typeroles=1
let g:haskell_enable_static_pointers=1

" polyglot
let g:polyglot_disabled=['haskell']

" YouCompleteMe
let g:ycm_path_to_python_interpreter = '/usr/bin/python3'
let g:ycm_confirm_extra_conf = 0

" vimtex
let g:tex_indent_brace = 0
let g:tex_flavor = 'latex'
let g:vimtex_indent_enabled = 0

let g:vimtex_latexmk_enabled=1
" TODO: rework this so it works a little better
" let g:vimtex_view_general_viewer = 'qpdfview'
" let g:vimtex_view_general_options = '--unique @pdf\#src:@tex:@line:@col'
" let g:vimtex_view_general_options_latexmk = '--unique'

if !exists('g:ycm_semantic_triggers')
    let g:ycm_semantic_triggers = {}
endif

let g:ycm_semantic_triggers.tex = [
            \ 're!\\[A-Za-z]*cite[A-Za-z]*(\[[^]]*\]){0,2}{[^}]*',
            \ 're!\\[A-Za-z]*ref({[^}]*|range{([^,{}]*(}{)?))',
            \ 're!\\hyperref\[[^]]*',
            \ 're!\\includegraphics\*?(\[[^]]*\]){0,2}{[^}]*',
            \ 're!\\(include(only)?|input){[^}]*',
            \ 're!\\\a*(gls|Gls|GLS)(pl)?\a*(\s*\[[^]]*\]){0,2}\s*\{[^}]*',
            \ 're!\\includepdf(\s*\[[^]]*\])?\s*\{[^}]*',
            \ 're!\\includestandalone(\s*\[[^]]*\])?\s*\{[^}]*',
            \ ]

" vim-clang-format
let g:clang_format#style_options = {
            \ "BreakBeforeBraces" : "Stroustrup",
            \ "AllowShortIfStatementsOnASingleLine" : "false",
            \ "IndentWidth" : "4",
            \ "ColumnLimit" : "72",
            \ "Standard" : "C++11"
            \ }

" vim-lightline
let g:lightline = {
            \ 'colorscheme': 'base16'
            \ }
