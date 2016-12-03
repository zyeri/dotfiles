" -------
" Plugins
" -------
call plug#begin('$HOME/.config/nvim/plugged')
    Plug 'chriskempson/base16-vim'
    Plug 'sheerun/vim-polyglot'

    Plug 'tpope/vim-git'
    Plug 'tpope/vim-repeat'
    Plug 'tpope/vim-eunuch'
    Plug 'tpope/vim-fugitive'
    Plug 'tpope/vim-surround'
    Plug 'tpope/vim-markdown'
    Plug 'tpope/vim-unimpaired'
    Plug 'tpope/vim-commentary'
    Plug 'tpope/vim-speeddating'

    Plug 'mileszs/ack.vim'
    Plug 'neomake/neomake'
    Plug 'metakirby5/codi.vim'
    Plug 'mhinz/vim-startify'
    Plug 'justinmk/vim-dirvish'
    " Plug 'junegunn/vim-peekaboo'
    Plug 'ludovicchabant/vim-gutentags'
    Plug 'gregsexton/gitv'
    Plug 'junegunn/gv.vim'

    Plug 'ctrlpvim/ctrlp.vim' | Plug 'nixprime/cpsm'
    Plug 'itchyny/lightline.vim' | Plug 'daviesjamie/vim-base16-lightline'

    Plug 'mbbill/undotree',     { 'on': 'UndotreeToggle' }
    Plug 'godlygeek/tabular',   { 'on': ['Tab', 'Tabularize'] }
    Plug 'majutsushi/tagbar',   { 'on': 'TagbarToggle' }

    Plug 'lervag/vimtex'
    Plug 'rust-lang/rust.vim'
    Plug 'neovimhaskell/haskell-vim'

    if has('nvim')
        Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
        Plug 'Shougo/neosnippet.vim' | Plug 'Shougo/neosnippet-snippets'
        Plug 'Shougo/echodoc.vim'

        " " includes completion
        Plug 'Shougo/neoinclude.vim'

        " vim completion
        Plug 'Shougo/neco-vim', { 'for': ['vim'] }
        Plug 'Shougo/neco-syntax', { 'for': ['vim'] }

        Plug 'wellle/tmux-complete.vim'

        Plug 'zchee/deoplete-go'
        Plug 'zchee/deoplete-clang'
        Plug 'zchee/deoplete-jedi' 

        Plug 'eagletmt/neco-ghc'
        Plug 'carlitux/deoplete-ternjs'
        " Plug 'SevereOverfl0w/deoplete-github'
        Plug 'fishbullet/deoplete-ruby'
        Plug 'zchee/deoplete-zsh'
        Plug 'sebastianmarkow/deoplete-rust'
    endif
call plug#end()

" nvim-specific stuff
if has('nvim')
    let g:python3_host_prog = '/usr/bin/python3'
    let g:python_host_prog = '/usr/bin/python2'

    if exists('&inccommand')
        set inccommand=split
    endif

    if exists('&termguicolors')
        set termguicolors
    endif

    tnoremap <Esc> <C-\><C-n>
endif

" use <Space> as the leader 
let mapleader = ','
let maplocalleader = '\\'

silent! colorscheme base16-tomorrow-night

" if available, use ag in place of grep
if executable('ag')
    set grepprg=ag\ --vimgrep\ $*
    set grepformat=%f:%l:%c:%m
endif

" dark terminal
set background=dark

" only show ten lines of insert-mode completion 
set pumheight=10

" show a colored column at 72 characters
set colorcolumn=72

" completion options
set completeopt=menu,menuone,longest

" options for diff mode
set diffopt=filler,icase,iwhite,vertical

" use spaces instead of tabs
set expandtab

" use the 'g' flag for :substitute by default
set gdefault

" don't unload buffers
set hidden

" show line numbers
set number relativenumber 

" briefly show matching brackets on insertion
set showmatch

" ignore case when searching unless pattern contains upper case characters
set smartcase

" number of spaces to insert on <Tab>
set shiftwidth=4 tabstop=4 softtabstop=4

" where to put new windows
set splitright
set splitbelow

" don't write swap files
set noswapfile

" cursor can be positioned where there are no actual characters
set virtualedit=onemore,block

" never softwrap text
set nowrap

" searching wraps around buffer
set wrapscan

" reload file if modified externally
set autoread

" control how neovim formats
set formatoptions=tcqj 

" history to remember
set history=10000

" list characters
set list
" set listchars=tab:>\ ,trail:-,nbsp:+
set listchars=tab:>\ ,nbsp:+

" dictionary 
set dictionary="/usr/share/dict/words"

" add dictionary completion
set complete+=k

" folding
set foldmethod=indent
set foldnestmax=3

" don't fold by default
set nofoldenable

" completion
set wildmenu

" ignore case in the wildmenu
set wildignorecase

" when more than one match, list all matches and complete the first
set wildmode=full

" ignore files matching these patterns
set wildignore+=*./git/*,*.o,*.obj
set wildignore+=log/**
set wildignore+=tmp/**
set wildignore+=*.png,*.jpg,*.gif
set wildignore+=*.so,*.swp,*.zip

" scrolling
set scrolloff=10

" keep 15 characters space to the sides 
set sidescrolloff=15
set sidescroll=1

" shorten messages
set shortmess=aIT

" " command line height
" set cmdheight=2

" don't show the current mode (insert/normal/etc) below the status line
set noshowmode

" persistent undo
set undofile
set undolevels=5000

" indentation options
set breakindent
set copyindent
set preserveindent
set smartindent

" -------------
" netrw options
" -------------
let g:netrw_banner = 0
let g:netrw_browse_split = 4
let g:netrw_altv = 1
let g:netrw_liststyle = 3
let g:netrw_preview = 1
let g:netrw_list_hide = netrw_gitignore#Hide()
let g:netrw_list_hide .= ',\(^\|\s\s\)\zs\.\S\+'

" --------
" bindings
" --------
" yank to end of line
nnoremap Y y$

" keep visual selecting when tabbing
vnoremap << <gv
vnoremap >> >gv

" toggle folds
nnoremap <silent> <Space> @=(foldlevel('.')?'za':'\<Space>')<cr>
vnoremap <Space> zf

" clear trailing whitespace (http://vi.stackexchange.com/a/2285)
nnoremap <F5> :let _s=@/<Bar>:%s/\s\+$//e<Bar>:let @/=_s<Bar><cr>

" kill the current buffer
nnoremap <silent><leader>K :bdelete<cr>

" configure latex
function! s:latex_setup() abort
    setlocal makeprg=latexmk\ -pdf\ %
    setlocal conceallevel=0

    let s:current_syntax = b:current_syntax
    unlet b:current_syntax

    syntax include @CPP syntax/cpp.vim
    syntax include @CPP after/syntax/cpp.vim

    syntax region texZone
                \ start="\\begin{cppcode}"
                \ end="\\end{cppcode}"
                \ contains=@CPP,texBeginEnd
                \ keepend
                " \ transparent
    hi link Snip SpeicalComment
endfunction

augroup cmds
    au!
    " email buffer
    au BufRead,BufNewFile *mutt-* setf mail
    " error format for clang
    au FileType c,cpp setl errorformat=%f:%l:%c:\ %t%s:\ %m
    " au FileType tex call s:latex_setup()
augroup END

" insert a hashbang for the current filetype
inoreabbrev <expr>#!! '#!/usr/bin/env' . (empty(&filetype) ? '' : ' '.&filetype)

" --------------------
" plugin configuration
" --------------------

" ------------
" vim-polyglot
" ------------
if exists('g:plugs["vim-polyglot"]')
    let php_sql_query = 1
    let php_sql_heredoc = 1
    let php_html_load = 1
    let php_html_in_strings = 1
    let php_html_in_heredoc = 1

    let g:polyglot_disabled = ['rust', 'tex', 'markdown', 'haskell', 'lhaskell']
endif

" -----
" ctrlp
" -----
if exists('g:plugs["ctrlp.vim"]')
    if executable('ag')
        let g:ctrlp_user_command = 'ag &s -l --nocolor -g ""'
        let g:ctrlp_use_caching = 0
    endif

    let g:ctrlp_match_func = { 'match': 'cpsm#CtrlPMatch' }
    let g:ctrlp_extensions = ['tag', 'line']

    let g:ctrlp_working_path_mode = 'ca'
    let g:ctrlp_open_new_file = 'h'
    let g:ctrlp_match_window = 'bottom,order:btt,min:1,max:10,results:10'
endif

" ------
" tagbar
" ------
if exists('g:plugs["tagbar"]')
    " change focus to tagbar when opening it
    let g:tagbar_autofocus = 1
    let g:tagbar_autoclose = 1

    " toggle tagbar
    nnoremap <F2> :TagbarToggle<CR>
endif

" ------
" vimtex
" ------
if exists('g:plugs["vimtex"]')
    let g:tex_flavor = 'latex'

    let g:vimtex_indent_enabled = 0
    let g:vimtex_latexmk_enabled = 1
    let g:vimtex_complete_enabled = 1
    let g:vimtex_complete_close_braces = 1
    let g:vimtex_fold_enabled = 1

    " don't open documents in viewer by default
    let g:vimtex_view_enabled = 0

    " enable vimtex mappings
    let g:vimtex_mappings_enabled = 1
endif

" -------------
" vim-lightline
" -------------
if exists('g:plugs["lightline.vim"]')
    let g:lightline = { 'colorscheme': 'base16' }
endif

" ----------
" vim-dirvish
" ----------
if exists('g:plugs["vim-dirvish"]')
    augroup dirvish_events
        au!
        " don't show hidden files
        au FileType dirvish silent keeppatterns g@\v/\.[^\/]+/?$@d

        " list directories first
        au FileType dirvish silent :sort r /[^\/]$/

        " Enable :Gstatus and friends
        au FileType dirvish call fugitive#detect(@%)
        au FileType dirvish nnoremap <silent><buffer> gr :<C-u>Dirvish %<CR>
        au FileType dirvish nnoremap <silent><buffer>
                    \ gh :silent keeppatterns g@\v/\.[^\/]+/?$@d<CR>
    augroup END
endif

" -----------
" haskell-vim
" -----------
if exists('g:plugs["haskell-vim"]')
    let g:haskell_enable_quantification     = 1
    let g:haskell_enable_recursivedo        = 1
    let g:haskell_enable_arrowsyntax        = 1
    let g:haskell_enable_pattern_synonyms   = 1
    let g:haskell_enable_typeroles          = 1
    let g:haskell_enable_static_pointers    = 1
endif

" --------
" deoplete
" --------
if exists('g:plugs["deoplete.nvim"]')
    " make sure input_patterns exists
    if !exists('g:deoplete#omni#input_patterns')
        let g:deoplete#omni#input_patterns = {}
    endif

    " make sure keyword_patterns exists
    if !exists('g:deoplete#keyword_patterns')
        let g:deoplete#keyword_patterns = {}
    endif
    
    " make sure sources exists
    if !exists('g:deoplete#sources')
        let g:deoplete#sources = {}
    endif

    " make sure omni_patterns exists
    if !exists('g:deoplete#omni_patterns')
        let g:deoplete#omni_patterns = {}
    endif

    " use deoplete
    let g:deoplete#enable_at_startup = 1

    " use smartcase
    let g:deoplete#enable_smart_case = 1
    
    " compatibility with snipmate snippets
    let g:neosnippet#enable_snipmate_compatibility = 1

    " Use auto delimiter feature
    call deoplete#custom#set('_', 'converters', ['converter_auto_delimiter', 'remove_overlap'])

    " omnifuncs
    augroup deoplete_autocmds
        au!
        au FileType css setlocal omnifunc=csscomplete#CompleteCSS
        au FileType html,markdown setlocal omnifunc=htmlcomplete#CompleteTags
        au FileType xml setlocal omnifunc=xmlcomplete#CompleteTags
    augroup END

    " C/C++ completion
    if exists('g:plugs["deoplete-clang"]')
        let g:deoplete#sources#clang#libclang_path = '/usr/lib/libclang.so'
        let g:deoplete#sources#clang#clang_header = '/usr/lib/include/clang'
        let g:deoplete#sources#clang#std = {'cpp': 'c++11', 'c': 'c11' }
        let g:deoplete#sources#clang#sort_algo = 'priority'
    endif

    " JavaScript completion
    if exists('g:plugs["deoplete-ternjs"]')
        let g:tern_request_timeout = 1
    endif

    " Rust completion
    if exists('g:plug["deoplete-rust"]')
        let g:deoplete#sources#rust#racer_binary = '/home/alex/.cargo/bin/racer'
        let g:deoplete#sources#rust#rust_source_path = '/home/alex/src/rust/src'
    endif

    " Python completion
    if exists('g:plugs["deoplete-jedi"]')
        " enable caching
        let g:deoplete#sources#jedi#enable_cache = 1

        " show docstring in preview window
        let g:deoplete#sources#jedi#show_docstring = 1

        " path to python binary
        if has('unix')
            let g:deoplete#sources#jedi#python_path = "/usr/bin/python3"
        elseif has('win32') || has('win64')
            let g:deoplete#sources#jedi#python_path = "C:\\Program Files\\Python35\\python.exe"
        endif
    endif

    " rtags completion
    if exists('g:plugs["deoplete-rtags"]')
        " rtags
    endif

    " tmux completion
    if exists('g:plugs["tmux-complete.vim"]')
        let g:tmuxcomplete#trigger = ''
    endif

    " " Git completion
    " if exists('g:plugs["deoplete-github"]')
    "     " setup sources and keyword patterns
    "     let g:deoplete#sources.gitcommit = ['github']
    "     let g:deoplete#keyword_patterns.gitcommit = '.+'

    "     call deoplete#util#set_pattern(
    "                 \ g:deoplete#omni#input_patterns,
    "                 \ 'gitcommit', [g:deoplete#keyword_patterns.gitcommit])
    " endif


    " Go completion
    if exists('g:plugs["deoplete-go"]')
        let g:deoplete#sources#go#gocode_binary = "/home/alex/src/goprojects/bin/gocode" "
    endif

    " Haskell completion
    if exists('g:plugs["neco-ghc"]')
        " use necoghc's omnifunc for completion
        augroup haskell
            au!
            au FileType haskell setlocal omnifunc=necoghc#omnifunc
        augroup END
        
        " show detailed type information
        let g:necoghc_enable_detailed_browse = 1
    endif

    " LaTeX completionn
    let g:deoplete#omni_patterns.tex =
                \ '\v\\%('
                \ . '\a*cite\a*%(\s*\[[^]]*\]){0,2}\s*\{[^}]*'
                \ . '|\a*ref%(\s*\{[^}]*|range\s*\{[^,}]*%(}\{)?)'
                \ . '|hyperref\s*\[[^]]*'
                \ . '|includegraphics\*?%(\s*\[[^]]*\]){0,2}\s*\{[^}]*'
                \ . '|%(include%(only)?|input)\s*\{[^}]*'
                \ . ')\m'
endif


" -------
" ack.vim
" -------
if exists('g:plugs["ack.vim"]')
    let g:ackpg = 'ag --vimgrep'
endif

if exists('g:plugs["undotree"]')
    nnoremap <F3> :UndotreeToggle<cr>
endif

if exists('g:plugs["ctrlp.vim"]')
    nnoremap <silent><C-b> :<C-U>CtrlPBuffer<cr>
    nnoremap <silent><C-t> :<C-u>CtrlPTag<cr>
endif

if exists('g:plugs["echodoc.vim"]')
    let g:echodoc_enable_at_startup = 1
endif

if exists('g:plugs["vim-markdown"]')
    let g:markdown_fenced_languages = ['cpp', 'python', 'bash=sh']
endif
