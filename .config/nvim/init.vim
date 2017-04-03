" init.vim

" use the comma key as <leader>
let mapleader = ','

" use the backslash key as <localleader>
let maplocalleader = '\\'

" -------
" Plugins
" -------
if has('nvim')
    call plug#begin('~/.local/share/nvim/plugged')
else
    call plug#begin('~/.vim/plugged')
endif
    " color schemes
    Plug 'chriskempson/base16-vim'
    Plug 'w0ng/vim-hybrid'
    Plug 'mhinz/vim-janah'
    Plug 'jacoborus/tender.vim'

    Plug 'sheerun/vim-polyglot'
    Plug 'nickhutchinson/vim-cmake-syntax', { 'for': ['cmake'] }

    " <3 tpope
    Plug 'tpope/vim-git'
    Plug 'tpope/vim-repeat'
    Plug 'tpope/vim-dispatch'
    Plug 'tpope/vim-fugitive'
    Plug 'tpope/vim-surround'
    Plug 'tpope/vim-markdown'
    Plug 'tpope/vim-dispatch'
    Plug 'tpope/vim-unimpaired'
    Plug 'tpope/vim-commentary'
    Plug 'tpope/vim-speeddating'
    Plug 'tpope/vim-projectionist'

    Plug 'mattn/calendar-vim'
    Plug 'chrisbra/NrrwRgn'
    Plug 'vim-scripts/SyntaxRange'
    Plug 'editorconfig/editorconfig-vim'

    Plug 'godlygeek/csapprox',  { 'for': 'fugitiveblame' }
    Plug 'Valloric/ListToggle'

    " this thing is cool as fuck
    Plug 'metakirby5/codi.vim'

    Plug 'AndrewRadev/splitjoin.vim'
    Plug 'AndrewRadev/linediff.vim'
    Plug 'scrooloose/nerdtree'

    Plug 'ctrlpvim/ctrlp.vim'
    Plug 'tacahiroy/ctrlp-funky'
    Plug 'FelikZ/ctrlp-py-matcher'

    Plug 'bling/vim-bufferline'
    Plug 'edkolev/tmuxline.vim'

    " Plug 'vim-airline/vim-airline'
    " Plug 'vim-airline/vim-airline-themes'

    Plug 'itchyny/lightline.vim'
    Plug 'cocopon/lightline-hybrid.vim'

    Plug 'mbbill/undotree',     { 'on': 'UndotreeToggle' }
    Plug 'godlygeek/tabular',   { 'on': ['Tab', 'Tabularize'] }

    Plug 'lervag/vimtex'
    Plug 'rust-lang/rust.vim'
    Plug 'neovimhaskell/haskell-vim'

    " TODO: find out why this breaks shit
    " Plug 'critiqjo/lldb.nvim'

    " Plug 'w0rp/ale'
    " Plug 'jceb/vim-orgmode'
    " Plug 'dhruvasagar/vim-table-mode'
    " Plug 'justinmk/vim-dirvish'

    if executable('ctags')
        Plug 'ludovicchabant/vim-gutentags'
    endif

    if executable('nvim')
        Plug 'equalsraf/neovim-gui-shim'
    endif

    if executable('python') || executable('python3')
        Plug 'jmcantrell/vim-virtualenv'
    endif
    
    " only install if the silver searcher, ack, or ripgrep are installed
    if executable('ag') || executable('ack') || executable('rg')
        Plug 'mileszs/ack.vim'
    endif

    if has('nvim')
        Plug 'neomake/neomake'
    endif

    if executable('git')
        Plug 'gregsexton/gitv'
        Plug 'jreybert/vimagit'
        Plug 'junegunn/gv.vim'
        Plug 'junegunn/vim-github-dashboard', { 'on': ['GHDashboard', 'GHActivity']      }
    endif

    " this doesn't like to install properly on Windows for some reason
    if !has('win32') || !has('win64')
        Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
    endif

    if executable('ctags')
        Plug 'majutsushi/tagbar'
    endif

    " if !exists('g:plugs["deoplete.nvim"]')
    "     Plug 'Valloric/YouCompleteMe' | Plug 'rdnetto/YCM-Generator', { 'branch': 'stable' }
    " endif

    if has('nvim') && executable('rustc') && !exists('g:plugs["YouCompleteMe"]')
        function! BuildComposer(info)
            if a:info.status != 'unchanged' || a:info.force
                !cargo build --release
            endif
        endfunction

        Plug 'euclio/vim-markdown-composer', { 
                    \   'do': function('BuildComposer'),
                    \   'on': ['ComposerStart', 'ComposerUpdate', 'ComposerOpen', 'ComposerJob'] 
                    \ }
    endif

    if has('nvim')
        Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
        Plug 'Shougo/neosnippet.vim' | Plug 'Shougo/neosnippet-snippets'
        Plug 'Shougo/context_filetype.vim'

        " " includes completion
        Plug 'Shougo/neoinclude.vim'

        " vim completion
        Plug 'Shougo/neco-vim', { 'for': ['vim'] }
        Plug 'Shougo/neco-syntax', { 'for': ['vim'] }

        if executable('tmux')
            Plug 'wellle/tmux-complete.vim'
        endif
        
        if executable('go') || executable('gccgo')
            Plug 'zchee/deoplete-go'
        endif

        if executable('clang')
            Plug 'zchee/deoplete-clang'
        endif

        if executable('python') || executable('python3')
            Plug 'zchee/deoplete-jedi'
        endif

        if executable('ghc')
            Plug 'eagletmt/neco-ghc'
        endif

        if executable('node')
            Plug 'carlitux/deoplete-ternjs'
        endif

        if executable('ruby')
            Plug 'fishbullet/deoplete-ruby'
        endif

        if executable('zsh')
            Plug 'zchee/deoplete-zsh'
        endif

        if executable('rustc') && executable('cargo')
            Plug 'sebastianmarkow/deoplete-rust'
        endif
    endif
call plug#end()

" ------------------------
"  Setup and Configuration
" ------------------------

" set the background type (light/dark) before activating a colorscheme
set background=dark

if filereadable(expand("~/.vimrc_background"))
    let base16colorspace=256
    source ~/.vimrc_background
endif

silent! colorscheme base16-tomorrow-night

" nvim-specific stuff
if has('nvim')
    let g:python3_host_prog = '/usr/bin/python3'
    let g:python_host_prog = '/usr/bin/python2'

    if exists('&inccommand')
        set inccommand=split
        " set inccommand=nosplit
    endif

    if exists('&termguicolors')
        set termguicolors
    endif

    tnoremap <Esc> <C-\><C-n>
endif

if executable("rg")
    set grepprg=rg\ --vimgrep\ --no-heading
    set grepformat=%f:%l:%c:%m,%f:%l:%m
elseif executable ('ag')
    let grepprg=ag\ --vimgrep
    set grepformat=%f:%l:%c:%m,%f:%l:%m
endif

" silly windows, not having a real shell
if has('win32') || has('win64')
    set shell=cmd.exe
    set shellcmdflag=/c
    set encoding=utf-8
endif

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
" set formatoptions=tcqj
set formatoptions=tqj

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
set wildoptions=tagfile

" scrolling
set scrolloff=10

" keep 15 characters space to the sides
set sidescrolloff=15
set sidescroll=1

" shorten messages
" set shortmess=aIT
set shortmess=aoOTI

" " command line height
" set cmdheight=2

" don't show the current mode (insert/normal/etc) below the status line
set noshowmode

" persistent undo
set undofile
set undolevels=5000

set tags=./tags;/
set breakindent
set copyindent
set preserveindent
set smartindent
set switchbuf=useopen
set lazyredraw
set laststatus=2

" --------
" Commands
" --------
" :Grep <keyword>
command! -nargs=1 -bar Grep execute 'silent! grep! <q-args>' | redraw! | copen

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

nnoremap <F5> :let _s=@/<Bar>:%s/\s\+$//e<Bar>:let @/=_s<Bar><CR>

" kill the current buffer
nnoremap <silent><leader>K :bdelete<cr>

" [b]uild [p]roject
nnoremap <leader>bp :Neomake<CR>

" insert a hashbang for the current filetype
inoreabbrev <expr>#!! '#!/usr/bin/env' . (empty(&filetype) ? '' : ' '.&filetype)

" --------
" autocmds
" --------
" autocmd FileType c,cpp setlocal errorformat=%f:%l:%c:\ %t%s:\ %m
" autocmd FileType haskell,lhaskell  setlocal omnifunc=necoghc#omnifunc

" ammend commit
autocmd FileType gitcommit nnoremap <buffer> <silent> <leader>cA :<C-U>Gcommit --amend --date="$(date)"<CR>

" autocmd FileType markdown setlocal tw=80
autocmd FileType cmake setlocal commentstring=#\ %s

autocmd BufReadPost fugitive:// setlocal bufhidden=delete

" for some reason polyglot sets it to javascript.jsx and ignores
" g:polyglot_disabled, so use an autocmd to set the correct filetype
au FileType javascript.jsx setl ft=javascript


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

    " prefer ripgrep and if that's not available, try the silver searcher
    if executable('rg')
        let s:ctrlp_command = 'rg %s --files --color=never --glob ""'
        let g:ctrlp_use_caching = 0
    elseif executable('ag')
        let s:ctrlp_command = 'ag %s -l --nocolor -g ""'
        let g:ctrlp_use_caching = 0
    else
        s:ctrlp_command = 'find %s -type f'
    endif

    " what commands ctrlp should use to find files
    let g:ctrlp_user_command = {
                \ 'types': {
                \ 1: ['.git', 'cd %s && git ls-files -co --exclude-standard'],
                \ 2: ['.hg', 'hg --cwd %s locate -I .'],
                \ },
                \ 'fallback': s:ctrlp_command
                \ }

    let g:ctrlp_match_func = { 'match': 'pymatcher#PyMatch' }

    " what ctrlp extensions to load
    let g:ctrlp_extensions = [ 
                \ 'tag', 'buffertag', 'quickfix', 'dir', 'changes',
                \ 'line', 'mixed'
                \ ]

    " default binding for CtrlP
    let g:ctrlp_map = '<C-p>'
    let g:ctrlp_cmd = 'CtrlP'

    " I symlink a lot of files (such as my dotfiles), so make sure to follow
    " those
    let g:ctrlp_follow_symlinks = 1

    " don't show hidden files
    let g:ctrlp_show_hidden = 0

    " maximum depth of a directory tree to search
    let g:ctrlp_max_depth = 32

    " set local directory to the directory of the currently open file, unless
    " it's the subdirectory of vim's current working dir.
    let g:ctrlp_working_path_mode = 'ca'

    " open new files in horizontal splits
    let g:ctrlp_open_new_file = 'h'

    " jump to buffer if file is a already open.
    let g:ctrlp_switch_buffer = 'et'

    " windows for ctrlp to reuse if possible
    let g:ctrlp_reuse_window = 'netrw\|help\|quickfix'

    " match window settings 
    let g:ctrlp_match_window = 'bottom,order:tt,min:1,max:10,results:10'

    " show $HOME as ~
    let g:ctrlp_tilde_home = 1 

    " " only show recent files in the current working dir
    " let g:ctrlp_mruf_relative = 1

    " mappings 
    nnoremap <C-b> :CtrlPBuffer<CR>
    nnoremap <leader>rf :CtrlPMRUFiles<CR>
    nnoremap <leader>li :CtrlPLine<CR>
    nnoremap <leader>t :CtrlPTag<CR>

    if exists('g:plugs["ctrlp-funky"]')
        let g:ctrlp_funky_matchtype = 'path'

        " search across multiple buffers
        let g:ctrlp_funky_multi_buffers = 1

        " set filetype for match window
        let g:ctrlp_funky_syntax_highlight = 1

        " mappings
        nnoremap <leader>fu :CtrlPFunky<CR>
    endif

endif

" ------
" tagbar
" ------
if exists('g:plugs["tagbar"]')
    " change focus to tagbar when opening it
    let g:tagbar_autofocus = 1
    let g:tagbar_autoclose = 1

    let g:tagbar_ctags_bin="/usr/bin/ctags"

    let g:tagbar_type_haskell = {
                \   'ctagsbin'  : 'hasktags',
                \   'ctagsargs' : '-x -c -o-',
                \   'kinds'     : [
                \       'm:modules:0:1',
                \       'd:data: 0:1',
                \       'd_gadt: data gadt:0:1',
                \       't:type names:0:1',
                \       'nt:new types:0:1',
                \       'c:classes:0:1',
                \       'cons:constructors:1:1',
                \       'c_gadt:constructor gadt:1:1',
                \       'c_a:constructor accessors:1:1',
                \       'ft:function types:1:1',
                \       'fi:function implementations:0:1',
                \       'o:others:0:1'
                \   ],
                \   'sro'        : '.',
                \   'kind2scope' : {
                \       'm' : 'module',
                \       'c' : 'class',
                \       'd' : 'data',
                \       't' : 'type'
                \   },
                \   'scope2kind' : {
                \       'module' : 'm',
                \       'class'  : 'c',
                \       'data'   : 'd',
                \       'type'   : 't'
                \   }
                \ }

    let g:tagbar_type_markdown = {
                \ 'ctagstype' : 'markdown',
                \ 'kinds' : [
                \   'h:Heading_L1',
                \   'i:Heading_L2',
                \   'k:Heading_L3'
                \   ]
                \ }

    let g:tagbar_type_make = {
                \   'kinds': [
                \       'm:macros',
                \       't:targets'
                \   ]
                \ }

    " toggle tagbar
    nnoremap <F2> :TagbarToggle<CR>
endif

" ------
" vimtex
" ------
if exists('g:plugs["vimtex"]')
    let g:tex_flavor = 'latex'

    " enable latexmk compilation
    " let g:vimtex_build_dir = './build'
    let g:vimtex_latexmk_enabled = 1

    " let g:vimtex_view_method = 'zathura'
    let g:vimtex_view_enabled = 0

    let g:vimtex_indent_enabled = 0
    let g:vimtex_complete_enabled = 1
    let g:vimtex_complete_close_braces = 1
    let g:vimtex_fold_enabled = 1

    " " enable vimtex mappings
    " let g:vimtex_mappings_enabled = 1
endif

" -------------
" vim-lightline
" -------------
if exists('g:plugs["lightline.vim"]')
    let g:bufferline_echo = 0

    let g:bufferline_active_buffer_let = ''
    let g:bufferline_active_buffer_right = ''
    let g:bufferline_show_bufnr = 0
    let g:bufferline_fname_mod = ':~:.'
    let g:bufferline_pathshorten = 1

    " TODO: make this work better
    let g:lightline = {
                \   'active': {
                \       'left': [ [ 'mode', 'paste' ],
                \                 [ 'readonly', 'filename', 'modified' ],
                \                 [ 'ctrlpmark' ] ],
                \   },
                \   'component_function': {
                \       'readonly': 'StatuslineReadonly',
                \       'ctrlpmark': 'StatuslineCtrlP'
                \   },
                \   'component_expand': {
                \       'tabs': 'lightline#tabs' 
                \   },
                \   'enabled': {
                \       'statusline': 1,
                \       'tabline':1
                \   },
                \   'separator': {
                \       'left': '',
                \       'right': ''
                \   },
                \ }


    function! StatuslineReadonly()
        return &ft !~? 'help' && &readonly ? '⭤' : ''
    endfunction

    function! StatuslineCtrlP()
        if expand('%:t') =~ 'ControlP' && has_key(g:lightline, 'ctrlp_item')
            call lightline#link('iR'[g:lightline.ctrlp_regex])
            return lightline#concatenate([g:lightline.ctrlp_prev, g:lightline.ctrlp_item
                        \ , g:lightline.ctrlp_next], 0)
        else
            return ''
        endif
    endfunction

    let g:ctrlp_status_func = {
                \   'main': 'CtrlPStatusFunc_1',
                \   'prog': 'CtrlPStatusFunc_2',
                \ }

    function! CtrlPStatusFunc_1(focus, byfname, regex, prev, item, next, marked)
        let g:lightline.ctrlp_regex = a:regex
        let g:lightline.ctrlp_prev = a:prev
        let g:lightline.ctrlp_item = a:item
        let g:lightline.ctrlp_next = a:next
        return lightline#statusline(0)
    endfunction

    function! CtrlPStatusFunc_2(str)
        return lightline#statusline(0)
    endfunction
endif

" ----------
" vim-dirvish
" ----------
" if exists('g:plugs["vim-dirvish"]')
" endif

" ------------
" vim-surround
" ------------

if exists('g:plugs["vim-surround"]')
    let g:surround_{char2nr('-')} = "<% \r %>"
    let g:surround_{char2nr('=')} = "<%= \r %>"
    let g:surround_{char2nr('8')} = "/* \r */"
    let g:surround_{char2nr('s')} = " \r"
    let g:surround_{char2nr('^')} = "/^\r$/"

    let g:surround_108 = "\\begin{\1environment: \1}\r\\end{\1\r}.*\r\1}"

    let g:surround_indent = 1
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

    " Be 'ye warned: Here be dragons.
    if !exists('g:deoplete#omni#input_patterns')
        let g:deoplete#omni#input_patterns = {}
    endif
    if !exists('g:deoplete#omni_patterns')
        let g:deoplete#omni_patterns = {}
    endif

    if !exists('g:deoplete#omni#functions')
        let g:deoplete#omni#functions = {}
    endif

    if !exists('g:deoplete#keyword_patterns')
        let g:deoplete#keyword_patterns = {}
    endif

    if !exists('g:deoplete#sources')
        let g:deoplete#sources = {}
    endif

    if !exists('g:deoplete#ignore_sources')
        let g:deoplete#ignore_sources = {}
    endif

    " use deoplete
    let g:deoplete#enable_at_startup = 1

    " use smartcase
    let g:deoplete#enable_smart_case = 1

    " skip the host check
    let g:deoplete#host_skip_check = 1

    " omni funcs
    let g:deoplete#omni#functions.html = 'htmlcomplete#CompleteTags'
    let g:deoplete#omni#functions.css  = 'csscomplete#CompleteCSS'
    let g:deoplete#omni#functions.xml  = 'xmlcomplete#CompleteTags'

    " LaTeX completionn
    let g:deoplete#omni_patterns.tex =
                \ '\v\\%('
                \ . '\a*cite\a*%(\s*\[[^]]*\]){0,2}\s*\{[^}]*'
                \ . '|\a*ref%(\s*\{[^}]*|range\s*\{[^,}]*%(}\{)?)'
                \ . '|hyperref\s*\[[^]]*'
                \ . '|includegraphics\*?%(\s*\[[^]]*\]){0,2}\s*\{[^}]*'
                \ . '|%(include%(only)?|input)\s*\{[^}]*'
                \ . ')\m'

    call deoplete#custom#set('_', 'converters', [
                \ 'converter_remove_paren',
                \ 'converter_remove_overlap',
                \ 'converter_truncate_abbr',
                \ 'converter_truncate_menu',
                \ 'converter_auto_delimiter',
                \ ])


    call deoplete#custom#set('ghc', 'sorters', ['sorter_word'])
    call deoplete#custom#set('clang', 'input_pattern', '\.\w*|\.->\w*|\w+::\w*')
    call deoplete#custom#set('clang', 'max_pattern_length', -1)

    imap <C-k> <Plug>(neosnippet_expand_or_jump)

    " C/C++ completion
    if exists('g:plugs["deoplete-clang"]')
        let g:deoplete#sources#clang#libclang_path = '/usr/lib/libclang.so'
        let g:deoplete#sources#clang#clang_header = '/usr/lib/clang/3.9.1/include'
        let g:deoplete#sources#clang#std = {'cpp': 'c++11', 'c': 'c11' }
        let g:deoplete#sources#clang#sort_algo = 'priority'
    endif

    " JavaScript completion
    if exists('g:plugs["deoplete-ternjs"]')
        let g:tern_request_timeout = 1
        let g:deoplete#omni#functions.javascript = 'tern#Complete'
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

        " holy fuck why does this break everything?
        " " path to python binary
        " if has('unix')
        "     let g:deoplete#sources#jedi#python_path = "/usr/bin/python3"
        "     " let g:deoplete#sources#jedi#python_path = "/usr/bin/python2"
        " elseif has('win32') || has('win64')
        "     let g:deoplete#sources#jedi#python_path = "C:\\Program Files\\Python35\\python.exe"
        " endif
    endif

    " rtags completion
    if exists('g:plugs["deoplete-rtags"]')
        " rtags
    endif

    " tmux completion
    if exists('g:plugs["tmux-complete.vim"]')
        let g:tmuxcomplete#trigger = ''
    endif


    " Go completion
    " if exists('g:plugs["deoplete-go"]')
    "     let g:deoplete#sources#go#gocode_binary = "/home/alex/src/goprojects/bin/gocode" "
    " endif

    " Haskell completion
    if exists('g:plugs["neco-ghc"]')
        let g:haskellmode_completion_ghc = 0
        let g:deoplete#omni#functions.haskell = 'necoghc#omnifunc'
        let g:deoplete#omni#functions.lhaskell = 'necoghc#omnifunc'

        " show detailed type information
        let g:necoghc_enable_detailed_browse = 1
    endif
endif

" -------------
" vim-gutentags
" -------------

if exists('g:plugs["vim-gutentags"]')
    if executable('hasktags')
        let g:gutentags_ctags_executable_haskell = 'hasktags'
    endif
endif

" -------
" ack.vim
" -------
if exists('g:plugs["ack.vim"]')
    " TODO: add support for the silver searcher and ack if ripgrep is not
    " available, possibly falling back to grep if none of the three are
    if executable('rg')
        let g:ackpreg = 'rg --vimgrep --no-heading'
    endif
endif

" --------
" undotree
" --------
if exists('g:plugs["undotree"]')
    let g:undotree_WindowLayout = 2
    nnoremap U :UndotreeToggle<CR>
endif

" -------
" echodoc.
" -------
if exists('g:plugs["echodoc.vim"]')
    let g:echodoc_enable_at_startup = 1
endif

" ------------
" vim-markdown
" ------------
if exists('g:plugs["vim-markdown"]')
    let g:markdown_fenced_languages = ['cpp', 'python', 'bash=sh']
endif

" --------------------
" vim-github-dashboard
" --------------------
if exists('g:plugs["vim-github-dashboard"]')
    let g:github_dashboard = { 'username': 'zyeri' }
endif


" ----------
" ListToggle
" ----------
if exists('g:plugs["ListToggle"]')
    " TODO: remember that I have these mappings, damn it
    let g:lt_location_list_toggle_map = '<leader>ll'
    let g:lt_quickfix_list_toggle_map = '<leader>qf'
endif

" -------
" neomake
" -------
if exists('g:plugs["neomake"]')
    let g:neomake_echo_current_error = 1
    let g:neomake_open_list = 2

    let g:neomake_make_maker = {
                \   'exe': 'make',
                \   'args': ['--build'],
                \   'errorformat': '%f:%l:%c: %m',
                \ }

    let g:neomake_makeclean_maker = {
                \   'exe': 'make',
                \   'args': ['clean']
                \ }

    let g:neomake_cpp_maker = {
                \ 'args': ['-fsyntax-only', '-Wall', '-Wextra'],
                \ 'errorformat':
                \ '%-G%f:%s:,' .
                \ '%-G%f:%l: %#error: %#(Each undeclared identifier is reported only%.%#,' .
                \ '%-G%f:%l: %#error: %#for each function it appears%.%#,' .
                \ '%-GIn file included%.%#,' .
                \ '%-G %#from %f:%l\,,' .
                \ '%f:%l:%c: %trror: %m,' .
                \ '%f:%l:%c: %tarning: %m,' .
                \ '%I%f:%l:%c: note: %m,' .
                \ '%f:%l:%c: %m,' .
                \ '%f:%l: %trror: %m,' .
                \ '%f:%l: %tarning: %m,'.
                \ '%I%f:%l: note: %m,'.
                \ '%f:%l: %m',
                \ }
    let g:neomake_cpp_enabled_makers = [ 'gcc' ]

endif

" -----------
" vim-airline
" -----------
if exists('g:plugs["vim-airline"]')
    let g:airline_theme='hybrid'
    let g:airline_powerline_fonts = 1

    " collapse inactive buffer sectionsa
    let g:airline_inactive_collapse = 1
endif

" --------
" nerdtree
" --------
if exists('g:plugs["nerdtree"]')
    nnoremap <C-\> :NERDTreeToggle<CR>
    inoremap <C-\> <Esc>:NERDTreeToggle<CR>

    autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif
endif

" ---------
"  codi.vim
" ---------

if exists('g:plugs["codi.vim"]') 

    " use python 3
    let g:codi#interpreters = {
                \   'python': {
                \       'bin': 'python3',
                \       'prompt': '^\(>>>\|\.\.\.\) ',
                \   },
                \ }

    " left-align the codi buffer
    let g:codi#rightalign = 0

endif

if exists('g:plugs["vim-markdown-composer"]')
    let g:markdown_composer_browser = '/usr/bin/google-chrome-stable'
    let g:markdown_composer_open_browser = 0
    let g:markdown_composer_syntax_theme = 'tomorrow-night'
    let g:markdown_composer_autostart = 0
endif

if exists('g:plugs["vim-pandoc"]')
    let g:pandoc#modules#disabled = ['folding', 'formatting']

    " if exists('g:plugs["vim-pandoc-after"]')
    "     let g:pandoc#after#modules#enabled = ['nrrwrgn', 'neosnippet', 'tablemode'] 
    " endif
endif

if exists('g:plugs["vim-instant-markdown"]')
    let g:instant_markdown_autostart = 0
endif

if exists('g:plugs["editorconfig-vim"]')
    let g:EditorConfig_exclude_patterns = ['fugitive://.*', 'scp://.*']
endif
