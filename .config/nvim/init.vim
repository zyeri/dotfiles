" -*- mode: vim; coding: utf-8; -*-
" vim: filetype=vim foldenable foldmethod=marker foldlevel=2 textwidth=0

" Platform detection {{{

" this needs to be set early for platform detection while loading
" plugins
if !exists('s:platform')
    if has('win64') || has('win32')
        let s:platform = 'windows'
    else
        let s:platform = tolower(substitute(system('uname'), '\n', '', ''))
    endif
endif

if s:platform ==? 'linux'
    let g:python_host_prog = "/usr/bin/python2"
    let g:python3_host_prog = "/usr/bin/python"
endif

" }}}
" Plugins {{{

call plug#begin('~/.config/nvim/plugged')
    Plug 'joshdick/onedark.vim'
    Plug 'MaxSt/FlatColor'
    Plug 'mhinz/vim-janah'
    Plug 'chriskempson/base16-vim'
    Plug 'KeitaNakamura/neodark.vim'
    Plug 'rakr/vim-one'

    " <3 tpope
    Plug 'tpope/vim-git'
    Plug 'tpope/vim-repeat'
    Plug 'tpope/vim-fugitive'
    Plug 'tpope/vim-surround'
    Plug 'tpope/vim-markdown'
    Plug 'tpope/vim-unimpaired'
    Plug 'tpope/vim-commentary'
    Plug 'tpope/vim-speeddating'

    Plug 'mbbill/undotree'
    Plug 'godlygeek/tabular'
    Plug 'justinmk/vim-dirvish'
    Plug 'google/vim-searchindex'
    Plug 'editorconfig/editorconfig-vim'
    Plug 'dhruvasagar/vim-table-mode'
    Plug 'kana/vim-textobj-user'

    if has('nvim')
        Plug 'equalsraf/neovim-gui-shim'
    endif

    Plug 'tweekmonster/helpful.vim'
    Plug 'tweekmonster/startuptime.vim'

    " if has('nvim') && s:platform ==? 'linux'
    " "     Plug 'Shougo/denite.nvim', { 'do': ':UpdateRemotePlugins' }
    "     Plug 'nixprime/cpsm'
    " endif

    Plug 'ctrlpvim/ctrlp.vim'
    Plug 'FelikZ/ctrlp-py-matcher'
    Plug 'tacahiroy/ctrlp-funky'

    Plug 'mileszs/ack.vim'
    Plug 'majutsushi/tagbar'
    Plug 'romainl/vim-qf'
    " Plug 'mtth/scratch.vim'
    " Plug 'airblade/vim-gitgutter'
    " Plug 'ludovicchabant/vim-gutentags'
    Plug 'junegunn/gv.vim'
    " Plug 'jreybert/vimagit'
    " Plug 'jceb/vim-orgmode'
    " Plug 'chrisbra/nrrwrgn'
    " Plug 'jmcantrell/vim-virtualenv'
    " Plug 'gregsexton/gitv'
    " Plug 'mhinz/vim-startify'
    " Plug 'unblevable/quick-scope'

    " Plug 'vim-airline/vim-airline' | Plug 'vim-airline/vim-airline-themes'
    " Plug 'itchyny/lightline.vim'

    if has('nvim')
        " Plug 'neomake/neomake'
        " Plug 'w0rp/ale'

        " completion
        Plug 'roxma/python-support.nvim'
        Plug 'roxma/nvim-completion-manager'
        " Plug 'roxma/ncm-github'
        " Plug 'roxma/nvim-cm-tern', { 'do': 'npm install' }

        Plug 'fgrsnau/ncm-otherbuf'
        Plug 'Shougo/neco-syntax'
        Plug 'Shougo/neoinclude.vim'
        " Plug 'mhartington/nvim-typescript', { 'do': 'npm install typescript' }

        Plug 'SirVer/ultisnips' | Plug 'honza/vim-snippets'

        " Rust
        Plug 'rust-lang/rust.vim'
        Plug 'racer-rust/vim-racer'
        Plug 'roxma/nvim-cm-racer'

        " Python
        " Plug 'davidhalter/jedi-vim'
        " Plug 'tell-k/vim-autopep8'
        " Plug 'jmcantrell/vim-virtualenv'

        " C/C++
        " Plug 'roxma/clang_complete'
        Plug 'roxma/ncm-clang'
    endif

    " Plug 'roxma/vim-tmux-clipboard'
    " Plug 'tmux-plugins/vim-tmux-focus-events'
    " Plug 'whatyouhide/vim-tmux-syntax'

    Plug 'othree/html5.vim'
    Plug 'mattn/emmet-vim'
    Plug 'chr4/nginx.vim'

    " Plug 'Shougo/neco-vim'
    Plug 'lervag/vimtex'
    Plug 'neovimhaskell/haskell-vim'
    Plug 'chrisbra/csv.vim'

    " Plug 'veegee/vim-pic'

    Plug 'pearofducks/ansible-vim'
    Plug 'lifepillar/pgsql.vim'
    " Plug 'leafgarland/typescript-vim'
    Plug 'vim-pandoc/vim-pandoc'
    Plug 'vim-pandoc/vim-pandoc-syntax'
    Plug 'jparise/vim-graphql'
    " Plug 'kovetskiy/sxhkd-vim'
    " Plug 'mboughaba/i3config.vim'
    " Plug 'pangloss/vim-javascript'
    Plug 'sheerun/vim-polyglot'
    Plug 'nickhutchinson/vim-cmake-syntax'
call plug#end()

" }}}
" Configuration {{{

" set the <leader> key
let mapleader = ','

if has('termguicolors') && $TERM !=? "rxvt-unicode-256color"
    set termguicolors
endif

if filereadable(expand("~/.vimrc_background")) && $TERM ==? "rxvt-unicode-256color"
    let base16colorspace=256
    source ~/.vimrc_background
endif

" set the background type (light/dark) before activating a colorscheme
set background=dark

let base16colorspace=256
silent! colorscheme base16-tomorrow-night

" let g:onedark_termcolors=256
" silent! colorscheme onedark

if has('shada')
    set shada=!,'100,<100,s100,h
endif

if has('inccommand')
    set inccommand=split
endif


if !has('nvim') && v:version >= 800
    packadd! matchit
else
    runtime macros/matchit.vim
endif

" if executable("rg")
"     set grepprg=rg\ --vimgrep\ --no-heading
"     set grepformat=%f:%l:%c:%m,%f:%l:%m
" elseif executable ('ag')
"     set grepprg=ag\ --vimgrep
"     set grepformat=%f:%l:%c:%m,%f:%l:%m
" endif

" persistent undo
if has('persistent_undo')
    set undodir=$HOME/.local/share/nvim/undo
    set undofile
    set undolevels=5000
endif

set cinoptions=>2,l1,p0,)50,*50,t0
set nostartofline
set breakindent
set breakindentopt=min:40
set pumheight=10
set colorcolumn=72
set completeopt=menu,menuone,longest
set diffopt=filler,vertical,foldcolumn:0
set expandtab
set gdefault
set hidden
set number
set showmatch
set ignorecase
set smartcase
" set shiftwidth=4 tabstop=4 softtabstop=4
set splitright
set splitbelow
set noswapfile
set virtualedit=onemore,block
set nowrap
set wrapscan
set autoread
set formatoptions=tqj
set history=10000
set list
set listchars=tab:>\ ,nbsp:+

set iskeyword+=:

if filereadable('/usr/share/dict/words')
    set dictionary='/usr/share/dict/words'
endif

if executable('rg')
    set grepprg=rg
endif

set tags=./tags;,tags;./.tags;,.tags;
set showfulltag
set nofoldenable
set foldignore=
set foldlevelstart=99
" set foldmethod=indent
set foldnestmax=10
set wildmenu
set wildignorecase
set wildmode=list:longest,full
set wildoptions=tagfile
set wildignore+=*./git/*,*.o,*.obj
set wildignore+=log/**,*/tmp/*,*.png,*.jpg,*.gif
set wildignore+=*.so,*.swp,*.zip
set scrolloff=5
set sidescrolloff=15
set sidescroll=1
set shortmess=aoOTIc
set noshowmode
set showcmd
set copyindent
set preserveindent
set smartindent
set lazyredraw
set laststatus=2
set switchbuf=useopen
set ttimeout
set ttimeoutlen=50
set tabstop=4
set shiftwidth=4
set softtabstop=4
set signcolumn=yes

" don't highlight past 512 columns
set synmaxcol=512
syntax sync minlines=512

" }}}
" Misc. Configuration {{{

" TeX {{{
let g:tex_flavor = 'latex'
let g:tex_comment_nospell = 1
" }}}
" netrw {{{

let g:netrw_banner = 0
let g:netrw_keepdir = 0
let g:netrw_liststyle = 3
" let g:netrw_sort_options = 'i'
let g:netrw_winsize = -28
let g:netrw_sort_sequence = '[\/]$,*'
let g:netrw_browse_split = 3

" }}}

" }}}
" Commands {{{

function! HighlightRepeats() range
    let lineCounts = {}
    let lineNum = a:firstline
    while lineNum <= a:lastline
        let lineText = getline(lineNum)
        if lineText != ""
            let lineCounts[lineText] = (has_key(lineCounts, lineText) ? lineCounts[lineText] : 0) + 1
        endif
        let lineNum = lineNum + 1
    endwhile
    exe 'syn clear Repeat'
    for lineText in keys(lineCounts)
        if lineCounts[lineText] >= 2
            exe 'syn match Repeat "^' . escape(lineText, '".\^$*[]') . '$"'
        endif
    endfor
endfunction

command! -range=% HighlightRepeats <line1>,<line2>call HighlightRepeats()

" display the callstrack for recent exceptions
command! Backtrace call exception#trace()

" :Grep <keyword>
command! -nargs=1 -bar Grep execute 'silent! grep! <q-args>' | redraw! | copen

" zero padding
command! -range ZeroPadPre    <line1>,<line2>s/^\([0-9]*\)/0\1/g
command! -range ZeroPadPost   <line1>,<line2>s/^\([0-9]*\)/\10/g

" sort lines by length
command! -range SortLength <line1>,<line2>!awk '{print length, $0}' | sort -n | cut -d" " -f2-

" }}}
" Bindings {{{

" yank to end of line
nnoremap Y y$

" clear trailing whitespace
nnoremap <F5> :let _s=@/<Bar>:%s/\s\+$//e<Bar>:let @/=_s<Bar><CR>

" keep visual selecting when tabbing
vnoremap << <gv
vnoremap >> >gv

" close quickfix and location list
nnoremap <leader>cq :cclose<CR>
nnoremap <leader>cl :lclose<CR>

" toggle folds
nnoremap <silent> <Space> @=(foldlevel('.')?'za':'\<Space>')<cr>
vnoremap <Space> zf

nnoremap <C-c>' :NW<CR>
inoremap <C-c>' <ESC>:NW<CR>

" nnoremap <C-c>' :echom "it works!"<CR>
" inoremap <C-c>' :echom "it works!"<CR>

" source: https://github.com/mhinz/dotfiles/blob/master/.vim/vimrc#L290
inoremap <silent> <c-g><c-t>  <c-r>=repeat(complete(col('.'), map(["%Y-%m-%d %H:%M:%S","%a, %d %b %Y %H:%M:%S %z","%Y %b %d","%d-%b-%y","%a %b %d %T %Z %Y"],'strftime(v:val)')+[localtime()]),0)<cr>

if has('nvim')
    nnoremap <leader>t :vsplit +terminal<CR>
    tnoremap <Esc> <C-\><C-n>
endif

" insert a hashbang for the current filetype
inoreabbrev <expr>#!! '#!/usr/bin/env' . (empty(&filetype) ? '' : ' '.&filetype)

" }}}
" autocmds {{{

augroup vimrc
    au!

    " start terminal buffers in insert mode
    if has('nvim')
        autocmd BufEnter term://* startinsert
    endif

    " autocmd FileType help let b:helpful=1

    " " TODO: remove this when I'm done taking the course
    " autocmd BufRead,BufNewFIle *.s  set ft=pic8
    " autocmd BufRead,BufNewFile *.js set ft=javascript

    autocmd FileType gitcommit
                \ nnoremap <buffer> <silent> <leader>cA
                \ :<C-U>Gcommit --amend --date="$(date)"<CR>

    autocmd FileType GV setlocal foldlevel=1

    " autocmd BufRead,BufNewFile *.csv set ft=csv

    " delete fugitive buffer when hidden
    autocmd BufReadPost fugitive:// setlocal bufhidden=delete

    autocmd User fugitive
                \  if fugitive#buffer().type() =~# '\v^%(tree|blob)$'
                \|   nnoremap <buffer> .. :edit %:h<cr>
                \| endif

augroup END

" }}}
" Syntax {{{

highlight link diffAdded     DiffAdd
highlight link diffRemoved   DiffDelete
highlight link diffFile      Directory
highlight link diffLine      Comment
highlight link diffIndexLine Comment
highlight link diffSubname   Comment

" }}}
" Plugins {{{

" vim-plug {{{

let g:plug_shallow = 0
let g:plug_window = 'enew'
let g:plug_pwindow = 'vertical rightbelow new'

" }}}
" vim-polyglot {{{

if exists('g:plugs["vim-polyglot"]')
    let g:polyglot_disabled = [
                \   'javascript',
                \   'rust',
                \   'tex',
                \   'markdown',
                \   'haskell',
                \   'lhaskell'
                \ ]
endif

" }}}
" ctrlp.vim {{{

if exists('g:plugs["ctrlp.vim"]')
    " let g:ctrlp_use_caching = 0
    if executable('rg')
        let g:ctrlp_use_caching = 0
        let g:ctrlp_user_command = 'rg %s --files --color=never --glob=""'
    endif

    " let g:ctrlp_match_func = { 'match': 'cpsm#CtrlPMatch' }

    let g:ctrlp_max_files = 200000
    let g:ctrlp_reuse_window = 'netrw\|help\|quickfix'
    let g:ctrlp_match_window = 'bottom,order:btt,min:1,max:15,results:15'

    " mappings
    " nnoremap <c-b> :ctrlpbuffer<cr>
    " nnoremap <c-f> :ctrlpline<cr>

    " leader mappings
    let g:ctrlp_map = '<C-p>'
    nnoremap <leader>. :CtrlPTag<CR>
    " nnoremap <leader>p :ctrlp<cr>
    " nnoremap <leader>b :ctrlpbuffer<cr>
    " nnoremap <leader>o :ctrlpmrufiles<cr>

    nnoremap <leader>b :CtrlPBuffer<CR>
    nnoremap <leader>r :CtrlPMRUFiles<CR>
    nnoremap <leader>f :CtrlPFunky<CR>
    nnoremap <leader><C-f> :CtrlPFunky<CR>
    nnoremap <leader><C-l> :CtrlPLine<CR>

    " nnoremap <C-x><C-r> :CtrlPMRUFiles<CR>


    if exists('g:plugs["ctrlp-funky"]')
        let g:ctrlp_funky_matchtype = 'path'
        let g:ctrlp_funky_multi_buffers = 1
        let g:ctrlp_funky_syntax_highlight = 1
    endif
endif

" }}}
" tabular {{{

if exists('g:plugs["tabular"]')
    vnoremap <silent> <Leader>cee    :Tabularize /=<CR>
    vnoremap <silent> <Leader>cet    :Tabularize /#<CR>
    vnoremap <silent> <Leader>ce     :Tabularize /
endif


" }}}
" tagbar {{{

if exists('g:plugs["tagbar"]')
    let g:tagbar_compact = 1
    let g:tagbar_autofocus = 1
    let g:tagbar_autoclose = 0

    nnoremap <F2> :TagbarToggle<CR>
endif

" }}}
" vim-surround {{{

if exists('g:plugs["vim-surround"]')
    let g:surround_{char2nr('-')} = "<% \r %>"
    let g:surround_{char2nr('=')} = "<%= \r %>"
    let g:surround_{char2nr('8')} = "/* \r */"
    let g:surround_{char2nr('s')} = " \r"
    let g:surround_{char2nr('^')} = "/^\r$/"

    let g:surround_108 = "\\begin{\1environment: \1}\r\\end{\1\r}.*\r\1}"

    let g:surround_indent = 1
endif

" }}}
" gutentags {{{

if exists('g:plugs["gutentags"]')
    " let g:gutentags_generate_on_empty_buffer = 1
    let g:gutentags_ctags_tagfile = '.tags'

    let g:gutentags_ctags_exclude = ['*node_modules*', 'tmp*', "package*json"]
endif

" }}}
" undotree {{{

if exists('g:plugs["undotree"]')
    let g:undotree_WindowLayout = 2
    nnoremap <F3> :UndotreeToggle<CR>
endif

" }}}
" neomake {{{

if exists('g:plugs["neomake"]')
    let g:neomake_open_list = 2
    let g:neomake_echo_current_error = 0

    let g:neomake_makeclean_maker = {
                \ 'exe': 'make',
                \ 'args': ['clean']
                \ }

    let g:neomake_javascript_jshint_maker = {
                \ 'args': ['--verbose'],
                \ 'errorformat': '%A%f: line %l\, col %v\, %m \(%t%*\d\)',
                \ }

    let g:neomake_cpp_gcc_maker = {
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

    let g:neomake_javascript_enabled_makers = ['jshint']
    let g:neomake_cpp_enabled_makers = ['gcc']
endif

" }}}
" codi.vim {{{

if exists('g:plugs["codi.vim"]')
    let g:codi#interpreters = {
                \   'python': {
                \       'bin': 'python3',
                \       'prompt': '^\(>>>\|\.\.\.\) ',
                \   },
                \   'haskell': {
                \       'prompt': 'λ',
                \   },
                \ }

    let g:codi#aliases = {
                \   'javascript.jsx': 'javascript',
                \ }

    " left-align the codi buffer
    let g:codi#rightalign = 0
endif

" }}}
" nerdtree {{{

if exists('g:plugs["nerdtree"]')
    nnoremap <F1> :NERDTreeToggle<CR>

    let g:NERDTreeIndicatorMapCustom = {
                \ "Modified"  : "✹",
                \ "Staged"    : "✚",
                \ "Untracked" : "✭",
                \ "Renamed"   : "➜",
                \ "Unmerged"  : "═",
                \ "Deleted"   : "✖",
                \ "Dirty"     : "✗",
                \ "Clean"     : "✔︎",
                \ 'Ignored'   : '☒',
                \ "Unknown"   : "?"
                \ }
endif

" }}}
" editorconfig {{{

if exists('g:plugs["editorconfig-vim"]')
    let g:EditorConfig_exclude_patterns = ['fugitive://.*', 'scp://.*']
endif

" }}}
" nvim-completion-manager {{{

if exists('g:plugs["nvim-completion-manager"]')

    let g:cm_smart_enable = 2
    let g:cm_matcher = { 'module': 'cm_matchers.abbrev_matcher', 'case': 'smartcase' }

    " python completion
    if exists('g:plugs["python-support.nvim"]')
        let g:python_support_python3_requirements = add(get(g:,'python_support_python3_requirements',[]),'jedi')
        let g:python_support_python3_requirements = add(get(g:,'python_support_python3_requirements',[]),'mistune')
        let g:python_support_python3_requirements = add(get(g:,'python_support_python3_requirements',[]),'psutil')
        let g:python_support_python3_requirements = add(get(g:,'python_support_python3_requirements',[]),'setproctitle')
    endif

    inoremap <expr> <CR> (pumvisible() ? "\<c-y>\<cr>" : "\<CR>")
    inoremap <expr> <Tab> pumvisible() ? "\<C-n>" : "\<Tab>"
    inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"

    if exists('g:plugs["ultisnips"]')
        let g:UltiSnipsSnippetDir = '~/.config/nvim/snippets'
        let g:UltiSnipsExpandTrigger = "<Plug>(ultisnips_expand)"
        let g:UltiSnipsJumpForwardTrigger   = "<c-j>"
        let g:UltiSnipsJumpBackwardTrigger  = "<c-k>"
        let g:UltiSnipsRemoveSelectModeMappings = 0

        inoremap <silent> <C-e> <C-r>=cm#sources#ultisnips#trigger_or_popup("\<Plug>(ultisnips_expand)")<cr>
    endif
endif

" }}}
" UltiSnips {{{

if exists('g:plugs["ultisnips"]')
    let g:UltiSnipsUsePythonVersion = 3
    let g:UltiSnipsEditSplit = 'vertical'
endif

" }}}
" ale {{{

if exists('g:plugs["ale"]')
    nnoremap <leader>l :lnext<CR>
    nnoremap <leader>p :lprevious<CR>
    nnoremap <leader>w :lrewind<CR>

    let g:ale_fixers = {
                \   'javascript': ['eslint'],
                \ }

    let g:ale_linters = {
                \   'jsx': ['stylelint', 'eslint'],
                \   'python': [],
                \ }

    " let g:ale_set_loclist = 0
    let g:ale_set_loclist = 1
    let g:ale_set_quickfix = 1

    let g:ale_lint_on_enter = 0
    let g:ale_lint_on_text_changed = 'never'
endif


" }}}
" ack.vim {{{

if exists('g:plugs["ack.vim"]')
    let g:ackprg = "rg --vimgrep --no-heading"
endif

" }}}
" vim-airline {{{

if exists('g:plugs["vim-airline"]')
    let g:airline_powerline_fonts = 1

    if !exists('g:airline_symbols')
        let g:airline_symbols = {}
    endif

    let g:airline#extensions#virtualenv#enabled = 1
    let g:airline#extensions#branch#enabled = 1


    let g:airline_skip_empty_sections = 1
    let g:airline_exclude_preview = 1

    " let g:airline#extensions#tabline#enabled = 1
    " let g:airline#extensions#tabline#fnamemod = ':t'
endif

" }}}
" lightline {{{

" if exists('g:plugs["lightline.vim"]')
"     let g:lightline =
"                 \ { 'colorscheme': '',

" endif

" }}}
" csv.vim {{{

if exists('g:plugs["csv.vim"]')
    let g:csv_delim=','
endif

" }}}
" vim-gitgutter {{{

if exists('g:plugs["vim-gitgutter"]')
    nmap ]h <Plug>GitGutterNextHunk
    nmap [h <Plug>GitGutterPrevHunk
    nmap <leader>ghp <Plug>GitGutterPreviewHunk

    let g:gitgutter_grep_command = 'rg'
endif

" }}}
" scratch.vim {{{

if exists('g:plugs["scratch.vim"]')
    let g:scratch_top = 0
    let g:scratch_no_mappings = 1

    nmap <leader>gs <plug>(scratch-insert-reuse)
    nmap <leader>gS <plug>(scratch-insert-clear)
    xmap <leader>gs <plug>(scratch-selection-reuse)
    xmap <leader>gS <plug>(scratch-selection-clear)
endif

" }}}
" vim-virtualenv {{{

if exists('g:plugs["vim-virtualenv"]')
    " TODO: configure this
endif

" }}}
" vim-ctrlspace {{{

if exists('g:plugs["vim-ctrlspace"]')
    if s:platform ==? 'linux'
        let g:CtrlSpaceFileEngine = 'file_engine_linux_amd64'

        if executable('rg')
            let g:CtrlSpaceGlobCommand = 'rg -l --color=never -g ""'
        endif
    endif

    nnoremap <silent><C-p> :CtrlSpace O<CR>
endif

" }}}
" vimagit {{{

if exists('g:plugs["vimagit"]')
endif

" }}}
" denite.nvim {{{

if exists('g:plugs["denite.nvim"]')
    if executable('rg')
        call denite#custom#var('file_rec', 'command', ['rg', '--hidden', '--files', '--glob', '!.git', '--glob', ''])

        call denite#custom#var('grep', 'command', ['rg'])
        call denite#custom#var('grep', 'default_opts', ['--vimgrep', '--no-heading'])

    endif

    call denite#custom#map('insert', '<C-j>', '<denite:move_to_next_line>', 'noremap')
    call denite#custom#map('insert', '<C-k>', '<denite:move_to_previous_line>', 'noremap')

    call denite#custom#map('insert', '<C-n>', '<denite:move_to_next_line>', 'noremap')
    call denite#custom#map('insert', '<C-p>', '<denite:move_to_previous_line>', 'noremap')

    call denite#custom#source('file_mru', 'matchers', ['matcher_fuzzy', 'matcher_project_files'])
    " call denite#custom#source('file_rec', 'matchers', ['matcher_cpsm'])

endif

" }}}
" vim-pandoc {{{

if exists('g:plugs["vim-pandoc"]')
    let g:pandoc#command#latex_engine = "lualatex"
    let g:pandoc#command#autoexec_command = "Pandoc pdf --template=template.tex --latex-engine=lualatex"

    let g:pandoc#syntax#codeblocks#embeds#use=1
    let g:pandoc#syntax#codeblocks#embeds#langs = ['bash=sh', 'bat=dosbatch']
endif

" }}}
" quick-scope {{{

if exists('g:plugs["quick-scope"]')
    let g:qs_highlight_on_keys = ['f', 'F', 't', 'T']
endif

" }}}
" pgsql.vim {{{

if exists('g:plugs["pgsql.vim"]')
    let g:sql_type_default = 'pgsql'
endif

" }}}
" vim-virtualenv {{{

if exists('g:plugs["vim-virtualenv"]')
    let g:virtualenv_directory = $HOME . "/.venvs"
endif

" }}}

" }}}

