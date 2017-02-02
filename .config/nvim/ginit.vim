" Windows-only options
if has('win32') || has('win64')
    set guifont=Consolas:h12
    set guioptions=
    set guiheadroom=0
endif

if has('nvim')
    if exists('g:GuiLoaded')
        Guifont! Roboto Mono:h12
    endif
endif


