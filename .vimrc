" ======================================================================================================================
" Note: You are suggested to use a wide(>120 chars) client to view or edit this file
" TODO: Sort the commands by a cleaner key
" Author: Isaac Delton
"
" My VIm configuration. Supports Windows and Linux(Only tested on Arch Linux, may not be the newest)
" Note that this file should not be directly used as your .vimrc.
version 8.0

" Dont execute this if already executed
if !exists("s:is_vimrc_executed")
let s:is_vimrc_executed=1

    " ==================================================================================================================
    " Clear Old keymaps if need to run again
    " ==================================================================================================================
    mapclear
    autocmd!

    " ==================================================================================================================
    " Auto-generated content
    " ==================================================================================================================
    if &cp | set nocp | endif  | " No compitiable mode
    " Setting cpo {{{
    let s:cpo_save=&cpo
    set cpo&vim
    let &cpo=s:cpo_save
    unlet s:cpo_save
    " }}} End setting cpo
    " Set encodings
    set encoding=utf-8
    set termencoding=utf-8
    set fileencodings=ucs-bom,utf-8,default,latin1,cp936
    " Set language of help document
    set helplang=cn
    " Highlight search result
    set hlsearch
    " Disables mouse in insert mode
    set mouse=nvcr

    nnoremap <silent> <Plug>NetrwBrowseX :call netrw#BrowseX(expand((exists("g:netrw_gx")? g:netrw_gx : '<cfile>'))
                \,netrw#CheckIfRemote())
    vnoremap <silent> <Plug>NetrwBrowseXVis :call netrw#BrowseXVis()
    set display=truncate
    set incsearch
    set langnoremap
    set nolangremap
    set nrformats=bin,hex
    set suffixes=.bak,~,.swp,.o,.info,.aux,.log,.dvi,.bbl,.blg,.brf,.cb,.ind,.idx,.ilg,.inx,.out,.toc,.png,.jpg
    set ttimeout
    set ttimeoutlen=100
    set wildmenu

    " ==================================================================================================================
    " Vundle and plugin settings
    " ==================================================================================================================
    set nocp
    filetype off                  " required
    call plug#begin($VIM . '/.vim/plugged')
    Plug 'junegunn/vim-plug'  | " Let vim-plug manage vim-plug
    " Powerful status line
    Plug 'vim-airline/vim-airline'
    Plug 'vim-airline/vim-airline-themes'
    " Color theme
    Plug 'altercation/vim-colors-solarized'
    " File explorer
    Plug 'scrooloose/nerdtree', { 'on' : 'NERDTreeToggle' }
    " Run commands async
    Plug 'skywind3000/asyncrun.vim', { 'on' : 'AsyncRun' }
    " Input method support(Linux only)
    if !has("win32")
        Plug 'vim-scripts/fcitx.vim'
    endif
    " Markdown support
    Plug 'godlygeek/tabular', { 'for' : 'markdown' }
    Plug 'plasticboy/vim-markdown', { 'for' : 'markdown' }
    Plug 'iamcco/markdown-preview.vim', { 'for' : 'markdown', 'on' : 'MarkdownPreview' }
    " LaTeX support
    Plug 'lervag/vimtex', { 'for' : 'tex' }
    " Completer(only used on linux)
    if !has("win32")
        Plug 'Valloric/YouCompleteMe', { 'dir' : '~/YouCompleteMe',
                    \'on' : 'YcmRestartServer', 'do' : 'python ./install.py' }
    endif
    call plug#end()

    " ==================================================================================================================
    " YouCompleteMe settings
    " ==================================================================================================================
    let g:ycm_warning_symbol = '!'

    " ==================================================================================================================
    " Airline settings
    " ==================================================================================================================
    set laststatus=2  | " Ensure that status line is shown
    set noshowmode  | " The mode will be shown in status line
    let g:airline_theme = 'solarized'
    let g:airline_solarized_bg = 'dark'

    " ==================================================================================================================
    " AsyncRun settings
    " ==================================================================================================================
    if has("win32")
        let g:asyncrun_encs='cp936'
    endif

    " ==================================================================================================================
    " Vimtex settings
    " ==================================================================================================================
    let g:vimtex_fold_enabled=1
    let g:vimtex_compiler_latexmk = {
                \   'options' : [
                \     '-xelatex',
                \   ],
                \ }

    " ==================================================================================================================
    " Settings
    " ==================================================================================================================
    " Open the syntax highlight
    syntax enable
    syntax on
    " Highlight corrent line
    set cursorline
    " Open the line number
    set number
    " Show entered commands
    set showcmd
    " Set the width of indent and tab
    set shiftwidth=4
    set tabstop=4
    set expandtab
    " Set C-style indent and options
    set cindent
    set cinoptions+=L0.5s:0g0N-s
    "set cinoptions+=(0
    " Let backspace available
    set bs=2
    " Open code folding
    set foldmethod=syntax
    " I dont need the .viminfo
    set viminfo='0,f0,<0,:0,@0,/0
    " 120 chars at most
    set textwidth=120
    " Why not use zsh?
    set shell=/bin/zsh

    " ==================================================================================================================
    " Mappings
    " ==================================================================================================================
    " Reset the leader
    let mapleader=' '
    " Fast fold code
    map <leader><space> za
    " Clear search
    nnoremap <silent><leader>/ :let @/=''<CR>
    " Make cursor move in the virtual lines
    noremap <silent> <expr> j (v:count == 0 ? 'gj' : 'j')
    noremap <silent> <expr> k (v:count == 0 ? 'gk' : 'k')
    " Emacs style motion in cmdline mode(q: may be better)
    cnoremap <C-B> <Left>
    cnoremap <C-F> <Right>
    cnoremap <M-B> <C-Left>
    cnoremap <M-F> <C-Right>
    " Run shell commands 
    nnoremap <leader>; :!
    nnoremap <leader>: :AsyncRun

    " ==================================================================================================================
    " Terminal settings
    " ==================================================================================================================
    if !has("win32")  | " There is not terminal on Windows
        tnoremap <Esc> <C-W>N  | " Dont be different
        "autocmd BufWinEnter * if &buftype == 'terminal' | setlocal bufhidden=hide nonu | endif
    endif

    " ==================================================================================================================
    " NERD Tree and Netrw settings
    " ==================================================================================================================
    let NERDTreeHijackNetrw=0  | " Use netrw as default directory viewer
    let g:netrw_liststyle=3

    " ==================================================================================================================
    " Gui settings
    " ==================================================================================================================
    if has("gui_running")
        " Color scheme
        colo solarized
        set background=light
        " Font
        set guifont=Source\ Code\ Pro
        " Ban the annoying bell(cant be seen on Linux gui)
        set vb
        " I dont need the controls
        set go=''
        " Win32 settings
        if has("win32")
            set guifont=Consolas
            set novb
            set shell=C:\\WINDOWS\\system32\\cmd.exe
        endif
    else  | " GUI ^^^ Term vvv
        " Color scheme
        colo desert
        set background=dark
    endif

    " ==================================================================================================================
    " Helper functions
    " ==================================================================================================================

    " ==================================================================================================================
    " Language settings: Cpp
    " ==================================================================================================================
    function! s:CppLanguageSettings()
        if has("win32")
            nnoremap <buffer><leader>cc :w<CR>:AsyncRun msbuild<CR>
        else
            nnoremap <buffer><leader>cc :w<CR>:AsyncRun g++ -c -std=c++17 -Wall -Wextra %<CR>
        endif
    endfunction

    " ==================================================================================================================
    " Language settings: HTML
    " ==================================================================================================================
    function! s:HTMLLanguageSettings()
        setlocal foldmethod=indent
    endfunction
    
    " ==================================================================================================================
    " Execute language settings
    " ==================================================================================================================
    autocmd FileType html,htm,xml call s:HTMLLanguageSettings()
    autocmd FileType cpp,cxx,c,h,hpp,hxx call s:CppLanguageSettings()

else | " New content goes here
    echohl WarningMsg
    echom "Vimrc had been executed, passing..."
    echohl None | " Just a prompt, can be ignored
endif | " End checking

" vim: set ft=vim :
