" ======================================================================================================================
" Note: You are suggested to use a wide(>120 chars) client to view or edit this file
" Author: Isaac Delton
" My VIm configuration: options

" {{{ Auto-generated
" ======================================================================================================================
" Auto-generated content
" ======================================================================================================================
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
filetype plugin indent on
" }}} End auto-generated

" {{{ Settings
" ======================================================================================================================
" Settings
" ======================================================================================================================
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
" Let backspace available
set bs=2
" Open code folding
set foldmethod=syntax
" I dont need the .viminfo
set viminfo='0,f0,<0,:0,@0,/0
" Width for line breaking and vertical prompt line
set textwidth=120
set colorcolumn=+0
" Use LF by default
set fileformat=unix
set fileformats=unix,dos
" Turn on exrc on current directory, and auto run .vimrc on current file
set exrc secure
autocmd DirChanged * if filereadable('.vimrc') | confirm so .vimrc | endif
autocmd BufWritePost .vimrc if filereadable('.vimrc') | so .vimrc | endif

" ======================================================================================================================
" Terminal settings
" ======================================================================================================================
" There's no difference between terminal and ordinary buffer(just like Emacs):)
tnoremap <Esc> <C-W>N
" }}} End settings

" {{{ Mappings and autocmds
" ======================================================================================================================
" Mappings
" ======================================================================================================================
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
nnoremap <leader>: :AsyncRun<space>
" Quick compile
nnoremap <silent><leader>cc :Compile<CR>
" Autoselect suggest in completion
inoremap <silent><expr> <Tab> pumvisible() ? '<C-N>' : '<Tab>'
inoremap <silent><expr> <CR> (pumvisible() ? '<C-X>' : '') . '<CR>'
" }}} End settings and autocmds

" {{{ GUI and System settings
" ======================================================================================================================
" Gui settings
" ======================================================================================================================
if has("gui_running")
    " Color scheme
    colo solarized
    set background=light
    " Status line theme
    let g:airline_theme = 'solarized'
    let g:airline_solarized_bg = 'dark'
    " Ban the annoying bell(cant be seen on Linux gui)
    set vb
    " I dont need the controls
    set go=''
    " Make error easier to see
    hi Error gui=undercurl
else  | " GUI ^^^ Term vvv
    " Color scheme
    colo desert
    set background=dark
    " Status line theme
    let g:airline_theme = 'deus'
endif

" ======================================================================================================================
" System settings
" ======================================================================================================================
if has("win32")  | " Windows
    set guifont=Consolas:h10
    set novb
    set shell=C:\\WINDOWS\\system32\\cmd.exe
else  | " Linux
    " Why not use zsh?
    set shell=/bin/zsh
    " Font
    set guifont=Monaco:h10
endif
" }}} System settings

