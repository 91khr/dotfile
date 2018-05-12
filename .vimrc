" ======================================================================================================================
" Note: You are suggested to use a wide(>120 chars) client to view or edit this file
" TODO: Sort the commands by a cleaner key
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
    set fileencodings=ucs-bom,utf-8,default,latin1
    " Set language of help document
    set helplang=cn
    " Highlight search result
    set hlsearch
    " Disables mouse in insert mode
    set mouse=nvcr
    set termencoding=utf-8

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
    set rtp+=$VIM/.vim/bundle/vim-plug
    let $HOME=$VIM
    call plug#begin($VIM . '/.vim/bundle/')
    Plug 'junegunn/vim-plug'
    " Powerful status line
    Plug 'vim-airline/vim-airline'
    Plug 'vim-airline/vim-airline-themes'
    " Color theme
    Plug 'altercation/vim-colors-solarized'
    " File explorer
    Plug 'scrooloose/nerdtree', { 'on' : 'NERDTreeToggle' }
    " Run commands async
    Plug 'skywind3000/asyncrun.vim', { 'on' : 'AsyncRun' }
    " Markdown support
    Plug 'godlygeek/tabular', { 'for' : 'markdown' }
    Plug 'plasticboy/vim-markdown', { 'for' : 'markdown' }
    Plug 'iamcco/markdown-preview.vim', { 'for' : 'markdown' }
    call plug#end()

    " ==================================================================================================================
    " YouCompleteMe settings
    " ==================================================================================================================
    if exists(":YcmCompleter")  | " I may not always install YCM
        let g:ycm_global_ycm_extra_conf='~/.vim/bundle/YouCompleteMe/third_party/ycmd/cpp/ycm/.ycm_extra_conf.py'
        let g:ycm_confirm_extra_conf=0
        let g:ycm_collect_identifiers_from_tag_files = 1
        let g:ycm_warning_symbol = '>!' | " I never see warnings
        let g:ycm_error_symbol = '>>'
        set completeopt-=preview  | " No preview window
        autocmd User YcmQuickFixOpened q | " No QuickFix window
        " Quick command for complete
        nnoremap <silent><leader>; :YcmCompleter FixIt<CR>
        nnoremap <silent><leader>? :YcmShowDetailedDiagnostic<CR>
        inoremap <expr><CR> pumvisible()?"<C-N><C-Y>":"<CR>"
        " Use <C-J> et <C-K> to view the complete list
        inoremap <expr><C-J> pumvisible()?"<C-N>":"<C-J>"
        inoremap <expr><C-K> pumvisible()?"<C-P>":"<C-K>"
    endif

    " ==================================================================================================================
    " Airline settings
    " ==================================================================================================================
    set laststatus=2  | " Ensure that status line is shown
    set noshowmode  | " The mode will be shown in status line
    "AirlineTheme solarized  | " make sure the status line theme is the same as editor
    let g:airline_solarized_bg = 'dark'

    " ==================================================================================================================
    " Settings
    " ==================================================================================================================
    " Open the syntax highlight
    syntax enable
    syntax on
    colo solarized
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
    " Enable pretty tab pages on gui
    set go=e
    " 120 chars at most
    set textwidth=120
    " Why not use zsh?
    "set shell=/bin/zsh

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
    " Line scrolling
    nnoremap <C-J> <C-E>
    nnoremap <C-K> <C-Y>
    " Screen scrolling
    nnoremap <expr><C-N> "Lzz2jzz"
    nnoremap <expr><C-P> "Hzz2kzz"
    " Run shell commands 
    nnoremap <leader>; :!
    " Switch tabpages
    inoremap <C-Tab> <C-O>gt
    nnoremap <C-Tab> gt
    inoremap <C-S-Tab> <C-O>gT
    nnoremap <C-S-Tab> gt

    " ==================================================================================================================
    " Terminal settings
    " ==================================================================================================================
    " tnoremap <Esc> <C-W>N  | " Dont be different
    "autocmd BufWinEnter * if &buftype == 'terminal' | setlocal bufhidden=hide nonu | endif 

    " ==================================================================================================================
    " NERD Tree and Netrw settings
    " ==================================================================================================================
    let NERDTreeHijackNetrw=0  | " Use netrw as default directory viewer
    let g:netrw_liststyle=3

    " ==================================================================================================================
    " Gui and term settings
    " ==================================================================================================================
    if has("gui_running")
        set background=light
        set guifont=Aix
        " set vb  | " Ban the annoying bell(cant be seen in gui)
    else  | " GUI ^^^ Term vvv
        set background=dark
    endif

    " ==================================================================================================================
    " Language settings: Cpp
    " ==================================================================================================================
    function! s:CppLanguageSettings()
        nnoremap <buffer><leader>cc :w<CR>:!g++ -c -std=c++17 -Wall -Wextra %<CR>
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
