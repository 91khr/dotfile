" ======================================================================================================================
" Note: You are suggested to use a wide(>120 chars) client to view or edit this file
" TODO: Sort the commands by a cleaner key
" Author: Isaac Delton
"
" My VIm configuration. Supports Windows and Linux(Only tested on Arch Linux, may not be the newest)
" Note that this file should not be directly used as your .vimrc.
version 8.0

" {{{ Guard
if !exists("s:is_vimrc_executed")
let s:is_vimrc_executed=1
" }}} Guard

    " {{{ Auto-generated
    " ==================================================================================================================
    " Clear Old settings
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
    " }}} End auto-generated

    " {{{ Plugins
    " ==================================================================================================================
    " Vim-plug and plugin settings
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

    " Markdown support
    Plug 'godlygeek/tabular', { 'for' : 'markdown' }
    Plug 'plasticboy/vim-markdown', { 'for' : 'markdown' }
    Plug 'iamcco/markdown-preview.vim', { 'for' : 'markdown', 'on' : 'MarkdownPreview' }
    " LaTeX support
    Plug 'lervag/vimtex', { 'for' : 'tex' }
    " Lua support

    " Linux-only plugins
    if !has("win32")
        " Input method support
        Plug 'vim-scripts/fcitx.vim'
        " Completer(Not worked well on Windows)
        Plug 'Valloric/YouCompleteMe', { 'dir' : '~/YouCompleteMe',
                    \'on' : 'YcmRestartServer', 'do' : 'python ./install.py' }
    endif

    " OI plugin
    Plug '91khr/VImOI'
    "Plug 'file:///C:/Users/Isaac/Documents/projects/VimOI'

    call plug#end()

    " ==================================================================================================================
    " YouCompleteMe settings
    " ==================================================================================================================
    let g:ycm_warning_symbol = '!'

    " ==================================================================================================================
    " VimOI settings
    " ==================================================================================================================
    let g:VimOI_CompileArgs = [ '/Od', '/nologo', '/utf-8', '/EHsc' ]

    " ==================================================================================================================
    " Airline settings
    " ==================================================================================================================
    set laststatus=2  | " Ensure that status line is shown
    set noshowmode  | " The mode will be shown in status line
    let g:airline#extensions#tabline#enabled = 1  | " Display buffers
    let g:airline#extensions#tabline#formatter = 'default'  | " Display file path in default way

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
    " NERD Tree and Netrw settings
    " ==================================================================================================================
    let NERDTreeHijackNetrw=0  | " Use netrw as default directory viewer
    let g:netrw_liststyle=3
    " }}} End plugins

    " {{{ Vim options
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
    " Let backspace available
    set bs=2
    " Open code folding
    set foldmethod=syntax
    " I dont need the .viminfo
    set viminfo='0,f0,<0,:0,@0,/0
    " 120 chars at most
    set textwidth=120
    " Use system clipboard
    set clipboard=unnamed

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
    nnoremap <leader>: :AsyncRun<space>

    " ==================================================================================================================
    " Terminal settings
    " ==================================================================================================================
    if !has("win32")  | " There is not terminal on Windows
        tnoremap <Esc> <C-W>N  | " Dont be different
        "autocmd BufWinEnter * if &buftype == 'terminal' | setlocal bufhidden=hide nonu | endif
    endif
    " }}} End vim options

    " {{{ GUI and System settings
    " ==================================================================================================================
    " Gui settings
    " ==================================================================================================================
    if has("gui_running")
        " Color scheme
        colo solarized
        set background=light
        " Status line theme
        let g:airline_theme = 'solarized'
        let g:airline_solarized_bg = 'dark'
        " Font
        set guifont=Source\ Code\ Pro
        " Ban the annoying bell(cant be seen on Linux gui)
        set vb
        " I dont need the controls
        set go=''
    else  | " GUI ^^^ Term vvv
        " Color scheme
        colo desert
        set background=dark
        " Status line theme
        let g:airline_theme = 'deus'
    endif

    " ==================================================================================================================
    " System settings
    " ==================================================================================================================
    if has("win32")  | " Windows
        set guifont=Consolas
        set novb
        set shell=C:\\WINDOWS\\system32\\cmd.exe
    else  | " Linux
        " Why not use zsh?
        set shell=/bin/zsh
    endif
    " }}} System settings

    " {{{ Language-specified
    " ==================================================================================================================
    " Helpers
    " ==================================================================================================================
    " Quick-compile mapping
    function! s:OutputUnableToCompile()
        echohl Error
        echom "找不到编译方法, 不能编译"
        echohl Normal
    endfunction
    nnoremap <silent><leader>cc :Compile<CR>
    command! -nargs=1 Compile call <SID>OutputUnableToCompile()

    " ==================================================================================================================
    " Language settings: Cpp
    " ==================================================================================================================
    function! s:CppLanguageSettings()
        command! -buffer -nargs=* Compile CppCompile % <args>
        " Set C-style indent and options
        set cindent
        set cinoptions+=L0.5s:0g0N-s
    endfunction

    " ==================================================================================================================
    " Language settings: HTML
    " ==================================================================================================================
    function! s:HTMLLanguageSettings()
        setlocal foldmethod=indent
    endfunction

    " ==================================================================================================================
    " Language settings: VimScript
    " ==================================================================================================================
    function! s:VimLanguageSettings()
        setlocal foldmethod=marker
    endfunction

    " ==================================================================================================================
    " Language settings: Markdown
    " ==================================================================================================================
    function! s:MarkdownLanguageSettings()
        function! s:CompileMarkdown(...)
            " Process options
            let options = ''
            for item in a:000
                let options .= ' ' . item
            endfor
            " Process output name
            let outname = expand('%:r') . '.html'
            " Compile...
            execute ":AsyncRun pandoc % -o " . outname . ' ' . options
        endfunction
        command! -buffer -nargs=* Compile call <SID>CompileMarkdown(<f-args>)
    endfunction
    
    " ==================================================================================================================
    " Execute language settings
    " ==================================================================================================================
    autocmd FileType html,htm,xml call s:HTMLLanguageSettings()
    autocmd FileType cpp,cxx,c,h,hpp,hxx call s:CppLanguageSettings()
    autocmd FileType vim call s:VimLanguageSettings()
    autocmd FileType md,markdown call s:MarkdownLanguageSettings()
    " }}} End language-specified

" {{{ Guard
else | " New content goes here
    echohl WarningMsg
    echom "Vimrc had been executed, passing..."
    echohl None | " Just a prompt, can be ignored
endif | " End checking
" }}} End guard

" vim: set ft=vim :
