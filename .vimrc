" ======================================================================================================================
" Note: You are suggested to use a wide(>120 chars) client to view or edit this file
" TODO: Sort the commands by a cleaner key
" Author: Isaac Delton
"
" My VIm configuration. Supports Windows and Linux(Only tested on Arch Linux, may not be the newest)
" Note that this file should not be directly used as your .vimrc.
version 8.0

" {{{ Guard
if !exists('g:execute_vimrc') || g:execute_vimrc

    " ==================================================================================================================
    " Clean up
    " ==================================================================================================================
    if !exists('g:execute_vimrc') || g:execute_vimrc != 2
        mapclear
        autocmd!
        set all&
    endif
    let g:execute_vimrc = 0
    " }}} End guard

    " {{{ Auto-generated
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
    filetype plugin indent on
    packadd vim-packager
    function! PackInit() abort
        packadd vim-packager
        call packager#init()
        call packager#add('kristijanhusak/vim-packager', {'type': 'opt'})

        " Status line
        call packager#add('vim-airline/vim-airline')
        call packager#add('vim-airline/vim-airline-themes')
        " Color theme
        call packager#add('altercation/vim-colors-solarized')

        " File explorer
        call packager#add('scrooloose/nerdtree', {'type': 'opt'})
        " Run shell command async
        call packager#add('skywind3000/asyncrun.vim', {'type': 'opt'})
        " Completer
        call packager#add('Valloric/YouCompleteMe', {'do': 'python ./install.py --clang-completer', 'type': 'opt'})
        call packager#add('Shougo/echodoc.vim', {'type': 'opt'})

        " Markdown support
        call packager#add('godlygeek/tabular', {'type': 'opt'})
        call packager#add('plasticboy/vim-markdown', {'type': 'opt'})
        " call packager#add('iamcco/markdown-preview.vim')  " I seldom use this plugin
        " Latex support
        " call packager#add('larvag/vimtex')  " I seldom use this plugin

        "System-specified plugins
        if has('win32')
        else
            " Input method support on linux
            call packager#add('vim-scripts/fcitx.vim', {'type': 'opt'})
        endif

        " OI plugin
        call packager#add('91khr/VimOI')
        " Zen-mode in vscode
        call packager#add('junegunn/goyo.vim', {'type': 'opt'})
    endfunction

    " Dynamic load plugin command
    function! s:AddPlugCmd(cmdname, exec, args)
        if has_key(a:args, 'cond') ? eval(a:args.cond) : !has(':' . a:cmdname)
            let cmdargs = (has_key(a:args, 'args') ? '-nargs=' . a:args.args : '') . ' '
            execute "command! " . cmdargs . a:cmdname . ' delcommand ' . a:cmdname . '|' . a:exec
        endif
    endfunction
    call s:AddPlugCmd('NERDTree', 'packadd nerdtree | NERDTree', {})
    call s:AddPlugCmd('AsyncRun', 'packadd asyncrun.vim | AsyncRun <args>', {'args': '*'})
    call s:AddPlugCmd('YcmOn', 'packadd YouCompleteMe | packadd echodoc.vim', {'cond': "!has(':YcmCompleter')"})
    call s:AddPlugCmd('Goyo', 'packadd goyo.vim | Goyo <args>', {'args': '*'})

    " Plugin commands
    command! PackUpdate call PackInit() | call packager#update('', {'do': 'call packager#status()'})
    command! PackClean call PackInit() | call packager#clean()
    command! PackStatus call PackInit() | call packager#status()
    command! PackInstall call PackInit() | call packager#install()

    " ==================================================================================================================
    " YouCompleteMe and Echodoc settings
    " ==================================================================================================================
    if has('win32')
        let g:ycm_global_ycm_extra_conf = '~/vimfiles/ycm_extra_conf.py'
    else
        let g:ycm_global_ycm_extra_conf = '~/.vim/ycm_extra_conf.py'
    endif
    let g:ycm_add_preview_to_completeopt = 0
    let g:ycm_server_log_level = 'info'
    let g:ycm_semantic_triggers = {
                \ 'c,cpp,python,java,go,erlang,perl': ['.', '->', '::', 're!\w{2}'],
                \ 'cs,lua,javascript': ['.', 're!\w{2}'],
                \ }
    let g:echodoc#enable_at_startup = 1
    let g:echodoc#enable_force_overwrite = 1
    set completeopt-=preview

    " ==================================================================================================================
    " VimOI settings
    " ==================================================================================================================
    let g:VimOI_CompileArgs = [ '/Od', '/nologo', '/utf-8', '/EHsc', '/W4', '/D_CRT_SECURE_NO_WARNINGS' ]

    " ==================================================================================================================
    " Airline settings
    " ==================================================================================================================
    set laststatus=2  | " Ensure that status line is shown
    set noshowmode  | " The mode will be shown in status line
    let g:airline#extensions#tabline#enabled = 1  | " Display buffers
    let g:airline#extensions#tabline#formatter = 'default'  | " Display file path in default way
    let g:airline#extensions#wordcount#formatter = 'cnfmt'

    " ==================================================================================================================
    " AsyncRun settings
    " ==================================================================================================================
    if has("win32")
        let g:asyncrun_encs = 'cp936'
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
    if has("win32")
        let g:vimtex_view_general_viewer = 'texworks'
    endif

    " ==================================================================================================================
    " NERD Tree and Netrw settings
    " ==================================================================================================================
    let NERDTreeHijackNetrw=0  | " Use netrw as default directory viewer
    let g:netrw_liststyle=3

    " ==================================================================================================================
    " Goyo settings
    " ==================================================================================================================
    let g:goyo_width = '80%'
    let g:goyo_height = '95%'
    let g:goyo_linenr = 1
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
    " Turn on exrc on current directory, and auto run .vimrc on current file
    set exrc
    set secure
    au DirChanged * if filereadable('.vimrc') | so .vimrc | endif  | "Not safe
    au BufWrite .vimrc if filereadable('.vimrc') | so .vimrc | endif

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
    " Quick compile
    nnoremap <silent><leader>cc :Compile<CR>
    " Quick diagnostic powered by YCM
    nnoremap <silent><leader>f :YcmCompleter FixIt<CR>

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
        " Font
        set guifont=Source\ Code\ Pro
    endif
    " }}} System settings

    " {{{ Language-specified
    " ==================================================================================================================
    " Default settings
    " ==================================================================================================================
    function! s:DefaultLanguageSettings()
        function! s:OutputUnableToCompile()
            echohl Error
            echom "找不到编译方法, 不能编译"
            echohl Normal
        endfunction
        command! -buffer Compile call <SID>OutputUnableToCompile()
    endfunction
    autocmd FileType * call s:DefaultLanguageSettings()

    " ==================================================================================================================
    " Language settings: Cpp
    " ==================================================================================================================
    function! s:CppLanguageSettings()
        command! -buffer -nargs=* Compile CppCompile % <args>
        " Set C-style indent and options
        set cindent
        set cinoptions+=L0.5s:0g0N-s
    endfunction
    autocmd FileType cpp,cxx,c,h,hpp,hxx call s:CppLanguageSettings()

    " ==================================================================================================================
    " Language settings: HTML
    " ==================================================================================================================
    function! s:HTMLLanguageSettings()
        setlocal foldmethod=indent
    endfunction
    autocmd FileType html,htm,xml call s:HTMLLanguageSettings()

    " ==================================================================================================================
    " Language settings: VimScript
    " ==================================================================================================================
    function! s:VimLanguageSettings()
        setlocal foldmethod=marker
        command! -buffer Compile so %
    endfunction
    autocmd FileType vim call s:VimLanguageSettings()

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
    autocmd FileType md,markdown call s:MarkdownLanguageSettings()
    " }}} End language-specified

    " {{{ Guard
else
    echohl TODO
    echo "If you want to execute vimrc again, set g:execute_vimrc to 1"
    echohl None
endif
" }}} End guard

" vim: set ft=vim :
