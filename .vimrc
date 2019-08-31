" ======================================================================================================================
" Note: You are suggested to use a wide(>120 chars) client to view or edit this file
" TODO: Sort the commands by a cleaner key
" Author: Isaac Delton
"
" My VIm configuration. Supports Windows and Linux(Only tested on Arch Linux, may not be the newest)
" Note that this file should not be directly used as your .vimrc.
version 8.0

" {{{ Guard and prepare
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

    " {{{ Helper functions and commands
    command! -nargs=1 -complete=file SoFile execute 'so ' . expand('<sfile>:h') . '/' . <q-args>
    " }}} End helper functions and commands

    " Set the options
    SoFile .vim/options.vim

    " {{{ Plugins
    " ==================================================================================================================
    " Vim-plug and plugin settings
    " ==================================================================================================================
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
        " Snippet :)
        call packager#add('sirver/ultisnips')
        " Rainbow quote
        call packager#add('luochen1990/rainbow')

        " File explorer
        call packager#add('scrooloose/nerdtree', {'type': 'opt'})
        " Run shell command async
        call packager#add('skywind3000/asyncrun.vim', {'type': 'opt'})
        " Completer
        " Switch to LSP when possible
        call packager#add('Valloric/YouCompleteMe', {'type': 'opt', 'do': 'python install.py --clang-completer'})
        call packager#add('Shougo/echodoc.vim', {'type': 'opt'})

        " Markdown support
        call packager#add('godlygeek/tabular', {'type': 'opt'})
        call packager#add('plasticboy/vim-markdown', {'type': 'opt'})
        " Latex support
        call packager#add('lervag/vimtex', {'type': 'opt'})
        call packager#add('xuhdev/vim-latex-live-preview', {'type': 'opt'})

        "System-specified plugins
        if has('win32')
        else
            " Input method support on linux
            call packager#add('vim-scripts/fcitx.vim')
        endif

        " OI plugin
        call packager#add('91khr/VimOI')
        " Zen-mode in vscode
        call packager#add('junegunn/goyo.vim', {'type': 'opt'})
    endfunction

    " Dynamic load plugin filetype
    function! s:AddPlugFT(filetype,plug)
        let ft = type(a:filetype) == type([]) ? join(a:filetype, ",") : a:filetype
        let plglist = type(a:plug) == type([]) ? join(a:plug, " | packadd ") : a:plug
        execute "autocmd FileType " . ft . " packadd " . plglist
                    \ . " | execute \"autocmd! FileType " . ft . "\" | execute \"set ft=\" . &ft"
    endfunction
    call s:AddPlugFT('md,markdown', ['tabular', 'vim-markdown'])
    call s:AddPlugFT('tex,plaintex', 'vimtex')

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
    call s:AddPlugCmd('LLPStartPreview', 'packadd vim-latex-live-preview | LLPStartPreview', {})

    " Plugin commands
    command! PackUpdate call PackInit() | call packager#update()
    command! PackClean call PackInit() | call packager#clean()
    command! PackStatus call PackInit() | call packager#status()
    command! PackInstall call PackInit() | call packager#install()

    " ==================================================================================================================
    " Language Server and Echodoc settings
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
    let g:ycm_warning_symbol = '->'
    let g:echodoc#enable_at_startup = 1
    let g:echodoc#enable_force_overwrite = 1
    set completeopt=menuone,noselect

    " ==================================================================================================================
    " VimOI settings
    " ==================================================================================================================
    let g:rainbow_active = 1
    if has('win32')
        let g:VimOI_CompileArgs = [ '/Od', '/nologo', '/utf-8', '/EHsc', '/W4', '/D_CRT_SECURE_NO_WARNINGS' ]
    else
        let g:VimOI_CompileArgs = [ '-Wall', '-Wextra' ]
    endif

    " ==================================================================================================================
    " Airline settings
    " ==================================================================================================================
    set laststatus=2  | " Ensure that status line is shown
    set noshowmode  | " The mode will be shown in status line
    let g:airline#extensions#tabline#enabled = 1  | " Display buffers
    let g:airline#extensions#tabline#formatter = 'default'  | " Display file path in default way
    let g:airline#extensions#wordcount#formatter = 'cnfmt'

    " ==================================================================================================================
    " Vimtex settings
    " ==================================================================================================================
    let g:vimtex_enabled = 1
    let g:vimtex_fold_enabled = 1
    let g:vimtex_compiler_latexmk = {
                \   'options' : [
                \     '-xelatex',
                \   ],
                \ }
    let g:livepreview_engine = 'xelatex'
    if has("win32")
        let g:livepreview_previewer = 'start'
    else
        let g:livepreview_previewer = 'zathura'
        set updatetime=400
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
        if !exists(':Compile')
            command! -buffer Compile call <SID>OutputUnableToCompile()
        endif
    endfunction
    autocmd FileType * call s:DefaultLanguageSettings()

    " ==================================================================================================================
    " Language settings: Cpp
    " ==================================================================================================================
    function! s:CppLanguageSettings()
        command! -buffer -nargs=* Compile CppCompile % <args>
        " Set C-style indent and options
        function! CppIndent()
            let l:prevline = prevnonblank(line('.') - 1)
            let l:prevctnt = getline(l:prevline)
            let l:indent = cindent('.')
            if l:prevctnt =~# '\m^\s*template<'
                let l:indent = cindent(l:prevline)
            elseif l:prevctnt =~# '^\s*class.*:.*'
                let l:indent = indent(l:prevline)
            endif
            return l:indent
        endfunction
        setlocal indentexpr=CppIndent()
        setlocal cinoptions+=L0.5s:0g0N-sj1
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
    " Language settings: Python
    " ==================================================================================================================
    function! s:PythonLanguageSettings()
        command! -buffer Compile !python %
    endfunction
    autocmd FileType python call s:PythonLanguageSettings()

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
            let outname = expand('%:t:r') . '.html'
            " Compile...
            execute ":AsyncRun pandoc % -o " . outname . ' ' . options
        endfunction
        command! -buffer -nargs=* Compile call <SID>CompileMarkdown(<f-args>)
    endfunction
    autocmd FileType md,markdown call s:MarkdownLanguageSettings()
    " }}} End language-specified

    " {{{ Guard and cleanup
    " Del helper commands
    delcommand SoFile
else
    echohl TODO
    echo "If you want to execute vimrc again, set g:execute_vimrc to 1"
    echohl None
endif
" }}} End guard

" vim: set ft=vim :
