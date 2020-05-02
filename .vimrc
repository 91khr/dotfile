" ======================================================================================================================
" Note: You are suggested to use a wide(>120 chars) client to view or edit this file
" TODO: Sort the commands by a cleaner key
" Author: Virginia Senioria
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
    function! IsExec(name)  " Like executable(), but fallback to wsl on Windows
        if executable(a:name) | return v:true  | " This is always correct
        elseif has('win32') && executable("bash")
            silent call system("wsl which " . a:name)
            return v:shell_error == 0
        else | return v:false
        endif
    endfunction
    " }}} End helper functions and commands

    " {{{ Plugins
    " {{{ Plugin list and setup options
    " ==================================================================================================================
    " Vim-packager and plugin settings
    " ==================================================================================================================
    packadd vim-packager
    function! PackInit() abort
        packadd vim-packager
        call packager#init({ 'dir': "~/.vim/pack/packager" })
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
        " Show marks
        call packager#add('kshenoy/vim-signature')

        " File explorer
        call packager#add('scrooloose/nerdtree', {'type': 'opt'})
        " Run shell command async
        call packager#add('skywind3000/asyncrun.vim', {'type': 'opt'})

        " Language server
        call packager#add('prabirshrestha/async.vim', {'type': 'opt'})
        call packager#add('prabirshrestha/vim-lsp', {'type': 'opt'})
        call packager#add('prabirshrestha/asyncomplete.vim', {'type': 'opt'})
        call packager#add('prabirshrestha/asyncomplete-lsp.vim', {'type': 'opt'})
        " Document support
        call packager#add('Shougo/echodoc.vim', {'type': 'opt'})

        " Markdown support
        call packager#add('godlygeek/tabular', {'type': 'opt'})
        call packager#add('plasticboy/vim-markdown', {'type': 'opt'})
        " Markdown preview
        call packager#add('iamcco/markdown-preview.nvim',
                    \ {'type': 'opt', 'do': {->mkdp#util#install()}})
        " Latex support
        call packager#add('lervag/vimtex', {'type': 'opt'})
        call packager#add('91khr/vim-latex-live-preview', {'type': 'opt'})

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
                    \ . " | execute \"autocmd! FileType " . ft . "\" | execute \"set ft=\" . &ft | e %"
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
    call s:AddPlugCmd('Goyo', 'packadd goyo.vim | Goyo <args>', {'args': '*'})
    call s:AddPlugCmd('LLPStartPreview', 'packadd vim-latex-live-preview | LLPStartPreview', {})
    call s:AddPlugCmd('LspOn', 'packadd async.vim | packadd vim-lsp |' .
                \ 'doautocmd User lsp_setup | call lsp#enable()', {})
    call s:AddPlugCmd('CompleteOn', 'packadd async.vim | packadd vim-lsp | ' .
                \ 'packadd asyncomplete.vim | packadd asyncomplete-lsp.vim |' .
                \ 'doautocmd User lsp_setup | call lsp#enable() | e', {})
    call s:AddPlugCmd('MarkdownPreview', 'packadd markdown-preview.nvim | e | MarkdownPreview', {})
    " }}} End plugin list and setup

    " Plugin commands
    command! PackUpdate call PackInit() | call packager#update()
    command! PackClean call PackInit() | call packager#clean()
    command! PackStatus call PackInit() | call packager#status()
    command! PackInstall call PackInit() | call packager#install()

    " {{{ Plugin options
    " ==================================================================================================================
    " Language Server and Echodoc settings
    " ==================================================================================================================
    " Register lsp when it enables
    " TODO: find way to enable autocomplete without re-registering server
    function s:RegisterLsp()
        let l:Getvar = { name -> exists(name) ? eval(name) : v:null }
        if IsExec('clangd')
            let l:cmdlst = (has('win32') ? ["wsl"] : []) + ["clangd", "-background-index"]
            let l:clangopt = {
                        \ 'name': 'clangd',
                        \ 'cmd': {_->l:cmdlst},
                        \ 'whitelist': ['c', 'cpp'],
                        \ }
            call lsp#register_server(l:clangopt)
            let s:CppServerAvailable = v:true
        endif
        if IsExec('lua-lsp')
            let l:cmdlst = (has('win32') ? ["wsl"] : []) + ["lua-lsp"]
            call lsp#register_server({
                        \ 'name': 'lua-lsp',
                        \ 'cmd': {_->l:cmdlst},
                        \ 'whitelist': ['lua'],
                        \ })
            let s:LuaServerAvailable = v:true
        endif
    endfunction
    autocmd User lsp_setup call s:RegisterLsp()
    " Well, it's exactly the same as autosetting,
    " but who knows if it will change one day?
    let g:asyncomplete_auto_completeopt = 0
    set completeopt=menuone,popup,noinsert,noselect
    " Enable echodoc
    let g:echodoc#enable_at_startup = 1
    let g:echodoc#enable_force_overwrite = 1

    " ==================================================================================================================
    " VimOI settings
    " ==================================================================================================================
    let g:rainbow_active = 1
    if has('win32')
        let g:VimOI_CompileArgs = [ '/Od', '/nologo', '/utf-8', '/EHsc', '/W4', '/D_CRT_SECURE_NO_WARNINGS' ]
    else
        let g:VimOI_CompileArgs = [ '-Wall', '-Wextra', '-DDEBUG' ]
    endif

    " ==================================================================================================================
    " Theme settings(Airline and solarized
    " ==================================================================================================================
    set laststatus=2  | " Ensure that status line is shown
    set noshowmode  | " The mode will be shown in status line
    let g:solarized_menu = 0  | " I don't use it, which would cause error
    let g:airline#extensions#tabline#enabled = 1  | " Display buffers
    let g:airline#extensions#tabline#formatter = 'default'  | " Display file path in default way
    let g:airline#extensions#wordcount#enabled = 1
    let g:airline#extensions#wordcount#filetypes = ['markdown', 'md', 'help', 'tex', 'plaintex', 'text']
    let g:airline#extensions#wordcount#formatter = 'cnfmt'

    " ==================================================================================================================
    " Vimtex, preview and markdown settings
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
    " Markdown options
    let g:vim_markdown_frontmatter = 1
    let g:vim_markdown_strikethrough = 1
    " Markdown preview
    let g:mkdp_refresh_slow = 1
    let g:mkdp_auto_close = 0
    let g:mkdp_browser = 'firefox'

    " ==================================================================================================================
    " Snippet settings
    " ==================================================================================================================
    " Never overwrite **MY** settings!
    let g:UltiSnipsExpandTrigger = '<C-S-M-Del>'
    let g:UltiSnipsListSnippets = '<C-S-M-Del>'

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
    " }}} End plugin options
    " }}} End plugins

    " Set the options
    SoFile .vim/options.vim

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
        if !exists(":Compile")
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
        if exists("s:CppServerAvailable") && s:CppServerAvailable
            setlocal omnifunc=lsp#complete
        endif
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
        command! -buffer Compile py3file %
    endfunction
    autocmd FileType python call s:PythonLanguageSettings()

    " ==================================================================================================================
    " Language settings: Racket
    " ==================================================================================================================
    function! s:RacketLanguageSettings()
        command! -buffer Compile !racket %
        if exists("s:RacketServerAvailable") && s:RacketServerAvailable
            setlocal omnifunc=lsp#complete
        endif
    endfunction
    autocmd FileType scheme call s:RacketLanguageSettings()

    " ==================================================================================================================
    " Language settings: Lua
    " ==================================================================================================================
    function! s:LuaLanguageSettings()
        command! -buffer Compile luafile %
        if exists("s:LuaServerAvailable") && s:LuaServerAvailable
            setlocal omnifunc=lsp#complete
        endif
    endfunction
    autocmd FileType lua call s:LuaLanguageSettings()

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

    " ==================================================================================================================
    " Language settings: Racket
    " ==================================================================================================================
    function! s:RacketLanguageSettings()
        command! -buffer -nargs=* Compile !racket %
    endfunction
    autocmd FileType scheme call s:RacketLanguageSettings()
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
