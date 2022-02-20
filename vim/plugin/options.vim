" {{{ Settings
" ======================================================================================================================
" Settings
" ======================================================================================================================
set nocp  | " Vi is old
set display=truncate,uhex
set incsearch
set langnoremap
set nolangremap
set nrformats=bin,hex
set suffixes=.bak,~,.swp,.o,.info,.aux,.log,.dvi,.bbl,.blg,.brf,.cb,.ind,.idx,.ilg,.inx,.out,.toc,.png,.jpg
set ttimeout
set ttimeoutlen=100
set wildmenu  | " Command mode completion(though hard to use ><)
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
" Open the syntax and corresponding highlight
filetype plugin indent on
syntax on
" Highlight corrent line
set cursorline
" Open the line number when is not pager
autocmd VimEnter * if get(g:, 'vimpager', 0) == 0 | set number | endif
" Show entered commands
set showcmd
" Set the width of indent and tab
set shiftwidth=4 tabstop=4 expandtab
" Let backspace more friendly
set bs=3
" Open code folding
set foldmethod=syntax
" .viminfo is not needed > <
set viminfofile=NONE
" Width for line breaking and vertical prompt line
set textwidth=120
set colorcolumn=+0
" Use LF by default
set fileformat=unix
set fileformats=unix,dos
" Set the splitting behavior
set splitright noequalalways
set switchbuf=usetab,split,uselast
" Status line :)
set laststatus=2  | " Ensure that status line is shown
set noshowmode  | " The mode will be shown in status line

" ======================================================================================================================
" Terminal settings
" ======================================================================================================================
" Use <Esc> (or its equivant) will cause E21 qwq
tnoremap <C-\> <C-W>N
" }}} End settings

" {{{ Mappings
" ======================================================================================================================
" Mappings
" ======================================================================================================================
" Reset the leader
let mapleader=' '
" Fast fold code
map <leader><space> za
" Clear search
nnoremap <silent><leader>/ <Cmd>let @/=''<CR>
" Make cursor move in the virtual lines
noremap <silent> <expr> j (v:count == 0 ? 'gj' : 'j')
noremap <silent> <expr> k (v:count == 0 ? 'gk' : 'k')
" Emacs style motion in cmdline mode(q: may be better)
cnoremap <C-B> <Left>
cnoremap <C-F> <Right>
cnoremap <A-B> <C-Left>
cnoremap <A-F> <C-Right>
cnoremap <C-A> <Home>
cnoremap <C-E> <End>
set cedit=\<C-K>
" Run shell commands -- :! is considered not useful
nnoremap <leader>; :AsyncRun<space>
" Quick compile & Run
nnoremap <silent><leader>cc <Cmd>Compile<CR>
nnoremap <silent><leader>cr <Cmd>Run<CR>
" Autoselect suggest in completion & expand snippets
inoremap <silent><expr><Tab>
            \ UltiSnips#CanExpandSnippet() ? "<C-R>=UltiSnips#ExpandSnippet()<CR>" :
            \ pumvisible() ? "<C-N>" :
            \ UltiSnips#CanJumpForwards() ? "<C-R>=UltiSnips#JumpForwards()<CR>" : "<Tab>"
inoremap <silent><expr><S-Tab>
            \ pumvisible() ? "<C-P>" :
            \ UltiSnips#CanJumpBackwards() ? "<C-R>=UltiSnips#JumpBackwards()<CR>" : "<Tab>"
xnoremap <silent><Tab> :call UltiSnips#SaveLastVisualSelection()<CR>gvs
snoremap <silent><expr><Tab> <Esc>:call UltiSnips#ExpandSnippet()<CR>
" Confirm completion
inoremap <silent><expr><CR> pumvisible() ? "<C-Y>" : "<CR>"
" LSP actions
noremap \a <Cmd>CocAction<CR>
noremap \? <Cmd>CocDiagnostics<CR>
nnoremap \\ <Cmd>call CocAction('doHover')<CR>
nmap gd <Plug>(coc-definition)
nmap gr <Plug>(coc-references)
nmap \j <Plug>(coc-diagnostic-next)
nmap \k <Plug>(coc-diagnostic-prev)
nmap \r <Plug>(coc-refactor)
nmap \= <Plug>(coc-format)
vmap \= <Plug>(coc-format-selected)
" }}} End mappings

" {{{ GUI and System settings
" ======================================================================================================================
" Gui settings
" ======================================================================================================================
if has("gui_running")
    " Color scheme
    colo solarized
    " Extracted from SolarizedXTerm
    "hi Terminal guibg=#002B36 guifg=#D2D2D2
    hi link Terminal Normal
    let g:terminal_ansi_colors = [
                \ "#222222", "#9E5641", "#6C7E55", "#CAAF2B", "#7FB8D8", "#956D9D",
                \ "#4c8ea1", "#808080", "#454545", "#CC896D", "#C4DF90", "#FFE080",
                \ "#B8DDEA", "#C18FCB", "#6bc1d0", "#cdcdcd", ]
    " Ban the annoying bell(cant be seen on Linux gui)
    set vb
    " I dont need the controls
    set go=''
    " Make error easier to see
    hi Error gui=undercurl
    " Use a larger default size x_x
    set columns=120 lines=40
else  | " GUI ^^^ Term vvv
    " Color scheme
    set background=dark
    colo desert
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
    set guifont=Monaco
endif
" }}} End GUI and system settings

" {{{ Package options
" ==================================================================================================================
" Language Server, Echodoc and UltiSnip settings
" ==================================================================================================================
set completeopt=menuone,popup,noinsert,noselect
let g:UltiSnipsExpandTrigger = "<nop>"  | " The config will handle it ><

" ==================================================================================================================
" Rainbow settings
" ==================================================================================================================
let g:rainbow_active = 1

" ==================================================================================================================
" VimOI settings
" ==================================================================================================================
if has('win32')
    let g:VimOI_CompileArgs = [ '/Od', '/nologo', '/utf-8', '/EHsc', '/W4', '/D_CRT_SECURE_NO_WARNINGS' ]
else
    let g:VimOI_CompileArgs = [ '-Wall', '-Wextra', '-DDEBUG' ]
endif

" ==================================================================================================================
" Vimtex, preview and markdown settings
" ==================================================================================================================
let g:tex_flavor = "latex"
let g:vimtex_enabled = 1
let g:vimtex_fold_enabled = 1
let g:vimtex_compiler_latexmk = {
            \     'options' : [
            \       '-xelatex',
            \     ],
            \ }
let g:livepreview_engine = 'xelatex'
if has("win32")
    let g:livepreview_previewer = 'start'
else
    let g:livepreview_previewer = 'zathura'
    set updatetime=400
endif
" Markdown options
let g:vim_markdown_math = 1
let g:vim_markdown_frontmatter = 1
let g:vim_markdown_strikethrough = 1
let g:vim_markdown_new_list_item_indent = 2
" Markdown preview
let g:mkdp_refresh_slow = 1
let g:mkdp_auto_close = 0
let g:mkdp_browser = 'firefox'

" ==================================================================================================================
" NERD Tree and Netrw settings
" ==================================================================================================================
let g:netrw_liststyle=3

" ==================================================================================================================
" Goyo settings
" ==================================================================================================================
let g:goyo_width = '80%'
let g:goyo_height = '95%'
let g:goyo_linenr = 1
" }}} End package options

