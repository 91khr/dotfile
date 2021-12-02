" ======================================================================================================================
" Note: Readers are suggested to use a wide(>120 chars) client to view or edit this file
" Author: Virginia Senioria
" Senioriae VIm configuration: C++ language configurations

" Compile options
if has("win32")
    " Require VimOI
    command! -buffer -bar -nargs=* Compile CppCompile % <args>
else
    command! -buffer -bar -nargs=* Compile w | exec "AsyncRun g++ % -std=c++20 -o %:t:r.out "
                \ .. "-Wall -Wextra -Weffc++ -Wpedantic -g "
                \ .. (has_key(g:, "compile_flags_cpp") ? g:compile_flags_cpp :
                \     get(b:, "compile_flags", []))->join(' ')
                \ .. <q-args>
endif
command! -buffer -bar Run Compile | autocmd User AsyncRunStop ++once
            \ if g:asyncrun_code == 0 | exec "botright term ./%:t:r.out" | call lightline#update() | endif

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

