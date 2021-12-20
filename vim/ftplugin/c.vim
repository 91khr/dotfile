import TermRun from "ftext.vim"

" Compile options
if has("win32")
    " Require VimOI
    command! -buffer -bar -nargs=* Compile CppCompile % <args>
else
    command! -buffer -bar -nargs=* Compile exec "AsyncRun g++ % -std=c++20 -o a.out "
                \ .. "-Wall -Wextra -Weffc++ -Wpedantic -g "
                \ .. (has_key(g:, "compile_flags_cpp") ? g:compile_flags_cpp :
                \     get(b:, "compile_flags", []))->join(' ')
                \ .. " " .. <q-args>
endif

function! s:Run(args)
    if g:asyncrun_code != 0 | return | endif
    call s:TermRun(["./a.out", a:args], #{ persist: v:true, unique: v:false })
    doautocmd WinEnter !.
endfunction
command! -buffer -bar -nargs=? Run Compile | autocmd User AsyncRunStop ++once call s:Run(<q-args>)

" Set C-style indent and options
function! CppIndent()
    let l:prevline = prevnonblank(line('.') - 1)
    let l:prevctnt = getline(l:prevline)
    let l:indent = cindent('.')
    if l:prevctnt =~# '\m^\s*template<.\{-}>\(\s*\(requires.*\)\?\)$'
        let l:indent = cindent(l:prevline)
    endif
    return l:indent
endfunction
setlocal indentexpr=CppIndent()
setlocal cinoptions+=L0.5s:0g0N-sj1

