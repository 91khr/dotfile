import "ftext.vim"

" Compile options
if has("win32")
    if s:ftext.CanCmd("Compile")
        " Require VimOI
        command! -buffer -bar -nargs=* Compile CppCompile % <args>
        let b:compile_overridable = 0
    endif
else
    exec s:ftext.MakeRun("cpp", "Compile",
                \ "'g++ % -std=c++20 -o %:h/a.out -Wall -Wextra -Weffc++ -Wpedantic -g ' .. {args}")
endif

function! s:Run(args)
    if g:asyncrun_code != 0 | return | endif
    eval s:ftext.TermRun([expand("%:p:h") .. "/a.out", a:args], #{ persist: v:true, unique: v:false })
    doautocmd WinEnter !.
endfunction
exec s:ftext.MakeRun("cpp", "Run", "call s:Run({args})")

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

