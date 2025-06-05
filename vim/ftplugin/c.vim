vim9script

import "ftext.vim"

# Compile options
if has("win32")
    if s:ftext.CanCmd("Compile")
        # Require VimOI
        command! -buffer -bar -nargs=* Compile CppCompile % <args>
        b:compile_overridable = 0
    endif
else
    ftext.CmdEngine.new("Compile",
                \ "AsyncRun g++ % -std=c++23 -o %:h/a.out -Wall -Wextra -Weffc++ -Wpedantic -g {args}")
                .Do()
endif

ftext.CmdEngine.new("Run", (...args) => {
    ftext.TermRun([expand("%:p:h") .. "/a.out"] + args, { persist: v:true, unique: v:false })
    doautocmd WinEnter !.
}).WaitAsyncCmd().Do()

# Set C-style indent and options
function CppIndent()
    let l:prevline = prevnonblank(line('.') - 1)
    let l:prevctnt = getline(l:prevline)
    let l:indent = cindent('.')
    if l:prevctnt =~# '\m^\s*template<.\{-}>\(\s*\(requires.*\)\?\)$'
        let l:indent = cindent(l:prevline)
    endif
    return l:indent
endfunction
&l:indentexpr = expand("<SID>") .. "CppIndent()"
setlocal cinoptions+=L0.5s,:0,g0,N-s,j1

