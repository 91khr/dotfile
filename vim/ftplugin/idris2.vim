vim9script
import "ftext.vim"
if ftext.CanCmd("Compile")
    command! -buffer -bar Compile w | ftext.TermRun([ "idris2", expand("%") ],
                \     { persist: false, unique: true })
    b:compile_overridable = 0
endif

if bufname() == "idris-response"
    set wfh
endif

