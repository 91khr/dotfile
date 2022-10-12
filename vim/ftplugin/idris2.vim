vim9script
import "ftext.vim"
if ftext.CanCmd("Compile")
    command! -buffer -bar Compile w | ftext.TermRun([ "idris2", expand("%"),
                \         get(g:, "compile_flags_idris2", []), get(b:, "compile_flags_idris2", []), <q-args> ]
                \     ->flattennew(),
                \     { persist: false, unique: true })
    b:compile_overridable = 0
endif

if bufname() == "idris-response"
    set wfh
endif

