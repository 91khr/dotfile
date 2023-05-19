vim9script
import "ftext.vim"

ftext.CmdEngine.new("Compile", (...args) => {
    w
    ftext.TermRun(([ "idris2", expand("%"),
                \         get(g:, "compile_flags_idris2", []), get(b:, "compile_flags_idris2", []) ] + args)
                \     ->flattennew(),
                \     { persist: false, unique: true })
}).Do()

if bufname() == "idris-response"
    set wfh
endif

