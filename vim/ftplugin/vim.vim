setlocal foldmethod=marker

import "ftext.vim"
if s:ftext.CanCmd("Compile")
    command! -buffer -bar -nargs=* Compile w | exec "AsyncRun gvim -c 'so " .. expand('%') .. "'"
                \ .. (empty(<q-args>) ? "" : " -c " .. shellescape(<q-args>))
    let b:compile_overridable = 0
endif
if s:ftext.CanCmd("Run")
    command! -buffer -bar Run w | so %
    let b:run_overridable = 0
endif

