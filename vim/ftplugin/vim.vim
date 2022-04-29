setlocal foldmethod=marker

import "ftext.vim"
if s:ftext.CanCmd("Compile")
    command! -buffer -bar Compile w | exec "AsyncRun gvim -c 'so " . expand('%') . "'"
    let b:compile_overridable = 0
endif
if s:ftext.CanCmd("Run")
    command! -buffer -bar Run w | so %
    let b:run_overridable = 0
endif

