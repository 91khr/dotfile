setlocal foldmethod=marker
if !exists(":Compile")
    command! -buffer -bar Compile w | exec "AsyncRun gvim -c 'so " . expand('%') . "'"
endif
if !exists(":Run")
    command! -buffer -bar Run w | so %
endif

