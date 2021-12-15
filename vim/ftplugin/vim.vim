setlocal foldmethod=marker
command! -buffer -bar Compile w | exec "AsyncRun gvim -c 'so " . expand('%') . "'"
command! -buffer -bar Run w | so %

