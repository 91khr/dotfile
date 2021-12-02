" ======================================================================================================================
" Note: Readers are suggested to use a wide(>120 chars) client to view or edit this file
" Author: Virginia Senioria
" Senioriae VIm configuration: VIm language configurations

setlocal foldmethod=marker
command! -buffer -bar Compile w | exec "AsyncRun gvim -c 'so " . expand('%') . "'"
command! -buffer -bar Run w | so %

