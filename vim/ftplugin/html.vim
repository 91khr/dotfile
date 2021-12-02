" ======================================================================================================================
" Note: Readers are suggested to use a wide(>120 chars) client to view or edit this file
" Author: Virginia Senioria
" Senioriae VIm configuration: HTML language configurations

setlocal foldmethod=indent

command! -buffer -bar -nargs=* Run w | exec "!" . (has("win32") ? "start" : "xdg-open") . ' '
            \ . expand("%") . ' ' . <q-args>

