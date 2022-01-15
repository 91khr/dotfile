setlocal foldmethod=indent

if !exists(":Run")
    command! -buffer -bar -nargs=* Run w | exec "!" . (has("win32") ? "start" : "xdg-open") . ' '
                \ . expand("%") . ' ' . <q-args>
endif

