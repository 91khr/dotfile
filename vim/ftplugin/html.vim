setlocal foldmethod=indent

import "ftext.vim"
if s:ftext.CanCmd("Run") && &ft == "html"
    command! -buffer -bar -nargs=* Run w | exec "!" . (has("win32") ? "start" : "xdg-open") . ' '
                \ . expand("%") . ' ' . <q-args>
    let b:run_overridable = 0
endif

