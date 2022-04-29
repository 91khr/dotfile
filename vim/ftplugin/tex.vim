let s:compile_cmd = "w | AsyncRun cd %:h; xelatex %:t"

import "ftext.vim"
if s:ftext.CanCmd("Compile")
    command! -buffer -bar Compile exec s:compile_cmd
    let b:compile_overridable = 0
endif
if s:ftext.CanCmd("Run")
    command! -buffer -bar Run exec s:compile_cmd .. " &&"
                \ (has('win') ? "start" : "xdg-open") . " %:r.pdf"
    let b:run_overridable = 0
endif

