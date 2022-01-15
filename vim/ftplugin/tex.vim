let s:compile_cmd = "w | AsyncRun cd %:h; xelatex %:t"
if !exists(":Compile")
    command! -buffer -bar Compile exec s:compile_cmd
endif
if !exists(":Run")
    command! -buffer -bar Run exec s:compile_cmd .. " &&"
                \ (has('win') ? "start" : "xdg-open") . " %:r.pdf"
endif

