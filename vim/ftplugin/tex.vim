let s:compile_cmd = "w | AsyncRun cd %:h; xelatex %:t"
command! -buffer -bar Compile exec s:compile_cmd
command! -buffer -bar Run exec s:compile_cmd .. " &&"
            \ (has('win') ? "start" : "xdg-open") . " %:r.pdf"

