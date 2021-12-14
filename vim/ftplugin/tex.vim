" ======================================================================================================================
" Note: Readers are suggested to use a wide(>120 chars) client to view or edit this file
" Author: Virginia Senioria
" Senioriae VIm configuration: Latex language configurations

let s:compile_cmd = "w | AsyncRun cd %:h; xelatex %:t"
command! -buffer -bar Compile exec s:compile_cmd
command! -buffer -bar Run exec s:compile_cmd .. " &&"
            \ (has('win') ? "start" : "xdg-open") . " %:r.pdf"

