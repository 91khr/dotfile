" ======================================================================================================================
" Note: Readers are suggested to use a wide(>120 chars) client to view or edit this file
" Author: Virginia Senioria
" Senioriae VIm configuration: Latex language configurations

let s:compile_cmd = "AsyncRun cd %:h; xelatex %:t"
command! -buffer -bar Compile w | exec s:compile_cmd
command! -buffer -bar Run Compile | exec s:compile_cmd .. "&&"
            \ (has('win') ? "start" : "xdg-open") . " %:r.pdf"

