function! s:CompileMarkdown(...)
    execute ":AsyncRun pandoc % -s -o %:t:r.html " . a:000->flattennew()->join(' ')
endfunction
command! -buffer -bar -nargs=* Compile w | call s:CompileMarkdown(<f-args>)
command! -buffer -bar -nargs=* Run exec "!" . (has("win32") ? "start" : "xdg-open") . ' ' .
            \ expand("%:t:r") . '.html ' . <q-args>

let b:coc_suggest_disable = 1

