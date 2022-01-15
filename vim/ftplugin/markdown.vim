function! s:CompileMarkdown(...)
    execute ":AsyncRun pandoc % -s -o %:t:r.html " . a:000->flattennew()->join(' ')
endfunction
if !exists(":Compile")
    command! -buffer -bar -nargs=* Compile w | call s:CompileMarkdown(<f-args>)
endif
if !exists(":Run")
    command! -buffer -bar -nargs=* Run exec "!" . (has("win32") ? "start" : "xdg-open") . ' ' .
                \ expand("%:t:r") . '.html ' . <q-args>
endif

let b:coc_suggest_disable = 1

