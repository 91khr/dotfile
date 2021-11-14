" ======================================================================================================================
" Note: Readers are suggested to use a wide(>120 chars) client to view or edit this file
" Author: Virginia Senioria
" Senioriae VIm configuration: Racket language configurations

" Util to open REPL & execute file
let b:replbuf = -1
function! s:RunREPL()
    if bufexists(b:replbuf)
        exec string(b:replbuf) .. "bw!"
    endif
    let l:curbuf = bufnr()
    botright call setbufvar(l:curbuf, "replbuf",
                \ term_start([ "racket", "-ie",
                \     printf("(enter! \"%s\")", expand("%")) ], #{
                \         term_finish: "close", term_rows: float2nr(0.35 * winheight(0)),
                \ }))
endfunction
command! -buffer Compile call s:RunREPL()
command! -buffer Run AsyncRun racket %

" Add some missing options ><
setlocal lispwords+=syntax-case

" Only preserve the outmost fold ><
autocmd BufEnter * ++once silent! %foldopen! | silent! %foldclose

