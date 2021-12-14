" ======================================================================================================================
" Note: Readers are suggested to use a wide(>120 chars) client to view or edit this file
" Author: Virginia Senioria
" Senioriae VIm configuration: Racket language configurations
import TermRun from "ftext.vim"

" Util to open REPL & execute file
command! -buffer -bar Compile w | call s:TermRun([ "racket", "-ie",
            \     printf("(enter! (file \"%s\"))", expand("%")) ],
            \     #{ persist: v:false, unique: v:true })
command! -buffer -bar Run w | botright term racket %

" Add some missing options ><
setlocal lispwords+=syntax-case

" Only preserve the outmost fold ><
autocmd BufEnter * ++once silent! %foldopen! | silent! %foldclose

