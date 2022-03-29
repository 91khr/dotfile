import "ftext.vim"

if !exists(":Compile")
    command! -buffer -bar Compile w | eval s:ftext.TermRun([ "racket", "-ie",
                \     printf("(enter! (file \"%s\"))", expand("%")) ],
                \     #{ persist: v:false, unique: v:true })
endif
if !exists(":Run")
    command! -buffer -bar Run w | botright term racket %
endif

" Add some missing syntaxes ><
setlocal lispwords+=syntax-case,match,for/list,for/fold

" Only preserve the outmost fold ><
autocmd BufEnter * ++once silent! %foldopen! | silent! %foldclose

