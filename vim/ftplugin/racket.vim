vim9script
import "ftext.vim"
import autoload "repl.vim"

ftext.CmdEngine.new("Compile", (...args) => {
    w
    ftext.TermRun([ "racket", "-ie",
                \     printf("(enter! (file \"%s\"))", expand("%:p")) ],
                \     { persist: false, unique: true, termfn: repl.MkRepl })
}).Do()
ftext.CmdEngine.new("Run", (...args) => {
    w
    botright term racket %:p
}).Do()

# Add some missing syntaxes ><
setlocal lispwords+=syntax-case,match
setlocal lispwords+=for,for*,for/list,for/fold,for/or,for/sum
setlocal lispwords+=generator

# Only preserve the outmost fold ><
autocmd BufEnter * ++once silent! %foldopen! | silent! %foldclose

