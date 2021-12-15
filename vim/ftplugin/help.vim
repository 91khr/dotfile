" Set indention & spelling when editing help files ><
setlocal autoindent
" No space check: they're used for formatting ><
let b:spacecheck_disabled = v:true
" If writing doc, enable spell check ><
if !&ro
    setl spell
endif

