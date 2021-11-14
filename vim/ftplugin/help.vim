" ======================================================================================================================
" Note: Readers are suggested to use a wide(>120 chars) client to view or edit this file
" Author: Virginia Senioria
" Senioriae VIm configuration: Help file configurations

" Set indention & spelling when editing help files ><
setlocal autoindent
" No space check: they're used for formatting ><
let b:spacecheck_disabled = v:true
" If writing doc, enable spell check ><
if ! &ro
    setl spell
endif

