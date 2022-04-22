syn keyword schemeSyntax syntax-case match for/list for/fold for/or for/sum
syn keyword schemeSyntax provide

syn keyword schemeFunction require
" Range and util functions
syn keyword schemeFunction foldl foldr in-naturals curry generator
" Formatting functions
syn keyword schemeFunction format printf string-join ~a ~r

syn match schemeImport /\%^\#lang .*/

