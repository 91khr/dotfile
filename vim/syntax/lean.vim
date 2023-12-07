syn match leanLineComment /--.*/
syn region leanBlockComment start=+/-+ end=+-/+ contains=leanComment

hi def link leanBlockComment Comment
hi def link leanLineComment Comment
