" Load markdown
unlet! b:current_syntax
runtime! syntax/markdown.vim
let b:current_syntax = "mail"

" Flowed text use trailing spaces to mark soft line break, so highlight them
syn match mailLineContinuation / \+$/
let linecont_hl = hlget("LineNr", v:true)[0]
let linecont_hl = extend(linecont_hl, #{
            \     name: "mailLineContinuation",
            \     cterm: extend(linecont_hl->get('cterm', {}), #{ reverse: v:true }),
            \     gui: extend(linecont_hl->get('gui', {}), #{ reverse: v:true }),
            \ }, "force")
call hlset([linecont_hl])

