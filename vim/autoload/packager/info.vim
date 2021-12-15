" A simple package manager -- info window

" {{{ Guard
"if exists('g:loaded_packager_info') && g:loaded_packager_info | finish | endif
let g:loaded_packager_info = v:true
" }}}

" {{{ Constants
let s:waiting_msg = "Pending..."
let s:default_name = "(default)"
" }}}

" {{{ Global stats
" Number, info buffer id
let s:bufnr = -1
" Tasks table copied from input,
" type: str(name) -> str(subtask, '' for default) -> {
"   // Will not decrease
"   status: enum(pending, running, finished),
"   code?: int,
"   output: [str]
" }
let s:tasks = {}
" Currently done tasks
let s:done_count = 0
" Info of tasks
" type: str(name) -> {
"   line: int,  // The line it is at
"   subdone: int,  // How many subtasks are done
" }
let s:info = {}
" }}} End global stats

" {{{ Helpers
" {{{ s:render_progress()
" Assume the buffer exists
function! s:render_progress()
    let l:total = len(s:tasks)
    call setbufline(s:bufnr, 1,
                \ printf("Status: %s/%s", s:done_count, l:total))
    let l:totlen = (winwidth(bufwinid(s:bufnr)) - 3) * 7 / 10
    let l:leftlen = l:totlen * s:done_count / l:total
    call setbufline(s:bufnr, 2,
                \ printf("[%s>%s]", repeat('=', l:leftlen),
                \     repeat('-', l:totlen - l:leftlen)))
endfunction
" }}} End s:render_progress

" {{{ s:render_all(opt: {isinit=false})
function! s:render_all(opt = #{ isinit: v:false })
    " Render the header
    call s:render_progress()
    call setbufline(s:bufnr, 3, '')
    " Render tasks
    let l:maxline = 4
    for l:name in keys(s:tasks)
        if a:opt.isinit
            let s:info[l:name].line = l:maxline
            let l:maxline += 1
        endif
        call s:render_item(l:name)
    endfor
endfunction
" }}} End s:render_all

" {{{ s:render_item(name: str)
function! s:render_item(name)
    " {{{ SetProp(name: str, subtask: str, line: int, len_before: int)
    function! SetProp(name, subtask, line, len_before)
        let l:task = s:tasks[a:name][a:subtask]
        if l:task.status == 0  | " Pending
                call prop_add(a:line, a:len_before, #{
                            \     length: strlen(s:waiting_msg),
                            \     bufnr: s:bufnr,
                            \     type: "waiting",
                            \ })
        elseif l:task.status == 2  | " Finished
            let l:outlen = strlen(empty(l:task.output) ? '' : l:task.output[-1])
            call prop_add(a:line, a:len_before, #{
                        \     length: l:outlen,
                        \     bufnr: s:bufnr,
                        \     type: l:task.code == 0 ? "normal_exit" : "error_exit",
                        \ })
        endif
    endfunction
    " }}} End SetProp
    let l:Output = { info -> info.status == 0 ? s:waiting_msg :
                \     empty(info.output) ? '' : l:info.output[-1] }

    let l:line = s:info[a:name].line
    let l:subtasks = s:tasks[a:name]
    if len(l:subtasks) <= 1
        " {{{ Single line task
        call setbufline(s:bufnr, l:line,
                    \ printf("%s: %s", a:name, l:Output(l:subtasks[''])))
        call SetProp(a:name, '', l:line, strlen(a:name) + 3)
        " }}} End single line task
    else
        " {{{ Task with subtasks
        let l:info = s:info[a:name]
        call setbufline(s:bufnr, l:line,
                    \ printf("%s: %d/%d", a:name, l:info.subdone, len(l:subtasks)))
        for [l:subname, l:subinfo] in items(l:subtasks)
            let l:line += 1
            let l:subname_display = l:subname == '' ? s:default_name : l:subname
            let l:preinfo = '  - ' . l:subname_display . ': '
            let l:content = l:preinfo . l:Output(l:subinfo)
            call setbufline(s:bufnr, l:line, l:content)
            call prop_add(l:line, 5, #{
                        \     length: strlen(l:subname_display),
                        \     bufnr: s:bufnr,
                        \     type: "task_name",
                        \ })
            call SetProp(a:name, l:subname, l:line, strlen(l:preinfo) + 1)
        endfor
        " }}} End task with subtasks
    endif

    call prop_add(s:info[a:name].line, 1, #{
                \     length: strlen(a:name),
                \     bufnr: s:bufnr,
                \     type: "task_name",
                \ })
endfunction
" }}} End s:render_item
" }}} End helpers

function! packager#info#init(tasks)
    let s:tasks = a:tasks
    let s:done_count = 0
    for l:nowtask in values(s:tasks)
        for l:subtask in values(l:nowtask)
            if !has_key(l:subtask, 'output')
                let l:subtask.output = []
            endif
            if !has_key(l:subtask, 'status')
                let l:subtask.status = has_key(l:subtask, 'code') ? 2 :
                            \ l:subtask.output != [] ? 1 : 0
            endif
            if l:subtask.status == 2
                let s:done_count += 1
            endif
        endfor
    endfor
    for l:subname in keys(s:tasks)
        let s:info[l:subname] = #{ subdone: 0 }
    endfor
endfunction

function! packager#info#show()
    split [Package Status]
    setlocal buftype=nofile bufhidden=delete noswapfile nonu nobuflisted
    let s:bufnr = bufnr('')
    call prop_type_add("task_name", #{ bufnr: s:bufnr, highlight: "Identifier" })
    call prop_type_add("waiting", #{ bufnr: s:bufnr, highlight: "Comment" })
    call prop_type_add("error_exit", #{ bufnr: s:bufnr, highlight: "ErrorMsg" })
    call prop_type_add("normal_exit", #{ bufnr: s:bufnr, highlight: "String" })
    call s:render_all(#{ isinit: v:true })
endfunction

function! packager#info#hide()
    execute s:bufnr . 'bw'
endfunction

function! packager#info#update(name, info)
    if !has_key(s:tasks, a:name)
        throw 'packager-E233: ' . a:name . ' not exist in packs, try force init?'
    endif
    let l:subtasks = s:tasks[a:name]
    for [l:subname, l:subinfo] in items(a:info)
        " If no such subtask, create it.
        if !has_key(l:subtasks, l:subname)
            throw 'packager-E233: subtask ' . l:subname .
                        \ ' not exist in task ' . a:name . ', try force init?'
        endif
        let l:target = l:subtasks[l:subname]
        if has_key(l:subinfo, 'code')
            if l:target.status < 2
                let s:info[a:name].subdone += 1
            endif
            let l:target.code = l:subinfo.code
            let l:target.status = 2
        endif
        if has_key(l:subinfo, 'output')
            let l:target.output = type(l:subinfo.output) == v:t_string ?
                        \ l:target.output + [l:subinfo.output] :
                        \ l:target.output + l:subinfo.output
            if l:target.status < 1
                let l:target.status = 1
            endif
        endif
    endfor
    if s:info[a:name].subdone == len(l:subtasks)
        let s:done_count += 1
        if bufexists(s:bufnr)
            call s:render_progress()
        endif
    endif
    if bufexists(s:bufnr)
        call s:render_item(a:name)
    endif
endfunction

" vim: foldmethod=marker
