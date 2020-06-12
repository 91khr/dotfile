" ======================================================================================================================
" Note: You are suggested to use a wide(>120 chars) client to view or edit this file
" Author: Virginia Senioria
" A simple package manager -- manager

" {{{ Guard
"if exists('g:loaded_packager_manager') && g:loaded_packager_manager | finish | endif
let g:loaded_packager_manager = v:true
" }}}

" {{{ Constants
let s:PackStatus = #{
            \     Missing: 0,
            \     NeedUpdate: 1,
            \     UpToDate: 2,
            \     Diverge: 3,
            \ }
" }}} End constants

" {{{ Global stats
" type: {
"   done: bool,
"   max_job: int,
"   path: str,
"   packs: (same as g:packager_config.packs)
" }
let s:conf = {}
" type: str(name) -> {
"   dirname: str,
"   fullpath: str,
"   url: str,
" }
let s:info = {}
" }}} End global stats

" {{{ packager#manager#init
function! packager#manager#init(opt = {})
    " Check if inited
    if has_key(s:conf, 'done') && s:conf.done && (!has_key(a:opt, 'force') || !a:opt.force)
        if has_key(a:opt, 'silent') && a:opt.silent | echo "No need init, quiting" | endif
        return
    endif
    " Load config
    let s:conf.max_job = has_key(g:packager_config, 'max_job') ? g:packager_config.max_job : 8
    let s:conf.path = has_key(g:packager_config, 'packpath') ? g:packger_config.packpath :
                \ expand(has('win32') ? '%userprofile%' : '$HOME') . '/.vim/pack/packager'
    let s:conf.done = v:true
    if !has_key(g:packager_config, 'packs')
        throw "packager-E514: entry 'packs' not provided in g:packager_config"
                    \ . ": You must present one, or why you use a package manager?"
    endif
    let s:conf.packs = []
    for l:pack_info in g:packager_config.packs
        " {{{ Init entry
        if type(l:pack_info) == v:t_string
            let l:nowpack = #{ name: l:pack_info }
        else
            let l:nowpack = l:pack_info
        endif
        if !has_key(l:nowpack, 'location')
            let l:nowpack.location = 'opt'
        endif
        let s:conf.packs += [l:nowpack]
        " }}} End init entry
        " {{{ Init info
        let l:info = {}
        " Resolve pack name
        if l:nowpack.name =~# '\v^(http|https)\:\/\/'
            let l:info.url = l:nowpack.name
        elseif l:nowpack.name =~# '\v^[^/]+\/[^/]+$'
            let l:info.url = "https://github.com/" . l:nowpack.name
        elseif l:nowpack.name =~# '\v^\w+\@[^:/]+\:'
            let l:info.url = l:nowpack.name
        endif
        let l:info.dirname = matchstr(l:nowpack.name, '\v\/\zs[^/]+$')
        let l:info.fullpath = s:conf.path . '/' . l:nowpack.location . '/' . l:info.dirname
        let s:info[l:nowpack.name] = l:info
        " }}} End init info
    endfor
endfunction
" }}} End packager#manager#init

" {{{ packager#manager#clean
function! packager#manager#clean(opt = {})
    if !has_key(s:conf, 'done') || !s:conf.done
        throw 'packager-E114: Attemp to clean packages before init.'
    endif
    let l:unlisted = #{ opt: [], start: [] }
    " {{{ Fetch unlisted packages
    let l:filelist = {}
    for l:location in ['opt', 'start']
        let l:basepath = s:conf.path . '/' . l:location
        for l:dir in readdir(l:basepath, {->isdirectory(l:basepath . '/' . v:val)})
            let l:filelist[l:dir] = l:location
        endfor
    endfor
    for l:pack in s:conf.packs
        let l:info = s:info[l:pack.name]
        if isdirectory(l:info.fullpath)
            let l:filelist[l:info.dirname] = ''
        endif
    endfor
    for [l:pack, l:location] in items(l:filelist)
        if !empty(l:location)
            let l:unlisted[l:location] += [l:pack]
        endif
    endfor
    unlet l:filelist
    " }}} End fetch unlisted packages
    " {{{ Show info and ask for confirmation
    if !has_key(a:opt, 'silent') || !a:opt.silent
        let l:display = {}
        for l:pack in l:unlisted.opt + l:unlisted.start
            let l:display[l:pack] = { '': {} }
        endfor
        call packager#info#init(l:display)
        call packager#info#show()
        redraw
    endif
    if (!has_key(a:opt, 'silent') || !a:opt.silent) && (!has_key(a:opt, 'confirm') || a:opt.confirm)
        if confirm("Are you going to remove all these packages?", "&Yes\n&No", "Yes", "Question") == 2
            return
        endif
    endif
    " }}} End show info and confirmation
    " {{{ Do remove
    for l:location in ['opt', 'start']
        for l:pack in l:unlisted[l:location]
            let l:code = delete(s:conf.path . '/' . l:location . '/' . l:pack, "rf")
            call packager#info#update(l:pack, { '':
                        \     l:code == 0 ?
                        \         #{ code: 0, output: ['Removed!'] } :
                        \         #{ code: l:code, output: ['Remove failed!'] } })
        endfor
    endfor
    " }}} End removing
endfunction
" }}} End packager#manager#clean

" {{{ packager#manager#status
function! packager#manager#status(opt = {})
    if !has_key(s:conf, 'done') || !s:conf.done
        throw 'packager-E114: Attemp to query status before init.'
    endif
    " {{{ Query status
    let l:status = {}
    for l:pack in s:conf.packs
        let l:path = s:info[l:pack.name].fullpath
        if !isdirectory(l:path)
            let l:status[l:pack.name] = s:PackStatus.Missing
            continue
        endif
        let l:local = system("git -C " . l:path . " rev-parse @")
        let l:remote = system("git -C " . l:path . " rev-parse @{u}")
        let l:base = system("git -C " . l:path . " merge-base @ @{u}")
        if l:local == l:remote
            let l:status[l:pack.name] = s:PackStatus.UpToDate
        elseif l:local == l:base
            let l:status[l:pack.name] = s:PackStatus.NeedUpdate
        else
            let l:status[l:pack.name] = s:PackStatus.Diverge
        endif
    endfor
    " }}} End query status
    " {{{ Show status
    if !has_key(a:opt, 'silent') || !a:opt.silent
        let l:display = {}
        for [l:nowname, l:nowstat] in items(l:status)
            let l:display[l:nowname] = { '': ({
                        \     s:PackStatus.Missing: #{ output: ['Not installed'] },
                        \     s:PackStatus.NeedUpdate: #{ output: ['Update required'] },
                        \     s:PackStatus.Diverge: #{ code: 1, output: ['Diverge from remote'] },
                        \     s:PackStatus.UpToDate: #{ code: 0, output: ['Up to date'] },
                        \ })[l:nowstat] }
        endfor
        call packager#info#init(l:display)
        call packager#info#show()
    endif
    " }}} End show status
    return l:status
endfunction
" }}} End packager#manager#status

" {{{ packager#manager#sync
function! packager#manager#sync(opt = {})
    if !has_key(s:conf, 'done') || !s:conf.done
        throw 'packager-E114: Attemp to sync packages before init.'
    endif
    " {{{ Gather package info
    let l:status = packager#manager#status(#{ silent: v:true })
    let l:status = filter(l:status, {-> v:val != s:PackStatus.UpToDate})
    let l:action = {}
    for [l:packname, l:packstat] in items(l:status)
        if l:packstat == s:PackStatus.Diverge
            " TODO: Add win32 support
            let l:action[l:packname] = 'echo Package diverged! && return 1'
        elseif l:packstat == s:PackStatus.Missing
            let l:action[l:packname] = printf('git clone --progress %s %s',
                        \ s:info[l:packname].url, s:info[l:packname].fullpath)
        elseif l:packstat == s:PackStatus.NeedUpdate
            let l:action[l:packname] = printf('git -C %s pull --ff-only --progress --rebase=false',
                        \ s:info[l:packname].fullpath)
        endif
    endfor
    let l:pending_packs = keys(l:status)
    " }}} End gathering package info
    " {{{ Job helpers
    let l:job_io_handler = #{ lastjob: 0 }
    if !has_key(a:opt, 'silent') || !a:opt.silent
        function! l:job_io_handler.CB(task, subtask)
            let l:fn = {}
            function! l:fn.OnOutput(ch, msg) closure
                let l:info = #{ output: split(a:msg, "\n") }
                let l:laststat = split(l:info.output[-1], "\r")[-1]
                call packager#info#update(a:task, { a:subtask: #{ output: l:laststat } })
            endfunction
            return funcref('l:fn.OnOutput', l:fn)
        endfunction
    else
        function! l:job_io_handler.CB(task, subtask)
            return {ch, msg->}
        endfunction
    endif
    function! l:job_io_handler.OnClose(name, code)
        call packager#info#update(a:name, { '': #{ code: a:code } })
        execute 'helptags ' . s:info[a:name].fullpath . '/doc'
        call self.RaiseJob()
    endfunction
    function! l:job_io_handler.RaiseJob() closure
        if self.lastjob < len(l:pending_packs)
            let l:taskname = l:pending_packs[self.lastjob]
            let self.lastjob += 1
            call job_start(l:action[l:taskname], #{
                        \     err_mode: "raw",
                        \     err_cb: self.CB(l:taskname, ''),
                        \     exit_cb: {ch, code->self.OnClose(l:taskname, code)},
                        \ })
        endif
    endfunction
    " }}} End job helpers
    " {{{ Show the info window
    if !has_key(a:opt, 'silent') || !a:opt.silent
        let l:display = {}
        " Add hook support
        for l:name in l:pending_packs
            let l:display[l:name] = { '': {} }
        endfor
        call packager#info#init(l:display)
        call packager#info#show()
    endif
    " }}} End showing the info window
    " Raise fetch jobs
    for i in range(s:conf.max_job)
        call l:job_io_handler.RaiseJob()
    endfor
endfunction
" }}} End packager#manager#sync

" vim: foldmethod=marker
