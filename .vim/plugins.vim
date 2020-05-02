" ======================================================================================================================
" Note: You are suggested to use a wide(>120 chars) client to view or edit this file
" Author: Virginia Senioria
" My VIm configuration: plugins, with some utilties

let s:pack_inited = v:False
let g:package_list = [
            \ ]

" {{{ Experimental components
function s:OpenInfoWin()
    vnew [Package Status]
    setlocal buftype=nofile bufhidden=delete noswapfile nowrite
    return bufnr('')
endfunction
" }}}

" vim: foldmethod=marker
