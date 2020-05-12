" ======================================================================================================================
" Note: You are suggested to use a wide(>120 chars) client to view or edit this file
" Author: Virginia Senioria
" A simple plugin manager

" {{{ Package info helper
function s:OpenInfoWin()
    vnew [Package Status]
    setlocal buftype=nofile bufhidden=delete noswapfile nowrite
    return bufnr('')
endfunction

function s:SetInfo(info)
endfunction
" }}}

" vim: foldmethod=marker
