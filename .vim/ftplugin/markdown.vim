" ======================================================================================================================
" Note: Readers are suggested to use a wide(>120 chars) client to view or edit this file
" Author: Virginia Senioria
" Senioriae VIm configuration: markdown language configurations

function! s:CompileMarkdown(...)
    " Process options
    let options = ''
    for item in a:000
        let options .= ' ' . item
    endfor
    " Process output name
    let outname = expand('%:t:r') . '.html'
    " Compile...
    execute ":AsyncRun pandoc % -o " . outname . ' ' . options
endfunction
command! -buffer -nargs=* Compile call <SID>CompileMarkdown(<f-args>)

