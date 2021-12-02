" ======================================================================================================================
" Note: You are suggested to use a wide(>120 chars) client to view or edit this file
" Author: Virginia Senioria
" Senioriae VIm configuration: status line

let g:solarized_menu = 0  | " In tested versions, this is an erroneous option
" {{{ Helpers
function! s:iswide()
    return winwidth(0) > 70
endfunction
function! s:func(name)
    return '<SNR>' . matchstr(expand('<sfile>'), '<SNR>\zs\d\+\ze_func$') . '_' . a:name
endfun
" }}} End helpers

" {{{ Components
function! s:FileName()
    if &ft == 'help' | return expand('%:t') | endif
    if expand('%:t') == ''
        if &buftype == 'quickfix'
            return getwininfo()[winnr() - 1].loclist ? '[Location List]' : '[Quickfix List]'
        endif
        let l:btlist = #{
                    \     nofile: '[Scratch]',
                    \     prompt: '[Prompt]',
                    \     popup: '[Popup]',
                    \ }
        return l:btlist->has_key(&bt) ? l:btlist[&bt] : '[No Name]'
    endif
    let l:relpath = fnamemodify(expand('%'), ':.')
    let l:pathsep = has("win32") ? '\' : '\/'
    return s:iswide() ? l:relpath :
                \ substitute(l:relpath, '\v([^/])([^/]*)' . l:pathsep, '\1' . l:pathsep, 'g')
endfunction

function! s:CocStatus()
    if !exists(":CocInfo") || !s:iswide() | return '' | endif
    return coc#status()
endfunction

function! s:SpaceStatus()
    if !s:iswide() || get(b:, "spacecheck_disabled", v:false) || &bt == 'terminal'
        return ''
    endif
    let pos = getcurpos()
    call cursor(1, 1)
    let trailing = search('\s\+$', 'ncw')
    if trailing != 0
        call cursor(pos[1:])
        return 'Trailing: ' .. trailing
    endif
    let mixing = search('^ \+', 'ncw') ? search('^\s*\t', 'ncw') : 0
    if mixing != 0
        call cursor(pos[1:])
        return 'Mixed indent: ' .. mixing
    endif
    call cursor(pos[1:])
    return ''
endfunction
" }}} End components

let g:lightline = #{
            \     colorscheme: 'solarized',
            \     component_function: #{
            \         filename: s:func('FileName'),
            \         cocstatus: s:func('CocStatus'),
            \     },
            \     component_expand: #{
            \         spacestatus: s:func('SpaceStatus'),
            \     },
            \     component: #{ },
            \     component_type: #{
            \         spacestatus: 'error',
            \     },
            \     component_visible_condition: #{
            \         spell: '&spell',
            \     },
            \     active: #{
            \         left: [ [ 'mode', 'paste', 'spell' ],
            \                 [ 'readonly', 'filename', 'modified' ],
            \                 [ 'cocstatus' ],
            \         ],
            \         right: [ [ 'lineinfo' ],
            \                  [ 'percent' ],
            \                  [ 'spacestatus' ],
            \                  [ 'fileformat', 'fileencoding', 'filetype' ],
            \         ],
            \     },
            \ }

let s:wideonly_component = #{
            \     fileformat: '&ff',
            \     fileencoding: '&fenc!=#""?&fenc:&enc',
            \     filetype: '&ft!=#""?&ft:"no ft"',
            \     spell: '&spell?&spelllang:""',
            \ }
let s:wide_condition = "winwidth(0) > 70"
for [name, exp] in items(s:wideonly_component)
    let g:lightline.component[name] = printf("%%{%s ? (%s) : ''}", s:wide_condition, exp)
    let cond = g:lightline.component_visible_condition
    if cond->has_key(name)
        let cond[name] = printf("(%s)&&(%s)", cond[name], s:wide_condition)
    else
        let cond[name] = s:wide_condition
    endif
endfor

