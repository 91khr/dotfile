let s:config_dir = expand("<sfile>:p:h:h:h")

" Senioria would often type chars triggering foldings qwq><
setlocal nofoldenable
" Make text flow and be prettier~ w
setlocal formatoptions+=aw
" No space check: flow will have trailing spaces as continuation ><
let b:spacecheck_disabled = v:true

" Preview composed HTML
command! -buffer -bar Compile w | exec "AsyncRun -mode=term -pos=bottom "
            \ . s:config_dir . "/mutt/gen_multipart_alternative.sh < %" | set ft=html

