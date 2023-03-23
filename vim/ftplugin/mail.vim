let s:config_dir = expand("<sfile>:p:h:h:h")

" Senioria would often type chars triggering foldings qwq><
setlocal nofoldenable
" Make text flow and be prettier~ w
setlocal formatoptions+=aw
" No space check: flow will have trailing spaces as continuation ><
let b:spacecheck_disabled = v:true

" Preview composed HTML
import "ftext.vim"
if s:ftext.CanCmd("Compile")
    command! -buffer -bar -nargs=* Compile w | exec "AsyncRun -mode=term -pos=bottom "
                \ .. s:config_dir .. "/mutt/gen_multipart_alternative.sh < %" .. <q-args>
                \ | set ft=html
    let b:compile_overridable = 0
endif
if s:ftext.CanCmd("Run")
    function! s:render_mail()
        let f = tempname() .. ".html"
        call job_start([&shell, &shcf, s:config_dir .. "/mutt/gen_multipart_alternative.sh | tail -n +3"], #{
                    \ in_io: "file", in_name: expand("%"),
                    \ out_io: "file", out_name: f,
                    \ exit_cb: { -> execute("!" .. (has("win32") ? "start" : "xdg-open") .. " " .. f) },
                    \ })
    endfunction
    command! -buffer -bar -nargs=* Run w | call s:render_mail()
    let b:run_overridable = 0
endif


