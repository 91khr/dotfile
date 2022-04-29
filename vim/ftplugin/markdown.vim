import "ftext.vim"

function! s:CompileMarkdown(...)
    execute ":AsyncRun pandoc % -s --mathml -o %:h/%:t:r.html " .. a:000->flattennew()->join(' ')
endfunction
if s:ftext.CanCmd("Compile")
    command! -buffer -bar -nargs=* Compile w | call s:CompileMarkdown(<f-args>)
    let b:compile_overridable = 0
endif
if s:ftext.CanCmd("Run")
    command! -buffer -bar -nargs=* Run exec "!" .. (has("win32") ? "start" : "xdg-open")
                \ .. " %:h/%:t:r.html " .. <q-args>
    let b:run_overridable = 0
endif

let b:coc_suggest_disable = 1

