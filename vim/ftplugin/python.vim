import "ftext.vim"
if s:ftext.CanCmd("Run")
    command! -buffer -bar -nargs=* Run w | botright eval s:ftext.TermRun(
                \ [ "python", expand("%") ] + split(<q-args>, " "),
                \ #{ persist: v:true, unique: v:false })
    let b:run_overridable = 0
endif

