import "ftext.vim"
if s:ftext.CanCmd("Run")
    command! -buffer -bar Run w | botright term lua %
    let b:run_overridable = 0
endif

