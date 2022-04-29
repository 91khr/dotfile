import "ftext.vim"
if s:ftext.CanCmd("Run")
    command! -buffer -bar Run w | botright term python %
    let b:run_overridable = 0
endif

