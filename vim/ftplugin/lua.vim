if !exists(":Run")
    command! -buffer -bar Run w | botright term lua %
endif
