import "packager/manager.vim" as pkg

function packager#manager#sync(opts)
    eval s:pkg.Sync(a:opts)
endfunction

function packager#manager#status(opts)
    eval s:pkg.Status(a:opts)
endfunction

function packager#manager#clean(opts)
    eval s:pkg.Clean(a:opts)
endfunction

