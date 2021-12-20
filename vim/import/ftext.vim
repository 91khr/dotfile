vim9script

export def TermRun(cmd: list<string>, args: dict<bool> = { persist: false, unique: false }): number
    def Post(nr: number): void
        if nr != bufnr() | return | endif
        set modifiable bufhidden=wipe
        normal i 
        set bt=nowrite
    enddef
    def Exec(): number
        var nr: number
        belowright nr = term_start(cmd, {
                    \     term_rows: float2nr(0.35 * winheight(0)),
                    \     term_finish: args.persist ? "open" : "close",
                    \     exit_cb: (_, _) => timer_start(10, (_) => Post(nr)),
                    \ })
        return nr
    enddef
    if args.unique
        if exists("b:replbuf") && bufexists(b:replbuf)
            exec ":" .. string(b:replbuf) .. "bw!"
        endif
        var curbuf = bufnr()
        var execbuf = Exec()
        setbufvar(curbuf, "replbuf", execbuf)
        return execbuf
    else
        return Exec()
    endif
enddef

