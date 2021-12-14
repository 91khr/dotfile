vim9script

export def TermRun(cmd: list<string>, args: dict<bool> = { persist: false, unique: false }): void
    TermRun_Impl(cmd, args.persist, args.unique)
enddef
def TermRun_Impl(cmd: list<string>, persist = false, unique = false): void
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
                    \     term_finish: persist ? "open" : "close",
                    \     exit_cb: (_, _) => timer_start(10, (_) => Post(nr)),
                    \ })
        return nr
    enddef
    if unique
        if exists("b:replbuf") && bufexists(b:replbuf)
            exec ":" .. string(b:replbuf) .. "bw!"
        endif
        var curbuf = bufnr()
        setbufvar(curbuf, "replbuf", Exec())
    else
        Exec()
    endif
enddef

