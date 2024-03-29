vim9script
# `:Repl`: used for a simple REPL window implemented with `:term`
# Usage: :Repl [shell command for the program]

def SetBufSize(buf: number)
    const winidx = v:event.windows->mapnew((_, nr) => winbufnr(nr))->index(buf)
    if winidx == -1
        return
    endif
    const win = v:event.windows[winidx]
    final state = getbufvar(buf, "repl_state")
    state.size[state.state] = winheight(win)
enddef

def ResizeBuf(neo: string)
    const buf = expand("<abuf>")->str2nr()
    final state = getbufvar(buf, "repl_state")
    state.state = neo
    exec ":" .. bufwinnr(buf) .. "windo resize " .. state.size[state.state]
enddef

# Make the REPL window
export def MkRepl(cmd: any, opt: dict<any> = {}): number
    const buf = term_start(cmd, extend({ stoponexit: true, cwd: expand("%:h") ?? expand("~") }, opt))
    const curheight = winheight(winnr())
    setbufvar(buf, "repl_state", { state: "active", size: { active: curheight, inactive: curheight } })
    autocmd_add([
        { event: "WinEnter", cmd: "ResizeBuf('active')" },
        { event: "WinLeave", cmd: "ResizeBuf('inactive')" },
    ]->mapnew((_, l) => l->extend({ group: "repl", bufnr: buf })) + [
        { event: "WinResized", group: "repl", pattern: "*", cmd: $"SetBufSize({buf})" },
    ])
    return buf
enddef
