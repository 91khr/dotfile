vim9script

def GetBytePos(): number
    const pos = getcurpos()
    return line2byte(pos[1]) + pos[2] - 2
enddef

def LuaComplete(start: number, base: string): any
    if start
        # Leave completion in comments and strings
        if ['luaComment', 'luaString', 'luaString2']->index(
                synID(line('.'), col('.'), true)->synIDattr('name')) != -1
            return -3
        endif
        return match(getline('.')[0 : col('.') - 1], '\v(\W|^)\zs\w+$')
    endif
    return v:none
enddef

def InitLua()
    b:job_lua_compl = job_start(['awesome-client'], {
        in_mode: "nl", out_mode: "nl",
        in_io: "pipe", out_io: "pipe",
    })
    &omnifunc = LuaComplete
    set completeopt+=popuphidden
    au BufUnload <buffer> job_stop(b:job_lua_compl)
enddef

augroup vimrc
    au!
    au BufRead *.lua InitLua()
    au BufRead *.vim,.vimrc syn clear luaParenError
augroup END
