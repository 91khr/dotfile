vim9script

var infobuf = -1
# name: string -> { lnum: int, lasthl: (match id),
#     status: ("pending" | "running" | "error_exit" | "ok_exit"),
#     output: [string] }
var packlist: dict<dict<any>>
var hlprops = {
    name: "Identifier",
    pending: "Comment",
    running: "Normal",
    error_exit: "WarningMsg",
    oper_exit: "Label",
    ok_exit: "String",
    }
var lnlist: list<string> = []
var popupid = -1

def ClosePopup()
    if popupid != -1
        popup_close(popupid)
        popupid = -1
    endif
enddef

def PopStatus()
    var ln = line('.') - 1
    ClosePopup()
    if ln >= len(packlist) || bufnr() != infobuf || empty(packlist[lnlist[ln]].output)
        return
    endif
    popupid = popup_atcursor(packlist[lnlist[ln]].output, {
        moved: "any",
        })
enddef

export def Show(packages: list<string>)
    if infobuf == -1 || bufwinid(infobuf) == -1
        # infobuf exists, but hidden, wipeout it
        if infobuf != -1
            exec ":" .. infobuf .. "bw"
        endif
        # Init the buf
        vsplit [Package Status]
        setlocal nonu bt=nofile bufhidden=delete noswapfile nobuflisted
        infobuf = bufnr()
    else
        packlist = {}
        setbufvar(infobuf, "&modifiable", true)
        deletebufline(infobuf, 1, "$")
    endif
    # Fill in the content
    append("$", repeat([''], packages->len()))
    normal G
    lnlist = packages
    for id in range(len(packages))
        var name = packages[id]
        packlist[name] = { lnum: id + 1, status: "", lasthl: -1, output: [] }
        Update(name, { text: ["Pending"], status: "pending" })
        packlist[name].output = []
        matchaddpos(hlprops['name'], [[id + 1, 1, len(name)]])
    endfor
    # Add popup for status
    autocmd CursorMoved <buffer> PopStatus()
    autocmd BufHidden <buffer> packlist = {}
enddef

export def Hide()
    if infobuf == -1 || !bufexists(infobuf)
        return
    endif
    ClosePopup()
    exec ":" .. infobuf .. "bw"
enddef

# status: { text: [string], status: ("pending" | "running" | "error_exit" | "ok_exit")? }
export def Update(name: string, status: dict<any>)
    if infobuf == -1 || bufwinid(infobuf) == -1
        infobuf = -1
        return
    endif
    var lnum = packlist[name].lnum
    var proplen: number
    if status->has_key("text")
        setbufvar(infobuf, "&modifiable", true)
        setbufline(infobuf, lnum, name .. ": " .. status.text[-1])
        setbufvar(infobuf, "&modifiable", false)
        proplen = len(status.text[-1])
        packlist[name].output += status.text
        PopStatus()
    else
        proplen = empty(packlist[name].output) ? len("Pending") : len(packlist[name].output[-1])
    endif
    packlist[name].status = get(status, "status", packlist[name].status)
    if packlist[name].lasthl != -1
        matchdelete(packlist[name].lasthl, bufwinid(infobuf))
    endif
    packlist[name].lasthl = matchaddpos(hlprops[packlist[name].status],
        [[packlist[name].lnum, len(name) + 3, proplen]],
        10, -1, { window: bufwinid(infobuf) })
enddef

# vim:fdm=marker
