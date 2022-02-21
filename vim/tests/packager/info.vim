vim9script

import "packager/info.vim" as packinfo

var events = [
    "'a', { text: 'hello', status: 'running' }",
    "'hello', { text: 'hi', status: 'running' }",
    "'a', { text: 'error', status: 'running' }",
    "'a', { status: 'ok_exit' }",
    "'hello', { text: 'ok_exit', status: 'ok_exit' }",
    "'a', { text: 'error exit', status: 'error_exit' }",
    ]
var eventid = -1
var packs = [ 'a', 'hello' ]

def PollEvent(...args: list<any>)
    if eventid == -1
        packinfo.Show(packs)
    elseif eventid == len(events)
        packinfo.Hide()
        eventid = -2
    else
        packinfo.Show(packs)
        exec "packinfo.Update(" .. events[eventid] .. ")"
        packinfo.Show(packs)
    endif
    eventid += 1
enddef

packinfo.Show(packs)
timer_start(800, PollEvent, { repeat: -1 })

