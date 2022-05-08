vim9script
import "packager/manager.vim" as packager

export def Installed(name: string): bool
    return isdirectory(packager.packpath .. name)
enddef

