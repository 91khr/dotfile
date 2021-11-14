" ======================================================================================================================
" Note: Readers are suggested to use a wide(>120 chars) client to view or edit this file
" Author: Virginia Senioria
" Additional mail syntax

" Flowed text use trailing spaces to mark soft line break, so highlight them
syn match mailLineContinuation / \+$/
hi link mailLineContinuation LineNr

