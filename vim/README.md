# Vim config

## Local `.vimrc`

Any directory containing a `.vimrc` will be asked for authorization to the directory,
thus the local config scripts can be loaded.
This authorization is shared with other apps which may require such authorization.

Due to some vim restrictions, buf-loading related autocommands
must be defined in `BufRead` event of `vimrc` group to be able to be executed
when the buffer is loaded at the startup.

When changing directory or writing to current `.vimrc`, current exrc should be unloaded.
A global variable `g:Undo_exrc` can be used to unload current exrc configuration,
which can be a string representing a ex command or a funcref;
it will be executed before the new exrc is loaded.

## TODO

- [x] Rewrite packager

