# Zsh config

## Local `.zshrc`

Any directory containing a `.zshrc` will be asked for authorization to the directory,
thus the local config scripts can be loaded.
This authorization is shared with other apps which may require such authorization.

When changing directory, current `.zshrc` will be unloaded by calling it again with `-u` as argument.

