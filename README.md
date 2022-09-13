# dotfile

Senioriae configuration collection.
Don't ask why there's no plural in the repo name:
forgot, and too lazy to modify x.

Each folder's name indicate a configuration set,
however there are some special folders:
(i.e. their purposes may not be easily inferred from the name)

- `profile`: X11 and Linux shell profiles
  (specific shell profiles like `bash` and `zsh` still go to their own folders)
- `utils`: Local utilities that are not big enough to create a repo for them :)

## Installation

There's an installation utility `install.cpp`,
just execute(on *NIX, where shell code is inserted at the beginning of the source,
so that the source can be automatically compiled and executed),
or compile and execute it.
Specific document can be find in the source or by `install.cpp --help`.

Some folders may contain a `dependencies.ini`,
in which the section name is the OS, and the lines are the package names.
The dependencies should be installed in order for the configuration to work properly.
Unfortunately, the installer has no ability to install the dependencies automatically yet.

## TODO

- [ ] Split Awesome and Emacs config into separate files
- [x] Add zsh autoload
- [ ] Add dependencies installer

