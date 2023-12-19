# useful commands
## emacs

| Command       | Library   | Description                                                         |
|---------------|-----------|---------------------------------------------------------------------|
| C-H v                     | helpful   | find a variable                                         |
| C-H o                     | helpful   | find everything                                         |
| S-l + = + =               | lsp       | scalafmt buffer                                         |
| C-x C-SPC                 | ?         |  go to last cursor position                             |
| C-l                       | native    |  half scroll, then scroll to cursor                     |
| c-c p k                   | helm?     | kill all open buffers for project                       |
| file-notify-rm-all-watches| native    |Remove all existing file notification watches from Emacs.|
| C-x t p                   | native    |new tab with current projectile project open.            |
| C-t R                     | dgibs func| rename tab to name of repository                        |
| C-c 5 c                   | native    | clone frame                                             |
| C-x +                     | native    | evenly space windows                                    |

## scala sbt comands
| Command                     |  Description                                                |
|-----------------------------|-------------------------------------------------------------|
| tpolecatDevMode             | turn off anoying compile errors                             |
| tpolecatCiMode              | make compiler exhaustive for CI                             |
| show scalacOptions          | shows scalac compiiler flags                                |
| show semanticdbOptions      | show semantic db compiler flags                             |
| inspect semanticdbOptions   | shows configuration of options                              |
| ++3.3                       | use scala version 3.3.x                                     | 
| reload plugins              | changes the current build to root project’s project/ build  |
| show ThisBuild/scalaVersion | view ThisBuild properties                                   |
| reload return               | changes back to the original build                          |


add the following to the end of an org file that you want org-ai-mode enabled it. Note, we're not enabling it by default because it conflicts with verb-mode. 
```
# Local Variables:
# eval: (org-ai-mode)
# End:
```

## only install a package in a shell:
```

# install software in a single shell sesion:
`nix shell nixpkgs#gh`

## lsp-mode using the wrong root directory for a project?
sometimes it will default to `~/` or `~/repositories` to fix this use the following command(s), then reimport metals projects
- `lsp-workplace-remove-all-folders`
- `lsp-workplace-folders-remove`

# when upgrading Mac verions do this because darwin is a clunky piece of shit
## osx overwrites the /etc/bashrc, zsh, and zprofile because they want you to suffer

update your bashrc or zshrc respectively to include the following encantation at the bottom of the file:
```
# Nix
if [ -e '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh' ]; then
  source '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh'
fi
# End Nix

```
start a new shell and you should have nix on the path again.


