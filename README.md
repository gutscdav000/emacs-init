# useful commands

| Command       | Library   | Description                                                      |
|---------------|-----------|------------------------------------------------------------------|
| C-H v                     | helpful   | find a variable                                      |
| C-H o                     | helpful   | find everything                                      |
| S-l + = + =               | lsp       | scalafmt buffer                                      |
| C-x C-SPC                 | ?         |  go to last cursor position                          |
| C-l                       | native    |  half scroll, then scroll to cursor                  |
| c-c p k                   | helm?     | kill all open buffers for project                    |
| file-notify-rm-all-watches| namive |Remove all existing file notification watches from Emacs.|



add the following to the end of an org file that you want org-ai-mode enabled it. Note, we're not enabling it by default because it conflicts with verb-mode. 
```
# Local Variables:
# eval: (org-ai-mode)
# End:
```