
#### Setup daemon

In your desktop environment / startup script add the command to startup an emacs daemon.

```bash
emacs --daemon # To start the server

emacsclient -c # -c to create new frame, -a to give an alternative start up editor, e.g. 'emacs' / 'nvim'
```
