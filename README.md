# Utility functions for creating comints

A tiny library to expose some of the awesome subprocess/terminal creating functionality
from emacs comint.el in a slightly easier to use fashion. 

# Sample usage:
```elisp
  ;;In elisp code
  (defproc "noir-server" "lein" '("run")
  "Run a noir server")
  (defproc echo-test "echo" `(,buffer-file-name)
  "Print `buffer-file-name'")
  (defproc echo-test2 "echo" `(,buffer-file-name)
  "Print `buffer-file-name' without selecting *echo* buffer."
  'display-buffer)
  (defproc xeyes "xeyes" nil
  "Run xeyes without popping up *xeyes* buffer"
  'ignore)
```
  
While editing:
  M-x run-proc 
    bash
  will bring up a comint running bash  
  
  
I kind of doubt anyone will actually use this beyond me, but if you do and you have something you want changed, make a pull request!
