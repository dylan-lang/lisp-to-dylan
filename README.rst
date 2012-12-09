This is Peter Norvig's Common Lisp to Dylan converter.

The included instruction to get this running are a bit out-of-date.
Here's how you can do it as of Dec 2012 on Ubuntu with SBCL and SLIME.

1. sudo aptitude install sbcl slime

2. Add the following to your ~/.emacs file::

     (setq inferior-lisp-program "/usr/bin/sbcl")
     (add-to-list 'load-path "/usr/share/emacs/site-lisp/slime")
     (add-to-list 'load-path "/usr/share/common-lisp/source/slime")
     (require 'slime)
     (slime-setup)

3. Add this to your ~/.sbclrc file::

     (setf *default-pathname-defaults* #P"/path/to/lisp-to-dylan/code/")

4. Invoke M-x slime and then in the REPL do this::

     (load "load.lisp")
     (load-ltd)
     (in-package "LTD")
