;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*- Author: Peter Norvig
;;;     File: load.lisp; Date: 11-Sep-95
(in-package :cl-user)

;;;; Common Lisp to Dylan Converter --- (Load-LTD) loads the system

(defun load-ltd (&key (compile nil))
  (with-compilation-unit ()
      (mapc #'(lambda (file) (load (if compile (compile-file file) file)))
            '("package.lisp" "misc.lisp" "options.lisp" "read.lisp" "dpp.lisp"  
              "ltd.lisp" "ltd-table.lisp" "loop.lisp" "tables.lisp"))))

(defun test-ltd ()
  (defpackage comp)
  (defpackage dtp)
  (defpackage mma)
  (defpackage excl)
  (ltd-files "../test/*.lisp"))

#+LispWorks
(defsystem ltd (:package user)
  :members 
  ("misc" "options" "read" "dpp" "ltd" "ltd-table" "loop" "tables"))
              
  
