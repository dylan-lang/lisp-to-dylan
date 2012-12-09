;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defpackage #:ltd-asd (:use :cl :asdf))
(in-package :ltd-asd)

(defsystem ltd
    :description "Lisp to Dylan translator"
    :version "0.1"
    :author "Peter Norvig"
    :maintainer "Peter S. Housel <housel@acm.org>"
    :serial t ;; for now...
    :components ((:file "package")
                 (:file "misc")
                 (:file "options")
                 (:file "read")
                 (:file "dpp")
                 (:file "ltd")
                 (:file "ltd-table")
                 (:file "loop")
                 (:file "tables")))
