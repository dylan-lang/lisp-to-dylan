;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*- Author: Peter Norvig
;;;     File: options.lisp; Date: 2/Feb/95
(in-package :ltd)

;;;; OPTIONS: FACILITY FOR DEFINING, SETTING. AND QUERYING OPTIONS

;;; There are better mechanisms for this in CLIM and in LispWorks,
;;; but I wanted something portable to bare CL.  The public interface is:

;;; (get-option name)            Fetch the value for this option name.
;;; (set-option name value)      Set the value.
;;; (new-options :name val...)   Define a new set of options.
;;; (member-of-option item name) Is item in name's option value?
;;; *options*                    The currently used set of options.
;;; *default-options*            Holds default values for options.

;;; Here is the implementation:

(defstruct (option (:type list))
  name value type doc)

(defparameter *default-options*
    `(
      ;; Options for the conversion from Lisp to Dylan
     (:empty-as |\#()| (member |\#()| |\#f|)
      "Should () translate as #() or #f")
     (:nil-as |\#f| (member |\#()| |\#f|)
      "Should NIL translate as #() or #f")
     (:when-as if (member if when)
      "Should WHEN translate to IF or to WHEN")
     (:unless-as if (member if unless)
      "Should UNLESS translate to IF or to UNLESS")
     (:cond-as if (member if case)
      "Should COND translate to IF or to CASE")
     (:defun-as define-method (member define-function define-method)
      "Should DEFUN translate to DEFINE METHOD or to DEFINE FUNCTION")
     (:convert-slot-value t boolean
      "Should (slot-value x slot) become 'x.slot'")
     (:use-cl-sequence-functions nil boolean
      "Should CL Library sequence functions be used when there is a Dylan function")
     (:class-name-arguments-to-condition-functions t boolean
      "Should, e.g., #'signal worry about calls like (signal 'condition ...)")
     (:macroexpand-hard-loops t boolean
      "If we can't convert a LOOP, should we macroexpand it")
     (:macroexpand-hard-format-strings t boolean
      "If we can't convert a format string, should we use FORMATTER")
     (:only-binary-arithmetic-ops t boolean
      "Should we assume that, e.g., #'+ will only be applied to two arguments")
     (:obey-in-package t boolean
      "Should we switch packages when encountering an IN-PACKAGE")
     (:errors-inline t boolean
      "Should warnings appear as comments in the Dylan code")

     ;; Options for pretty-printing style (indenting, etc.)
     (:print-package nil boolean
      "Should the package of a symbol be printed [rather than ignored]")
     (:tab-stop 2 (integer 1 8)
      "Number of spaces to indent for each block")
     (:single-returns-wrapped t boolean
      "Should one-element return lists print as '=> (x)' [rather than '=> x']")
     (:prefer-dot-notation nil boolean
      "Should we print most everything as 'x.f' [rather than 'f(x)']")
     (:undotted-functions
      (make singleton signal error warning assert open close)
      (or (member t) list)
      "A list of functions that never get printed in dot notation")
     (:semicolon-before-end t boolean
      "Should we print the semicolon in 'x; end' [rather than 'x end']")
     (:space-in-call nil boolean
      "Should we print a function call as 'f (x)' [rather than 'f(x)']")
     (:comments // (member // /*)
      "Should comments print with '//' or '/*'")
     (:end-name t (or t nil)
      "Should we print 'end method f' [rather than just 'end method']")
     (:end-construct t (or (member t) list)
      "A list of constructs, e.g. (block class), for which we print 'end block'
[rather than just 'end'], or T to cover every construct") 
     ))

(defparameter *options* (copy-tree *default-options*))

(defun new-options (&rest inits &key (default *default-options*) (? nil)
                          &allow-other-keys)
  ;; Build and install a new options list.  You can:
  ;; (1) specify a default with, e.g., :default *old-options*
  ;; (2) override values with, e.g., :unless-as 'if :tab-stop 4
  ;; (3) set all values to "ask user" with :? t
  (setf *options* (copy-tree default)) 
  (when ?
    (dolist (option *options*)
      (setf (option-value option) :?)))
  (loop for (key val) on inits by #'cddr do
        (unless (member key '(:? :default))
          (set-option key val)))
  *options*)

(defun set-option (name value &optional (ask? t))
  "Set an option name to a value, if legal.  Returns t if legal."
  (let ((option (find-option name)))
    (cond ((null option) (warn "No such option name as ~A; ignored." name)
           nil)
          ((legal-option-value? name value)
           (setf (option-value option) value)
           t)
          (ask?
           (format *query-io* "~&The legal values are ~A"
                   (type->string (option-type option)))
           (get-option name t))
          (t nil))))

(defun get-option (name &optional (ask? t))
  "Get the value of the named option, asking if necessary."
  (let* ((option (find-option name))
         (value (if option (option-value option))))
    (cond ((null option) (warn "No such option name as ~A; ignored." name))
          ((and (eq value :?) ask?)
           (format *query-io*
                   "~&(Type a one-time answer like ~S, or type ALWAYS ~:*~S ~%~
                   to avoid this question in the future.)~%"
                   (let ((*options* *default-options*))
                     (get-option name)))
           (format *query-io* "~A? " (option-doc option))
           (let* ((value (read *query-io*)))
             (cond ((eq value 'always) (set-option name (read *query-io*)))
                   ((legal-option-value? name value) value)
                   (t (set-option name value t)))))
          (t value))))

(defun find-option (name) (assoc name *options*))

(defun legal-option-value? (name value)
  (or (eq value :?)
      (typep value (option-type (find-option name)))))

(defun member-of-option (item name)
  "Is ITEM a member of (OPTION NAME), or is (OPTION NAME) equal to t?"
  (or (eq (get-option name) t) (member item (get-option name))))

(defun type->string (type)
  (cond ((eq type 'boolean)
         "T or NIL (for yes or no, respectively)")
        ((equal type '(or (member t) list))
         "either a list of names, or T to indicate any name")
        ((atom type)
         (format nil "a ~A" type))
        ((eq (first type) 'integer)
         (format nil "an integer from ~D to ~D" (second type) (third type)))
        ((and (starts-with type 'member) (= (length type) 2))
         (format nil "~S" (second type)))
        ((starts-with type 'member)
         (format nil "one of the set {~{~S~^, ~}}" (rest type)))
        ((starts-with type 'and)
         (format nil "~{~A~^ and ~}" (mapcar #'type->string (rest type))))
        ((starts-with type 'or)
         (format nil "~{~A~^ or ~}" (mapcar #'type->string (rest type))))))
