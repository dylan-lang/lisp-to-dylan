;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*- Author: Peter Norvig
;;;     File: convert.lisp; Date: 28-Aug-95
(in-package :ltd)

;;;; LTD: CONVERT FROM COMMON LISP TO DYLAN - TOP-LEVEL FUNCTION

(defun ltd-files (files &key (width 79) (output (make-pathname :type "dylan")))
  "Convert a list of Common Lisp files to Dylan."
  (let ((*print-right-margin* width)
        (*package* *package*))
    (dolist (file (expand-files files))
      (with-open-file (out (merge-pathnames output file) :direction :output
                           :if-exists :supersede)
        (with-open-file (in file :direction :input)
          (format t "Converting ~A~%" file)
          (restart-case 
              (loop until (eq *eof* (ltd-exp in out)))
            (nil () :report (lambda (s) (format s "Skip file ~A" file)))))))
    (report-unimplemented-functions)))

(defun ltd-exp (in out)
  "Read a Lisp expression from stream IN and write Dylan to stream OUT."
  (restart-case
      (let ((exp (ltd-read in)))
        (unless (eq exp *eof*)
          (dpp-exp (cvt-exp exp) :stream out)
          (format out ";~%~%")
          (clrhash *file-position-table*))
        exp)
    (nil () :report "Skip to the next expression in this file."
         '|Input expression skipped due to translation error.|)))

;;;; MACROS FOR DEFINING TRANSLATION TABLES

;;; We support three tables, keyed on Lisp symbols, whose values are
;;; the equivalents in Dylan (or a function to compute the equivalent).

(defun get-cvt-constant (cl) (when (symbolp cl) (get cl 'cvt-constant)))
(defun get-cvt-fn (cl)       (when (symbolp cl) (get cl 'cvt-fn)))
(defun get-cvt-type (cl)     (when (symbolp cl) (get cl 'cvt-type)))

(defmacro ltd-constant (cl dylan)
  ;; Define a translation between a Lisp and Dylan constant
  (assert (symbolp cl))
  `(setf (get ',cl 'cvt-constant)
         ,(if (symbolp dylan) `',dylan `#'(lambda () ,dylan))))

(defmacro ltd-type (cl dylan)
  ;; Define a translation from a Lisp to a Dylan type
  (assert (symbolp cl))
  `(setf (get ',cl 'cvt-type) ',dylan))

(defmacro ltd-fn (cl dylan)
  "Store, under the function symbol in Lisp, a function to convert to Dylan."
  ;; The function will be passed EXP, the complete expression to be converted.
  ;; This is either of the form (f x y z) or #'f
  (when (symbolp cl)
    (setf cl `(,cl . args)))          ; Coerce to canonical form
  `(progn
     (setf (get ',(op cl) 'cvt-fn)
           #'(lambda (exp)
               ,(cond
                 ((symbolp dylan)
                  `(if (call? exp)
                       (cons ',dylan (cvt-exps (args exp)))
                       ',dylan))
                 ((starts-with dylan 'function)
                  `(,(second/ dylan) exp))
                 ((starts-with dylan 'cl?)
                  `(encapsulate-let
                    (converting-bind ,(args cl) (args exp) ,dylan)))
                 (t `(cond
                       ((call? exp)
                        (encapsulate-let
                         (converting-bind ,(args cl) (args exp) 
                           ,@(when (find-anywhere 'ignore (args cl))
                                   '((declare (ignore ignore))))
                           ;; Don't warn for ARGS added above in
                           ;; coercion to canonical form.
                           ,@(when (find-anywhere 'args (args cl))
                                   '((declare (ignore args))))
                           ,dylan)))
                       ,@(if (or (dotted? cl) (find-anywhere '&opt cl))
                             `((t (cvt-erroneous ; ??? could do better
                                   exp (second/ exp)
                                   "Can't convert complex function ~A." 
                                   (second/ exp))))
                             `((t (let ((dylan-args '(:args ,@(args cl)))
                                        (dylan-body (cvt-exp ',cl)))
                                    (list 'method dylan-args dylan-body))))))))))
     ',(first-atom cl)))

(defmacro ltd-unimplemented-functions (&rest fns)
  "These functions are not yet implemented."
  `(map nil #'(lambda (fn)
                (case (get-cvt-fn fn)
                  ((nil) (setf (get fn 'cvt-fn) 'not-yet-implemented))
                  ((not-yet-implemented))
                  (t (warn "~A is already implemented!" fn)))) 
        ',fns))

(defmacro ltd-unimplemented-types (&rest types)
  "These types are not yet implemented."
  `(map nil #'(lambda (type)
                (setf (get type 'cvt-type)
                      (add-comment (format nil "Type ~A unimplemented" type)
                                   '<object>)))
        ',types))

(defvar *unimplemented* (make-hash-table :test #'eq))

(defun incf-unimplemented (fn) (incf (gethash fn *unimplemented* 0)))

(defun not-yet-implemented (exp)
  ;; Warn, then just convert each arg and make a function call
  (let ((fn (if (call? exp) (op exp) (second/ exp))))
    (incf-unimplemented fn)
    (if (call? exp)
        `(,(cvt-erroneous exp fn "Function ~A not yet implemented." fn)
          ,@(cvt-exps (args exp)))
      (cvt-erroneous exp fn "Function ~A not yet implemented." fn))))

(defun report-unimplemented-functions ()
  (let ((result nil))
    (maphash #'(lambda (k v) (push (list v k) result))
             *unimplemented*)
    (format t "~%Counts of unimplmented functions:~%")
    (loop for (n fn) in (sort result #'> :key #'first) 
          do (format t "~4D ~A~%" n fn))
    (clrhash *unimplemented*)))

;;;; CONVERTING BASIC EXPRESSIONS

(defun cvt-exp (exp)
  "Convert a CL expression to Dylan."
  (cond ((and (symbolp exp) (not (keywordp exp))
              (or (get-cvt-constant exp) (constantp exp)))
         (cvt-constant exp))
        ((comment? exp) (setf (com-code exp) (cvt-exp (com-code exp))) exp)
        ((atom exp) exp)
        ((get-cvt-fn (op exp))
         (funcall (get-cvt-fn (op exp)) exp))
        ((and (symbolp (op exp)) (macro-function (op exp)))
         (cvt-macro exp))
        (t `(,(cvt-fn `(function ,(op exp)))
             ,@(cvt-exps (args exp))))))

(defun cvt-exps (exps)
  "Like (mapcar #'cvt-exp exps), but handles dotted lists."
  (if (atom exps)
      exps
      (cons (cvt-exp (first exps)) (cvt-exps (rest/ exps)))))

(defun cvt-constant (var)
  "Convert a constant's name from CL to Dylan."
  ;; Use the entries from the cvt-constant table.
  ;; If it is of the form *xxx* or +xxx+, strip the ** or ++.
  ;; Otherwise, just tack a $ at the beginning.
  (let ((str (symbol-name var))
        (con (get-cvt-constant var)))
    (cond ((and con (symbolp con)) con)
          ((and con (functionp con)) (funcall con))
          ((starts-with str #\$) var)
          ((bracketed-with str #\*)
           (mksymbol '$ (subseq str 1 (- (length str)))))
          ((bracketed-with str #\+)
           (mksymbol '$ (subseq str 1 (- (length str)))))
          (t (mksymbol '$ var)))))

(defun extract-declarations (body)
  "Return three values: doc string, list of (declare)s, body."
  (let ((doc nil)
        (declarations nil))
    (loop (cond ((starts-with (first/ body) 'declare)
                 (push (pop body) declarations))
                ((and (null doc) (length>1 body) (stringp (first/ body)))
                 (setf doc (pop body)))
                (t (RETURN))))
    (values doc (nreverse declarations) body)))

(defun extract-just-declarations (body)
  (multiple-value-bind (doc declares bod) (extract-declarations body)
    (declare (ignore doc bod))
    declares))

(defun extract-values-declaration (declarations)
  "Some programs use (declare (values type1 type2)).  Get it if there."
  (dolist (declaration declarations)
    (dolist (decl (rest/ declaration))
      (when (starts-with decl 'values)
        (return-from extract-values-declaration
          `(:return ,@(rest/ decl)))))))

(defun cvt-body (body &key (name nil))
  "Convert a body. Handle doc, declares, return-from.  Returns a list of forms."
  (labels ((strip-begin (exp)
              (if (and (starts-with exp 'begin) (= (length (strip exp)) 2))
                  (strip-begin (second/ exp))
                  exp)))
    (multiple-value-bind (doc decls body) (extract-declarations body)
      (declare (ignore decls))
      (let ((forms (handle-return-froms
                    (mapcar #'(lambda (exp) (strip-begin (cvt-exp exp)))
                            (or body '(|\#f|)))
                    name)))
        (if doc
            (cons (add-comment doc (first/ forms)) (rest/ forms))
            forms)))))

(defun encapsulate-let (exp)
  ;; In Dylan, a LET can appear only at the top level of a body.
  ;; So wrap a LET in a BEGIN, and strip the BEGIN from wihin cvt-body
  (if (starts-with exp 'let)
      `(begin ,exp)
      exp))
          
;;;; MISC

(defun add-type-declaration (variable declarations)
  (let ((decl (get-type-declaration variable declarations)))
    (if decl `(|::| ,variable ,decl) variable)))

(defun get-type-declaration (variable declarations)
  "If there is a type declaration for variable, get it."
  (dolist (declaration declarations)
    (dolist (decl (rest/ declaration))
      ;; Handle (type fixnum ... variable ...)
      (when (and (starts-with decl 'type) (member variable (cddr decl)))
        (RETURN-FROM get-type-declaration (cvt-type (second/ decl))))
      ;; Handle (fixnum ... variable ...))
      (when (and (consp decl) (get-cvt-type (first/ decl))
                 (member variable (rest/ decl)))
        (RETURN-FROM get-type-declaration (cvt-type (first/ decl)))))))

(defun cvt-erroneous (exp replacement &rest format-args)
  "Can't convert exp; just return replacement, but print warnings."
  (let ((*print-length* 3)
        (*print-level* 2))
    (safe-destructuring-bind (start . end)
          (or (gethash exp *file-position-table*) '(? . ?))
      (apply #'warn format-args)
      (warn "  at ~D to ~D in ~A.~%" start end exp))
    (if (get-option :errors-inline)
        (add-comment
         (concatenate 'string "LTD: " (apply #'format nil format-args))
         replacement)
        replacement)))

(defun false? (x)
  ;; Is X a Lisp or Dylan expression for EMPTY LIST or FALSE?
  (member (strip x) '(nil |\#f| |()|)))

(defun call? (exp)
  "Is this a function call, e.g. (f x), as opposed to #'f or 'f?"
  (and (consp (strip exp))
       (not (member (first/ (strip exp)) '(quote function)))))
