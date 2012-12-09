;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*- Author: Peter Norvig
;;;     File: misc.lisp; Date: 8-Sep-95
(in-package :ltd)

;;;; MISC. FUNCTIONS: UTILITY FUNCTIONS

;;;; DYLAN-SPECIFIC UTILITY FUNCTIONS

;;; The following functions with a / are like their slash-less counterparts,
;;; except they work for arguments with comments and for |()|.

(defun first/ (exp) (first (strip-nil exp)))
(defun rest/ (exp) (rest (strip-nil exp)))
(defun second/ (exp) (second (strip-nil exp)))
(defun null/ (exp) (null (strip-nil exp)))
(defun assoc/ (item a-list) 
  (find (strip item) (strip a-list) 
        :key #'(lambda (x) (first-atom (strip x)))))

(defun strip-nil (exp)
  "Strip comment, and convert |()| to ()."
  (if (eq (strip exp) '|()|) '() (strip exp)))

(defun strip (exp)
  "Strip off the comment."
  (if (comment? exp) (com-code exp) exp))

(defmacro ifd (pred then &optional else)
  ;; Dylan if: false for nil or #f
  `(if (not (false? ,pred)) ,then ,else))

(defmacro once (var &body body)
  ;; Called once-only on Lisp Machines, return (Dylan) code built by body,
  ;; binding (in Dylan) any variables if they have non-trivial values
  (assert (symbolp var))
  (let ((temp (gensym (string var))))
  `(if (or (constantp ,var) (atom ,var))
       (progn ,@body)
       (list 'let ',temp ,var
             (let ((,var ',temp)) ,@body)))))

(defun maybe-begin (args)
  "Take a list of args (a body) and wrap a BEGIN around it if necessary."
  (case (length args)
    (0  '|\#f|)
    (1 (if (starts-with (first/ args) 'let) `(begin ,@args) (first/ args)))
    (t `(begin ,@args))))  

;;;; GENERAL LISP UTILITY FUNCTIONS

(defun op (exp) (first/ exp))
(defun args (exp) (rest/ exp))

(defun last1 (x) (first (last x)))

(defun nconc1 (list element) (nconc list (list element)))

(defun mklist (x)
  "Return x if is a list, otherwise (list x)."
  (if (listp x) x (list x)))

(defun mksymbol (&rest parts)
  "Concatenate the parts and intern as a symbol."
  (intern (format nil "~{~A~}" parts)))

(defun first-atom (x)
  "The first (leftmost) atom in a nested list."
  (if (atom x) x (first-atom (first/ x))))

(defun length=1 (x)
  "Is this a list of length 1?"
  (and (consp x) (null (rest/ x))))

(defun length>1 (x)
  "Is this a list of length greater than 1?"
  (and (consp x) (rest/ x)))

(defun dotted? (exp)
  ;; Is this a dotted list -- one with a non-null last tail?
  (and (consp exp) (not (null (rest/ (last exp))))))

(defun starts-with (sequence item)
  "Is the first argument a sequence that starts with this item?"
  (setq sequence (strip sequence))
  (and (typecase sequence
         (list (not (null sequence)))
         (vector (> (length sequence) 0)))
       (eql (elt sequence 0) item)))

(defun ends-with (sequence item)
  "Is the first argument a sequence that ends with this item?"
  (and (typecase sequence
         (list (not (null sequence)))
         (vector (> (length sequence) 0)))
       (eql (elt sequence (- (length sequence) 1)) item)))

(defun bracketed-with (sequence item)
  "Is the first argument a sequence that starts and ends with this item?"
  (and (starts-with sequence item)
       (ends-with sequence item)))

(defun expand-files (files)
  "Return a list of files matching the specification."
  (mapcan #'directory (mklist files)))

(defun find-anywhere (item tree)
  "Does item appear anywhere in tree?"
  (or (equal item tree)
      (and (consp tree)
           (or (find-anywhere item (car tree))
               (find-anywhere item (cdr tree))))))

;;;; DESTRUCTURING BIND, AND VARIANTS

(defmacro safe-destructuring-bind (form exp &body body)
  ;; This is similar to destructuring-bind, except
  ;; (1) Missing args are silently ignored
  ;; (2) No & keywords, except &optional (abbreviated &opt). Dot at end ok.
  (*ing-bind-fn form exp body #'(lambda (x) (declare (ignore x)) 'identity)))

(defmacro converting-bind (form exp &body body)
  ;; Like safe-destructuring-bind, except
  ;; (3) variables in FORM are converted according to their name
  (*ing-bind-fn
   form exp body
   #'(lambda (arg)
       (case arg
         ((f pred) 'cvt-fn)
         ((name ignore asis) 'identity)
         ((body) 'cvt-body)
         ((type class) 'cvt-type-exp)
         ((keys) 'cvt-keys) ; Which does NOT convert; just handles :test-not
         ((stdin) '(lambda (x)
                    (if (null/ x) '*standard-input* (cvt-exp x))))
         ((stdout) '(lambda (x)
                     (if (null/ x) '*standard-output* (cvt-exp x))))
         (otherwise
          (if (ends-with (string arg) #\*)
              'cvt-exps 
              'cvt-exp))))))

(defun *ing-bind-fn (form exp body converter)
  (let ((var (gensym))
         (vars nil))
    (loop (let ((v (if (atom form) form (first form))))
            (cond ((null/ form) (RETURN))
                  ((member v '(&opt &optional)) nil)
                  ((or (not (symbolp v)) (member v lambda-list-keywords))
                   (error "Don't support ~A" v))
                  ((and (atom form) (not (null form)))
                   (push `(,v (,(funcall converter v) ,var)) vars))
                  (t (push `(,v (,(funcall converter v) (pop ,var))) vars)))
            (if (atom form) (RETURN) (pop form))))
    `(let* ((,var ,exp) ,@(nreverse vars))
       (declare (ignorable ,var))
       ,@body)))
