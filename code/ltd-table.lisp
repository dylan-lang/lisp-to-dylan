;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*- Author: Peter Norvig
;;;     File: ltd-table.lisp; Date: /95
(in-package :ltd)

;;;; Functions called from the (ltd-fn ...) table in tables.lisp


;;;; CLtL2 CH 2: DATA TYPES

(defun cvt-type-exp (exp)
  ;; Convert a CL type specification (with the quote!) to Dylan.
  (if (starts-with (strip exp) 'quote)
      (cvt-type (second/ exp))
      (cvt-exp exp)))

(defun cvt-type (spec)
  ;; Convert a CL type specification (without the quote!) to Dylan.
  ;; If the spec is a name that is known from a cvt-type declaration,
  ;; then look it up.  If it is an unknown name, just wrap <> around
  ;; it.  If the spec is an (or ...), do each branch. For example,
  ;; (or standard-class my-class) converts to (type-union <class> <my-class>).
  (cond ((null spec) nil)
        ((starts-with spec 'eql)
         `(singleton ,@(mapcar #'cvt-exp (rest/ spec))))
        ((starts-with spec 'or)
         `(type-union ,@(mapcar #'cvt-type (args spec))))
        ((starts-with spec 'integer)
         `(limited <integer>
                   ;; Or is it :from and :to ??
               ,@(when (second/ spec) `(:min ,(second/ spec)))
               ,@(when (third spec) `(:max ,(third spec)))))
        ((starts-with spec 'member)
         `(one-of ,@(mapcar #'kwote (rest/ spec))))
        ((not (symbolp spec))
         (cvt-erroneous spec spec "Can't convert type specification."))
        ((get-cvt-type spec))
        ((starts-with spec #\$) spec)
        (t (mksymbol '< spec '>))))

;;;; CLtL2 CH 4: TYPE SPECIFIERS

(defun cvt-deftype (exp)
  ;; Converts parameterless types only
  (safe-destructuring-bind (name arglist body1 . body) (args exp)
    (if (and (false? arglist) (starts-with 'quote (strip body1)) (false? body))
        `(define-constant ,(cvt-type name) 
           ,(cvt-type-exp (second/ (strip body1))))
        (cvt-erroneous exp '|\#f| "Can't handle complex deftypes."))))

;;;; CLtL2 CH 5: PROGRAM STRUCTURE

(defun cvt-defun (exp)
  "Convert a defun or basic defmethod into a define-method."
  `(,(get-option :defun-as) ,(second/ exp) 
     ,@(cvt-parms-and-body (nthcdr 2 exp) (second/ exp))))

(defun cvt-lambda (exp &optional)
  `(method ,@(cvt-parms-and-body (rest/ exp))))

(defun cvt-parms-and-body (parms-and-body &optional name)
  (safe-destructuring-bind (parms . body) parms-and-body
    (let* ((declarations (extract-just-declarations body))
           (vals (extract-values-declaration declarations)))
      `(,(cvt-parms parms :declarations declarations)
        ,@(ifd vals (list vals))
        ,@(cvt-body body :name name)))))

(defun cvt-parms (lambda-list &key body
                              (declarations (extract-just-declarations body)))
  ;; Convert &*** words, handle specializers and defaults, add in declarations
  ;; Return &aux bindings as second argument.
  (when (eq (strip lambda-list) '|()|) (setq lambda-list '()))
  (let ((key? nil) ; have we seen a &key (or &optional) yet?
        (aux? nil) ; have we seen an &aux yet?
        (auxs nil))
    (labels ; arg is one of var, (var val), ((key var) val), (var type)
        ((key (arg) (cond ((atom arg) nil)
                          ((atom (first/ arg)) nil)
                          (t (first/ (first/ arg)))))
         (var (arg) (cond ((atom arg) arg)
                          ((atom (first/ arg)) (first/ arg))
                          (t (second/ (first/ arg)))))
         (val (arg) (if (and (consp arg) key?) (cvt-exp (second/ arg)) nil))
         (var-and-typ (arg) (if (and (not key?) (consp arg))
                                `(|::| ,(var arg) ,(cvt-type (second/ arg)))
                                (add-type-declaration (var arg) declarations)))
         (var-and-typ-and-val (arg) (if (val arg)
                                        `(= ,(var-and-typ arg) ,(val arg))
                                        (var-and-typ arg)))
         (cvt-arg (arg)
          (cond (aux? (push (list (var arg) (val arg)) auxs) nil)
                ((member arg '(&body &whole &environment))
                 (cvt-erroneous lambda-list arg "Can't handle macros."))
                ((eq arg '&aux) (setq aux? t) nil)
                ((eq arg '&optional) (setq key? t) '|\#key|)
                ((eq arg '&rest) ' |\#rest|)
                ((eq arg '&key) (if key? NIL (progn (setq key? t) '|\#key|)))
                ((eq arg '&allow-other-keys) '|\#all-keys|)
                ((key arg) `(:args-bare ,(key arg) ,(var-and-typ-and-val arg)))
                (t (var-and-typ-and-val arg))))
         (cvt-args (args)
           (cond ((atom args) args)
                 (t (let* ((one (cvt-arg (first/ args)))
                           (two (cvt-args (rest/ args))))
                      (if one (cons one two) two))))))
         (values `(:args ,@(cvt-args lambda-list))
                 (nreverse auxs)))))

;;;; CLtL2 CH 6: PREDICATES

(defun cvt-to-binary (exp)
  "Convert (+ a b c d) to (+ (+ (+ a b) c) d)."
  (if (not (call? exp))
      (if (get-option :only-binary-arithmetic-ops)
          (second/ exp) ; E.g. #'+ => +
          (let ((id (case (second/ exp) ((+ -) 0) ((* /) 1))))
            `(method (:args |\#rest| args)
                     (reduce ,id (second/ op) args))))
      (let ((args (cvt-exps (args exp))))
        (case (length args)
          (0 (ecase (op exp)
               ((+) 0)
               ((*) 1)
               ((- /) (cvt-erroneous exp 0 "Missing arguments to ~A."
                                     (op exp)))
               ((and &) '|\#t|)
               ((or \|) '|\#f|)))
          (1 (case (op exp)
               ((-) (if (numberp (first/ args)) (eval exp) exp))
               ((/) `(/ 1 ,(first args)))
               (t (first/ args))))
          (2 (cons (op exp) args))
          (otherwise
           (let ((result (list (op exp) (first/ args) (second/ args))))
             (dolist (arg (nthcdr 2 args))
               (setf result (list (op exp) result arg)))
             result))))))

;;;; CLtL2 CH 7: CONTROL STRUCTURE

(defun cvt-fn (exp)
  "Convert a CL expression, interpreted as a function, to Dylan."
  ;; If the exp is of the form 'fn or #'fn, use the cvt-fn tables.
  ;; Otherwise, just convert as a regular expression.
  (cond ((and (or (starts-with exp 'function) (starts-with exp 'quote))
              (consp exp) (length=1 (args exp)))
         (let ((fn (second/ exp)))
           (cond ((starts-with fn 'setf)   (mksymbol (second/ fn) '-setter))
                 ((starts-with fn 'lambda) (cvt-lambda fn))
                 ((not (symbolp fn))       (cvt-exp fn))
                 ((get-cvt-fn fn)          (funcall (get-cvt-fn fn) exp))
                 (t                        fn))))
        (t (cvt-exp exp))))

(defun cvt-setf (exp)
  (maybe-begin (loop for (var val) on (args exp) by #'cddr
                     collect `(:= ,(cvt-exp var) ,(cvt-exp val)))))

(defun binding-var (binding) (if (consp binding) (first/ binding) binding))
(defun binding-val (binding) (if (consp binding) (second/ binding) '|\#f|))

(defun cvt-let (exp)
  (safe-destructuring-bind (bindings . body) (args exp)
    (when (eq bindings '|()|) (setq bindings '()))
    (let ((declarations (extract-just-declarations body)))
      (if (let-can-use-serial-binding? bindings)
          (cvt-let* exp)
          `(begin
            (let (:list ,@(mapcar
                          #'(lambda (b)
                              (add-type-declaration
                               (binding-var b) declarations))
                          bindings))
              (values ,@(mapcar #'(lambda (b) (cvt-exp (binding-val b))) 
                                bindings))
              ,@(cvt-body body)))))))

(defun cvt-compiler-let (exp)
  (cvt-erroneous exp (cvt-let exp) "COMPILER-LET converted to LET"))

(defun cvt-let* (exp)
  "Convert let* to nested lets." 
  (safe-destructuring-bind (vars . body) (rest/ exp)
    (move-comment
     vars
     (let ((code (cvt-body body))
           (declarations (extract-just-declarations body)))
       (when (eq vars '|()|) (setq vars '()))
       (if (null vars)
           (maybe-begin code)
         `(begin
           ,(dolist (binding (reverse vars) (first/ code))
              (move-comment
               binding
               (let* ((var (binding-var binding))
                      (val (cvt-exp (binding-val binding)))
                      (special? (bracketed-with (string var) #\*)))
                 (setf code
                       (if special?
                           `((dynamic-bind
                              (= ,(add-type-declaration var declarations)
                                 ,val) ,@code))
                         `((let ,(add-type-declaration var declarations)
                             ,val ,@code)))))))))))))


(defun let-can-use-serial-binding? (bindings)
  ;; If the code is using LET (i.e., parallel binding) and none of the bindings
  ;; depend on the result of a previous binding, then we can convert it to use
  ;; serial binding.  This makes the resulting Dylan code look a lot nicer.
  ;; This suggests that Dick Waters is right in using let* for the default.
  (let ((vars-so-far nil))
    (loop for binding in bindings
          as var = (binding-var binding)
          as val = (binding-val binding)
          do (if (some #'(lambda (v) (find-anywhere v val))
                       vars-so-far)
                 (RETURN-FROM let-can-use-serial-binding? nil)
                 (push var vars-so-far))))
  t)

(defun cvt-flet (exp)
  (safe-destructuring-bind (bindings . body) (args exp)
    ;; Convert (f (args) body) to (f (lambda (args) body)); then use let
    (flet ((fix-fn (b) `(,(binding-var b) (lambda ,@(rest/ b)))))
      (cvt-exp `(let* ,(mapcar #'fix-fn bindings) ,@body)))))

(defun cvt-labels (exp)
  (safe-destructuring-bind (bindings . body) (args exp)
    `(local (:list-bare
             ,@(mapcar #'(lambda (b)
                          (move-comment
                           b `(:local-method ,(first/ b)
                               ,@(cvt-parms-and-body (rest/ b) (first/ b)))))
                      (strip bindings)))
            ,@(cvt-body body))))

(defun cvt-symbol-macrolet (exp)
  ;; This is important, because with-slots macroexpands into symbol-macrolet.
  ;; Unfortunately, symbol-macrolet is a special form, not a macro, so we have
  ;; to expand it ourselves.  We do a half-way job ??, converting ALL symbols,
  ;; even ones that are not in an evaluation context.  So you lose on things 
  ;; like (symbol-macrolet ((x 1)) (list 'x (let ((x 2)) x))), but do fine on
  ;; e.g. (symbol-macrolet ((x (slot-value y 'x))) (setf x (* x 2)))
  (safe-destructuring-bind (bindings . body) (args exp)
    (maybe-begin
     (cvt-exps (nsublis (mapcar #'(lambda (b) (cons (first/ b) (second/ b)))
                                bindings)
                        body)))))

(defun cvt-if (exp)
  (destructuring-bind (pred conseq &optional (else nil else?)) (args exp)
    (let ((then (cvt-exp conseq)))
      `(if ,(cvt-exp pred)
           ,@(if (starts-with then 'begin) (args then) (list then))
           ,@(cvt-else else else?)))))

(defun cvt-else (else else?)
  ;; ELSE is a LISP expression. Return a list of (:else[if] ...) clauses
  (cond ((null else?) nil)
        ((starts-with else 'if)
         (destructuring-bind (p then &optional (else2 nil else2?)) (args else)
           `((:elseif ,(cvt-exp p) ,(cvt-exp then))
             ,@(cvt-else else2 else2?))))
        (t (let ((dylan-else (cvt-exp else)))
             (if (starts-with dylan-else 'begin)
                 `((:else ,@(args dylan-else)))
               `((:else ,dylan-else)))))))

(defun cvt-cond (exp)
  ;; Convert cond to either IF or CASE.  Worry about (cond ((test))).
  (cond ((null (args exp)) '|\#f|)
        ((some #'length=1 (mapcar #'strip (butlast (args exp)))) ; E.g. ((test))
         `(begin (let _that |\#f|
                   ,(cvt-cond `(cond
                                ,@(mapcar
                                   #'(lambda (c)
                                       (if (length=1 (strip c))
                                           `((setq _that ,(first/ c)) _that)
                                           c))
                                   (args exp)))))))
        ((eq (get-option :cond-as) 'if)
         `(if ,(cvt-exp (first/ (first/ (args exp))))
             ,@(cvt-exps (rest/ (first/ (args exp))))
             ,@(mapcar #'(lambda (clause)
                           (move-comment
                            clause
                            `(:elseif ,@(cvt-exps clause))))
                       (butlast (rest/ (args exp))))
             ,(let ((final (last1 (rest/ (args exp)))))
                (move-comment
                 final
                 (cond ((length=1 final) `(:else ,(cvt-exp (first/ final))))
                       ((eq t (first/ final)) `(:else ,@(cvt-exps (rest/ final))))
                       (t `(:elseif ,@(cvt-exps final))))))))

        (t `(case ,@(mapcar
                     #'(lambda (clause)
                         `(:branch ,@(cvt-exps clause)))
                     (args exp))))))

(defmacro must-be-call (result)
  `(if (call? exp) ,result (second/ exp)))

(defun kwote (x) (list 'quote x))

(defun cvt-case (exp)
  (cvt-ecase
   (if (member (first/ (first/ (last exp))) '(t otherwise))
       exp
       (append exp '((otherwise nil))))))

(defun cvt-ecase (exp)
  (must-be-call
   (safe-destructuring-bind (keyform . clauses) (args exp)
    `(select ,keyform ,@(mapcar 'cvt-case-clause clauses)))))

(defun cvt-case-clause (clause)
  `(:branch ,(cond
              ((member (move-comment clause (first/ clause))
                       '(otherwise t))
               'otherwise)
              ((atom (first/ clause))
               (kwote (first/ clause)))
              (t (cons :list (mapcar #'kwote (first/ clause)))))
   ,@(cvt-body (rest/ clause))))

(defun cvt-typecase (exp)
  (must-be-call
   (destructuring-bind (x . clauses) (args exp)
     `(select (:for-clause ,(cvt-exp x) by instance?)
              ,@(mapcar #'(lambda (c)
                            (move-comment
                             c `(:branch ,(cvt-exp (first/ c))
                             ,@(cvt-exps (rest/ c)))))
                      clauses)))))

(defun cvt-return-from (exp)
  (let ((result (cvt-exp (third exp))))
    `(,(mksymbol 'return-from- (second/ exp))
       ,@(if (starts-with result 'values) (args result) (list result)))))

(defun handle-returns (body &optional name)
  ;; Wrap a BLOCK around body if it uses a return or return-from-name.
  ;; Returns a list of forms
  (let ((return-from-name (mksymbol 'return-from- name)))
    (if (or (find-return return-from-name body 'return))
        `((block ,(if name return-from-name 'return) ,@body))
        body)))

(defun handle-returns1 (exp &optional name)
  ;; Like handle-returns, but for a single expression, not a list of them
  (first/ (handle-returns (list exp) name)))

(defun handle-return-froms (body name)
  ;; Wrap a BLOCK around body if it uses a (return-from name)
  ;; Returns a list of forms
  (let ((return-from-name (mksymbol 'return-from- name)))
    (if (find-return return-from-name body)
        `((block ,(if name return-from-name 'return) ,@body))
        body)))

(defun find-return (name body &optional name2)
  (setq body (strip body))
  (flet ((match (x) (or (eq x name) (and name2 (eq x name2)))))
    (cond ((atom body) nil)
          ((match (first/ body)) t)
          ((and (starts-with body 'block) (match (second/ body)))
           nil)
          (t (or (find-return name (first/ body) name2)
                 (find-return name (rest/ body) name2))))))
  
(defun cvt-do (exp)
  ;; Need to fix this for do*
  (safe-destructuring-bind (vars endtest-and-values . body) (args exp)
    (setq vars (if (eq vars '|()|) '() (mapcar #'mklist vars)))
    (if (eq endtest-and-values '|()|) (setf endtest-and-values '()))
    (let* ((endtest (cvt-exp (move-comment endtest-and-values 
                                           (first/ endtest-and-values))))
           (values (cvt-exps (rest/ endtest-and-values)))
           (bindings (mapcar #'(lambda (binding)
                                 (move-comment binding (first/ binding))
                                 (let* ((var (pop binding))
                                        (init (pop binding))
                                        (then (if binding (pop binding) init)))
                                   `(:for-clause ,var = ,init then ,then)))
                             vars)))
      (handle-returns1
       `(for (:list-bare ,@bindings
              ,@(when endtest-and-values `((:for-clause :until ,endtest))))
             ,@(cvt-body body)
             ,@(when values
                 (if (= (length values) 1)
                     `((:finally ,@values))
                     `((:finally (values ,@values))))))))))

(defun cvt-dolist (exp)
  (destructuring-bind ((name list &optional result) . body) (args exp)
    (handle-returns1 `(for (:for-clause ,name in ,(cvt-exp list))
                          ,@(cvt-body body)
                          ,@(ifd result `((:finally ,(cvt-exp result))))))))

(defun cvt-dotimes (exp)
  (destructuring-bind ((name count &optional result) . body) (args exp)
    (handle-returns1 `(for (:for-clause ,name from 0 below ,(cvt-exp count))
                          ,@(cvt-body body)
                          ,@(ifd result `((:finally ,(cvt-exp result))))))))

(defun cvt-tagbody (exp)
  ;; Make these a set of mutually recursive local methods.
  ;; This way (GO label) translates as (label).
  ;; I.e. (tagbody ... A *** B ---) translates as
  ;; (labels ((A () *** (B)) (B () ---)) ... (A))
  (if (notany #'symbolp (args exp))
      (maybe-begin (cvt-exps (args exp)))
      (let ((args (args exp)))
        (flet ((chunk () ; eat a chunk up to next label
                (let ((p (position-if #'symbolp args)))
                  (if p
                      (prog1
                          (nconc (subseq args 0 p)
                                 (list (list (mksymbol 'go- (elt args p)))))
                        (setf args (subseq args p)))
                      (prog1 args (setf args nil))))))
          (let ((fns nil)
                (body (chunk)))
            (loop while args
                  do (push `(,(mksymbol 'go- (pop args)) () ,@(chunk)) fns))
            (cvt-exp `(labels ,fns ,@body)))))))

(defun cvt-multiple-value-call (exp)
  (cvt-exp `(apply ,(first/ (args exp))
                   (nconc ,@(loop for arg in (rest/ (args exp))
                                        collect `(multiple-value-list ,arg))))))

(defun cvt-multiple-value-bind (exp)
  (safe-destructuring-bind (vars form . body) (args exp)
    `(let ,(cvt-parms vars :body body) ,(cvt-exp form) ,@(cvt-body body))))

(defun cvt-multiple-value-setq (exp)
  ;; I can't believe "(a, b, c) := values(1, 2, 3)" isn't in the language!
  (let* ((temps (loop for var in (second/ exp) collect (mksymbol '_ var)))
         (sets (loop for var in (second/ exp)
                     for temp in temps
                     collect `(:= ,var ,temp))))
  `(begin (let (:args |\#rest| ,@temps) ,(cvt-exp (third exp))
            ,@sets))))

(defun cvt-tag (tag)
  (cond ((starts-with tag 'quote) (second/ tag))
        ((constantp tag) tag)
        (t (cvt-erroneous tag (cvt-exp tag)
                          "Can't convert a run-time catch tag."))))

;;;; CLtL2 CH 8: MACROS

(defun cvt-macro (exp)
  ;; Macroexpand and convert
  ;; We use this for lots of things: rotatef, psetf, ...
  ;; If there is no macro,
  (let ((expansion (safe-macroexpand exp)))
    (cond ((eq expansion exp) (mapcar #'cvt-exp (strip exp)))
          (t (cvt-exp expansion)))))

(defun safe-macroexpand (exp) (macroexpand (to-normal-lisp exp)))

(defun to-normal-lisp (exp)
  "Convert back to normal lisp: eliminate comments and |()|."
  (cond ((comment? exp) (to-normal-lisp (com-code exp)))
        ((eq exp '|()|) '())
        ((consp exp) (recons (to-normal-lisp (car exp)) 
                             (to-normal-lisp (cdr exp)) 
                             exp))
        ((stringp exp) exp)
        ((vectorp exp) (coerce (map-into exp #'to-normal-lisp exp) 'vector))
        (t exp)))

(defun recons (x y x-y)
  "Cons x and y, but reuse x-y if it would be the same."
  (if (and (eql x (car x-y)) (eql y (cdr x-y)))
      x-y
      (cons x y)))

;;;; CLtL2 CH 11: PACKAGES

(defun cvt-in-package (exp)
  (when (get-option :obey-in-package)
    (eval exp)
    (format nil "~(~A~)" exp)))

(defun cvt-export (exp)
  (must-be-call
   (safe-destructuring-bind (symbols &opt package) (rest/ exp)
     (if (and (constantp (strip symbols)) (constantp package))
         `(define-module ,(or (eval package) (intern (package-name *package*)))
            (:clause export ,@(mklist (eval symbols))))
       (cvt-erroneous exp exp "Can't handle dynamic EXPORT")))))

(defun cvt-defpackage (exp)
  (must-be-call
   (safe-destructuring-bind (name . options) (rest/ exp)
     `(define-module ,name 
        ,@(loop for option in options
                when (starts-with option ':use)
                collect `(:clause use ,@(rest/ option))
                when (starts-with option ':export)
                collect `(:clause export ,@(rest/ option)))))))

;;;; CLtL2 CH 12: NUMBERS

(defun cvt-to-binary-compares (exp)
  "Convert (< a b c d) to (& (& (< a b) (< b c)) (< c d))."
  (if (not (call? exp))
      (if (get-option :only-binary-arithmetic-ops)
          (second/ exp) ; I.e., #'< => <
          `(cl-reduce-compares ,(second/ exp)))
      (let ((op (op exp))
            (args (cvt-exps (args exp))))
        (case (length args)
          ((0 1) '|\#t|)
          ((2) (cons op args))
          (otherwise
           ;; Need to bind non-atomic elements of args
           (let ((bindings nil))
             (flet ((maybe-bind (x) (cond ((consp (strip x))
                                           (let ((var (gensym)))
                                             (push (list var x) bindings)
                                             var))
                                          (t x))))
               (let* ((args `(,(first/ args)
                               ,@(mapcar #'maybe-bind (butlast (rest/ args)))
                               ,(first/ (last args))))
                      (result (list (op exp) (first/ args) (second/ args))))
                 (pop args) ; Get rid of first/ arg
                 (loop while (length>1 args) do
                       (setf result `(& ,result ,(list (op exp) (first/ args)
                                                       (second/ args))))
                       (pop args))
                 (wrap-bindings bindings result)))))))))

(defun wrap-bindings (bindings code)
  ;; Wrap code with LETs for the (var val) pairs in bindings.
  (loop for (var val) in bindings 
        do (setf code `(let ,var ,val ,code)))
  (if bindings
      `(begin ,code)
      code))

(defun cvt-division (exp)
  ;; Translate, e.g., (floor m &optional n) to (floor/ m n) or (floor m)
  (case (length (args exp))
    (2 `(,(mksymbol (op exp) '/) ,@(cvt-exps (args exp))))
    (t `(,(op exp) ,@(cvt-exps (args exp))))))

;;;; CLtL2 CH 14: SEQUENCES

(defun cvt-reduce (exp)
  (destructuring-bind (f s . keys)
      (args exp)
    (let ((function (mkpred keys (cvt-fn f)))
          (sequence (mkseq (cvt-exp s) keys)))
      (if (member :initial-value keys)
          `(reduce ,function ,(getf keys :initial-value) ,sequence)
          `(reduce1 ,function ,sequence)))))

(defun cvt-if-not (exp)
  ;; Invert the test in a sequence-function-IF-NOT 
  (if (call? exp)
      (let ((name (string (first/ exp)))
            (pos (case (op exp)
                   ((subst-if-not nsubst-if-not substitutue-if-not 
                                  nsubstitute-if-not)
                    2)
                   (otherwise 1))))
        (setf exp (copy-list exp))
        (setf (elt exp pos) `(complement ,(elt exp pos)))
        (setf (first exp) (mksymbol (subseq name 0 (- (length name) 7))))
        (cvt-exp exp))
      `(method (:args x y |\#rest| r)
               (apply ,@(cvt-if-not `(,(second/ exp) x y)) r))))

(defun cvt-keys (keys)
  ;; Leave the key/value pair alone, except
  ;; replace :test-not f with :test (complement f)
  (loop for sublist on keys by #'cddr
        do (when (and (eq (first/ sublist) ':test-not) (length>1 sublist))
             (setf (first sublist) ':test)
             (setf (second sublist) `(complement ,(second/ sublist)))))
  keys)

(defun cl? (exp dylan &optional bad-keys (cl-fn-name (mksymbol 'cl- (op dylan))))
  ;; Return DYLAN unless:
  ;; (1) If exp is of the form #'f, then use cl-fn-name
  ;; (2) If one of the bad-keys is used, use cl-fn-name applied to args
  (cond ((not (call? exp)) cl-fn-name)
        ((or (dotted? exp) (intersection bad-keys (args exp)))
         (cons cl-fn-name (cvt-exps (args exp))))
        (t dylan)))

(defun mktest (keys &optional pred binary?)
  ;; Return `:test test' or (), given a list of Lisp keyword/value pairs,
  ;; which may contain :test and :key (:test-not is already converted).
  ;; If binary? is true, apply TEST to key of two args
  ;; If PRED is given, there can be no :test, but maybe a key.
  ;; First, handle case where KEYS is not a list
  (when (listp keys)
    (let* ((test (or pred (getf keys :test '==)))
           (key (getf keys :key))
           (code
            (cvt-exp
             (cond ((and binary? key)
                    `(lambda (x y)
                      (funcall ,test (funcall ,key x) (funcall ,key y))))
                   (binary? test)
                   (key `(lambda (x y) (funcall ,test x (funcall ,key y))))
                   (t test)))))
      (if (eq code '==) nil `(:test ,code)))))

(defun mkpred (keys pred) 
  ;; Make a binary comparison predicate, possibly using :key argument from keys
  ;; pred is already converted to Dylan; keys are not.
  (when (listp keys)
    (let ((key (getf keys :key)))
      (if key `(compose ,pred ,(cvt-fn key)) pred))))

(defun mkseq (sequence keys &optional (start :start) (end :end))
  ;; Take a sequence (in Dylan) and a list of key/values (in Lisp)
  ;; and extract :start and :end if provided.
  (when (listp keys)
    (let ((starts (if (getf keys start) `(:start ,(cvt-exp  (getf keys start)))))
          (ends (if (getf keys end) `(:end ,(cvt-exp  (getf keys end))))))
      (if (or starts ends)
          `(copy-subsequence ,sequence ,@starts ,@ends)
          sequence))))

(defun mkcount (keys)
  ;; If there is a :count keyword, get it.
  (when (listp keys)
    (if (getf keys :count) `(:count ,(cvt-exp (getf keys :count))))))

(defun mkeof (eofs)
  ;; Eofs is of the form (&optional eof-errorp eof-value recursive-p)
  ;; If eof-errorp is non-nil, we return nothing; otherwise
  ;; we extract eof-value
  (if (and (consp eofs) (not (null (first/ eofs))))
      `(:on-end-of-stream ,(cvt-exp (second/ eofs)))
      nil))
      
;;;; CLtL2 CH 16: HASH TABLES

(defun cvt-make-hash-table (exp)
  (destructuring-bind (&key test size rehash-size rehash-threshold) (args exp)
    (declare (ignore rehash-size rehash-threshold))
    (let ((type (cond ((member test '('equal '#'equal) :test #'equal) '<equal-table>)
                      ((member test '('eq 'eql '#'eq '#'eql) :test #'equal) '<object-table>)
                      (t '<table>))))
      `(make ,type ,@(ifd test `(:test ,(cvt-fn test)))
             ,@(ifd size `(:size ,(cvt-exp size)))))))

;;;; CLtL2 CH 17: ARRAYS

(defun cvt-make-array (exp)
  (safe-destructuring-bind (dimensions . keys) (args exp)
    (let* ((element-type (cvt-type-exp (getf keys :element-type)))
           (type (cond ((or (integerp dimensions)
                            (starts-with dimensions 'length)
                            (and (starts-with dimensions 'quote)
                                 (length=1 (second/ dimensions))))
                        (cond ((eq element-type '<character>)
                               '<string>)
                              ((getf keys :adjustable)
                               '<stretchy-vector>)
                              (t '<vector>)))
                       (t '<array>)))
           (code `(make ,type
                        ,@(if (eql type '<array>)
                              `(:dimensions ,(cvt-exp dimensions))
                              `(:size ,(cvt-exp dimensions))))))
      (when (getf keys :fill)
        (setf code `(fill! ,code ,(cvt-exp (getf keys :fill)))))
      (when (getf keys :fill-pointer)
        (setf code `(begin (let _vector ,code
                             (:= (size _vector)
                              ,(cvt-exp (getf keys :fill-pointer))) _vector))))
      code)))

;;;; CLtL2 CH 19: STRUCTURES

(defun cvt-defstruct (exp)
  ;; See also cvt-defclass
  (safe-destructuring-bind (name-and-options . slots) (args exp)
    (let* ((name (first-atom (strip name-and-options)))
           (options (if (listp (strip name-and-options))
                        (rest/ (strip name-and-options))))
           (supers (mapcar #'cvt-type (rest/ (assoc/ :include options))))
           (comment nil))
      (when (eq slots '|()|) 
        (setq slots '()))
      (when (stringp (first/ slots))
        (setq comment (pop slots)))
      (let ((code `(define-class ,(cvt-type name) 
                                 (:list ,@(or supers '(<object>)))
                                 ,@(cvt-defstruct-slots slots name options))))
        (if comment
            (make-com :comment comment :code code)
            code)))))


(defun cvt-defstruct-slots (slots struct-name options)
  ;; Converts a set of defstruct slot definitions to a set of Dylan slots
  ;; See also cvt-defclass-slots
  (let* ((option (assoc/ :conc-name options))
         (conc-name (if (consp (strip option)) (second/ option)
                      (mksymbol struct-name '-))))
    (flet ((convert-defstruct-slot 
            (slot)
            (move-comment 
             slot
             (let* ((name (if (listp slot) (first/ slot) slot))
                    (new-name (mksymbol conc-name name))
                    (exp new-name)
                    (type (if (listp slot) (getf (cddr slot) :type))))
               (when type
                 (setf exp `(|::| ,exp ,(cvt-type type))))
               (when (and (consp slot) (>= (length slot) 2))
                 (setf exp `(= ,exp ,(cvt-exp (second/ slot)))))
               (push new-name *dotted-functions*)
               `(:slot ,exp :init-keyword
                ,(intern (symbol-name new-name) :keyword))))))
      (mapcar #'convert-defstruct-slot (strip-nil slots)))))

;;;; CLtL2 CH 22: INPUT/OUTPUT

(defun cvt-format (exp)
  (must-be-call
  (destructuring-bind (stream string . args) (args exp)
    (let ((str (cvt-format-string string)))
      (cond ((and (stringp str) (eq (strip stream) 't))
             `(format-out ,str ,@(cvt-exps args)))
            ((stringp str)
             `(,(op exp) ,(cvt-exp stream) ,str ,@(cvt-exps args)))
            (t `(,str ,(cvt-exp stream) ,@(cvt-exps args))))))))


(defun cvt-format-string (control)
  ;; Handle ~X => %X; ~% => newline; % => %%
  ;; Notice that #\newline and #\tab are printed as \n and \t (by dpp-string).
  (if (not (stringp control))
      (cvt-exp control)
      (let* ((length (length control))
             (i 0)
             (unhandled nil)
             (result (make-array length :element-type 'character
                                 :adjustable t :fill-pointer 0)))
        (flet ((emit (ch) (vector-push-extend ch result))
               (consume () (if (< i length) (prog1 (aref control i) (incf i)))))
          (loop for ch = (consume) while ch do
                (case ch
                  (#\% (emit #\%) (emit #\%))
                  (#\~ (let ((ch2 (consume)))
                         (if (null ch2)
                             (emit #\~)
                             (case (char-upcase ch2)
                               ((#\D #\B #\O #\X #\C) (emit #\%) (emit ch2))
                               ((#\A) (emit #\%) (emit #\S))
                               ((#\S) (emit #\%) (emit #\=))
                               ((#\~) (emit #\~))
                               ((#\& #\%) (emit #\newline))
                               (otherwise (pushnew ch2 unhandled))))))
                  (otherwise (emit ch)))))
        (cond ((null unhandled) result)
              ((get-option :macroexpand-hard-format-strings)
               (cvt-exp (macroexpand `(formatter ,control))))
              (t (cvt-erroneous control result 
                                "Unhandled format characters: ~{~~~C~^,~}"
                                (nreverse unhandled)))))))

(defun extract-stream (keys &optional (default '*standard-output*))
  "Return the STREAM arg from the list of Dylan key/vals, followed by the keys."
  ;; I.e. (extract-stream '(:pretty t :stream s)) => (s :pretty t)
  (cons (or (getf (mapcar #'strip keys) :stream) default)
        (let ((n (position :stream keys :key #'strip)))
          (if n
              (append (subseq keys 0 n) (subseq keys (+ n 2)))
            keys))))
        

;;;; CLtL2 CH 23: FILE SYSTEM INTERFACE

(defun cvt-with-open-file (exp)
  (must-be-call
   (destructuring-bind ((var file . options) . body) (args exp)
     `(with-open-file (= ,(cvt-exp var) (:args ,(cvt-exp file) 
                                               ,@(cvt-exps options)))
       ,@(cvt-body body)))))

;;;; CLtL2 CH 28: COMMON LISP OBJECT SYSTEM

(defun cvt-compute-applicable-methods (exp)
  (converting-bind (f . arg*) (args exp)
   `(begin (let (:list _a _b)
             (apply sorted-applicable-methods ,f . ,arg*)
             (concatenate _a _b)))))

(defun cvt-defclass (exp)
  (safe-destructuring-bind (name supers slots . options) (args exp)
    (when (null/ supers)
      (setf supers (ecase (op exp)
                     (defclass '(t))
                     (define-condition '(condition)))))
    `(define-class ,(cvt-type name) 
       (:list ,@(mapcar #'cvt-type (or supers '(t))))
                   ,@(cvt-defclass-slots slots options))))

(defun cvt-defclass-slots (slots options)
  ;; Converts a set of CLOS slot definitions to a set of Dylan slot definitions
  (let ((default-initargs (rest/ (assoc/ :default-initargs options))))
    (flet ((convert-slot (slot)
            (move-comment
             slot
             (if (symbolp slot)
                 slot
               (destructuring-bind (name &key reader writer accessor
                                         type allocation initarg documentation
                                         (initform nil initform-p)
                                         (initvalue nil initvalue-p))
                   slot
                 (let ((getter (or reader accessor
                                   name)) ; norvig change to swm's code
                       (setter writer)
                       (default-initarg (getf default-initargs initarg)))
                   (when default-initarg
                     (setq initvalue default-initarg))
                   (when type
                     (setf getter `(|::| ,getter ,(cvt-type type))))
                   (when initform-p
                     (setf getter `(= ,getter ,(cvt-exp initform))))
                   (push getter *dotted-functions*)
                   (add-comment documentation
                   `(:slot ,getter ,@(and setter `(:setter ,setter))
                           ,@(and initarg `(:init-keyword ,initarg))
                           ,@(and initvalue-p `(:init-value ,initvalue))
                           ,@(and allocation `(:allocation ,allocation))))))))))
      (mapcar #'convert-slot (strip-nil slots)))))

(defun cvt-defgeneric (exp)
  (safe-destructuring-bind (name parms . options) (args exp)
    ;; Silently ignores options other than :documentation
    (add-comment
     (second/ (find :documentation options :key #'first-atom))
     `(define-generic ,name ,(cvt-parms parms)))))

(defun cvt-defmethod (exp)
  "Just like cvt-defun, but handle make-instance, initialize-instance."
  ;; Just give up on method qualifiers.
  (safe-destructuring-bind (name parms . body) (args exp)
    (cond ((typep parms '(and symbol (not null)))
           (cvt-erroneous exp
            (cvt-defmethod `(defmethod ,name ,@(rest/ body)))
            "Defmethod qualifier ~A ignored." name))
          ((eq name 'make-instance)
           (cvt-defmethod `(defmethod make ,parms ,@body)))
          ((eq name 'initialize-instance)
           (cvt-defmethod `(defmethod initialize ,parms ,@body)))
          (t (cvt-defun exp)))))

(defun cvt-with-slots (exp)
  ;; Just expand macro, except if the argument is atomic, avoid local var.
  (if (atom (strip (third exp)))
      (cvt-exp `(symbol-macrolet 
                    ,(mapcar #'(lambda (v)
                                  `(,v (slot-value ,(strip (third exp)) ',v)))
                              (strip (second/ exp)))
                  ,@(cdddr exp)))
    (cvt-macro exp)))

;;;; CLtL2 CH 29: CONDITIONS

(defun cvt-condition-function (exp)
  ;; Convert a function like error or signal. There are three cases for args:
  ;; (1) condition-object &rest args
  ;; (2) format-string &rest args
  ;; (3) condition-class-name &rest args
  ;; Dylan supports the first two.  We don't always handle the third.
  (if (not (call? exp))
      (second/ exp) ; Close, but doesn't handle (3). ??
      (let* ((arg1 (first/ (args exp)))
             (class-name? (and (starts-with arg1 'quote)
                               (symbolp (second/ arg1))))
             (arg1-val (cond ((stringp arg1) (cvt-format-string arg1))
                             (class-name? (cvt-type arg1))
                             (t (cvt-exp arg1)))))
        `(,(op exp) ,arg1-val ,@(cvt-exps (rest/ (args exp)))))))
