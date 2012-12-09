;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: User; -*-
(in-package :ltd)

;;;; CVT-LOOP and friends

;;; Code taken from Norvig's PAIP, modified to generate Dylan instead of 
;;; Lisp, and augmented to handle more loop clauses, and to handle loop 
;;; symbols in different packages.

(defstruct loops ;; LOOPS stands for LOOP Structure (can't use LOOP).
  "A structure to hold parts of a loop as it is built."
  (code nil)     ;; The original complete Lisp code
  (exps nil)     ;; What remains to be parsed
  (vars nil)     ;; List of (var val), in Lisp (reversed)
  (fors nil)     ;; List of DYLAN code
  (body nil)     ;; List of Lisp code (reversed)
  (prologue nil) ;; List of Lisp code (reversed)
  (epilogue nil) ;; List of Lisp code (reversed)
  (result nil)   ;; The final result (Lisp)
  (name nil)     ;; Name for return-from
  (conditionals nil)) ;; Stack of currently active when/unless's

(defun cvt-loop (exp)
  "Supports both ANSI and simple LOOP. Warning: Not all of LOOP is supported."
  (if (listp (strip (first/ (args exp))))
      ;; No keyword implies simple loop:
      (handle-returns1 `(while |\#t| ,@(cvt-body (args exp))))
    ;; otherwise process loop keywords:
    (let ((l (make-loops :code exp :exps (args exp))))
      (CATCH 'LOOP-ERROR
             (loop (if (null (loops-exps l)) (RETURN))
               (parse-clause l))
             (fill-loop-template l)))))

(defun parse-clause (l)
  (let ((key (pop (loops-exps l))))
    (funcall (get-loop-fn key) l key (pop (loops-exps l)))))

(defun fill-loop-template (l)
  "Use a loops-structure instance to fill the template."
  (let ((code (handle-returns1
               `(for (:list-bare ,@(nreverse (loops-fors l)))
                     ,@(cvt-exps (nreverse (loops-body l)))
                     ,@(if (or (loops-epilogue l) (loops-result l))
                           `((:finally ,@(cvt-exps (nreverse (loops-epilogue l)))
                                       ,(cvt-exp (loops-result l))))))
               (loops-name l))))
    (loop for (var val) in (loops-vars l) do
          (setf code `(let ,(cvt-exp var) ,(cvt-exp val) ,code)))
    (when (loops-prologue l)
      (setf code `(begin ,@(cvt-exps (nreverse (loops-prologue l))) ,code)))
    (encapsulate-let code)))

(defun loop-error (l key exp)
  (cond ((get-option :macroexpand-hard-loops)
         (let ((expanded (handle-loop-finish (safe-macroexpand (loops-code l)))))
           (warn "Can't handle ~A ~A in loop; macroexpanding." key exp)
           (THROW 'LOOP-ERROR (cvt-exp expanded))))
        (t (push (cvt-erroneous (loops-code l) nil
                                "Can't handle ~A ~A in loop." key exp)
                 (loops-body l)))))


(defvar *loop-fns* (make-hash-table :test #'equal))

(defun get-loop-fn (key)
  (let ((sym (strip key)))
    (or (and (symbolp sym) (gethash (string sym) *loop-fns*))
        'loop-error)))

(defmacro def-loop (keys (l next-exp &optional (key-var 'key)) &rest body)
  "Define a new LOOP keyword or keywords."
  `(setf ,@(mapcan
            #'(lambda (key)
                `((gethash ,(string key) *loop-fns*)
                  #'(lambda (,l ,key-var ,next-exp) 
                      (declare (ignorable ,key-var))
                      ,@body)))
            (mklist keys))))

(defun add-var (l var init)
  "Add a variable to the loop."
  (unless (assoc/ (strip var) (loops-vars l))
    (push (list var init) (loops-vars l))))

(defun handle-loop-finish (exp)
  (if (and (starts-with exp 'macrolet) (length=1 (second/ exp))
           (eq 'loop-finish (first/ (first/ (second/ exp)))))
      (subst (third (first/ (second/ exp))) '(loop-finish) 
             `(progn ,@(cddr exp))
             :test #'equal)
      exp))
      
(defun parse-loop-key (l &rest keys)
  "If the next exp in L is one of keys, pop it and return true."
  (when (apply #'loop=? (first/ (loops-exps l)) keys)
    (pop (loops-exps l))
    t))

(defun loop=? (exp &rest options)
  ;; Is exp a symbol that is spelled the same as one of the options?
  (and (symbolp (strip exp))
       (member (strip exp) options :test #'string-equal)))

;;;; Loop Clauses 26.6 (p 716 CLtL2) Iteration Control

(def-loop (FOR AS) (l var)
  (when (not (symbolp (strip var)))
    (loop-error l 'for var))
  (setq var (parse-var l var))
  (cond
   ((parse-loop-key l "IN" "ACROSS")
    (push `(:for-clause ,var in ,(cvt-exp (pop (loops-exps l)))) 
          (loops-fors l)))
   ((parse-loop-key l "ON")
    (let ((by (if (parse-loop-key l "BY")
                  (cvt-exp (pop (loops-exps l)))
                'tail)))
      (push `(:for-clause ,var = ,(cvt-exp (pop (loops-exps l))) 
                          then (,by ,var))
            (loops-fors l))
      (push `(:for-clause :until (empty? ,var)) (loops-fors l))))
   ((parse-loop-key l "BEING")
    (parse-loop-key l "EACH" "THE")
    (cond ((parse-loop-key l "HASH-VALUE" "HASH-VALUES")
           (push `(:for-clause ,var in ,(cvt-exp (pop (loops-exps l))))
                 (loops-fors l)))
          ((parse-loop-key l "HASH-KEY" "HASH-KEYS")
           (push `(:for-clause ,var in 
                               (key-sequence ,(cvt-exp (pop (loops-exps l)))))
                 (loops-fors l)))
          (t (loop-error l 'for var))))
   ((parse-loop-key l "=")
    (let* ((init (cvt-exp (pop (loops-exps l))))
           (next (if (parse-loop-key l "THEN")
                     (cvt-exp (pop (loops-exps l)))
                   init)))
      (push `(:for-clause ,var = ,init then ,next) (loops-fors l))))
   (t (try-loop-for-arithmetic l var))))

(defun try-loop-for-arithmetic (l var)
  (let ((start 0)
        (to nil)
        (by nil)
        (negative? nil))
    (loop (let ((subkey (first/ (loops-exps l))))
            (cond ((loop=? subkey "TO" "BELOW" "ABOVE")
                   (when (loop=? subkey "ABOVE") (setq negative? t))
                   (pop (loops-exps l))
                   (setf to (list subkey (cvt-exp (pop (loops-exps l))))))
                  ((loop=? subkey "UPTO" "DOWNTO") ;; Convert to TO
                   (when (loop=? subkey "DOWNTTO") (setq negative? t))
                   (pop (loops-exps l))
                   (push 'to (loops-exps l)))
                  ((loop=? subkey "FROM" "DOWNFROM" "UPFROM")
                   (when (loop=? subkey "DOWNFROM") (setq negative? t))
                   (pop (loops-exps l))
                   (setq start (cvt-exp (pop (loops-exps l)))))
                  ((loop=? subkey "BY")
                   (pop (loops-exps l))
                   (setf by (list subkey (cvt-exp (pop (loops-exps l))))))
                  (t (RETURN)))))
    ;; A bit tricky here: Lisp's BY clause are always positive,
    ;; Dylan's are negative for decrement.  E.g., we want
    ;; (loop for x downfrom 10 to 0 by 2) => for(x from 10 to 0 by -2)
    ;; (loop for x downfrom 10 to 0)      => for(x from 10 to 0 by -1)
    ;; (loop for x downfrom 10 above 0)   => for(x from 10 above 0)
    (when negative?
      (cond (by (setf (second by) `(-- ,(second by))))
            ((null to) (setf by '(by -1)))
            ((loop=? (first to) "ABOVE") 'ignore)
            (t (setf by '(by -1)))))
    (push `(:for-clause ,var from ,start ,@to ,@by) (loops-fors l))))

(def-loop repeat (l times)
  "(LOOP REPEAT n ...) does loop body n times" 
  (push `(:for-clause _ from 1 to ,(cvt-exp times)) (loops-fors l)))

;;;; Loop Clauses 26.7 End-Test Control 

(def-loop while (l test) 
  (push `(:for-clause :while ,(cvt-exp test)) (loops-fors l)))

(def-loop until (l test) 
  (push `(:for-clause :until ,(cvt-exp test)) (loops-fors l)))

(def-loop always (l test)
  (setf (loops-result l) 't)
  (add-body l `(if (not ,test) (return 'nil))))

(def-loop never (l test)
  (setf (loops-result l) 't)
  (add-body l `(if ,test (return 'nil))))

(def-loop thereis (l test) 
  (setf (loops-result l) 'nil)
  (add-var l '_ nil)
  (add-body l `(if (setq _ ,test (return _)))))

;;;; Loop Clauses 26.8 Value Accumulation

(def-loop (collect collecting) (l exp)
  (accumulate l exp '(make <deque>) '(push-last INTO VAL)))

(def-loop (nconc nconcing) (l exp) 
  (accumulate l exp '|()| '(setq INTO (nconc INTO VAL))))
(def-loop (append appending) (l exp) 
  (accumulate l exp '|()| '(setq INTO (append INTO VAL))))

(def-loop (count counting) (l exp) (accumulate l exp 0 '(if VAL (incf INTO))))

(def-loop (sum summing) (l exp)  (accumulate l exp 0 '(incf INTO VAL)))

(def-loop (maximize maximizing) (l exp) 
  (accumulate l exp 'nil '(setq INTO (if INTO (max INTO VAL) VAL))))
(def-loop (minimize minimizing) (l exp)  
  (accumulate l exp 'nil '(setq INTO (if INTO (min INTO VAL) VAL))))

(defun accumulate (l val init form)
  (let ((into '_acc))
    (when (parse-loop-key l "INTO")
      (setq into (pop (loops-exps l))))
    (when (null (loops-result l))
      (setf (loops-result l) into))
    (add-var l into init)
    (add-body l (sublis `((INTO . ,into) (VAL . ,val)) form))))

;;;; 26.9. Variable Initializations

(def-loop with (l var)
  (let ((vars nil) (vals nil))
    (push var (loops-exps l))
    (loop 
        (push (parse-var l) vars)
      (push (if (parse-loop-key l "=") (pop (loops-exps l)) '|\#f|) vals)
      (unless (parse-loop-key l "AND") (RETURN)))
    (cond ((= (length vars) 1)
           (add-var l (first/ vars) (first/ vals)))
          (t (add-var l `(:args ,@vars) `(values ,@vals))))))

(defun parse-var (l &optional given-var)
  "Parse and return var [type-spec]" ; See CLtL2 p. 743
  (let ((var (or given-var 
                 (if (symbolp (first/ (loops-exps l))) (pop (loops-exps l))))))
    (if (or (parse-loop-key l "OF-TYPE")
            (every #'numeric-type? (mklist (first (loops-exps l)))))
        `(|::| ,var ,(cvt-type (pop (loops-exps l))))
      var)))

(defun numeric-type? (x) 
  (member (strip x) '(fixnum float t nil)))

;;;; 26.10 Conditional Execution

;;; This is a little tricky.  We keep a stack of conditionals in each
;;; loop structure, and make sure that add-body puts code into these
;;; when appropriate.

(defun add-body (l exp) 
  (if (loops-conditionals l)
      (let ((target  (first (loops-conditionals l))))
        ;; Target is of form (if ... (progn ...)),
        ;; or (if ... (progn ...) (progn ...)) within an ELSE.
        ;; So we NCONC onto the last (progn ...)
        (nconc1 (last1 target) exp))
    (push exp (loops-body l))))

(def-loop (when if unless) (l test key) 
  ;; WHEN expr clauses [ELSE clauses] [END]
  ;; clauses -> clause {AND clause}*
  (let ((target (list 'if 
                      (if (loop=? key "UNLESS") `(not ,test) test)
                      (list 'progn))))
    (add-body l target)
    (push target (loops-conditionals l))
    (parse-clauses l)
    (when (parse-loop-key l "ELSE")
      (setf (first (loops-conditionals l))
            (nconc1 (first (loops-conditionals l)) (list 'progn)))
      (parse-clauses l))
    (pop (loops-conditionals l))
    (parse-loop-key l "END")))   

(defun parse-clauses (l)
  ;; Conditional clauses are either:
  ;; collect/append/sum/count/minimize/maximize do/return when/unless/else
  ;; But we don't make that restriction; we parse any clause.
  (parse-clause l)
  (when (parse-loop-key l "AND") (parse-clauses l)))

(defun maybe-set-it (test exps)
  "Return value, but if the variable IT appears in exps,
  then return code that sets IT to value."
  (if (find-anywhere 'it exps)
      `(setq it ,test)
      test))

;;;; 26.11 Unconditional Execution

(def-loop (do doing) (l exp)
  (add-body l exp)
  (loop (if (symbolp (first/ (loops-exps l))) (RETURN))
        (add-body l (pop (loops-exps l)))))

(def-loop return (l exp) 
  (add-body l `(return ,exp)))

;;;; 26.12 Miscellaneous Features

(def-loop initially (l exp)
  (push exp (loops-prologue l))
  (loop (if (symbolp (first/ (loops-exps l))) (RETURN))
        (push (pop (loops-exps l)) (loops-prologue l))))

(def-loop finally (l exp)
  (push exp (loops-epilogue l))
  (loop (if (symbolp (first/ (loops-exps l))) (RETURN))
        (push (pop (loops-exps l)) (loops-epilogue l))))

(def-loop named (l exp) (setf (loops-name l) exp))
