;;; -*- Mode: lisp; Syntax: common-lisp; -*- Author: Peter Norvig
;;;     File: dpp.lisp; Date: 29-Aug-95 
(in-package :ltd)

;;;; DPP: DYLAN PRETTY-PRINTER

;;; Use (dpp-exp x) to write x as Dylan.
;;; Example:
;;;   (dpp-exp '(define-method m (:args x y) (+ (^ x 2) (^ y 2)))
;;;            :right-margin 30 :miser-width 0)
;;; =>
;;;   define method m (x,y)
;;;     x ^ 2 + y ^ 2;
;;;   end method m

;;;; SPECIAL VARIABLES

(defparameter *dylan-pp-dispatch* (copy-pprint-dispatch))
(defparameter *precedence* 0 "Precedence of last operator.")
(defparameter *in-literal* nil "Are we in the process of printing a literal?")
(defparameter *dotted-functions* nil "Functions that print as object.fn")

;;;; MAIN FUNCTION

(defun dpp-exp (x &rest keys)
  "Pretty print (as Dylan code) x, which is in prefix pseudo-Dylan."
  (let ((*precedence* 0))
    (apply #'write x :pretty t #+LispWorks :pprint 
           #-Lispworks :pprint-dispatch *dylan-pp-dispatch* keys)
    (values)))

;;;; MACRO FOR DEFINING PRETTY-PRINT DISPATCH ROUTINES

(defmacro dpp (type code &key (priority 0))
  "Define a dylan pretty-printer method for a given type."
  ;; If TYPE is of the form 'type, define dispatcher for that type.
  ;; If TYPE is of the form (symbol), define for cons starting with symbol.
  ;; CODE can reference S (the stream) and X (the object to print),
  ;; Or it can be a function that gets passed S and X.
  (let ((value (if (starts-with code 'function) `',(second/ code)
                   `#'(lambda (s x) ,code))))
    (if (and (starts-with type 'quote) (= (length type) 2))
        `(set-pprint-dispatch ,type ,value ,priority *dylan-pp-dispatch*)
        `(setf (get ',(first type) 'dpp) ,value))))


;;;; PRETTY-PRINT DISPATCH TABLES

(dpp 'cons            #'dpp-cons)
(dpp 'symbol          #'dpp-symbol)
(dpp 'com             #'dpp-comment)
(dpp 'atom            #'dpp-literal :priority -1)

(dpp (:args)          (dpp-args s (rest/ x)))
(dpp (:args-bare)     (dpp-args s (rest/ x) "" ""))
(dpp (:body)          #'dpp-body)
(dpp (:body-bare)     (dpp-body s `(:body (nil) ,@(rest/ x)) nil))
(dpp (:branch)        #'dpp-branch)
(dpp (:clause)        #'dpp-clause)
(dpp (:cleanup)       #'dpp-unindented)
(dpp (:else)          #'dpp-unindented)
(dpp (:elseif)        #'dpp-unindented1)
(dpp (:exception)     #'dpp-unindented1)
(dpp (:finally)       #'dpp-unindented)
(dpp (:for-clause)    #'dpp-for-clause)
(dpp (:keyword)       (dpp-keyword-with-colon s (second/ x)))
(dpp (:list)          (dpp-list s (rest/ x)))
(dpp (:list-bare)     (dpp-list s (rest/ x) "" ""))
(dpp (:local-method)  #'dpp-local-method)
(dpp (:return)        #'dpp-return)
(dpp (:slot)          (format s "~@<slot ~W~:>" `(:args-bare ,@(rest/ x))))
(dpp (:slot-keyword)  #'dpp-slot-keyword)

(dpp (aref)           (format s "~W[~:I~{~W~^, ~_~}]" (second/ x) (nthcdr 2 x)))
(dpp (begin)          (format s "~@<begin~W~:>" `(:body (end) ,@(rest/ x))))
(dpp (block)          #'dpp-conditional)
(dpp (case)           (format s "~@<case~W~:>" `(:body (end case) ,@(rest/ x))))
(dpp (define-class)   (dpp-define-method s x "class"))
(dpp (define-constant)(dpp-define-variable s x "constant"))
(dpp (define-generic) #'dpp-define-generic)
(dpp (define-method)  #'dpp-define-method)
(dpp (define-function)(dpp-define-method s x "function"))
(dpp (define-variable)#'dpp-define-variable)
(dpp (define-module)  #'dpp-define-module)
(dpp (element)        #'dpp-element)
(dpp (dynamic-bind)   #'dpp-conditional)
(dpp (for)            #'dpp-conditional)
(dpp (if)             #'dpp-conditional)
(dpp (let)            #'dpp-let)
(dpp (method)         #'dpp-method)
(dpp (let-handler)    (dpp-let s x "let handler"))
(dpp (local)          (format s "local~W" `(:body-bare ,@(rest/ x))))
(dpp (quote)          (dpp-literal s (second/ x)))
(dpp (select)         #'dpp-conditional)
(dpp (unless)         #'dpp-conditional)
(dpp (until)          #'dpp-conditional)
(dpp (while)          #'dpp-conditional)
(dpp (with-open-file) #'dpp-conditional)

;;;; PRETTY-PRINTERS FOR SYNTACTIC COMPONENTS

;;; (Listed in roughly the order they appear in the table above.)

(defun dpp-cons (s x)
  "Pretty-print an x that is a cons of any kind."
  (let* ((fn (first/ x))
         (dispatch (if (symbolp fn) (get fn 'dpp))))
    ;; There are 6 possibilities:
    (cond (*in-literal* (dpp-literal s x))  ; e.g. #(1, 2)
          (dispatch (funcall dispatch s x)) ; e.g. if (a) b; else c; end
          ((unary? x) (dpp-unary s x))      ; e.g. - x
          ((binary? x) (dpp-binary s x))    ; e.g. x + y
          ((dot-notation-call? x)           ; e.g. object.slot
           (dpp-binary s `(|.| ,(second/ x) ,(first/ x))))
          (t (dpp-call s x))                ; e.g. f(x, y)
          )))

(defun dpp-call (s x)
  ;; Print a function call in normal notation, e.g., f(x, y)
  (pprint-logical-block (s nil)
    (destructuring-bind (fn . args) x
      (write (if (consp (strip fn)) `(:list ,fn) fn) :stream s)
      (if (get-option :space-in-call) (write-string " " s))
      (dpp-args s args))))

(defun dpp-symbol (s x)
  ;; Symbols print as names, e.g. sym or \+, unless we are in a literal.
  ;; They print as #"sym" in a literal.
  ;; Keywords print as literal, e.g., #"key", except in dpp-keyword-with-colon.
  (let ((str (dylan-symbol-string x)))
    (cond ((null *print-escape*) (write-string str s))
          ((or (keywordp x) *in-literal*) (dpp-literal s x))
          ((operator? x) (write-char #\\ s) (write-string str s))
          (t (write-string str s)))))

(defun dylan-symbol-string (x)
  ;; Decide whether to include package
  (let* ((str (string-downcase (symbol-name x)))
         (package (if (and (get-option ':print-package)
                            (not (operator? x))
                            (not (keywordp x)))
                       (string-downcase (package-shortest-name 
                                         (symbol-package x))))))
    (if package (concatenate 'string package "/" str) str)))

(defun dylan-name-string (x)
  ;; Convert symbol x to a legal Dylan name
  (if (member x '(|\#key| |\#rest| |\#all-keys|))
      (symbol-name x)
      (let ((str (nsubstitute-if-not #\% #'dylan-name-char?
                                     (dylan-symbol-string x))))
        (when (not (dylan-name-start-char? (char str 0)))
          (setf (char str 0) #\%))
        str)))

(defun dylan-name-char? (ch)
  ;; Can this char be in a Dylan name?
  (or (dylan-name-start-char? ch) (find ch "~+-?/")))

(defun dylan-name-start-char? (ch)
  ;; Can this character start a Dylan name?
  (or (alpha-char-p ch) (digit-char-p ch) (find ch "!&*<=>|^$%@")))

(defun package-shortest-name (package)
  (let ((name (package-name package)))
    (dolist (nick (package-nicknames package))
      (when (< (length nick) (length name))
        (setf name nick)))
    name))

(defun dpp-literal (s x)
  (typecase x
    (null      (write-string "#()" s))
    (string    (dpp-string s x))
    (character (dpp-string s x #\'))
    (com       (let ((*in-literal* t)) (dpp-comment  s x)))
    (cons      (write-char #\# s)
               (let ((*in-literal* t)) (dpp-list s x)))
    (vector    (write-char #\# s)
               (let ((*in-literal* t)) (dpp-list s (coerce x 'list) "[" "]")))
    (symbol    (write-char #\# s) (dpp-string s (dylan-symbol-string x)))
    (complex   (dpp-exp `(+ ,(realpart x) (* ,(imagpart x) $i)) :stream s))
    ((and rational (not integer))
     (write (if *in-literal* (float x) x) :stream s :pretty nil)) ; Avoid 2/3
    (t         (write x :stream s :pretty nil))))

(defun dpp-string (s string &optional (quote-char #\"))
  (setf string (string string)) ; Coerce it if it is a character
  (cond ((null *print-escape*) (write-string string s)) ; For printing comments
        (t (write-char quote-char s)
           (dotimes (i (length string))
             (let ((ch (char string i)))
               (cond ((eql ch quote-char) (write-char #\\ s) (write-char ch s))
                     ((eql ch #\newline) (write-string "\\n" s))
                     ((eql ch #\tab) (write-string "\\t" s))
                     ((eql ch #\\) (write-string "\\\\" s))
                     ((graphic-char-p ch) (write-char ch s))
                     (t (write-string "\\0" s)
                        (write (char-code ch) :stream s)))))
           (write-char quote-char s))))

(defun dpp-args (s args &optional (prefix "(") (suffix ")"))
  "Given ((+ 0 1) 2 :key 3), print (0 + 1, 2, key: 3)"
  ;; We could use dpp-list if it weren't for keywords.
  ;; The caller (e.g. dpp-call) should have set up the proper indentation.
  (pprint-logical-block (s nil :prefix prefix :suffix suffix)
    (loop while args do
          (cond ((atom args) (dpp-exp args :stream s) (setq args nil)) ; ???
                ((and (dylan-keyword? (first/ args)) (rest/ args))
                 (dpp-keyword-with-colon s (pop args))
                 (format s " ~W" (pop args)))
                ((and (member (first/ args) '(|\#key| |\#rest| |\#all-keys|))
                      (rest/ args))
                 (format s "~A ~W" (pop args) (pop args)))
                ((dylan-keyword? (first/ args))
                 (dpp-literal s (pop args)))
                (t (write (pop args) :stream s)))
          (when args (format s ", ~:_")) ; a fill-style newline
          )))

(defun dpp-body (s x &optional (newline-first? :linear))
  "Print a body of exps, each followed by a semicolon, maybe terminated by end."
  ;; This does NOT establish a block; it uses the caller's block.
  ;; It DOES insert a conditional newline (or a space) before each exp.
  ;; Use (:body-bare . exps) if you don't want the newline before the first exp.
  ;; Example x = (:body (end method m-name) blah blah)
  (destructuring-bind ((&optional (end t) construct name) &rest body)
      (rest/ x)
    (loop while (consp body) do
          (let* ((exp (pop body))
                 (indent (indentation exp)))
            (when indent (pprint-indent :block indent s))
            (write-char #\space s)
            (if newline-first?
                (pprint-newline newline-first? s)
                (setf newline-first? :linear))
            (write exp :stream s)
            (if body (write-string ";" s))))
    (when end ;; Print some of '; end construct name', depending on options.
      (format s "~A~0I ~_end"
              (if (get-option :semicolon-before-end) ";" ""))
      (when (and construct (member-of-option construct :end-construct))
        (format s " ~A" construct)
        (when (and name (get-option :end-name))
          (format s " ~A" name))))))

(defun dpp-branch (s x)
  (format s "~@<~W~:>" (second/ x))
  (format s "~VI~_ =>~@<~W~:>"  (* 2 (get-option :tab-stop))
          `(:body-bare ,@(nthcdr 2 x))))

(defun dpp-comment (s x)
  (ifd (com-comment x)
       (ecase (get-option :comments)
         (// (pprint-logical-block (s nil :per-line-prefix "// ")
               (write-string (com-comment x) s))
             (pprint-newline :mandatory s))
         (/* (pprint-logical-block (s nil :prefix "/* " :suffix " */")
               (write-string (com-comment x) s)))))
  (write (com-code x) :stream s))

(defun dpp-unindented (s x)
  "Given (:else x y z), print the else at column 0, then a block, no end."
  (format s "~A~W" (first/ x) `(:body (nil) ,@(rest/ x))))

(defun dpp-unindented1 (s x)
  "Given (:elseif p y), print elseif (p) at column 0, then a block, no end."
  (format s "~A (~W)~W" (first/ x) (second/ x) `(:body (nil) ,@(nthcdr 2 x))))

(defun dpp-for-clause (s x)
  ;; The only hitch is to avoid making = print as \=
  (pprint-logical-block (s (rest/ x))
    (let ((args (rest/ x)))
      (loop while args do
            (let ((arg (pop args)))
              (if (symbolp arg)
                  (write-string (dylan-name-string arg) s)
                  (write arg :stream s))
              (if args (write-string " " s)))))))

(defun dpp-keyword-with-colon (s x)
  ;; Print a keyword in the form `key:'
  (write-string (dylan-name-string x) s)
  (if (keywordp x) (write-char #\: s)))

(defun dpp-list (s x &optional (prefix "(") (suffix ")"))
  "Print a list, filled, with optional prefix and suffix."
  ;; Given ((+ 0 1) 2 :key 3), print (0 + 1, 2, #"key", 3)
  (pprint-logical-block (s nil :prefix prefix :suffix suffix)
    ;(format s "~{~W~^, ~:_~}" x)
    (loop while (and (consp x) (consp (rest/ x))) do
          (format s "~W, ~:_" (pop x)))
    ;; If x is of form (exp) or (exp . dot), print exp
    (when (consp x)
      (format s "~W" (pop x)))
    ;; Now x is an atom; print ` . x' if it is non-null
    (when (not (null x))
      (format s "~:_ . ~W" x))))

(defun dpp-local-method (s x)
  (destructuring-bind (name args . body) (rest/ x)
    ;; The ~:I sets the tab stop at the start of the name; used by :return
    (format s "~@<method ~:I~A ~W~W~:>"
            name args `(:body (end method ,name) ,@body))))

(defun dpp-return (s x)
  "Print the return argument/types."
  (let ((args (rest/ x)))
    (format s "=> ~W"
            (if (and (length=1 args) (not (get-option :single-returns-wrapped)))
                (first/ args)
                `(:list ,@args)))))

(defun dpp-slot-keyword (s x)
  "Print the 'keyword x:, init-value: #t' from a define class."
  (destructuring-bind (name . args) (rest/ x)
    (format s "~@<keyword ~W~@[, ~W~]~:>"
            `(:keyword ,name) (if args `(:args-bare ,@args)))))

(defun dpp-conditional (s x)
  "Print an if, unless, select, until, while, for, or block expression."
  (destructuring-bind (construct test . body) x
    (format s "~@<~A (~W)~W~:>" construct test
            `(:body (end ,construct) ,@body))))

(defun dpp-define-method (s x &optional (keyword 'method))
  ;; This is also used for define-class, since they have the same structure.
  (destructuring-bind (name parms . body) (rest/ x)
    ;; The ~:I sets the tab stop at the start of the name; used by :return
    (format s "~@<define ~A ~:I~A ~W~W~:>"
            keyword name parms `(:body (end ,keyword ,name) ,@body))))

(defun dpp-clause (s x)
  "E.g. (:clause export a b c) prints export a, b, c"
  (destructuring-bind (header . names) (rest/ x)
    (format s "~@<~W~{ ~W~^,~}~:>" header names)))

(defun dpp-define-variable (s x &optional (keyword "variable"))
  "Used for define {variable,constant}."
  (format s "~@<define ~A ~W =~VI ~_~W~:>"
          keyword (second/ x) (get-option :tab-stop) (third x)))

(defun dpp-define-module (s x)
  (destructuring-bind (name . clauses) (rest/ x)
    (format s "~@<define module ~W~W~:>" 
            name `(:body (end module ,name) ,@clauses))))

(defun dpp-define-generic (s x)
  (format s "~@<define generic ~A ~W ~_~W~:>"
          (second/ x) (third x) `(:args-bare ,@(nthcdr 3 x))))

(defun dpp-element (s x)
  (cond ((= (length x) 3) (format s "~W" `(aref ,@(rest/ x))))
        (t (dpp-call s x))))

(defun dpp-let (s x &optional (keywords "let"))
  ;; Note that this is a let binding for a single val.
  ;; It fits within an existing body.
  (destructuring-bind (var val . body) (rest/ x)
    (format s "~A ~<~W ~_= ~W;~:>~W"
            keywords (list var val) `(:body (nil) ,@body))))
    
(defun dpp-method (s x)
  "Print a method (x) ... expression."
  (destructuring-bind (args . body) (rest/ x)
    (format s "~@<method ~W~W~:>" args `(:body (end method) ,@body))))

;;;; HANDLING INFIX OPERATORS

(defparameter *unary*
  '((- 7) (~ 7))
  "List of unary operators and their precedence.")

(defparameter *binary*
  '((|.| 8) (|::| 8) 
    (^ 6)
    (* 5) (/ 5)
    (+ 4) (- 4)
    (= 3) (== 3) (~= 3) (< 3) (> 3) (<= 3) (>= 3)
    (& 2) (\| 2)
    (:= 1) (|:=| 1))
  "List of binary operators and their precedence.")

(defun unary? (x)
  (and (consp x) (length=1 (args x)) (assoc (op x) *unary*)))

(defun binary? (x)
  (and (consp x) (consp (args x)) (length=1 (rest/ (args x))) (assoc (op x) *binary*)))

(defun operator? (symbol)
  (or (assoc symbol *unary*) (assoc symbol *binary*)))

(defun dpp-unary (s list)
  (let* ((prec (second/ (unary? list)))
         (nest (<= prec *precedence*))
         (*precedence* prec))
    (format s "~A~A ~W~A"
            (if nest "(" "") (first/ list) (second/ list) (if nest ")" ""))))

(defun dpp-binary (s list)
  (let* ((prec (second/ (binary? list)))
         (nest (<= prec *precedence*)))
    (destructuring-bind (op x y) list
      (pprint-logical-block
          (s nil
             :prefix (if nest "(" "")
             :suffix (if nest ")" ""))
        (let ((*precedence* (- prec 1)))
          (write x :stream s))
        (format s (case op (|.| "~_.") (:= "~_ := ") (t "~_ ~A ")) op)
        (let ((*precedence* prec))
          (write y :stream s))))))

;;;; AUXILLIARY FUNCTIONS

(defun unindented? (x)
  "Is this an expression that should be printed unindented?"
  (and (consp x) (symbolp (first/ x))
       (member (get (first/ x) 'dpp) '(dpp-unindented dpp-unindented1))))

(defun indentation (exp)
  (cond ((unindented? exp) 0)
        ((starts-with exp ':local-method)
         (+ #.(length "local ") (get-option :tab-stop)))
        ((starts-with exp ':return) nil)
        (t (get-option :tab-stop))))

(defun dot-notation-call? (x)
  "Is x of suitable form for dot notation, e.g. (SLOT VAR)."
  (and (consp x) (symbolp (first/ x)) (length=1 (args x)) 
       (dot-function? (first/ x))
       (or (atom (second/ x)) (dot-notation-call? (second/ x)))))

(defun dot-function? (fn-name)
  ;; Should this function, when called, be printed in dot notation?
  ;; Note that :prefer-dot-notation and :undotted-functions are relatively
  ;; static user-defined options, while *dotted-functions* changes dynamically,
  ;; based on defstructs and defclasses.
  (if (get-option :prefer-dot-notation)
      (not (member-of-option fn-name :undotted-functions))
      (member fn-name *dotted-functions*)))

(defun dylan-keyword? (symbol)
  "True of keywords and #rest, #key, #next, #all-keys."
  (or (keywordp symbol) (dylan-method-keyword? symbol)))

(defun dylan-method-keyword? (symbol)
   "True of #rest, #key, #next, #all-keys."
  (member symbol '(|\#rest| |\#key| |\#next| |\#all-keys|)))
