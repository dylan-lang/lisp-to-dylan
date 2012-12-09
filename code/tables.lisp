;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*- Author: Peter Norvig
;;;     File: tables.lisp; Date: 31-Aug-95
(in-package :ltd)

;;;; CLtL2 CONSTANTS

(ltd-constant |()|              (get-option :empty-as))
(ltd-constant nil               (get-option :nil-as))
(ltd-constant |\#f|             |\#f|)
(ltd-constant t                 |\#t|)

;;;; CLtL2 CH 2: DATA TYPES

(ltd-type adustable-array       <stretchy-vector>)
(ltd-type array                 <array>)
(ltd-type atom                  <atom>)
(ltd-type base-char             <character>)
(ltd-type base-string           <string>)
(ltd-type bignum                <integer>)
(ltd-type bit                   (limited <integer> :from 0 :to 1))
(ltd-type built-in-class        <class>)
(ltd-type character             <character>)
(ltd-type class                 <class>)
(ltd-type compiled-function     <function>)
(ltd-type complex               <complex>)
(ltd-type condition             <condition>)
(ltd-type cons                  <pair>)
(ltd-type double-float          <double-float>)
(ltd-type end-of-file           <end-of-stream-error>)
(ltd-type extended-char         <character>)
(ltd-type error                 <error>)
(ltd-type fixnum                <integer>)
(ltd-type file-error            <file-error>)
(ltd-type float                 <float>)
(ltd-type function              <function>)
(ltd-type generic-function      <function>)
(ltd-type hash-table            <table>)
(ltd-type integer               <integer>)
(ltd-type keyword               <symbol>)
(ltd-type lambda-expression     <function>)
(ltd-type list                  <list>)
(ltd-type long-float            <double-float>)
(ltd-type method                <method>)
(ltd-type null                  (singleton |\#f|))
(ltd-type number                <number>)
(ltd-type rational              <rational>)
(ltd-type real                  <real>)
(ltd-type sequence              <sequence>)
(ltd-type serious-condition     <serious-condition>)
(ltd-type short-float           <short-float>)
(ltd-type simple-error          <simple-error>)
(ltd-type simple-string         <simple-string>)
(ltd-type simple-vector         <simple-vector>)
(ltd-type simple-warning        <simple-warning>)
(ltd-type single-float          <short-float>)
(ltd-type standard-class        <class>)
(ltd-type standard-generic-function <generic-function>)
(ltd-type standard-method       <method>)
(ltd-type standard-object       <object>)
(ltd-type stream                <stream>)
(ltd-type file-stream           <file-stream>)
(ltd-type string-stream         <string-stream>)
(ltd-type string                <string>)
(ltd-type symbol                <symbol>)
(ltd-type t                     <object>)
(ltd-type type-error            <type-error>)
(ltd-type vector                <vector>)
(ltd-type warning               <warning>)

(ltd-unimplemented-types
 bit-vector package pathname random-state ratio readtable structure)

;;;; CLtL2 CH 4: TYPE SPECIFIERS

(ltd-fn  deftype                 #'cvt-deftype)
(ltd-fn (coerce x type)          `(as ,type ,x))
(ltd-fn  type-of                 object-class)

;;;; CLtL2 CH 5: PROGRAM STRUCTURE

(ltd-fn  defun                   #'cvt-defun)
(ltd-fn  lambda                  #'cvt-lambda)
(ltd-fn (defvar name &opt x d)   (add-comment d `(define-variable ,name ,x)))
(ltd-fn (defparameter name &opt x d) (add-comment d `(define-variable ,name ,x)))
(ltd-fn (defconstant name &opt x d) (add-comment d `(define-constant ,name ,x)))
(ltd-fn (eval-when ignore . body)  (maybe-begin body))

;;;; CLtL2 CH 6: PREDICATES

(ltd-fn (typep x type)           `(instance? ,x ,type))
(ltd-fn (subtypep type class)    `(subclass? ,type ,class))
(ltd-fn  null                    empty?) 
(ltd-fn (symbolp x)              `(instance? ,x <symbol>))
(ltd-fn (atom x)                 `(not (instance? ,x <list>)))
(ltd-fn (consp x)                `(instance? ,x <pair>))
(ltd-fn (listp x)                `(instance? ,x <list>))
(ltd-fn (numberp x)              `(instance? ,x <number>))
(ltd-fn (integerp x)             `(instance? ,x <integer>))
(ltd-fn (rationalp x)            `(instance? ,x <rational>))
(ltd-fn (floatp x)               `(instance? ,x <float>))
(ltd-fn (realp x)                `(instance? ,x <real>))
(ltd-fn (complexp x)             `(instance? ,x <complex>))
(ltd-fn (characterp x)           `(instance? ,x <character>))
(ltd-fn (stringp x)              `(instance? ,x <string>))
(ltd-fn (vectorp x)              `(instance? ,x <vector>))
(ltd-fn (simple-vectorp x)       `(instance? ,x <simple-vector>))
(ltd-fn (simple-stringp x)       `(instance? ,x <simple-string>))
(ltd-fn (arrayp x)               `(instance? ,x <array>))
(ltd-fn (functionp x)            `(instance? ,x <function>))
(ltd-fn (adjustable-array-p x)   `(instance? ,x <stretchy-vector>))
(ltd-fn (compiled-function-p x)  `(instance? ,x <function>))
(ltd-fn (hash-table-p x)         `(instance? ,x <table>))
(ltd-fn (sequencep x)            `(instance? ,x <sequence>))
(ltd-fn  eq                      ==)
(ltd-fn  eql                     ==)
(ltd-fn  equal                   =)
(ltd-fn  equalp                  =) ; ?? not quite right
(ltd-fn  not                     ~)
(ltd-fn (and . asis)             (cvt-to-binary `(& ,@asis)))
(ltd-fn (or . asis)              (cvt-to-binary `(\| ,@asis)))

;;;; CLtL2 CH 7: CONTROL STRUCTURE

(ltd-fn  quote                   #'identity)
(ltd-fn  function                #'cvt-fn)
(ltd-fn (symbol-value x)         (identity x)) ;; Not quite right
(ltd-fn (symbol-function f)      (identity f)) ;; Not quite right
(ltd-fn  setq                    #'cvt-setf)
(ltd-fn  psetq                   #'cvt-macro)
(ltd-fn  setf                    #'cvt-setf)
(ltd-fn  psetf                   #'cvt-macro)
(ltd-fn  shiftf                  #'cvt-macro)
(ltd-fn  rotatef                 #'cvt-macro)
(ltd-fn (apply f . arg*)         `(apply ,f . ,arg*))
(ltd-fn (funcall f . arg*)       `(,f . ,arg*))
(ltd-fn (progn . body)           `(begin . ,body))
(ltd-fn (prog1 x . x*)           `(begin (let _ ,x ,@x* _)))
(ltd-fn (prog2 x y . x*)         `(begin ,x (let _ ,y ,@x* _)))
(ltd-fn  let                     #'cvt-let)
(ltd-fn  let*                    #'cvt-let*)
(ltd-fn  compiler-let            #'cvt-compiler-let)
(ltd-fn  flet                    #'cvt-flet)
(ltd-fn  labels                  #'cvt-labels)
(ltd-fn  symbol-macrolet         #'cvt-symbol-macrolet)
(ltd-fn  if                      #'cvt-if)
(ltd-fn (when x . body)          `(,(get-option :when-as) ,x ,@body))
(ltd-fn (unless x . body)        (if (eq (get-option :unless-as) 'if)
                                     `(if (~ ,x) ,@body) `(unless ,x . ,body)))
(ltd-fn  cond                    #'cvt-cond)
(ltd-fn  case                    #'cvt-case)
(ltd-fn  typecase                #'cvt-typecase)
(ltd-fn (block name . body)      (handle-returns1 (maybe-begin body) name))
(ltd-fn  return-from             #'cvt-return-from)
(ltd-fn (return &opt x)          (if (starts-with x 'values) `(return ,@(rest/ x))
                                   `(return ,x)))
(ltd-fn  loop                    #'cvt-loop)
(ltd-fn  do                      #'cvt-do)
(ltd-fn  do*                     #'cvt-do)
(ltd-fn  dolist                  #'cvt-dolist)
(ltd-fn  dotimes                 #'cvt-dotimes)
(ltd-fn (mapcar f list . list*)  `(map ,f ,list . ,list*))
(ltd-fn (mapc f list . list*)    (once list `(begin (do ,f ,list . ,list*)
                                                    ,(strip list))))
(ltd-fn (mapcan f . list*)       `(apply concatenate! (map ,f . ,list*)))
(ltd-fn (mapcon . asis)          `(apply concatenate! ,(cvt-exp `(maplist . ,asis))))
(ltd-fn  tagbody                 #'cvt-tagbody)
(ltd-fn  prog                    #'cvt-macro)
(ltd-fn  prog*                   #'cvt-macro)
(ltd-fn (go name)                `(,(mksymbol 'go- name)))
(ltd-fn  values                  values)
(ltd-fn (values-list v)          `(apply values ,v))
(ltd-fn (multiple-value-list x)  `(let (:args |\#rest| _) ,x _))
(ltd-fn  multiple-value-call     #'cvt-multiple-value-call)
(ltd-fn (multiple-value-prog1 x . x*) `(let (:args |\#rest| _) ,x ,@x*
                                         (apply values _)))
(ltd-fn  multiple-value-bind     #'cvt-multiple-value-bind)
(ltd-fn  multiple-value-setq     #'cvt-multiple-value-setq)
(ltd-fn (nth-value n x)          `(element (begin (let (:args |\#rest| _) ,x _)) ,n))
(ltd-fn (catch name . body)      `(block ,(cvt-tag name) . ,body))
(ltd-fn (unwind-protect x . body) `(block nil ,x (:cleanup ,@body)))
(ltd-fn (throw name x)           `(,(cvt-tag name) ,x))

;;;; CLtL2 CH 8: MACROS

(ltd-fn (defmacro name . ignore) (progn (incf-unimplemented 'defmacro)
                                   (cvt-erroneous exp `',name "No macros.")))
(ltd-fn (defsetf name . ignore)  (progn (incf-unimplemented 'defsetf)
                                   (cvt-erroneous exp `',name "No setf macros.")))
(ltd-fn  destructuring-bind      #'cvt-macro) ; Note no &optional in Dylan

;;;; CLtL2 CH 9: DECLARATIONS

(ltd-fn  declare                 '|\#f|)
(ltd-fn (locally . body)         (maybe-begin body))
(ltd-fn  proclaim                '|\#f|)
(ltd-fn  declaim                 '|\#f|)
(ltd-fn (the type x)             (progn type x)) 

;;;; CLtL2 CH 10: SYMBOLS

(ltd-fn  get                     symbol-get-property)
(ltd-fn  symbol-plist            symbol-plist)
(ltd-fn  remprop                 symbol-remove-property)
(ltd-fn (getf p i &opt d)        `(get-property! ,p ,i ,@(ifd d `(:default ,d))))
(ltd-fn  remf                    remove-property!)
(ltd-fn (symbol-name s)          `(as <string> ,s))
(ltd-fn (make-symbol str)        `(as <symbol> ,str))
(ltd-fn (gensym &opt x)          `(generate-symbol ,@(ifd x `((:string ,x)))))
(ltd-fn (gentemp &opt x ignore)  `(generate-symbol ,@(ifd x `((:string ,x)))))
(ltd-fn (keywordp x)             `(instance? ,x <symbol>)) ; ??? not right
(ltd-fn (intern s)               `(as <symbol> ,s))

;;;; CLtL2 CH 11: PACKAGES

(ltd-fn  in-package              #'cvt-in-package)
(ltd-fn  export                  #'cvt-export)
(ltd-fn  defpackage              #'cvt-defpackage)

;;;; CLtL2 CH 12: NUMBERS

(ltd-fn  zerop                   zero?)
(ltd-fn  plusp                   positive?)
(ltd-fn  minusp                  negative?)
(ltd-fn  oddp                    odd?)
(ltd-fn  evenp                   even?)
(ltd-fn  =                       #'cvt-to-binary-compares)
(ltd-fn (/= . arg*)              (cvt-to-binary-compares `(~= . ,arg*)))
(ltd-fn  <                       #'cvt-to-binary-compares)
(ltd-fn  >                       #'cvt-to-binary-compares)
(ltd-fn  <=                      #'cvt-to-binary-compares) 
(ltd-fn  >=                      #'cvt-to-binary-compares) 
(ltd-fn  max                     max)
(ltd-fn  min                     min)
(ltd-fn  +                       #'cvt-to-binary)
(ltd-fn  -                       #'cvt-to-binary)
(ltd-fn  *                       #'cvt-to-binary)
(ltd-fn  /                       #'cvt-to-binary)
(ltd-fn (1+ x)                   `(+ ,x 1))
(ltd-fn (1- x)                   `(- ,x 1))
(ltd-fn  incf                    inc!)
(ltd-fn  decf                    dec!)
(ltd-fn  gcd                     gcd)
(ltd-fn  lcm                     lcm)
(ltd-fn  exp                     exp)
(ltd-fn  expt                    ^)
(ltd-fn  log                     log)
(ltd-fn  sqrt                    sqrt)
(ltd-fn (isqrt x)                `(truncate (sqrt ,x)))
(ltd-fn  abs                     abs)
(ltd-fn (signum x)               (once x `(if (> ,x 0) 1 (:elseif (< ,x 0) -1)
                                              (:else 0))))
(ltd-fn  sin                     sin)
(ltd-fn  cos                     cos)
(ltd-fn  tan                     tan)
(ltd-fn  asin                    asin)
(ltd-fn  acos                    acos)
(ltd-fn  atan                    atan)
(ltd-fn  sinh                    sinh)
(ltd-fn  cosh                    cosh)
(ltd-fn  tanh                    tanh)
(ltd-fn  asinh                   asinh)
(ltd-fn  acosh                   acosh)
(ltd-fn  atanh                   atanh)
(ltd-fn (float x &opt y)         `(as ,(ifd y `(class-of ,y) '<float>) ,x))
(ltd-fn (rational x)             `(as <rational> ,x))
(ltd-fn  rationalize             rationalize)
(ltd-fn  numerator               numerator)
(ltd-fn  denominator             denominator)
(ltd-fn  floor                   #'cvt-division)
(ltd-fn  ceiling                 #'cvt-division)
(ltd-fn  truncate                #'cvt-division)
(ltd-fn  round                   #'cvt-division)
(ltd-fn  mod                     modulo)
(ltd-fn  rem                     remainder)
(ltd-fn (ffloor . asis)          `(as <float> ,(cvt-exp `(floor . ,asis))))
(ltd-fn (fceiling . asis)        `(as <float> ,(cvt-exp `(ceiling . ,asis))))
(ltd-fn (ftruncate . asis)       `(as <float> ,(cvt-exp `(truncate . ,asis))))
(ltd-fn (fround . asis)          `(as <float> ,(cvt-exp `(round . ,asis))))
(ltd-fn  realpart                real-part)
(ltd-fn  imagpart                imag-part)
(ltd-fn  logior                  logior)
(ltd-fn  logxor                  logxor)
(ltd-fn  logand                  logand)
(ltd-fn  logeqv                  logeqv)
(ltd-fn  lognot                  lognot)
(ltd-fn  logbitp                 logbit?)
(ltd-fn  ash                     ash)
(ltd-fn  (random &opt x)         `(random-uniform :to ,(or x most-positive-fixnum)))

;;;; CLtL2 CH 13: CHARACTERS

(ltd-fn  standard-char-p         standard-char?)
(ltd-fn  graphic-char-p          graphic-char?)
(ltd-fn  alpha-char-p            alpha-char?)
(ltd-fn  upper-case-p            upper-case?)
(ltd-fn  lower-case-p            lower-case?)
(ltd-fn  both-case-p             both-case?)
(ltd-fn  digit-char-p            digit-char?)
(ltd-fn  alphanumericp           alphanumeric?)
(ltd-fn  char=                   =)
(ltd-fn  char/=                  /=)
(ltd-fn  char<                   <)
(ltd-fn  char>                   >)
(ltd-fn  char<=                  <=)
(ltd-fn  char>=                  >=)
(ltd-fn  char-equal              char-equal?)
(ltd-fn  char-not-equal          char-not-equal?)
(ltd-fn  char-lessp              char-less?)
(ltd-fn  char-greaterp           char-greater?)
(ltd-fn  char-not-greaterp       char-not-greater?)
(ltd-fn  char-not-lessp          char-not-less?)
(ltd-fn (char-code c)            `(as <integer> ,c))
(ltd-fn (code-char i)            `(as <character> ,i))
(ltd-fn (character x)            `(as <character> ,x))
(ltd-fn  char-upcase             as-uppercase)
(ltd-fn  char-downcase           as-lowercase)
(ltd-fn  digit-char              digit-char)
(ltd-fn (char-int c)             `(as <integer> ,c))
(ltd-fn (int-char i)             `(as <character> ,i))

;;;; CLtL2 CH 14: SEQUENCES

(ltd-fn  elt                     element)
(ltd-fn  subseq                  copy-sequence)
(ltd-fn  copy-seq                copy-sequence)
(ltd-fn  length                  size)
(ltd-fn  reverse                 reverse)
(ltd-fn  nreverse                reverse!)
(ltd-fn (make-sequence type n . key*) `(make ,type :size ,n :fill
                                             ,(getf key* :initial-element '|\#f|)))
(ltd-fn (concatenate type . arg*)`(concatenate-as ,type . ,arg*))
(ltd-fn (map type f . s*)        `(map-as ,type ,f . ,s*))
(ltd-fn (map-into r f . s*)      `(map-into ,r ,f . ,s*))
(ltd-fn (some f . x*)            `(any? ,f . ,x*))
(ltd-fn (every f . x*)           `(every? ,f . ,x*))
(ltd-fn (notany f . x*)          `(not (any? ,f . ,x*)))
(ltd-fn (notevery f . x*)        `(not (every? ,f . ,x*)))
(ltd-fn  reduce                  #'cvt-reduce)
(ltd-fn  fill                    fill!)
(ltd-fn (replace a b . keys)     `(replace-subsequence! ,(mkseq a keys :start1 :end1)
                                                        ,(mkseq b keys :start2 :end2)))
(ltd-fn (delete i s  . keys)     (cl? exp `(remove! ,(mkseq s keys) ,i ,@(mktest keys)
                                                   ,@(mkcount keys))))
(ltd-fn (delete-if pred s . keys)(cl? exp `(choose (complement ,(mkpred keys pred))
                                                   ,(mkseq s keys))))
(ltd-fn  delete-if-not           #'cvt-if-not)
(ltd-fn (remove i s  . keys)     (cl? exp `(remove ,(mkseq s keys) ,i ,@(mktest keys)
                                                   ,@(mkcount keys))))
(ltd-fn (remove-if pred s . keys)(cl? exp `(choose (complement ,(mkpred keys pred))
                                                   ,(mkseq s keys))))
(ltd-fn  remove-if-not           #'cvt-if-not)
(ltd-fn  delete-duplicates       cl-remove-duplicates!)
(ltd-fn  remove-duplicates       cl-remove-duplicates)
(ltd-fn (substitute n o s . keys)(cl? exp `(replace-elements
                                            ,s ,(mkpred keys `(curry == ,o))
                                            (always ,n) ,@(mkcount keys)) '(:from-end)))
(ltd-fn (substitute-if n pred s . keys) (cl? exp `(replace-elements
                                                   ,s ,(mkpred keys pred) (always ,n)
                                                   ,@(mkcount keys)) '(:from-end)))
(ltd-fn  substitute-if-not       #'cvt-if-not)
(ltd-fn (nsubstitute n o s . keys)(cl? exp `(replace-elements!
                                            ,s ,(mkpred keys `(curry == ,o))
                                            (always ,n) ,@(mkcount keys)) '(:from-end)))
(ltd-fn (nsubstitute-if n pred s . keys) (cl? exp `(replace-elements!
                                                   ,s ,(mkpred keys pred) (always ,n)
                                                   ,@(mkcount keys)) '(:from-end)))
(ltd-fn  nsubstitute-if-not       #'cvt-if-not)
(ltd-fn  find                    cl-find)
(ltd-fn  find-if                 cl-find-if)
(ltd-fn  find-if-not             #'cvt-if-not)
(ltd-fn (position i s . keys)    (cl? exp `(find-key ,(mkseq s keys) (curry == ,i))))
(ltd-fn (position-if pred s . keys) (cl? exp `(find-key ,(mkseq s keys) ,(mkpred keys pred))))
(ltd-fn  position-if-not         #'cvt-if-not)
(ltd-fn  count                   cl-count)
(ltd-fn  count-if                cl-count-if)
(ltd-fn  count-if-not            #'cvt-if-not)
(ltd-fn (mismatch a b . keys)    `(cl-mismatch ,a ,b . ,(cvt-exps keys)))
(ltd-fn (search a b . keys)      (cl? exp `(subsequence-position
                                            ,(mkseq a keys :start1 :end1)
                                            ,(mkseq b keys :start2 :end2))))
(ltd-fn (sort s pred . keys)     `(sort! ,s ,@(mktest keys pred 2)))
(ltd-fn (stable-sort s pred . keys) `(sort! ,s  :stable |\#t| ,@(mktest keys pred 2)))
(ltd-fn (merge type a b pred . keys) `(cl-merge ,type ,a ,b ,(mkpred keys pred)))

;;;; CLtL2 CH 15: LISTS

(ltd-fn  car                     head)
(ltd-fn  cdr                     tail)
(ltd-fn  cadr                    second)
(ltd-fn (caar x)                 `(head (head ,x)))
(ltd-fn (cdar x)                 `(tail (head ,x)))
(ltd-fn (cddr x)                 `(tail (tail ,x)))
(ltd-fn  caddr                   third)
(ltd-fn  cons                    pair)
(ltd-fn (endp l)                 `(not (pair? ,l)))
(ltd-fn  list-length             size) 
(ltd-fn (nth i l)                `(element ,l ,i))
(ltd-fn  first                   first)
(ltd-fn  second                  second)
(ltd-fn  third                   third)
(ltd-fn (fourth s)               `(element ,s 3))
(ltd-fn (fifth s)                `(element ,s 4))
(ltd-fn (sixth s)                `(element ,s 5))
(ltd-fn (seventh s)              `(element ,s 6))
(ltd-fn (eighth s)               `(element ,s 7))
(ltd-fn (ninth s)                `(element ,s 8))
(ltd-fn (tenth s)                `(element ,s 9))
(ltd-fn  rest                    tail)
(ltd-fn (nthcdr i l)             `(nth-tail ,l ,i))
(ltd-fn (last s &opt n)          (once s `(copy-sequence 
                                           ,s :start (- (size ,s) ,(ifd n n 1)))))
(ltd-fn  list                    list)
(ltd-fn (list* . arg*)           `(apply list . ,arg*))
(ltd-fn (make-list n . key*)     (let ((init (getf key* :initial-element)))
                                   `(make <list> :size ,n ,@(ifd init `(:fill ,init)))))
(ltd-fn  append                  concatenate)
(ltd-fn  copy-list               copy-sequence)
(ltd-fn (copy-alist a)           `(map (method (:list x) (pair (head x) (tail x))) ,a))
(ltd-fn (revappend x y)          `(concatenate (reverse ,x) ,y))
(ltd-fn  nconc                   concatenate!)
(ltd-fn (nreconc x y)            `(concatenate! (reverse! ,x) ,y))
(ltd-fn  push                    push!)
(ltd-fn  pushnew                 #'cvt-macro)
(ltd-fn  pop                     pop!)
(ltd-fn (butlast l)              (once l `(copy-sequence ,l (- (size ,l) 1))))
(ltd-fn (nbutlast l)             (once l `(copy-sequence ,l (- (size ,l) 1))))
(ltd-fn (rplaca c a)             `(:= (head ,c) ,a))
(ltd-fn (rplacd c d)             `(:= (tail ,c) ,d))
(ltd-fn  subst                   replace-in-tree)
(ltd-fn  nsubst                  replace-in-tree)
(ltd-fn  sublis                  replace-multiple-in-tree)
(ltd-fn  nsublis                 replace-multiple-in-tree)
(ltd-fn (member i l . keys)      `(member? ,i ,l ,@(mktest keys)))
(ltd-fn (member-if pred l . keys)`(any? ,(mkpred keys pred) ,l))
(ltd-fn  member-if-not           #'cvt-if-not)
(ltd-fn (adjoin i l . keys)      `(add! ,i ,l . ,(mktest keys)))
(ltd-fn (union a b . keys)       `(union ,a ,b ,@(mktest keys nil 2)))
(ltd-fn (nunion a b . keys)      `(union ,a ,b ,@(mktest keys nil 2)))
(ltd-fn (intersection a b . keys) `(intersection ,a ,b ,@(mktest keys nil 2)))
(ltd-fn (nintersection a b . keys) `(intersection ,a ,b ,@(mktest keys nil 2)))
(ltd-fn  subsetp                 subset?)
(ltd-fn (set-difference a b . keys) `(set-difference ,a ,b ,@(mktest keys nil 2)))
(ltd-fn (nset-difference a b . keys) `(set-difference ,a ,b ,@(mktest keys nil 2)))
(ltd-fn (acons k d a)            `(cons (cons ,k ,d) ,a))
(ltd-fn (pairlis k d &opt a)     (ifd a `(append! (map cons ,k ,d) ,a)
                                      `(map cons ,k ,d)))
(ltd-fn  assoc                   cl-assoc)
(ltd-fn  assoc-if                cl-assoc-if)
(ltd-fn  assoc-if-not            #'cvt-if-not)

;;;; CLtL2 CH 16: HASH TABLES

(ltd-fn  make-hash-table         #'cvt-make-hash-table)
(ltd-fn (gethash k h &opt d)     `(element ,h ,k ,@(ifd d `(:default ,d))))
(ltd-fn (remhash key tab)        `(remove-key! ,tab ,key))
(ltd-fn (maphash f tab)          (once tab `(do ,f (key-sequence ,tab) ,tab)))
(ltd-fn (clrhash tab)            `(:= (size ,tab) 0))
(ltd-fn  hash-table-count        size)
(ltd-fn  with-hash-table-iterator #'cvt-macro)
(ltd-fn  hash-table-size         size)
(ltd-fn  hash-table-test         key-test)
(ltd-fn  sxhash                  object-hash)

;;;; CLtL2 CH 17: ARRAYS

(ltd-fn  make-array              #'cvt-make-array)
(ltd-fn  vector                  vector)
(ltd-fn  aref                    aref)
(ltd-fn  svref                   element)
(ltd-fn  array-rank              rank)
(ltd-fn  array-dimension         dimension)
(ltd-fn  array-dimensions        dimensions)
(ltd-fn  array-total-size        size)
(ltd-fn  bit                     element)
(ltd-fn  sbit                    element)
(ltd-fn  fill-pointer            size)
(ltd-fn (vector-push e v)        `(add! ,v ,e))
(ltd-fn (vector-push-extend e v) `(add! ,v ,e))
(ltd-fn  vector-pop              pop)

;;;; CLtL2 CH 18: STRINGS

(ltd-fn  char                    element)
(ltd-fn  schar                   element)
(ltd-fn  string=                 =)
(ltd-fn  string-equal            string-equal?)
(ltd-fn  string<                 <)
(ltd-fn  string>                 >)
(ltd-fn  string<=                <=)
(ltd-fn  string>=                >=)
(ltd-fn  string/=                /=)
(ltd-fn  string-lessp            string-less?)
(ltd-fn  string-greaterp         string-greater?)
(ltd-fn  string-not-greaterp     string-not-greater?)
(ltd-fn  string-not-lessp        string-not-less?)
(ltd-fn  string-not-equal        string-not-equal?)
(ltd-fn (make-string n . key*)   `(make <string> :size ,n
                                        :fill ,(getf key* :initial-element #\space)))
(ltd-fn  string-trim             string-trim)
(ltd-fn  string-left-trim        string-left-trim)
(ltd-fn  string-right-trim       string-right-trim)
(ltd-fn (string-upcase s . keys) `(as-uppercase! ,(mkseq s keys)))
(ltd-fn (string-downcase s . keys) `(as-lowercase! ,(mkseq s keys)))
(ltd-fn  string-capitalize       string-capitalize)
(ltd-fn (nstring-upcase s . keys) `(as-uppercase! ,(mkseq s keys)))
(ltd-fn (nstring-downcase s . keys) `(as-lowercase! ,(mkseq s keys)))
(ltd-fn  nstring-capitalize      string-capitalize!)
(ltd-fn (string x)               `(as <string> ,x))

;;;; CLtL2 CH 19: STRUCTURES

(ltd-fn  defstruct               #'cvt-defstruct)

;;;; CLtL2 CH 20: EVALUATOR

(ltd-fn constantp                constant?)

;;;; CLtL2 CH 21: STREAMS

(ltd-fn (streamp x)              `(instance? ,x <stream>))
(ltd-fn  open-stream-p           stream-open?)
(ltd-fn  stream-element-type     stream-element-type)
(ltd-fn (close stream . ignore)  `(close ,stream))

;;;; CLtL2 CH 22: INPUT/OUTPUT

(ltd-fn (read-line &opt stdin . eofs) `(read-line ,stdin ,(mkeof eofs)))
(ltd-fn (read-char &opt stdin . eofs) `(read-element ,stdin ,(mkeof eofs)))
(ltd-fn (unread-char ch &opt stdout)  `(unread-element ,stdout ,ch))
(ltd-fn (peek-char ignore stdin . eofs) `(peek ,stdin . ,(mkeof eofs)))
(ltd-fn (read-byte s . eofs)          `(read-element ,s ,(mkeof eofs)))
;; The following printing is not quite right ???
(ltd-fn (write x . keys)              `(print ,x ,@(extract-stream keys)))
(ltd-fn (prin1 x &opt stdout)         `(print ,x ,stdout))
(ltd-fn (print x &opt stdout)         `(print ,x ,stdout))
(ltd-fn (pprint x &opt stdout)        `(print ,x ,stdout :pretty |\#t|))
(ltd-fn (princ x &opt stdout)         `(print ,x ,stdout))

(ltd-fn (write-char ch  &opt stdout)  `(write-element ,stdout ,ch))
(ltd-fn (write-string s &opt stdout . keys) `(write ,stdout ,(mkseq s keys)))
(ltd-fn (write-line x &opt stdout)    `(print ,x ,stdout))
(ltd-fn (write-byte b  stream)        `(write-element ,stream ,b))
(ltd-fn (terpri &opt stdout)          `(write-element ,stdout #\newline))
(ltd-fn (fresh-line &opt stdout)      `(write-element ,stdout #\newline))
(ltd-fn (finish-output &opt stdout)   `(force-output ,stdout))
(ltd-fn (force-output &opt stdout)    `(force-output ,stdout))
(ltd-fn (clear-output &opt stdout)    `(discard-output ,stdout))
(ltd-fn format                        #'cvt-format)

;;;; CLtL2 CH 23: FILE SYSTEM INTERFACE

(ltd-fn (open fl . k*)           `(make <file-stream> :locator ,fl . ,k*))
(ltd-fn  with-open-file          #'cvt-with-open-file)
(ltd-fn (file-position s &opt p) (ifd p `(:= (stream-position ,s) ,p)
                                      `(stream-position ,s)))

;;;; CLtL2 CH 24: ERRORS

(ltd-fn  break                     break)
(ltd-fn (check-type p type &opt s) (add-comment s `(check-type ,p ,type)))
(ltd-fn (assert x . ignore)        `(assert ,x))
(ltd-fn  etypecase                 #'cvt-macro)
(ltd-fn  ctypecase                 #'cvt-macro)
(ltd-fn  ecase                     #'cvt-ecase)
(ltd-fn  ccase                     #'cvt-macro)

;;;; CLtL2 CH 25: MISCELLANEOUS FEATURES

(ltd-fn (load-time-value x)      (progn x))
(ltd-fn  identity                identity)

;;;; CLtL2 CH 28: COMMON LISP OBJECT SYSTEM

(ltd-fn  add-method              add-method)
(ltd-fn  call-next-method        next-method)
(ltd-fn  class-of                object-class)
(ltd-fn  compute-applicable-methods #'cvt-compute-applicable-methods)
(ltd-fn  defclass                #'cvt-defclass)
(ltd-fn  defgeneric              #'cvt-defgeneric)
(ltd-fn  defmethod               #'cvt-defmethod)
(ltd-fn (find-class type)        (progn type))
(ltd-fn  generic-flet            #'cvt-flet)
(ltd-fn  generic-function        #'cvt-fn)
(ltd-fn  generic-labels          #'cvt-labels)
(ltd-fn  initialize-instance     initialize)
(ltd-fn (make-instance type . x*)`(make ,type . ,x*))
(ltd-fn (next-method-p)          'next-method)
(ltd-fn  slot-boundp             slot-initialized?)
(ltd-fn (slot-value x f)         `(|.| ,x ,f))
(ltd-fn  with-accessors          #'cvt-macro)
(ltd-fn  with-slots              #'cvt-with-slots)

;;;; CLtL2 CH 29: CONDITIONS

(ltd-fn  error                   #'cvt-condition-function)
(ltd-fn  cerror                  #'cvt-condition-function)
(ltd-fn (warn . asis)            (cvt-exp `(format-out ,@asis)))
(ltd-fn  signal                  #'cvt-condition-function)
(ltd-fn (ignore-errors . x*)     `(block nil ,@x* (:exception <error> |\#f|)))
(ltd-fn  define-condition        #'cvt-defclass)
(ltd-fn  make-condition          make)
(ltd-fn (abort &opt e)           (ifd e `(error ,e) `(abort)))

;;;; ANS COMMON LISP SPEC

(ltd-fn  class-direct-subclasses   direct-subclasses)
(ltd-fn  class-direct-superclasses direct-superclasses)
(ltd-fn  class-precedence-list     all-superclasses)
(ltd-fn (complement f)             (if (and (starts-with (strip f) 'complement)
                                            (length=1 (args f)))
                                       (first/ (args f)) `(complement ,f)))
(ltd-fn  constantly                always)
(ltd-fn  function-lambda-list      function-arguments)
(ltd-fn  generic-function-methods  generic-function-methods)
(ltd-fn  method-specializers       function-specializers)
(ltd-fn  ordinary-char-p           ordinary-char?)
(ltd-fn  whitespace-char-p         whitespace-char?)

;;;; UNIMPLEMENTED FUNCTIONS

(ltd-unimplemented-functions
 adjust-array                 get-output-stream-string     pprint-indent 
 applyhook                    get-properties               pprint-linear 
 apropos                      get-setf-method              pprint-newline pprint-pop 
 apropos-list                 get-setf-method-multiple-value pprint-tab 
 arithmetic-error-operands    get-universal-time           pprint-tabular 
 arithmetic-error-operation   handler-bind handler-case     
 array-row-major-index        hash-table-rehash-size       prin1-to-string 
 augment-environment          hash-table-rehash-threshold   
 bit-and                      host-namestring              princ-to-string 
 bit-andc1                    import                        
 bit-andc2                                                 probe-file 
 bit-eqv                      input-stream-p               progv 
 bit-ior                      inspect                      provide 
 bit-nand                     integer-decode-float          
 bit-nor                      integer-length               random-state-p 
 bit-not                      interactive-stream-p         rassoc 
 bit-orc1                     invalid-method-error         rassoc-if 
 bit-orc2                     invoke-debugger              rassoc-if-not 
 bit-xor                      invoke-restart               read 
 boole                        invoke-restart-interactively read-char-no-hang 
 boundp                       ldb                          read-delimited-list 
 broadcast-stream-streams     ldb-test                     read-from-string 
 byte                         ldiff                        read-preserving-whitespace 
 byte-position                lisp-implementation-type     readtable-case 
 byte-size                    lisp-implementation-version  readtablep 
 cell-error-name              list-all-packages 
 char-bit                     listen                       rename-file 
 char-bits                    load                         rename-package 
 char-font                    load-logical-pathname-translations require 
 char-name                    logandc1                     restart-bind 
 cis                          logandc2                     restart-case 
 clear-input                  logcount                     restart-name 
 compile                      logical-pathname             room 
 compile-file                 logical-pathname-translations row-major-aref 
 compile-file-pathname        logtest                      scale-float 
                              long-site-name               set 
                              loop-finish
 compiler-macro-function      machine-instance             set-char-bit 
 compiler-macroexpand         machine-type                  
 compiler-macroexpand-1       machine-version              set-dispacth-macro-character 
 complex                      macro-function               set-exclusive-or 
 compute-restarts             macroexpand                  set-macro-character 
 concatenated-stream-streams  macroexpand-1                set-pprint-dispatch 
 conjugate                    macrolet                     set-syntax-from-char 
                              make-broadcast-stream        shadow 
 continue                     make-char                    shadowing-import 
 copy-pprint-dispatch         make-concatenated-stream     short-site-name 
 copy-readtable               make-dispatch-macro-character simple-bit-vector-p 
 copy-symbol                  make-echo-stream             simple-condition-format-arguments 
 copy-tree                    make-load-form-saving-slots  simple-condition-format-string 
 declaration-information      make-package                 sleep 
 decode-float                 make-pathname                slot-exists-p 
 decode-universal-time        make-random-state            slot-makunbound 
 delete-package               make-string-input-stream     software-type 
 deposit-field                make-string-output-stream    software-version 
 describe                     make-synonym-stream          special-form-p 
 directory                    make-two-way-stream          step 
 directory-namestring         makunbound                   store-value 
 disassemble                  mapl                         stream-error-stream 
 documentation                maplist                      stream-external-format 
 dpb                          mask                          
 dribble                      mask-field                    
 echo-stream-input-stream     merge-pathnames              
 echo-stream-output-stream    method-combination-error     subst-if 
 ed                           muffle-warning               subst-if-not 
 enclose                      name-char                    
 encode-universal-time        namestring                   symbol-package 
 enough-namestring            ensure-generic-function                    
 eval                         nset-exclusive-or            synonym-stream-symbol 
 evalhook                     nset-exclusive-or            tailp 
                                                           time 
 fboundp                                                   trace 
 fdefinition                  nsubst-if                    translate-logical-pathname 
 file-author                  nsubst-if-not                translate-pathname 
 file-error-pathname          output-stream-p              tree-equal 
 file-length                  package-error-package        truename 
 file-namestring              package-name                 two-way-stream-input-stream 
 file-string-length           package-nicknames            two-way-stream-output-stream 
 file-write-date              package-shadowing-symbols    type-error-datum 
 find-all-symbols             package-use-list             type-error-expected-type 
 find-all-symbols             package-used-by-list         unexport 
 find-package                 packagep                     unintern 
 find-restart                 parse-integer                unuse-package 
 find-symbol                  parse-macro                  upgraded-array-element-type 
 float-digits                 parse-namestring             upgraded-complex-part-type 
 float-precision              pathname                     use-package 
 float-radix                  pathname-device              use-value 
 float-sign                   pathname-directory           user-homedir-pathname 
 fmakunbound                  pathname-host                variable-information 
 function-information         pathname-match-p              
 function-lambda-expression   pathname-name                wild-pathname-p 
                              pathname-type                with-added-methods 
                              pathname-version             
                              pathnamep                    
 get-decoded-time             phase                        write-to-string 
 get-dispatch-macro-character pprint-exit-if-list-exhausted y-or-n-p 
 get-internal-run-time        pprint-dispatch              yes-or-no-p 
 get-macro-character          pprint-fill 
) 
