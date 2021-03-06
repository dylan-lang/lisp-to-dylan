;;; *********************************************************************
;;; Copyright (c) 1989, Lawrence Erlbaum Assoc. All rights reserved.
;;; 
;;; Use and copying of this software and preparation of derivative works
;;; based upon this software are permitted.  Any distribution of this
;;; software or derivative works must comply with all applicable United
;;; States export control laws.
;;; 
;;; This software is made available AS IS. The author makes no warranty
;;; about the software, its performance or its conformity to any
;;; specification.
;;; *********************************************************************


(DEFMACRO INSIST (FNNAME &REST EXPS)
 `(AND ,@(MAKE-INSIST-FORMS FNNAME EXPS)))

(DEFUN MAKE-INSIST-FORMS (FNNAME EXPS)
 (AND (NOT (NULL EXPS))
      (CONS `(OR ,(CAR EXPS)
                 (ERROR "~S failed in ~S"
                        ',(CAR EXPS) ',FNNAME))
            (MAKE-INSIST-FORMS FNNAME (CDR EXPS)))))

(DEFMACRO DEFINE-TABLE (FN VARS PLACE)
 (LET ((KEY (CAR VARS))
       (SET-FN (GENTEMP "SET-FN."))
       (VAL (GENTEMP "VAL.")))
  `(PROGN (DEFUN ,FN (,KEY) (GETF ,PLACE ,KEY))
          (DEFUN ,SET-FN (,KEY ,VAL)
           (SETF (GETF ,PLACE ,KEY) ,VAL))
          (DEFSETF ,FN ,SET-FN)
          ',FN)))

(DEFUN DELETE-KEY (TABLE KEY)
 (REMF TABLE KEY) TABLE)

(DEFUN TABLE-KEYS (TABLE)
 (AND TABLE
      (CONS (CAR TABLE)
            (TABLE-KEYS (CDR (CDR TABLE))))))

(SETF *FOR-KEYS* NIL)
(DEFINE-TABLE FOR-KEY (KEY) *FOR-KEYS*)

(DEFMACRO FOR (&REST FOR-CLAUSES)
 (LET ((WHEN-PART (MEMBER ':WHEN FOR-CLAUSES)))
  (FOR-EXPANDER (FOR-VAR-FORMS FOR-CLAUSES)
                (AND WHEN-PART (CAR (CDR WHEN-PART)))
                (FOR-BODY FOR-CLAUSES))))

(DEFUN FOR-VAR-FORMS (L)
 (AND L (LISTP (CAR L))
      (CONS (CAR L) (FOR-VAR-FORMS (CDR L)))))

(DEFUN FOR-BODY (L)
 (AND L (OR (AND (FOR-KEY (CAR L)) L)
            (FOR-BODY (CDR L)))))

(DEFUN FOR-EXPANDER (VAR-FORMS WHEN-FORM BODY-FORMS)
 (INSIST FOR
         (NOT (NULL VAR-FORMS))
         (NOT (NULL BODY-FORMS)))
 (LET ((VARS (MAPCAR #'CAR VAR-FORMS))
       (LISTS (MAPCAR #'(LAMBDA (VAR-FORM)
                         (CAR (CDR (CDR VAR-FORM))))
                      VAR-FORMS))
       (MAPFN-BODY (FUNCALL (FOR-KEY (CAR BODY-FORMS))
                      WHEN-FORM
                      `(PROGN ,@(CDR BODY-FORMS)))))
  `(,(CAR MAPFN-BODY)
    #'(LAMBDA ,VARS ,(CAR (CDR MAPFN-BODY)))
    ,@LISTS)))

(DEFMACRO DEFINE-FOR-KEY (KEY VARS MAPFN BODY)
 `(PROGN (SETF (FOR-KEY ',KEY)
               #'(LAMBDA ,VARS (LIST ,MAPFN ,BODY)))
         ',KEY))

(DEFINE-FOR-KEY :ALWAYS (TEST BODY)
 'EVERY
 (COND (TEST `(OR (NOT ,TEST) ,BODY)) (T BODY)))

(DEFINE-FOR-KEY :DO (TEST BODY)
 'MAPC (COND (TEST `(AND ,TEST ,BODY)) (T BODY)))
 
(DEFINE-FOR-KEY :FILTER (TEST BODY)
 'MAPCAN
 (LET ((FBODY `(LET ((X ,BODY)) (AND X (LIST X)))))
  (COND (TEST `(AND ,TEST ,FBODY)) (T FBODY))))

(DEFINE-FOR-KEY :FIRST (TEST BODY)
 'SOME (COND (TEST `(AND ,TEST ,BODY)) (T BODY)))
 
(DEFINE-FOR-KEY :SAVE (TEST BODY)
 (COND (TEST 'MAPCAN) (T 'MAPCAR))
 (COND (TEST `(AND ,TEST (LIST ,BODY)))
       (T BODY)))

(DEFINE-FOR-KEY :SPLICE (TEST BODY)
 'MAPCAN
 `(COPY-LIST
    ,(COND (TEST `(AND ,TEST ,BODY)) (T BODY))))

(SETF *MOP-TABLES* NIL)

(DEFINE-TABLE MOP-TABLE (TABLE-NAME) *MOP-TABLES*)

(DEFINE-TABLE MOP-ABSTS (MOP) (MOP-TABLE 'ABSTS))
(DEFINE-TABLE MOP-ALL-ABSTS (MOP)
 (MOP-TABLE 'ALL-ABSTS))
(DEFINE-TABLE MOP-SPECS (MOP) (MOP-TABLE 'SPECS))
(DEFINE-TABLE MOP-SLOTS (MOP) (MOP-TABLE 'SLOTS))
(DEFINE-TABLE MOP-TYPE (MOP) (MOP-TABLE 'TYPE))

(DEFUN MOPP (X)
 (OR (NUMBERP X) (AND (SYMBOLP X) (MOP-TYPE X))))

(DEFUN INSTANCE-MOPP (X)
 (AND (MOPP X)
      (OR (NUMBERP X)
          (EQL (MOP-TYPE X) 'INSTANCE))))

(DEFUN ABST-MOPP (X)
 (AND (MOPP X)
      (EQL (MOP-TYPE X) 'MOP)))

(DEFUN ABSTP (ABST SPEC)
 (OR (EQL ABST SPEC)
     (MEMBER ABST (MOP-ALL-ABSTS SPEC))))

(DEFUN PATTERNP (X) (ABSTP 'M-PATTERN X))

(DEFUN GROUPP (X) (ABSTP 'M-GROUP X))

(DEFUN SLOT-ROLE (SLOT) (CAR SLOT))
(DEFUN SLOT-FILLER (SLOT) (CAR (CDR SLOT)))
(DEFUN MAKE-SLOT (ROLE MOP) (LIST ROLE MOP))

(DEFUN ROLE-SLOT (ROLE X)
 (INSIST ROLE-SLOT
         (OR (MOPP X) (LISTP X)))
 (ASSOC ROLE
        (COND ((MOPP X) (MOP-SLOTS X))
              (T X))))

(DEFUN ROLE-FILLER (ROLE X)
 (SLOT-FILLER (ROLE-SLOT ROLE X)))

(DEFUN ADD-ROLE-FILLER (ROLE MOP FILLER)
 (INSIST ADD-ROLE-FILLER
         (MOPP MOP) (NULL (ROLE-FILLER ROLE MOP)))
 (FORMAT T "~&~S:~S <= ~S" MOP ROLE FILLER)
 (SETF (MOP-SLOTS MOP)
       (CONS (MAKE-SLOT ROLE FILLER)
             (MOP-SLOTS MOP)))
 FILLER)

(DEFUN LINK-ABST (SPEC ABST)
 (INSIST LINK-ABST (ABST-MOPP ABST) (MOPP SPEC)
                   (NOT (ABSTP SPEC ABST)))
 (COND ((NOT (ABSTP ABST SPEC))
        (SETF (MOP-ABSTS SPEC)
              (CONS ABST (MOP-ABSTS SPEC)))
        (SETF (MOP-SPECS ABST)
              (CONS SPEC (MOP-SPECS ABST)))
        (REDO-ALL-ABSTS SPEC)))
 SPEC)

(DEFUN UNLINK-ABST (SPEC ABST)
 (COND ((ABSTP ABST SPEC)
        (SETF (MOP-ABSTS SPEC)
              (REMOVE ABST (MOP-ABSTS SPEC)))
        (SETF (MOP-SPECS ABST)
              (REMOVE SPEC (MOP-SPECS ABST)))
        (REDO-ALL-ABSTS SPEC)))
 SPEC)

(DEFUN REDO-ALL-ABSTS (MOP)
 (SETF (MOP-ALL-ABSTS MOP) (CALC-ALL-ABSTS MOP))
 (FOR (SPEC :IN (MOP-SPECS MOP))
    :DO (REDO-ALL-ABSTS SPEC)))

(DEFUN CALC-ALL-ABSTS (MOP)
 (REMOVE-DUPLICATES
  (CONS MOP (FOR (ABST :IN (MOP-ABSTS MOP))
               :SPLICE (MOP-ALL-ABSTS ABST)))))

(DEFUN NEW-MOP (NAME ABSTS TYPE SLOTS)
 (INSIST NEW-MOP
         (SYMBOLP NAME)
         (FOR (ABST :IN ABSTS) :ALWAYS (MOPP ABST)))
 (OR TYPE (SETF TYPE (CALC-TYPE ABSTS SLOTS)))
 (OR NAME (SETF NAME (SPEC-NAME ABSTS TYPE)))
 (SETF (MOP-TYPE NAME) TYPE)
 (AND SLOTS (SETF (MOP-SLOTS NAME) SLOTS))
 (FOR (ABST :IN ABSTS) :DO (LINK-ABST NAME ABST))
 NAME)

(DEFUN CALC-TYPE (ABSTS SLOTS)
 (OR (FOR (ABST :IN ABSTS)
        :WHEN (PATTERNP ABST)
        :FIRST 'MOP)
     (AND (NULL SLOTS) 'MOP)
     (FOR (SLOT :IN SLOTS)
        :WHEN (NOT (INSTANCE-MOPP (SLOT-FILLER SLOT)))
        :FIRST 'MOP)
    'INSTANCE))

(DEFUN SPEC-NAME (ABSTS TYPE)
 (GENTEMP (FORMAT NIL (COND ((EQL TYPE 'MOP) "~S.")
                            (T "I-~S."))
                      (CAR ABSTS))))

(DEFUN CLEAR-MEMORY ()
 (SETF *MOP-TABLES* NIL)
 (NEW-MOP 'M-ROOT NIL 'MOP NIL)
 (SETF (MOP-ALL-ABSTS 'M-ROOT)
       (CALC-ALL-ABSTS 'M-ROOT))
 'M-ROOT)

(DEFUN ALL-MOPS () (TABLE-KEYS (MOP-TABLE 'TYPE)))

(DEFUN REMOVE-MOP (NAME)
 (FOR (ABST :IN (MOP-ABSTS NAME))
    :DO (UNLINK-ABST NAME ABST))
 (FOR (TABLE-NAME :IN (TABLE-KEYS *MOP-TABLES*))
    :DO (SETF (MOP-TABLE TABLE-NAME)
              (DELETE-KEY (MOP-TABLE TABLE-NAME)
                          NAME))))

(DEFUN INHERIT-FILLER (ROLE MOP)
 (FOR (ABST :IN (MOP-ALL-ABSTS MOP))
    :FIRST (ROLE-FILLER ROLE ABST)))

(DEFUN GET-FILLER (ROLE MOP)
 (OR (ROLE-FILLER ROLE MOP)
   (LET ((FILLER (INHERIT-FILLER ROLE MOP)))
    (AND FILLER
      (OR (AND (INSTANCE-MOPP FILLER) FILLER)
        (AND (ABSTP 'M-FUNCTION FILLER) FILLER)
        (LET ((FN (GET-FILLER 'CALC-FN FILLER)))
         (AND FN
           (LET ((NEW-FILLER (FUNCALL FN FILLER MOP)))
            (AND NEW-FILLER
                 (ADD-ROLE-FILLER ROLE MOP 
                                  NEW-FILLER))))))))))

(DEFUN PATH-FILLER (PATH MOP)
 (AND (FOR (ROLE :IN PATH)
         :ALWAYS (SETF MOP (GET-FILLER ROLE MOP)))
      MOP))

(DEFUN SLOTS-ABSTP (MOP SLOTS)
 (AND (ABST-MOPP MOP)
      (NOT (NULL (MOP-SLOTS MOP)))
      (FOR (SLOT :IN (MOP-SLOTS MOP))
         :ALWAYS (SATISFIEDP (SLOT-FILLER SLOT)
                     (GET-FILLER (SLOT-ROLE SLOT)
                                 SLOTS)
                     SLOTS))))

(DEFUN SATISFIEDP (CONSTRAINT FILLER SLOTS)
 (COND ((NULL CONSTRAINT))
       ((PATTERNP CONSTRAINT)
        (FUNCALL (INHERIT-FILLER 'ABST-FN CONSTRAINT)
                 CONSTRAINT FILLER SLOTS))
       ((ABSTP CONSTRAINT FILLER))
       ((INSTANCE-MOPP CONSTRAINT) (NULL FILLER))
       (FILLER (SLOTS-ABSTP CONSTRAINT FILLER))
       (T NIL)))

(DEFUN MOP-INCLUDESP (MOP1 MOP2)
 (AND (EQL (MOP-TYPE MOP1) (MOP-TYPE MOP2))
      (FOR (SLOT :IN (MOP-SLOTS MOP2))
         :ALWAYS (EQL (SLOT-FILLER SLOT)
                      (GET-FILLER (SLOT-ROLE SLOT)
                                  MOP1)))
      MOP1))

(DEFUN MOP-EQUALP (MOP1 MOP2)
 (AND (MOP-INCLUDESP MOP2 MOP1)
      (MOP-INCLUDESP MOP1 MOP2)))

#|
(DEFUN GET-TWIN (MOP)
 (FOR (ABST :IN (MOP-ABSTS MOP))
    :FIRST (FOR (SPEC :IN (MOP-SPECS ABST))
              :WHEN (NOT (EQL SPEC MOP))
              :FIRST (MOP-EQUALP SPEC MOP))))
|#

(DEFUN GET-TWIN (MOP)
 (FOR (ABST :IN (MOP-ABSTS MOP))
    :FIRST (FOR (SPEC :IN (MOP-SPECS ABST))
              :WHEN (NOT (EQL SPEC MOP))
              :FIRST (AND (MOP-INCLUDESP SPEC MOP)
			  (OR (NOT (GROUPP MOP))
			      (MOP-INCLUDESP MOP SPEC))
			  SPEC))))

(DEFUN REFINE-INSTANCE (INSTANCE)
 (FOR (ABST :IN (MOP-ABSTS INSTANCE))
    :WHEN (MOPS-ABSTP (MOP-SPECS ABST) INSTANCE)
    :FIRST (UNLINK-ABST INSTANCE ABST)
           (REFINE-INSTANCE INSTANCE)))

(DEFUN MOPS-ABSTP (MOPS INSTANCE)
 (NOT (NULL (FOR (MOP :IN MOPS)
               :WHEN (SLOTS-ABSTP MOP INSTANCE)
               :SAVE (LINK-ABST INSTANCE MOP)))))

(DEFUN INSTALL-INSTANCE (INSTANCE)
 (REFINE-INSTANCE INSTANCE)
 (LET ((TWIN (GET-TWIN INSTANCE)))
  (COND (TWIN (REMOVE-MOP INSTANCE) TWIN)
        ((HAS-LEGAL-ABSTS-P INSTANCE) INSTANCE)
        (T (REMOVE-MOP INSTANCE) NIL))))

(DEFUN HAS-LEGAL-ABSTS-P (INSTANCE)
 (FOR (ABST :IN (MOP-ABSTS INSTANCE))
    :WHEN (NOT (LEGAL-ABSTP ABST INSTANCE))
    :DO (UNLINK-ABST INSTANCE ABST))
 (MOP-ABSTS INSTANCE))

(DEFUN LEGAL-ABSTP (ABST INSTANCE)
 (AND (MOP-SLOTS ABST)
      (FOR (SPEC :IN (MOP-SPECS ABST))
         :ALWAYS (INSTANCE-MOPP SPEC))))

(DEFUN INSTALL-ABSTRACTION (MOP)
 (LET ((TWIN (GET-TWIN MOP)))
  (COND (TWIN (REMOVE-MOP MOP) TWIN)
        (T (REINDEX-SIBLINGS MOP)))))

(DEFUN REINDEX-SIBLINGS (MOP)
 (FOR (ABST :IN (MOP-ABSTS MOP))
    :DO (FOR (SPEC :IN (MOP-SPECS ABST))
           :WHEN (AND (INSTANCE-MOPP SPEC)
                      (SLOTS-ABSTP MOP SPEC))
           :DO (UNLINK-ABST SPEC ABST)
               (LINK-ABST SPEC MOP)))
  MOP)

(DEFUN SLOTS->MOP (SLOTS ABSTS MUST-WORK)
 (INSIST SLOTS->MOP
         (NOT (NULL ABSTS))
         (FOR (ABST :IN ABSTS) :ALWAYS (MOPP ABST)))
 (OR (AND (NULL SLOTS) (NULL (CDR ABSTS)) (CAR ABSTS))
     (LET ((TYPE (AND SLOTS (ATOM (CAR SLOTS))
                     (CAR SLOTS))))
      (AND TYPE (SETF SLOTS (CDR SLOTS)))
      (LET ((MOP (NEW-MOP NIL ABSTS TYPE SLOTS)))
       (LET ((RESULT
               (COND ((INSTANCE-MOPP MOP)
                      (INSTALL-INSTANCE MOP))
                     (T (INSTALL-ABSTRACTION MOP)))))
        (INSIST SLOTS->MOP 
                (OR RESULT (NULL MUST-WORK)))
        RESULT)))))

(DEFMACRO DEFMOP (NAME ABSTS &REST ARGS)
 (LET ((TYPE (AND ARGS (ATOM (CAR ARGS)) (CAR ARGS))))
  (LET ((SLOT-FORMS (COND (TYPE (CDR ARGS))
                          (T ARGS))))
   `(NEW-MOP ',NAME ',ABSTS ',TYPE
              (FORMS->SLOTS ',SLOT-FORMS)))))

(DEFUN FORMS->SLOTS (SLOT-FORMS)
 (FOR (SLOT-FORM :IN SLOT-FORMS)
    :SAVE
      (COND ((ATOM SLOT-FORM) SLOT-FORM)
        (T (MAKE-SLOT (SLOT-ROLE SLOT-FORM)
             (LET ((ABST (CAR (CDR SLOT-FORM))))
              (INSIST FORMS->SLOTS (ATOM ABST))
              (AND ABST
                (SLOTS->MOP
                 (FORMS->SLOTS (CDR (CDR SLOT-FORM)))
                 (LIST ABST)
                 T))))))))

(DEFUN GROUP-SIZE (X)
 (AND (GROUPP X) (LENGTH (MOP-SLOTS X))))

(DEFUN GROUP->LIST (GROUP)
 (AND GROUP
      (INSIST GROUP->LIST (GROUPP GROUP))
      (FOR (INDEX :IN (MAKE-M-N 1 (GROUP-SIZE GROUP)))
         :FILTER (ROLE-FILLER INDEX GROUP))))

(DEFUN LIST->GROUP (L)
 (COND ((NULL L) 'I-M-EMPTY-GROUP)
       (T (SLOTS->MOP
            (FOR (X :IN L)
                 (I :IN (MAKE-M-N 1 (LENGTH L)))
               :SAVE (MAKE-SLOT I X))
            '(M-GROUP)
            T))))

(DEFUN MAKE-M-N (M N)
 (INSIST MAKE-M-N (INTEGERP M) (INTEGERP N))
 (COND ((EQL M N) (LIST N))
       ((< M N) (CONS M (MAKE-M-N (+ M 1) N)))
       (T (CONS M (MAKE-M-N (- M 1) N)))))

(DEFUN DAH (MOP)
 (PPRINT (TREE->LIST MOP #'SPECS->LIST NIL)))

(DEFUN DPH (MOP)
 (PPRINT (TREE->LIST MOP #'SLOTS->FORMS NIL)))

(DEFUN SPECS->LIST (MOP VISITED)
 (FOR (SPEC :IN (MOP-SPECS MOP))
    :SAVE (TREE->LIST SPEC #'SPECS->LIST VISITED)))

(DEFUN SLOTS->FORMS (MOP VISITED)
 (FOR (SLOT :IN (MOP-SLOTS MOP))
    :SAVE (CONS (SLOT-ROLE SLOT)
                (MOP->FORM (SLOT-FILLER SLOT)
                           VISITED))))

(DEFUN MOP->FORM (MOP VISITED)
 (TREE->LIST MOP #'SLOTS->FORMS VISITED))

(DEFUN TREE->LIST (MOP FN VISITED)
 (COND ((MEMBER MOP VISITED) (LIST MOP))
       (T (SETF VISITED (CONS MOP VISITED))
          `(,MOP ,@(FUNCALL FN MOP VISITED)))))

(DEFUN CONSTRAINT-FN (CONSTRAINT FILLER SLOTS) T)

(DEFUN NOT-CONSTRAINT (CONSTRAINT FILLER SLOTS)
 (INSIST NOT-CONSTRAINT (NOT (NULL FILLER)))
 (NOT (SATISFIEDP (GET-FILLER 'OBJECT CONSTRAINT)
                  FILLER SLOTS)))

(DEFUN GET-SIBLING (PATTERN MOP)
 (FOR (ABST :IN (MOP-ABSTS MOP))
    :FIRST (FOR (SPEC :IN (MOP-SPECS ABST))
              :WHEN (AND (INSTANCE-MOPP SPEC)
                         (NOT (EQL SPEC MOP))
                         (NOT (ABSTP
                                'M-FAILED-SOLUTION
                                SPEC)))
              :FIRST SPEC)))

