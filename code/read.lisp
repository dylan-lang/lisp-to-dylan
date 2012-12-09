;;;; LTD-READ: PRESERVES COMMENTS AND REMEMBERS FILE POSITIONS
(in-package :ltd)

(defvar *eof* "eof")
(defvar *file-position-table* (make-hash-table :test #'eq))
(defvar *read-list* (get-macro-character #\())
(defvar *lisp-array* (get-dispatch-macro-character #\# #\A))
(defvar *lisp-readtable* *readtable*)
(defvar *buffer*
  (make-array 200 :element-type 'character :fill-pointer 0 :adjustable t))

(defstruct (com (:predicate comment?))
  ;; We spell it COM because LispWorks has a class called comment
  (comment "") (code nil))

(defun add-comment (comment code)
  "Precede the code with a comment, or just return code if comment is null."
  (if comment (make-com :comment comment :code code) code))

(defmacro move-comment (place exp)
  ;; Strip the comment off of place, then eval exp and put comment there.
  (let ((comment (gensym)))
  `(let ((,comment (get-comment ,place)))
     (setf ,place (strip ,place))
     (add-comment ,comment ,exp))))

(defun get-comment (exp)
  (if (comment? exp) (com-comment exp) nil))

(defmacro record-file-positions (stream exp)
  ;; Caution: not hygienic
  `(let* ((start (file-position ,stream))
          (value ,exp)
          (end (file-position ,stream)))
    (when (or (consp value) (stringp value))
      (setf (gethash value *file-position-table*) (cons start end)))
    value))

(defun ltd-read-list (stream char)
  ;; Check for () and record positions
  (case (peek-char t stream nil)
    (#\) (read-char stream) '|()|)
    (otherwise (record-file-positions
                stream (funcall *read-list* stream char)))))

(defun read-array (stream char &optional arg)
  "Read an array using the native lisp array reader."
  (let ((*readtable* *lisp-readtable*))
    (funcall *lisp-array* stream char arg)))

(defun read-char-if (stream char)
  (if (eql (peek-char nil stream nil nil) char)
      (read-char stream)
      nil))

(defun collect-comments (stream char &optional arg comment-so-far)
  ;; Gather up comments, then either attach to following exp
  ;; or return no values (thus silently ignoring the comments).
  (declare (ignore arg))
  (let* ((comment
          (case char
            (#\; (loop while (read-char-if stream #\;)) ; Flush leading ;s
             (read-line stream nil ""))
            (#\| (ltd-read-hash-comment stream))
            (t "")))
         (comments (if comment-so-far
                       (format nil "~A~%~A" comment-so-far comment)
                     comment)))
    (case (peek-char t stream nil)
      ((#\. #\)) (values));; Ignore the comment
      ((#\;) (collect-comments stream #\; nil comments))
      ((#\#)
       ;; Deal with #|, #+, #-
       (read-char stream)
       (case (peek-char nil stream nil)
         ((#\|) (read-char stream)
          (collect-comments stream #\| nil comments))
         ((#\+ #\-) (add-comments-to-conditional stream comments))
         (t (unread-char #\# stream)
            (add-comment comments (ltd-read stream)))))
      (otherwise (add-comment comments (ltd-read stream))))))

(defun add-comments-to-conditional (stream comments)
  ;; We've read # and peeked at + or -
  ;; Add comments to next expression, if it is in,
  ;; or else ignore it and pass comments back to collect-comments
  (let* ((fn (ecase (read-char stream) (#\+ 'identity) (#\- 'not)))
         (*package* (find-package :keyword))
         (feature (read stream)))
    (if (funcall fn (member feature *features*)) ;; should handle and/or/not
        (add-comment comments (ltd-read stream))
      (let ((*read-suppress* t))
        (read stream nil nil) ;; Ignore the next exp
        (collect-comments stream nil 0 comments)))))
        

(defun ltd-read-hash-comment (stream)
  (setf (fill-pointer *buffer*) 0)
  (let ((level 1))
    (loop
     (let ((char (read-char stream nil :eof t)))
       (case char
         (:eof (warn "EOF during #| ... |# comment") (RETURN))
         (#\# (cond ((read-char-if stream #\|)
                     (incf level))
                    (t (vector-push-extend #\# *buffer*))))
         (#\| (cond ((read-char-if stream #\#)
                     (decf level)
                     (when (eql level 0)
                       (RETURN)))
                    (t (vector-push-extend #\| *buffer*))))
         (otherwise (vector-push-extend char *buffer*))))))
  (coerce *buffer* 'string))

(defparameter *ltd-readtable*
  (let ((table (copy-readtable *readtable*)))
    (set-macro-character #\; 'collect-comments nil table)
    (set-macro-character #\( 'ltd-read-list nil table)
    (set-dispatch-macro-character #\# #\| 'collect-comments table)
    (set-dispatch-macro-character #\# #\A 'read-array table)
    (set-dispatch-macro-character #\# #\a 'read-array table)
    table))

(defun ltd-read (&optional (stream *standard-input*))
  "Read a Lisp expression, preserving comments and file positions."
  (let* ((*readtable* *ltd-readtable*))
    (record-file-positions stream (read stream nil *eof*))))
