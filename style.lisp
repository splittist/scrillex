;;;; style.lisp

(cl:in-package #:scrillex)

(defun read-forms (stream)
  (let ((eof (gensym "EOF")))
    (loop for read = (read stream nil eof)
       until (eql read eof)
       collect read)))

(defclass source ()
  ((%items :initarg :items :initform () :accessor items)))

(defun make-source (&optional items)
  (make-instance 'source :items items))

(defgeneric peek-source (source)
  (:method ((source source))
    (if (null (items source))
	(values nil nil)
	(values (car (items source)) t))))

(defgeneric pop-source (source)
  (:method ((source source))
    (if (null (items source))
	(values nil nil)
	(values (pop (items source)) t))))

(defgeneric push-source (source item)
  (:method ((source source) item)
    (push item (items source))))

(defun to-camelcase (symbol &key upper-first special-words)
  (let ((parts (split-sequence:split-sequence #\- (string symbol))))
    (apply
     #'serapeum:string+
     (loop for part in parts
	for firstp = t then nil
	for special = (member part special-words :test #'string-equal)
	collect (cond ((and firstp (not upper-first))
		       (string-downcase part))
		      (special (first special))
		      (t (string-capitalize part)))))))

(defun collect-forms (source)
  (loop for (next left) = (multiple-value-list (peek-source source))
     until (or (null left)
	       (keywordp next)
	       (listp next))
     collecting (handle-form source)))

(defun make-element (tag source)
  (let ((attrs (collect-forms source)))
    (when (oddp (length attrs))
      (setf attrs (cons "val" attrs)))
    (format t "<w:~A ~{w:~A=\"~A\" ~}>" tag attrs))
  (let ((next (peek-source source)))
    (etypecase next
      (symbol (format t "</w:~A>" tag) (handle-form source))
      (cons (handle-form source)(format t "</w:~A>" tag))
      (null nil))))

(defun handle-form (source)
  (let ((form (pop-source source)))
    (typecase form
      (null nil)
      (symbol (to-camelcase form))
      (string form)
      (cons (make-element (handle-form (make-source (list (first form)))) (make-source (rest form))))
      (t (princ-to-string form)))))

(defparameter *test*
  '(:style type "table" custom-style 1 style-id "mdtable"
    (:name "MD Table"
     :ui-priority 1
     :q-format
     :p-pr
     (:spacing after 0)
     :tbl-pr
     (:tbl-borders
      (:top single sz 4 space 0 color auto
       :left single sz 4 space 0 color auto
       :right single sz 4 space 0 color auto
       :bottom single sz 4 space 0 color auto
       :inside-h single sz 4 space 0 color auto
       :inside-v single sz 4 space 0 color auto))
     (:tbl-cell-mar
      (:left w 108 type dxa
       :right w 108 type dxa)))))
