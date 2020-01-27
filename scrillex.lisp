;;;; scrillex.lisp

(cl:in-package #:scrillex)

(defun parse-scribble-args (arglist)
  (multiple-value-bind (required optional rest keywords allow-other-keys-p aux)
      (alexandria:parse-ordinary-lambda-list arglist)
    (format t "Required: ~D; Optional: ~D; Keywords: ~A"
	    (length required)
	    (length optional)
	    (mapcar #'caar keywords))))

#|
(define-scrillex-function name (lambda-list) text-var
			  "docstring"
			  (body)
			  (forms))

skip required
skip optional while not member keyword keys*
skip pairs while first member keyword args*
* if allow-other-keys, skip all keywords
remainder is text-var


|#

(defun collect-lambda-list (num-required num-optional keys aok args)
  (labels ((keyp (symbol)
	     (if aok ;; TODO :allow-other-keys
		 (keywordp symbol)
		 (find symbol keys))))
    (let (lambda-list)
      (dotimes (i num-required)
	(push (pop args) lambda-list))
      (do ((i 0 (1+ i))
	   (opt (car args)(car args)))
	  ((or (= num-optional i)
	       (keyp opt)))
	(push (pop args) lambda-list))
      (do ((arg (car args) (car args)))
	  ((not (keyp arg)))
	(push (pop args) lambda-list)
	(push (pop args) lambda-list))
      (values (reverse lambda-list) args))))

(defmacro define-scrillex-function (name lambda-list text-var &body body)
  (multiple-value-bind (required optional rest keywords aok aux)
      (alexandria:parse-ordinary-lambda-list lambda-list)
    (declare (ignore rest aux))
    (multiple-value-bind (forms decls doc)
	(alexandria:parse-body body)
      (let ((num-required (length required))
	    (num-optional (length optional))
	    (keys (mapcar #'caar keywords))
	    (args (gensym "ARGS"))
	    (ll (gensym "LL"))
	    (rest (gensym "REST")))
	`(defun ,name (&rest ,args)
	   ,@(when doc (list doc))
	   ,@decls
	   (multiple-value-bind (,ll ,rest)
	       (collect-lambda-list ,num-required ,num-optional ,keys ,aok ,args)
	     (destructuring-bind ,lambda-list ,ll
	       (let ((,text-var ,rest))
		 ,@forms))))))))

(defparameter *current-paragraph* nil)

(defun p (&rest args)
  (let* ((root (plump:make-root))
	 (para (plump:make-element root "w:p"))
	 (*current-paragraph* para))
    (dolist (arg args root)
      (cond
	((and (stringp arg)
	      (string= #\Newline arg))
	 nil)
	((stringp arg)
	 (make-run arg))
	(t
	 (error "unknown thingy: ~A" arg))))
    para))

(defun has-space-p (arg)
  (or (alexandria:starts-with #\Space arg)
      (alexandria:ends-with #\Space arg)))

(defun make-run (arg)
  (let* ((run (plump:make-element *current-paragraph* "w:r"))
	 (text (plump:make-element run "w:t")))
    (plump:make-text-node text arg)
    (when (has-space-p arg)
      (setf (plump:attribute text "space") "preserve"))
    run))
