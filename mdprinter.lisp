;;;; mdprinter.lisp

(cl:in-package #:scrillex)

;;;; 3bmd markdown printer

#|

Use:

(3bmd:parse-string-and-print-to-stream string stream :format :wml)

(3bmd:parse-and-print-to-stream file stream :format :wml)

(3bmd:print-doc-to-stream doc stream :format :wml)

Not yet implemented:

:link, :mailto, :explicit-link, :reference-link and references generally.
  These are out-of-band in docx packages.

:image Same

:html, :raw-html -- doesn't seem useful

Numbering reset is also out of band

Note: relies on 3bmd::expand-tabs to get rid of tabs

|#

(defparameter *in-code* nil)
(defparameter *in-block-quote* nil)
(defparameter *in-paragraph* nil)
(defparameter *in-run* nil)
(defparameter *in-text* nil)
(defparameter *run-props* '()) ;; stack of :emph, :strong, :code and :hyperlink
(defparameter *in-list* nil) ;; stack of :counted-list and :bullet-list

(defun preservep (string)
  (or (alexandria:starts-with #\Space string :test #'char=)
      (alexandria:ends-with #\Space string :test #'char=)))

(defun open-paragraph (stream &optional style-name ilvl numid)
  (when *in-paragraph* (close-paragraph stream))
  (setf *in-paragraph* t)
  (write-string "<w:p>" stream)
  (when style-name
    (format stream "<w:pPr><w:pStyle w:val=\"~A\" />" style-name)
    (when (and ilvl numid)
      (format stream "<w:numPr><w:ilvl w:val=\"~D\" /><w:numId w:val=\"~D\" /></w:numPr>"
	      ilvl numid))
    (write-string "</w:pPr>" stream)))

(defun close-paragraph (stream)
  (when *in-run* (close-run stream))
  (write-string "</w:p>" stream)
  (setf *in-paragraph* nil)
  (terpri stream)) ;;FIXME: readability

(defun open-run (stream)
  (write-string "<w:r>" stream)
  (when *run-props*
    (write-string "<w:rPr>" stream)
    (dolist (rp *run-props*)
      (ecase rp
	(:emph (write-string "<w:i w:val=\"true\" /><w:iCs w:val=\"true\" />" stream))
	(:strong (write-string "<w:b w:val=\"true\" /><w:bCs w:val=\"true\" />" stream))
	(:code (write-string "<w:rFonts w:ascii=\"Consolas\" />" stream))
	(:hyperlink (write-string "<w:rStyle w:val=\"Hyperlink\" />" stream)))) 
    (write-string "</w:rPr>" stream))
  (setf *in-run* t))

(defun close-run (stream)
  (when *in-text*
    (close-text stream))
  (write-string "</w:r>" stream)
  (setf *in-run* nil))

(defun open-text (stream string)
  (unless *in-run*
    (open-run stream))
  (when *in-text*
    (close-text stream))
  (format stream "<w:t~@[ xml:space=\"preserve\"~]>" (preservep string))
  (setf *in-text* t))

(defun close-text (stream)
  (format stream "</w:t>")
  (setf *in-text* nil))

(defparameter *references* nil)

(defun print-escaped (string stream)
  (loop for c across string
     when (eql c #\&) do (write-string "&amp;" stream)
     else when (eql c #\<) do (write-string "&lt;" stream)
     else when (eql c #\>) do (write-string "&gt;" stream)
     else do (write-char c stream)))

(defgeneric print-tagged-element (tag stream rest))

(defmethod print-tagged-element ((tag (eql :ellipsis)) stream rest)
  (if *in-code*
      (dolist (elem rest) (print-element elem stream))
      (print-element (string #\horizontal_ellipsis) stream)))

(defmethod print-tagged-element ((tag (eql :single-quoted)) stream rest)
  (if *in-code*
      (print-element "'" stream)
      (print-element (string #\left_single_quotation_mark) stream))
  (dolist (elem rest) (print-element elem stream))
  (if *in-code*
      (print-element "'" stream)
      (print-element (string #\right_single_quotation_mark) stream)))

(defmethod print-tagged-element ((tag (eql :double-quoted)) stream rest)
  (if *in-code*
      (print-element "\"" stream)
      (print-element (string #\left_double_quotation_mark) stream))
  (dolist (elem rest) (print-element elem stream))
  (if *in-code*
      (print-element "\"" stream)
      (print-element (string #\right_double_quotation_mark) stream)))

(defmacro define-smart-quote-entity (name replacement)
  `(defmethod print-tagged-element ((tag (eql ,name)) stream rest)
     (if *in-code*
	 (dolist (elem rest) (print-element elem stream))
	 (print-element (string ,replacement) stream))))

(define-smart-quote-entity :em-dash #\em_dash)
(define-smart-quote-entity :en-dash #\en_dash)
(define-smart-quote-entity :left-right-single-arrow #\left_right_arrow)
(define-smart-quote-entity :left-single-arrow #\leftwards_arrow)
(define-smart-quote-entity :right-single-arrow #\rightwards_arrow)
(define-smart-quote-entity :left-right-double-arrow #\left_right_double_arrow)
(define-smart-quote-entity :left-double-arrow #\leftwards_double_arrow)
(define-smart-quote-entity :right-double-arrow #\rightwards_double_arrow)

(defmethod print-tagged-element ((tag (eql :line-break)) stream rest)
  (when *in-text* (close-text stream))
  (unless *in-run* (open-run stream))
  (write-string "<w:cr />" stream))

(defmethod print-tagged-element ((tag (eql :horizontal-rule)) stream rest)
  (write-string "<w:p><w:pPr><w:pBdr><w:bottom w:val=\"single\" w:sz=\"12\" w:color=\"auto\" w:space=\"1\" /></w:pBdr></w:pPr></w:p>" stream)
  (terpri stream))

(defmethod print-tagged-element ((tag (eql :paragraph)) stream rest)
  (let ((paragraph-style (cond (*in-block-quote* "Quote")
			    ;   (*in-code* "Code")
			       (t nil))))
    (open-paragraph stream paragraph-style))
  (dolist (elem rest) (print-element elem stream))
  (close-paragraph stream))

(defmethod print-tagged-element ((tag (eql :block-quote)) stream rest)
  (let ((*in-block-quote* t))
    (dolist (elem rest) (print-element elem stream))))

(defmethod print-tagged-element ((tag (eql :heading)) stream rest)
  (open-paragraph stream (format nil "heading ~D" (getf rest :level)))
  (dolist (elem (getf rest :contents)) (print-element elem stream))
  (close-paragraph stream))

(defmethod print-tagged-element ((tag (eql :counted-list)) stream rest)
  (push :counted-list *in-list*)
  (dolist (elem rest) (print-element elem stream))
  (pop *in-list*))

(defmethod print-tagged-element ((tag (eql :bullet-list)) stream rest)
  (push :bullet-list *in-list*)
  (dolist (elem rest) (print-element elem stream))
  (pop *in-list*))

(defmethod print-tagged-element ((tag (eql :list-item)) stream rest)
  (let ((numid (ecase (first *in-list*)
		 (:counted-list 1)
		 (:bullet-list 2)))
	(ilvl (1- (length *in-list*))))
    (open-paragraph stream "ListParagraph" ilvl numid)
    (dolist (elem rest) (print-element elem stream))))

(defmethod print-tagged-element ((tag (eql :code)) stream rest)
  (when *in-run* (close-run stream))
  (push :code *run-props*)
  (push t *in-code*)
  (dolist (elem rest) (print-element elem stream))
  (when *in-run* (close-run stream))
  (pop *in-code*)
  (pop *run-props*))

(defmethod print-tagged-element ((tag (eql :emph)) stream rest)
  (when *in-run* (close-run stream))
  (push :emph *run-props*)
  (dolist (elem rest) (print-element elem stream))
  (when *in-run* (close-run stream))
  (pop *run-props*))

(defmethod print-tagged-element ((tag (eql :strong)) stream rest)
  (when *in-run* (close-run stream))
  (push :strong *run-props*)
  (dolist (elem rest) (print-element elem stream))
  (when *in-run* (close-run stream))
  (pop *run-props*))

;;; Unimplemented tags

(defun print-hyperlink (link stream)
  (when *in-run* (close-run stream))
  (push :hyperlink *run-props*)
  (dolist (elem link) (print-element elem stream))
  (when *in-run* (close-run stream))
  (pop *run-props*))
  

(defmethod print-tagged-element ((tag (eql :link)) stream rest)
  (print-hyperlink (list (car rest)) stream))

(defmethod print-tagged-element ((tag (eql :mailto)) stream rest)
  (print-hyperlink (list (car rest)) stream))

(defmethod print-tagged-element ((tag (eql :explicit-link)) stream rest)
  (print-hyperlink (getf rest :label) stream))

(defmethod print-tagged-element ((tag (eql :reference-link)) stream rest)
  (let* ((label (getf rest :label))
	 (def (or (getf rest :definition) label))
	 (ref (3bmd::lookup-reference def)))
    (cond (ref
	   (print-hyperlink label stream))
	  (t
	   (print-element "[" stream)
	   (dolist (elem label) (print-element elem stream))
	   (print-element "]" stream)
	   (alexandria:when-let (tail (getf rest :tail))
	     (print-element tail stream))))))

(defmethod print-tagged-element ((tag (eql :image)) stream rest)
  (let* ((rest (cdr (first rest)))
	 (label (getf rest :label)))
    (print-element "![" stream)
    (dolist (elem label) (print-element elem stream))
    (print-element "]" stream)))

(defun print-stuff (stuff stream)
  (when *in-run* (close-run stream))
  (dolist (elem stuff) (print-element elem stream))
  (when *in-run* (close-run stream)))

(defmethod print-tagged-element ((tag (eql :html)) stream rest)
  )

(defmethod print-tagged-element ((tag (eql :raw-html)) stream rest)
  )

(defmethod print-tagged-element ((tag (eql :entity)) stream rest) ;;FIXME check
  (dolist (string rest)
    (if (member string '("&lt;" "&amp;" "&gt;") :test #'string=)
	(write-string string stream)
	(write-string (plump:decode-entities string) stream))))

(defmethod print-tagged-element ((tag (eql :verbatim)) stream rest)
  (dolist (string rest)
    (let ((lines (split-sequence:split-sequence #\Newline string)))
      (dolist (line lines)
	(open-paragraph stream)
	(push :code *run-props*)
	(open-text stream line)
	(print-escaped line stream)
	(pop *run-props*)
	(close-run stream)
	(close-paragraph stream)))))

(defmethod print-tagged-element ((tag (eql :plain)) stream rest)
  (print-stuff rest stream))


(defmethod print-tagged-element ((tag (eql :reference)) stream rest)
  )

;;; print-element

(defgeneric print-element (elem stream))

(defmethod print-element ((elem null) stream)
  "")

(defmethod print-element ((elem (eql :apostrophe)) stream)
  (if *in-code*
      (print-element "'" stream)
      (print-element (string #\right_single_quotation_mark) stream)))

(defmethod print-element ((elem string) stream)
  (cond ((string= #\Newline elem)
	 (open-text stream " ")
	 (write-string " " stream))
	(t
	 (open-text stream elem)
	 (print-escaped elem stream))))

(defmethod print-element ((elem cons) stream)
  (if (symbolp (car elem))
      (print-tagged-element (car elem) stream (cdr elem))
      (error "Unknown cons? ~S" elem)))

(defmethod 3bmd::print-doc-to-stream-using-format (doc stream (format (eql :wml)))
  (let ((*references* (3bmd::extract-refs doc))
	*in-code* *in-block-quote* *in-paragraph* *in-run* *in-text* *run-props* *in-list*)
    (dolist (elem doc)
      (print-element elem stream))
    (fresh-line stream)))

