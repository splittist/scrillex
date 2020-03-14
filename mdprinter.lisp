;;;; mdprinter.lisp

(cl:in-package #:scrillex)

;;;; 3bmd markdown printer

#|

Use:

(3bmd:parse-string-and-print-to-stream string stream :format :wml)

(3bmd:parse-and-print-to-stream file stream :format :wml)

(3bmd:print-doc-to-stream doc stream :format :wml)

Not yet implemented:

:image out of band

Note: relies on 3bmd::expand-tabs to get rid of tabs

|#

(defparameter *document* nil)

(defparameter *html-is-wml* nil)

(defparameter *in-code* nil)
(defparameter *in-block-quote* nil)
(defparameter *in-paragraph* nil)
(defparameter *para-justification* nil) ;; stack of '3bmd-grammar::(left center right)
(defparameter *in-run* nil)
(defparameter *in-text* nil)
(defparameter *run-props* '()) ;; stack of :emph, :strong, :code and :hyperlink
(defparameter *in-list* nil) ;; stack of :counted-list and :bullet-list

(defun preservep (string)
  (or (alexandria:starts-with #\Space string :test #'char=)
      (alexandria:ends-with #\Space string :test #'char=)))

(defun sym-jc (sym)
  (ecase sym
    ((3bmd-grammar::left) "start")
    ((3bmd-grammar::center) "center")
    ((3bmd-grammar::right) "end")))

(defun open-paragraph (stream &key style-name ilvl numid)
  (when *in-paragraph* (close-paragraph stream))
  (setf *in-paragraph* t)
  (write-string "<w:p>" stream)
  (when (or style-name *para-justification*)
    (write-string "<w:pPr>" stream)
    (when style-name
      (format stream "<w:pStyle w:val=\"~A\" />" style-name))
    (when (and ilvl numid)
      (format stream "<w:numPr><w:ilvl w:val=\"~D\" /><w:numId w:val=\"~D\" /></w:numPr>"
	      ilvl numid))
    (when *para-justification*
      (format stream "<w:jc w:val=\"~A\" />" (sym-jc (first *para-justification*))))
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
	(:code (write-string "<w:rStyle w:val=\"mdcode\" />" stream));"<w:rFonts w:ascii=\"Consolas\" />" stream))
	(:hyperlink (write-string "<w:rStyle w:val=\"mdlink\" />" stream)))) 
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

(defun escape-string (string)
  (with-output-to-string (s)
    (print-escaped string s)))

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
  (let ((paragraph-style (cond (*in-block-quote* "mdquote")
			      ; (*in-code* "mdcode")
			       (t nil))))
    (unless *in-list*
      (open-paragraph stream :style-name paragraph-style)))
  (dolist (elem rest) (print-element elem stream))
  (close-paragraph stream))

(defmethod print-tagged-element ((tag (eql :block-quote)) stream rest)
  (let ((*in-block-quote* t))
    (dolist (elem rest) (print-element elem stream))))

(defmethod print-tagged-element ((tag (eql :heading)) stream rest)
  (open-paragraph stream :style-name (format nil "mdheading~D" (getf rest :level)))
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
		 (:bullet-list 3)))
	(ilvl (1- (length *in-list*))))
    (open-paragraph stream :style-name "mdlistparagraph":ilvl ilvl :numid numid)
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

(defun internalp (link)
  (alexandria:starts-with #\# link :test #'char=))

(defmethod print-tagged-element ((tag (eql :link)) stream rest)
  (when *in-run* (close-run stream))
  (let ((id (if *document*
		(opc:relationship-id
		 (docxplora:ensure-hyperlink *document* (car rest)))
		(car rest)))) ; Fake ID if not in document
    (format stream "<w:hyperlink w:id=\"~A\">" id))
  (push :hyperlink *run-props*)
  (print-element (car rest) stream)
  (when *in-run* (close-run stream))
  (pop *run-props*)
  (write-string "</w:hyperlink>" stream))

(defmethod print-tagged-element ((tag (eql :mailto)) stream rest)
  (when *in-run* (close-run stream))
  (let ((id (if *document*
		(opc:relationship-id
		 (docxplora:ensure-hyperlink *document*
					     (car rest)))
		(car rest))))
    (format stream "<w:hyperlink w:id=\"~A\">" id))
  (push :hyperlink *run-props*)
  (print-element (car rest) stream)
  (when *in-run* (close-run stream))
  (pop *run-props*)
  (write-string "</w:hyperlink>" stream))

(defmethod print-tagged-element ((tag (eql :explicit-link)) stream rest)
  (when *in-run* (close-run stream))
  (let ((source (getf rest :source)))
    (setf source (if (internalp source)
		     (format nil "w:anchor=\"~A\"" source)
		     (format nil "w:id=\"~A\""
			     (opc:relationship-id
			      (docxplora:ensure-hyperlink *document* source)))))
    (format stream "<w:hyperlink ~A ~@[w:tooltip=\"~A\"~]>"
	    source
	    (alexandria:when-let (title (getf rest :title))
	      (escape-string title))))
  (push :hyperlink *run-props*)
  (dolist (elem (getf rest :label)) (print-element elem stream))
  (when *in-run* (close-run stream))
  (pop *run-props*)
  (write-string "</w:hyperlink>" stream))

(defmethod print-tagged-element ((tag (eql :reference-link)) stream rest)
  (let* ((label (getf rest :label))
	 (def (or (getf rest :definition) label))
	 (ref (3bmd::lookup-reference def)))
    (cond (ref
	   (when *in-run* (close-run stream))
	   (let ((source (first ref)))
	     (setf source (if (internalp source)
			      (format nil "w:anchor=\"~A\"" source)
			      (format nil "w:id=\"~A\""
				      (opc:relationship-id
				       (docxplora:ensure-hyperlink *document* source)))))
	     (format stream "<w:hyperlink ~A ~@[w:tooltip=\"~A\"~]>"
		     source
		     (second ref)))
	   (push :hyperlink *run-props*)
	   (dolist (elem (getf rest :label)) (print-element elem stream))
	   (when *in-run* (close-run stream))
	   (pop *run-props*)
	   (write-string "</w:hyperlink>" stream))
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

;; w:drawing wp:inline>a:graphic>a:graphicData>pic:pic>pic:nvPicPr>pic:cNvPr[name=...png]^pic:blipFill>a:blip[r:embed=rId...]

#||
add-media/picture?-part document file
  get media type from file  -- trivial-mimes?
  create part with appropriate content_type
  read bytes into part content
  create-relationship md (rel /word/document.xml /word/media/filename) (rt image)
  return values part rId

add-inline-picture document file
  let id nthvalue 1 m-v-b add-media-part document file
  create xml with name and rId - goes within run
  need dimensions? how to get [png, jpeg, gif, bmp, tiff]

||#

(defun print-stuff (stuff stream)
  (when *in-run* (close-run stream))
  (dolist (elem stuff) (print-element elem stream))
  (when *in-run* (close-run stream)))

(defmethod print-tagged-element ((tag (eql :html)) stream rest)
  (when *html-is-wml*
    (format stream "~{~a~}" rest)))

(defmethod print-tagged-element ((tag (eql :raw-html)) stream rest)
  (when *html-is-wml*
    (format stream "~{~a~}" rest)))

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

(defmethod print-tagged-element ((tag (eql '3bmd::table)) stream rest)
  (write-string "<w:tbl><w:tblPr><w:tblStyle w:val=\"mdtable\" /></w:tblPr>" stream)
  (dolist (row (getf rest :head)) (print-table-row stream row))
  (dolist (row (getf rest :body)) (print-table-row stream row))
  (write-string "</w:tbl><w:p/>" stream)) ; Add paragraph to avoid adjacent tables being joined

(defun print-table-row (stream row)
  (write-string "<w:tr>" stream)
  (dolist (cell row)
    (write-string "<w:tc>" stream)
    (when (third cell) (push (third cell) *para-justification*))
    (print-tagged-element :paragraph stream (rest (second cell)))
    (when (third cell) (pop *para-justification*))
    (write-string "</w:tc>" stream))
  (write-string "</w:tr>" stream))

(defmethod print-tagged-element ((tag (eql '3bmd-grammar::th)) stream rest)
  )

(defmethod print-tagged-element ((tag (eql '3bmd-grammar::td)) stream rest)
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
	*in-code* *in-block-quote* *in-paragraph* *in-run* *in-text* *run-props* *in-list*
	*para-justification*)
    (let ((root
	   (plump:parse
	    (with-output-to-string (s)
	      (dolist (elem doc)
		(print-element elem s))
	      (fresh-line s)))))
      (coalesce-all-adjacent-run-text root)
      (plump:serialize root stream))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *md-heading-sizes* #(30 24 21 18 16 14))

(defun make-md-heading-style (i)
  (let* ((root (plump:make-root))
	 (style (docxplora:make-element/attrs root "w:style" "w:type" "paragraph" "w:customStyle" "1" "w:styleId" (format nil "mdheading~D" (1+ i)))))
    (docxplora:make-element/attrs style "w:name" "w:val" (format nil "MD Heading ~D" (1+ i)))
    (docxplora:make-element/attrs style "w:uiPriority" "w:val" "1")
    (docxplora:make-element/attrs style "w:qFormat")
    (let ((para-props (plump:make-element style "w:pPr")))
      (docxplora:make-element/attrs para-props "w:keepLines")
      (docxplora:make-element/attrs para-props "w:keepNext")
      (docxplora:make-element/attrs para-props "w:outlineLvl" "w:val" (format nil "~D" i))
      (docxplora:make-element/attrs para-props "w:spacing" "w:after" (format nil "~D" (- 240 (* 20 i)))))
    (let ((run-props (plump:make-element style "w:rPr")))
      (docxplora:make-element/attrs run-props "w:b")
      (docxplora:make-element/attrs run-props "w:sz" "w:val" (format nil "~D" (* 2 (aref *md-heading-sizes* i)))))
    style))

(defun make-md-code-style ()
  (let* ((root (plump:make-root))
	 (style (docxplora:make-element/attrs root "w:style" "w:type" "character" "w:customStyle" "1" "w:styleId" "mdcode")))
    (docxplora:make-element/attrs style "w:name" "w:val" "MD Code")
    (docxplora:make-element/attrs style "w:uiPriority" "w:val" "1")
    (docxplora:make-element/attrs style "w:qFormat")
    (let ((run-props (plump:make-element style "w:rPr")))
      (docxplora:make-element/attrs run-props "w:noProof")
      (docxplora:make-element/attrs run-props "w:rFonts" "w:ascii" "Consolas")
      (docxplora:make-element/attrs run-props "w:sz" "w:val" "20")
      (docxplora:make-element/attrs run-props "w:shd" "w:val" "1" "w:color" "auto" "w:fill" "DCDCDC"))
    style))

(defun make-md-link-style ()
  (let* ((root (plump:make-root))
	 (style (docxplora:make-element/attrs root "w:style" "w:type" "character" "w:customStyle" "1" "w:styleId" "mdlink")))
    (docxplora:make-element/attrs style "w:name" "w:val" "MD Link")
    (docxplora:make-element/attrs style "w:uiPriority" "w:val" "1")
    (docxplora:make-element/attrs style "w:qFormat")
    (let ((run-props (plump:make-element style "w:rPr")))
      (docxplora:make-element/attrs run-props "w:u" "w:val" "single")
      (docxplora:make-element/attrs run-props "w:color" "w:val" "0000FF"))
    style))

(defun make-style-base (type id name)
  (let* ((root (plump:make-root))
	 (style (docxplora:make-element/attrs root "w:style" "w:type" type "w:customStyle" "1" "w:styleId" id)))
    (docxplora:make-element/attrs style "w:name" "w:val" name)
    (docxplora:make-element/attrs style "w:uiPriority" "w:val" "1")
    (docxplora:make-element/attrs style "w:qFormat")
    style))

(defun make-md-quote-style ()
  (let* ((style (make-style-base "paragraph" "mdquote" "MD Quote"))
	 (para-props (plump:make-element style "w:pPr"))
	#+(or) (run-props (plump:make-element style "w:rPr"))
	 (para-border (plump:make-element para-props "w:pBdr")))
    (docxplora:make-element/attrs para-border "w:left" "w:val" "single" "w:sz" "24" "w:space" "4" "w:color" "A9A9A9")
    (docxplora:make-element/attrs para-props "w:shd" "w:val" "1" "w:color" "auto" "w:fill" "DCDCDC")
    (docxplora:make-element/attrs para-props "w:spacing" "w:before" "240" "w:after" "240")
    #+(or)(docxplora:make-element/attrs run-props "w:i")
    style))

(defun make-md-list-paragraph-style ()
  (let* ((style (make-style-base "paragraph" "mdlistparagraph" "MD ListParagraph"))
	 (para-props (plump:make-element style "w:pPr")))
    (docxplora:make-element/attrs para-props "w:ind" "w:left" "720")
    (plump:make-element para-props "w:contextualSpacing")
    style))

(defun make-md-table-style ()
  (let* ((style (make-style-base "table" "mdtable" "MD Table"))
	 (para-props (plump:make-element style "w:pPr"))
	 (table-props (plump:make-element style "w:tblPr"))
	 (borders (plump:make-element table-props "w:tblBorders"))
	 (cell-margins (plump:make-element table-props "w:tblCellMar")))
    (docxplora:make-element/attrs para-props "w:spacing" "w:after" "0")
    (docxplora:make-element/attrs borders "w:top" "w:val" "single" "w:sz" "4" "w:space" "0" "w:color" "auto")
    (docxplora:make-element/attrs borders "w:left" "w:val" "single" "w:sz" "4" "w:space" "0" "w:color" "auto")
    (docxplora:make-element/attrs borders "w:bottom" "w:val" "single" "w:sz" "4" "w:space" "0" "w:color" "auto")
    (docxplora:make-element/attrs borders "w:right" "w:val" "single" "w:sz" "4" "w:space" "0" "w:color" "auto")
    (docxplora:make-element/attrs borders "w:insideH" "w:val" "single" "w:sz" "4" "w:space" "0" "w:color" "auto")
    (docxplora:make-element/attrs borders "w:insideV" "w:val" "single" "w:sz" "4" "w:space" "0" "w:color" "auto")
    (docxplora:make-element/attrs cell-margins "w:left" "w:w" "108" "w:type" "dxa")
    (docxplora:make-element/attrs cell-margins "w:right" "w:w" "108" "w:type" "dxa")
    style))

(defvar *md-bullets* #(#\Bullet #\White_Bullet #\Black_Small_Square))

(defvar *md-number-formats* #("decimal" "lowerLetter" "lowerRoman"))

(defun make-md-numbering ()
  (let* ((root (plump:make-root))
	 (abstractnum (docxplora:make-element/attrs root "w:abstractNum" "w:abstractNumId" "1"))
	 (abstractbul (docxplora:make-element/attrs root "w:abstractNum" "w:abstractNumId" "2"))
	 (numbase (docxplora:make-element/attrs root "w:num" "w:numId" "1"))
	 (numrestart (docxplora:make-element/attrs root "w:num" "w:numId" "2"))
	 (bulletbase (docxplora:make-element/attrs root "w:num" "w:numId" "3")))
    (docxplora:make-element/attrs abstractnum "w:multiLevelType" "w:val" "multilevel")
    (dotimes (i 9)
      (let ((ilvl (docxplora:make-element/attrs abstractnum "w:lvl" "w:ilvl" (princ-to-string i))))
	(docxplora:make-element/attrs ilvl "w:start" "w:val" "1")
	(docxplora:make-element/attrs ilvl "w:numFmt" "w:val" (aref *md-number-formats* (mod i 3)))
	(docxplora:make-element/attrs ilvl "w:lvlText" "w:val" (format nil "%~D." (1+ i)))
	(docxplora:make-element/attrs ilvl "w:lvlJc" "w:val" "left")
	(let ((para-props (plump:make-element ilvl "w:pPr")))
	  (docxplora:make-element/attrs para-props "w:ind" "w:left" (princ-to-string (* (1+ i) 720)) "w:hanging" "360"))))
    (docxplora:make-element/attrs abstractbul "w:multiLevelType" "w:val" "hybridMultilevel")
    (dotimes (i 9)
      (let ((ilvl (docxplora:make-element/attrs abstractbul "w:lvl" "w:ilvl" (princ-to-string i))))
	(docxplora:make-element/attrs ilvl "w:start" "w:val" "1")
	(docxplora:make-element/attrs ilvl "w:numFmt" "w:val" "bullet")
	(docxplora:make-element/attrs ilvl "w:lvlText" "w:val" (string (aref *md-bullets* (mod i 3))))
	(docxplora:make-element/attrs ilvl "w:lvlJc" "w:val" "left")
	(let ((para-props (plump:make-element ilvl "w:pPr")))
	  (docxplora:make-element/attrs para-props "w:ind" "w:left" (princ-to-string (* (1+ i) 720)) "w:hanging" "360"))))
    (docxplora:make-element/attrs numbase "w:abstractNumId" "w:val" "1")
    (docxplora:make-element/attrs bulletbase "w:abstractNumId" "w:val" "2")
    (docxplora:make-element/attrs numrestart "w:abstractNumId" "w:val" "1")
    (dotimes (i 9)
      (let ((lvloverride (docxplora:make-element/attrs numrestart "w:lvlOverride" "w:ilvl" (princ-to-string i))))
	(docxplora:make-element/attrs lvloverride "w:startOverride" "w:val" "1")))
    root))

(defun add-md-numbering (numbering-part)
  (let ((numbering (first (plump:get-elements-by-tag-name (opc:xml-root numbering-part) "w:numbering"))))
    (setf (plump:children numbering) (plump:children (make-md-numbering)))))

(defparameter *md-styles*
  (list (make-md-heading-style 0)
	(make-md-heading-style 1)
	(make-md-heading-style 2)
	(make-md-heading-style 3)
	(make-md-heading-style 4)
	(make-md-heading-style 5)
	(make-md-code-style)
	(make-md-link-style)
	(make-md-quote-style)
	(make-md-table-style)
	(make-md-list-paragraph-style)
	))

(defun add-md-styles (document)
  (dolist (style *md-styles*)
    (alexandria:if-let (existing-style (docxplora:find-style-by-id document (plump:attribute style "w:styleId")))
      (progn
	(docxplora:remove-style document existing-style)
	(docxplora:add-style document style))
      (docxplora:add-style document style))))

(defun coalesce-all-adjacent-run-text (root)
  (let ((runs (lquery:with-master-document (root) (lquery:$ "w::r"))))
    (serapeum:do-each (run runs)
      (docxplora:coalesce-adjacent-text run))))

(defun md->docx (infile outfile)
  (let* ((3bmd:*smart-quotes* t)
	 (3bmd-tables:*tables* t)
	 (document (docxplora:make-document))
	 (*document* (docxplora:add-main-document document))
	 (xml (plump:parse
	       (with-output-to-string (s)
		 (3bmd:parse-and-print-to-stream infile s :format :wml)))))
    (let* ((mdroot (opc:xml-root *document*))
	   (body (lquery:with-master-document (mdroot) (lquery:$1 "w::body"))))
      (docxplora:add-style-definitions document)
      (add-md-styles document)
      (let ((numbering-part (docxplora:add-numbering-definitions document)))
	(add-md-numbering numbering-part))
      (setf (plump:children body) (plump:children xml))
      (docxplora:save-document document outfile))))
