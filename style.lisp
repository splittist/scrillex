;;;; style.lisp

(cl:in-package #:scrillex)
      
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
