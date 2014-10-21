;;;; cl-ascii-table.lisp

(in-package #:ascii-table)

;; model
(defclass ascii-table ()
  ((header
    :initarg :header
    :reader header
    :initform nil)
   (cols
    :initarg :cols
    :accessor cols
    :initform nil)
   (cols-count
    :initarg :cols-count
    :accessor cols-count
    :initform 0)
   (cols-widths
    :initarg :cols-widths
    :accessor cols-widths
    :initform nil)
   (rows
    :accessor rows
    :initform nil)))

;; interface
(defgeneric add-row (ascii-table columns))
(defgeneric add-separator (ascii-table))
(defgeneric display (ascii-table &optional out))

;; constructor
(defun make-table (columns &key (header nil))
  (make-instance 'ascii-table
                 :header header
                 :cols columns
                 :cols-count (length columns)
                 :cols-widths
                 (mapcar #'compute-col-width columns)))

;; implementation
(defmethod add-row ((table ascii-table) columns)
  (unless (= (length columns) (cols-count table))
    (error "Invalid number of columns in row."))
  (setf (rows table)
        (cons columns (rows table)))
  (setf (cols-widths table)
        (mapcar #'compute-col-width columns (cols-widths table)))
  (values))

(defmethod add-separator ((table ascii-table))
  (setf (rows table) (cons nil (rows table)))
  (values))

(defmethod display ((table ascii-table) &optional (out *standard-output*))
  ;(when (header table)
  ;  (ascii-table-display-header table out))
  (display-separator table out)
  (display-row table (cols table) out)
  (display-separator table out)
  (loop for row in (reverse (rows table)) do
        (display-row table row out))
  (display-separator table out))

(defmethod print-object ((obj ascii-table) out)
  (print-unreadable-object (obj out :type t)
    (format out "HEADER:~a COLUMNS:~a" (header obj) (cols obj))))

;; display helpers
(defun display-separator (table out)
  (loop for len in (cols-widths table)
        do (format out "+~a" (make-string len :initial-element #\-)))
  (format out "+~%"))

(defun display-row (table row out)
  (if row
      (progn
        (map nil (lambda (value len)
                   (display-col value len out))
             row (cols-widths table))
        (format out "|~%"))
    (display-separator table out)))

(defun display-col (value len out)
  (if (numberp value)
      (format out (format nil "|~~~a<~a~~> " (1- len) value))
    (format out (format nil "| ~~~a@<~a~~>" (1- len) value))))
             

;; utils                          
(defun compute-col-width (col &optional (len 0))
  (max len (+ 2 (length (format nil "~a" col)))))


(defun test ()
  (let ((table (make-table '("Name" "Age"))))
    (format t "TABLE : ~a~%~%" table)
    (add-row table '("Bob" 42))
    (add-row table '("Bill" 12))
    (add-separator table)
    (add-row table '("Jane" 23))
    (display table)))
