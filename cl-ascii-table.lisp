;;;; cl-ascii-table.lisp

(in-package #:ascii-table)

(defvar *default-value-formatter*
  (lambda (value)
    (format nil "~A"
            value)))

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
    :initform nil)
   (cols-formatters
    :initarg :cols-formatters
    :accessor cols-formatters
    :initform nil)))

;; interface
(defgeneric add-row (ascii-table columns))
(defgeneric add-separator (ascii-table))
(defgeneric display (ascii-table &optional out))

;; constructor
(defun make-table (columns &key (header nil) formatters)
  (when (> (length columns)
           (length formatters))
    (setf formatters
          (append formatters
                  (make-list (- (length columns)
                                 (length formatters))
                             :initial-element *default-value-formatter*))))
  
  (make-instance 'ascii-table
                 :header header
                 :cols columns
                 :cols-count (length columns)
                 :cols-widths (mapcar #'compute-col-width columns)
                 :cols-formatters formatters))

;; implementation
(defmethod add-row ((table ascii-table) columns)
  (unless (= (length columns) (cols-count table))
    (error "Invalid number of columns in row."))
  (setf (rows table) (cons columns (rows table)))
  (setf (cols-widths table)
        (mapcar #'compute-col-width
                columns
                (cols-widths table)
                (cols-formatters table)))
  (values))

(defmethod add-separator ((table ascii-table))
  (setf (rows table) (cons nil (rows table)))
  (values))

(defmethod display ((table ascii-table) &optional (out *standard-output*))
  (when (header table)
    (display-header table out))
  (display-separator table out)
  (display-row table (cols table) out)
  (display-separator table out)
  (loop for row in (reverse (rows table)) do
        (display-row table row out))
  (display-separator table out))

(defmethod print-object ((obj ascii-table) out)
  (print-unreadable-object (obj out :type t)
    (format out "HEADER:'~a' COLUMNS:~a" (header obj) (cols obj))))

;; display helpers
(defun display-header (table out)
  (let* ((header (header table))
         (header-len (length header))
         (widths (cols-widths table))
         (count (cols-count table))
         (len (1- (+ count (reduce #'+ widths))))
         (content
          (if (>= header-len len) header
            (let ((left-margin (+ header-len (floor (/ (- len header-len) 2)))))
              (string-pad header left-margin)))))
    (format out ".~a.~%" (make-string len :initial-element #\-))
    (format out "|~a|~%" (string-pad-right content len))))

(defun display-separator (table out)
  (loop for len in (cols-widths table)
        do (format out "+~a" (make-string len :initial-element #\-)))
  (format out "+~%"))

(defun display-row (table row out)
  (if row
      (progn
        (map nil (lambda (value len formatter)
                   (display-col value len out formatter))
              row
              (cols-widths table)
              (cols-formatters table))
        (format out "|~%"))
      (display-separator table out)))

(defun display-col (value len out &optional (formatter *default-value-formatter*))
  (let ((formatted-value (funcall formatter value)))
    (if (numberp value)
        (format out "|~a " (string-pad formatted-value (1- len)))
        (format out "| ~a" (string-pad-right formatted-value (1- len))))))

;; utils                          
(defun compute-col-width (col &optional (len 0) (formatter *default-value-formatter*))
  (let* ((formatted-value (funcall formatter col)))
    (max len (+ 2 (length formatted-value)))))

(defun string-pad (text len)
  (format nil "~v<~a~>" len text))

(defun string-pad-right (text len)
  (format nil "~v@<~a~>" len text))

(defun test ()
  (let ((table (make-table '("Name  " "Age  ") :header "People")))
    (format t "TABLE : ~a~%~%" table)
    (add-row table '("Bob" 42))
    (add-row table '("Bill" 12))
    (add-separator table)
    (add-row table '("Jane" 23))
    (display table)))
