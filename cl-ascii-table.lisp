;;;; cl-ascii-table.lisp

(in-package #:cl-ascii-table)

;; model
(defclass ascii-table ()
  ((header      :initarg :header :reader atable-header)
   (cols        :initarg :cols :accessor atable-cols)
   (cols-count  :accessor atable-cols-count)
   (cols-widths :accessor atable-cols-widths)
   (rows        :accessor atable-rows :initform nil)))

(defmethod print-object ((obj ascii-table) out)
  (print-unreadable-object (obj out :type t)
    (with-slots (header cols cols-count cols-widths) obj
      (format out "header:~a cols:~a cols-count:~a cols-widths:~a"
              header cols cols-count cols-widths))))

(defun make-ascii-table (header &rest columns)
  (let ((table (make-instance 'ascii-table
                              :header header
                              :cols   columns)))
    (setf (atable-cols-count table) (length columns))
    (setf (atable-cols-widths table) (mapcar #'compute-col-width columns))
    table))

(defun ascii-table-add-row (table &rest columns)
  (setf (atable-rows table)
        (cons columns (atable-rows table)))
  (setf (atable-cols-widths table)
        (mapcar #'compute-col-width columns (atable-cols-widths table)))
  t)

(defun ascii-table-add-separator (table)
  (setf (atable-rows table) (cons nil (atable-rows table)))
  t)

;; table display
(defun ascii-table-display (table &optional (out *standard-output*))
  ;(when (atable-header table)
  ;  (ascii-table-display-header table out))
  (ascii-table-display-separator table out)
  (ascii-table-display-row table (atable-cols table) out)
  (ascii-table-display-separator table out)
  (loop for row in (reverse (atable-rows table))
        do
        (ascii-table-display-row table row out))
  (ascii-table-display-separator table out))

(defun ascii-table-display-separator (table out)
  (loop for len in (atable-cols-widths table)
        do (format out "+~a" (make-string len :initial-element #\-)))
  (format out "+~%"))

(defun ascii-table-display-row (table row out)
  (if row
      (progn
        (map nil (lambda (value len)
                   (ascii-table-display-col value len out))
             row (atable-cols-widths table))
        (format out "|~%"))
    (ascii-table-display-separator table out)))

(defun ascii-table-display-col (value len out)
  (if (numberp value)
      (format out (format nil "|~~~a<~a~~> " (1- len) value))
    (format out (format nil "| ~~~a@<~a~~>" (1- len) value))))
             

;; utils                          
(defun compute-col-width (col &optional (len 0))
  (max len (+ 2 (length (format nil "~a" col)))))


(defun test ()
  (let ((table (make-ascii-table nil "Name" "Age")))
    (ascii-table-add-row table "Bob" 42)
    (ascii-table-add-row table "Bill" 12)
    (ascii-table-add-separator table)
    (ascii-table-add-row table "Jane" 23)
    (ascii-table-display table t)))