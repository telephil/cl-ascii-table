;;;; package.lisp

(defpackage #:cl-ascii-table
  (:nicknames #:atable)
  (:use #:cl)
  (:export
     #:make-ascii-table
     #:ascii-table-add-row
     #:ascii-table-add-separator
     #:ascii-table-display))

