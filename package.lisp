;;;; package.lisp

(defpackage #:ascii-table
  (:nicknames #:table)
  (:use #:cl)
  (:export
     #:make-table
     #:add-row
     #:add-separator
     #:*default-value-formatter*
     #:display))


