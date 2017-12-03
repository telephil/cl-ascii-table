;;;; cl-ascii-table.asd

(asdf:defsystem #:cl-ascii-table
  :description "Common Lisp library to present tabular data in ascii-art table."
  :author "Philippe MECHAI <philippe.mechai@gmail.com>"
  :homepage "https://github.com/telephil/cl-ascii-table/"
  :bug-tracker "https://github.com/telephil/cl-ascii-table/issues"
  :source-control (:git "git@github.com:telephil/cl-ascii-table.git")
  :license "MIT"
  :serial t
  :components ((:file "package")
               (:file "cl-ascii-table")))

