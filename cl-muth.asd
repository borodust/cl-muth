(in-package :cl-user)

(defpackage :cl-muth.definition
  (:use :cl :asdf))

(in-package :cl-muth.definition)

(defsystem cl-muth
  :description "Multithreading utilities"
  :version "0.0.1"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :license "MIT"
  :depends-on (alexandria bordeaux-threads log4cl)
  :serial t
  :components ((:file "packages")
	       (:file "guarded-reference")
	       (:file "blocking-queue")
	       (:file "looper")
	       (:file "latch")
               (:file "thread-pool")))
