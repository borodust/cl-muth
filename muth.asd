(defpackage :muth-def
  (:use :cl :asdf))

(in-package :muth-def)

(defsystem :muth
  :description "Multithreading utilities"
  :version "0.0.1"
  :author "Pavel Korolev <dev at borodust.org>"
  :depends-on (alexandria bordeaux-threads log4cl)
  :serial t
  :components ((:file "packages")
	       (:file "guarded-reference")
	       (:file "blocking-queue")
	       (:file "looper")
	       (:file "latch")))
