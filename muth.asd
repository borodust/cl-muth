(defpackage :muth-def
  (:use :cl :asdf))

(in-package :muth-def)

(defsystem :muth
  :description "Multithreading utilities"
  :version "0.0.1"
  :author "Pavel Korolev <dev at borodust.org>"
  :depends-on (alexandria bordeaux-threads)
  :serial t
  :components ((:file "packages")
	       (:file "muth")))
