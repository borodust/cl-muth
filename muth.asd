(defpackage :muth-def
  (:use :cl :asdf))

(in-package :muth-def)

(defsystem :muth
  :author "Pavel Korolev <dev at borodust.org>"
  :description "Multithreading utilities"
  :version "0.0.1"
  :depends-on (alexandria bordeaux-threads)
  :serial t
  :components ((:file "packages")
	       (:file "muth")))
