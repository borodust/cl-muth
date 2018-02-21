(asdf:defsystem cl-muth
  :description "Multithreading utilities"
  :version "1.0.0"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :license "MIT"
  :depends-on (alexandria bordeaux-threads)
  :serial t
  :components ((:file "packages")
	       (:file "guarded-reference")
	       (:file "blocking-queue")
	       (:file "latch")
               (:file "thread-pool")))
