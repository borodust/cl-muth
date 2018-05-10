(asdf:defsystem cl-muth
  :description "Multithreading utilities"
  :version "1.0.0"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :license "MIT"
  :defsystem-depends-on (trivial-features)
  :depends-on (alexandria bordeaux-threads)
  :serial t
  :components ((:file "packages")
               (:file "utils")
	       (:file "guarded-reference")
	       (:file "blocking-queue")
	       (:file "latch")
               (:file "thread-pool")
               (:module sbcl
                :if-feature :sbcl
                :serial t
                :components ((:file "atomic")))
               (:module ccl
                :if-feature :ccl
                :serial t
                :components ((:file "atomic")))
               (:module other
                :if-feature (:not (:or :ccl :sbcl))
                :serial t
                :components ((:file "atomic")))
               (:file "atomic")))
