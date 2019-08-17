(asdf:defsystem cl-muth
  :description "Multithreading utilities"
  :version "1.0.0"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :license "MIT"
  :defsystem-depends-on (trivial-features)
  :depends-on (alexandria bordeaux-threads bodge-queue bodge-heap)
  :pathname "src/"
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
                :components ((:file "atomic")
                             (:file "spin-lock")))
               (:module ccl
                :if-feature :ccl
                :serial t
                :components ((:file "atomic")
                             (:file "spin-lock")))
               (:module other
                :if-feature (:not (:or :ccl :sbcl))
                :serial t
                :components ((:file "atomic")
                             (:file "spin-lock")))
               (:file "atomic")
               (:file "spin-lock")
               (:file "blocking-timed-queue")
               (:file "scheduler")))


(asdf:defsystem cl-muth/tests
  :description "cl-muth tests"
  :version "1.0.0"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :license "MIT"
  :depends-on (alexandria bordeaux-threads cl-muth fiveam)
  :pathname "t/"
  :serial t
  :components ((:file "packages")
               (:file "suite")
               (:file "utils")
	       (:file "blocking-queue")
               (:file "thread-pool")
               (:file "scheduler")))
