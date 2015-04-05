(defpackage :muth
  (:use :cl :alexandria :bordeaux-threads)
  (:export guard-object
	   with-guarded-object))
