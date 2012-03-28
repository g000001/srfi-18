;;;; srfi-18.asd -*- Mode: Lisp;-*-

(cl:in-package :asdf)

(defsystem :srfi-18
  :serial t
  :depends-on (:fiveam)
  :components ((:file "package")
               #+sbcl (:file "sbcl-prim")
               (:file "srfi-18")))

(defmethod perform ((o test-op) (c (eql (find-system :srfi-18))))
  (load-system :srfi-18)
  (or (flet ((_ (pkg sym)
               (intern (symbol-name sym) (find-package pkg))))
         (let ((result (funcall (_ :fiveam :run) (_ :srfi-18.internal :srfi-18))))
           (funcall (_ :fiveam :explain!) result)
           (funcall (_ :fiveam :results-status) result)))
      (error "test-op failed") ))
