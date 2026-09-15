(defsystem "cl-m-expression"
  :version "0.0.1"
  :author ""
  :license ""
  :depends-on ("esrap")
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description ""
  :in-order-to ((test-op (test-op "cl-m-expression/tests"))))

(defsystem "cl-m-expression/tests"
  :author ""
  :license ""
  :depends-on ("cl-m-expression"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for m-expression"
  :perform (test-op (op c) (symbol-call :rove :run c)))
