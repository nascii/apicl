(defsystem "apicl"
  :version "0.1.0"
  :author "Dmitry Moskowski"
  :license ""
  :depends-on ("clack")
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description "api boilerplate"
  :in-order-to ((test-op (test-op "apicl/tests"))))

(defsystem "apicl/tests"
  :author "Dmitry Moskowski"
  :license ""
  :depends-on ("apicl"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for apicl"
  :perform (test-op (op c) (symbol-call :rove :run c)))
