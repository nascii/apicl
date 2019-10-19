(defpackage apicl
  (:use :cl :clack)
  (:export :run))
(in-package :apicl)

(defun run ()
  (clack:clackup
   (lambda (env)
     (declare (ignore env))
     '(200 (:content-type "text/plain") ("hello!")))))
