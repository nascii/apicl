(defpackage apicl/tests/main
  (:use :cl
        :apicl
        :rove))
(in-package :apicl/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :apicl)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
