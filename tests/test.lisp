(defpackage #:arm-iss-test
  (:use #:cl #:fiveam #:arm-iss)
  (:export #:run-tests))

(in-package #:arm-iss-test)

(def-suite :arm-iss)
(in-suite :arm-iss)

(test passing-dummy-test
      (is (= 0 (dummy-fn))))

(test failing-dummy-test
      (is (= 0 1)))
