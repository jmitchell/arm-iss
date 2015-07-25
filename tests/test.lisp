(defpackage #:arm-iss-test
  (:use #:cl #:fiveam #:arm-iss)
  (:export #:run-tests))

(in-package #:arm-iss-test)

(def-suite :arm-iss)
(in-suite :arm-iss)

(test new-register-is-expected-length
  (let ((reg (make-instance 'register)))
    (with-slots (bitfield) reg
      (is (= 32 (length bitfield))))))

(test set-register-bit-range
  (let ((reg (make-instance 'register)))
    (is (equal (bits reg 4 0) #*00000))
    (setf (bits reg 4 0) #*11100)
    (is (equal (bits reg 4 0) #*11100))
    (is (equal (bit-at reg 4) 1))
    (is (equal (bit-at reg 0) 0))

    (is (equal (bit-at reg 15) 0))
    (setf (bit-at reg 15) 1)
    (is (equal (bit-at reg 15) 1))))

(test check-and-set-program-status-register-flags
  (let ((psr (make-instance 'program-status-register)))
    (loop
       for flag in (mapcar #'cdr *psr-flags*)
       do (progn
            (is (= (flag-state psr flag) 0)
                (setf (flag-state psr flag) 1)
                (is (= (flag-state psr flag) 1)))))))

(test check-and-set-program-status-modes
  (let ((psr (make-instance 'program-status-register)))
    (is (equal (mode psr) :svc))
    (setf (mode psr) :usr)
    (is (equal (mode psr) :usr))
    (setf (mode psr) :sys)
    (is (equal (mode psr) :sys))))
