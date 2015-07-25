(defpackage #:arm-iss-test
  (:use #:cl #:fiveam #:arm-iss)
  (:export #:run-tests))

(in-package #:arm-iss-test)

(def-suite :arm-iss)
(in-suite :arm-iss)

(test new-register-is-expected-length
  (let ((reg (make-instance 'register)))
    (is (= 32 (width reg)))))

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

(test logical-shift-left
  (let ((bf (make-instance 'bitfield :width 32)))
    (setf (bits bf 31 0) #*11111111111111111111111111111111)
    (setf bf (logical-shift-left bf 10))
    (is (equal (bits bf 31 0) #*11111111111111111111110000000000))
    (setf bf (logical-shift-left bf 5))
    (is (equal (bits bf 31 0) #*11111111111111111000000000000000))
    (setf bf (logical-shift-left bf 17))
    (is (equal (bits bf 31 0) #*00000000000000000000000000000000))))

(test logical-shift-right
  (let ((bf (make-instance 'bitfield :width 32)))
    (setf (bits bf 31 0) #*11111111111111111111111111111111)
    (setf bf (logical-shift-right bf 10))
    (is (equal (bits bf 31 0) #*00000000001111111111111111111111))
    (setf bf (logical-shift-right bf 5))
    (is (equal (bits bf 31 0) #*00000000000000011111111111111111))
    (setf bf (logical-shift-right bf 17))
    (is (equal (bits bf 31 0) #*00000000000000000000000000000000))))

(test arithmetic-right-shift
  (let ((bf (make-instance 'bitfield :width 32)))
    (setf (bits bf 31 0) #*11111111111111111111111111111111)
    (setf bf (arithmetic-right-shift bf 10))
    (is (equal (bits bf 31 0) #*11111111111111111111111111111111))
    (setf bf (arithmetic-right-shift bf 5))
    (is (equal (bits bf 31 0) #*11111111111111111111111111111111))
    (setf bf (arithmetic-right-shift bf 17))
    (is (equal (bits bf 31 0) #*11111111111111111111111111111111))

    (setf (bits bf 31 0) #*10111111111111111111111111111111)
    (setf bf (arithmetic-right-shift bf 10))
    (is (equal (bits bf 31 0) #*11111111111011111111111111111111))
    (setf bf (arithmetic-right-shift bf 5))
    (is (equal (bits bf 31 0) #*11111111111111110111111111111111))
    (setf bf (arithmetic-right-shift bf 17))
    (is (equal (bits bf 31 0) #*11111111111111111111111111111111))

    (setf (bits bf 31 0) #*00111111111111111111111111111111)
    (setf bf (arithmetic-right-shift bf 10))
    (is (equal (bits bf 31 0) #*00000000000011111111111111111111))
    (setf bf (arithmetic-right-shift bf 5))
    (is (equal (bits bf 31 0) #*00000000000000000111111111111111))
    (setf bf (arithmetic-right-shift bf 17))
    (is (equal (bits bf 31 0) #*00000000000000000000000000000000))))

(test rotate-right
  (let ((bf (make-instance 'bitfield :width 32)))
    (setf (bits bf 31 0) #*11101111001001101010100010010001)
    (setf bf (rotate-right bf 10))
    (is (equal (bits bf 31 0) #*00100100011110111100100110101010))))

(test rotate-right-extended
  (let ((bf (make-instance 'bitfield :width 32)))
    (setf (bits bf 31 0) #*11101111001001101010100010010001)
    (setf bf (rotate-right-extended bf 1))
    (is (equal (bits bf 31 0) #*11110111100100110101010001001000))))

;; TODO: Test interactions between barrel shifter operations and
;; CPSR. Be mindful of possible timing constraints imposed by pipeline
;; model.
