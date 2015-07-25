;;;; arm-iss.lisp

(in-package #:arm-iss)

;; TODO: Develop interpretations of bitfields like signed and unsigned
;; integers.

;; TODO: Implement barrel shifter operations in terms of high-level
;; bitfield interpretations. Barrel shifter doesn't need to know about
;; registers; an intermediary device will lookup the appropriate
;; register's bitfield and relay those values to the barrel shifter.

;; TODO: Enforce cleaner abstraction layers in ASDF systems and
;; packages. Tests should operate on the highest-level interfaces of
;; any given abstraction, but this doesn't mean all low-level
;; abstraction interfaces (e.g. barrel shifter functions) should be
;; exported by the :arm-iss package.


(defclass bitfield ()
  ((width
    :initarg :width
    :reader width
    :initform (error "bitfield :width must be specified.")
    :type (integer 1 *))
   (bit-array
    :accessor bit-array
    :type simple-bit-vector)))

(defmethod initialize-instance :after
    ((bf bitfield) &rest args)
  (declare (ignore args))
  (unless (slot-boundp bf 'bit-array)
    (setf (slot-value bf 'bit-array)
          (make-array (width bf) :element-type 'bit))))

(defgeneric bits (bitfield hi lo))
(defgeneric (setf bits) (value bitfield hi lo))
(defgeneric bit-at (bitfield index))
(defgeneric (setf bit-at) (value bitfield index))

(defmethod bits ((bf bitfield) hi lo)
  (reverse (subseq (bit-array bf) lo (1+ hi))))

(defmethod (setf bits) (value (bf bitfield) hi lo)
  (setf (subseq (bit-array bf) lo (1+ hi))
        (reverse value))
  value)

(defmethod bit-at ((bf bitfield) index)
  (bit (bits bf index index) 0))

(defmethod (setf bit-at) (value (bf bitfield) index)
  (setf (bits bf index index)
        (make-array 1
                    :element-type 'bit
                    :initial-element value)))


(defvar *register-width* 32)

(defclass register (bitfield)
  ((width
    :initform *register-width*)))

(defclass program-status-register (register)
  ())

(defmethod initialize-instance :after
    ((psr program-status-register) &rest args)
  (declare (ignore args))
  (setf (mode psr) :svc))


(defvar *psr-flags*
  '((31 . :N)
    (30 . :Z)
    (29 . :C)
    (28 . :V)
    (7 . :I)
    (6 . :F)
    (5 . :T)))

(defun flag-bit-index (name)
  (let ((pair (rassoc name *psr-flags*)))
    (if pair
        (car pair)
        (error "Unsupported FLAG-NAME; must be one of *PSR-FLAGS*"))))

(defgeneric flag-state (psr flag-name))

(defmethod flag-state ((psr program-status-register) flag-name)
  (bit-at psr (flag-bit-index flag-name)))

(defgeneric (setf flag-state) (value psr flag-name))

(defmethod (setf flag-state) (value (psr program-status-register) flag-name)
  (setf (bit-at psr (flag-bit-index flag-name)) value))

(defvar *psr-modes*
  '((#*10000 . :usr)
    (#*10001 . :fiq)
    (#*10010 . :irq)
    (#*10011 . :svc)
    (#*10111 . :abt)
    (#*11011 . :und)
    (#*11111 . :sys)))

(defgeneric mode (psr))
(defgeneric (setf mode) (value psr))

(defmethod mode ((psr program-status-register))
  (let* ((mode-bits (bits psr 4 0))
         (pair (assoc mode-bits *psr-modes* :test #'equal)))
    (if pair
        (cdr pair)
        (error "PROGRAM-STATUS-REGISTER's mode bits are in an unrecognized state"))))

(defmethod (setf mode) (value (psr program-status-register))
  (let ((pair (rassoc value *psr-modes*)))
    (if pair
        (setf (bits psr 4 0) (car pair))
        (error "Unrecognized mode identifier"))))
