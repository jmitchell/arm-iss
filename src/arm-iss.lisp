;;;; arm-iss.lisp

(in-package #:arm-iss)

(defvar *register-width* 32)

(defclass register ()
  ((bitfield
    :initform (make-array *register-width*
                          :element-type 'bit)
    :type (simple-bit-vector *register-width*))))

(defgeneric bits (register hi lo))
(defgeneric (setf bits) (value register hi lo))
(defgeneric bit-at (register index))
(defgeneric (setf bit-at) (value register index))

(defmethod bits ((reg register) hi lo)
  (with-slots (bitfield) reg
    (reverse (subseq bitfield lo (1+ hi)))))

(defmethod (setf bits) (value (reg register) hi lo)
  (with-slots (bitfield) reg
    (setf (subseq bitfield lo (1+ hi))
          (reverse value)))
  value)

(defmethod bit-at ((reg register) index)
  (bit (bits reg index index) 0))

(defmethod (setf bit-at) (value (reg register) index)
  (setf (bits reg index index)
        (make-array 1
                    :element-type 'bit
                    :initial-element value)))


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
