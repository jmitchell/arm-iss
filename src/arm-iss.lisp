;;;; arm-iss.lisp

(in-package #:arm-iss)

(defmacro assert-in-range (val lo hi)
  `(assert (and (>= ,val ,lo)
                (<= ,val ,hi))
           nil
           "~a must be between ~a and ~a, inclusive" ',val ,lo ,hi))

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


(defgeneric bitfield->unsigned-integer (bitfield))

(defmethod bitfield->unsigned-integer ((bf bitfield))
  (loop
     for b across (reverse (bits bf (1- (width bf)) 0))
     for i = 0 then (1+ i)
     sum (if (zerop b)
             0
             (ash 1 i))))

(defgeneric unsigned-integer->bitfield (n &key width))

(defmethod unsigned-integer->bitfield (n &key (width *register-width*))
  (assert (typep n '(integer 0 *))
          nil
          "N must be a non-negative integer")
  (assert (typep width '(integer 1 *))
          nil
          "WIDTH must be a positive integer")
  (let ((bf (make-instance 'bitfield :width width)))
    (setf (bits bf (1- width) 0)
          (reverse
           (concatenate 'simple-bit-vector
                       (loop
                          repeat width
                          for v = n then (ash v -1)
                          when (> v 0)
                          collect (if (evenp v) 0 1)))))
    bf))

;; TODO: Develop conversion methods between bitfields and signed
;; integers.



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


;; BARREL SHIFTER OPERATIONS

(defgeneric logical-shift-left (bf shift-amount))
(defgeneric logical-shift-right (bf shift-amount))
(defgeneric arithmetic-right-shift (bf shift-amount))
(defgeneric rotate-right (bf shift-amount))
(defgeneric rotate-right-extended (bf carry))

(defmethod logical-shift-left ((bf bitfield) shift-amount)
  (assert-in-range shift-amount 0 (1- (width bf)))
  (let* ((new-bf (make-instance 'bitfield :width (width bf)))
         (hi (- (width bf) shift-amount))
         (bits-to-copy (bits bf (1- hi) 0)))
    (setf (bits new-bf (1- (width bf)) shift-amount)
          bits-to-copy)
    ;; TODO: verify condition flag conformance.
    new-bf))

(defmethod logical-shift-right ((bf bitfield) shift-amount)
  (assert-in-range shift-amount 1 (width bf))
  (let* ((new-bf (make-instance 'bitfield :width (width bf)))
         (bits-to-copy (bits bf (1- (width bf)) shift-amount)))
    (setf (bits new-bf (- (1- (width bf)) shift-amount) 0)
          bits-to-copy)
    ;; TODO: verify condition flag conformance.
    new-bf))

(defmethod arithmetic-right-shift ((bf bitfield) shift-amount)
  (assert-in-range shift-amount 1 (width bf))
  (let* ((new-bf (logical-shift-right bf shift-amount))
         (w (width bf))
         (sticky-index (1- w))
         (sticky-bit (bit-at bf sticky-index)))
    (setf (bits new-bf sticky-index (- w shift-amount))
          (make-array shift-amount :element-type 'bit :initial-element sticky-bit))
    ;; TODO: verify condition flag conformance.
    new-bf))

(defmethod rotate-right ((bf bitfield) shift-amount)
  (assert-in-range shift-amount 1 (1- (width bf)))
  (let* ((w (width bf))
         (hi-src (bits bf (1- w) shift-amount))
         (lo-src (bits bf (1- shift-amount) 0))
         (new-bf (make-instance 'bitfield :width w)))
    ;; TODO: verify condition flag conformance.
    (setf (bits new-bf (1- (- w shift-amount)) 0) hi-src
          (bits new-bf (1- w) (- w shift-amount)) lo-src)
    new-bf))

(defmethod rotate-right-extended ((bf bitfield) carry)
  (let* ((new-bf (logical-shift-right bf 1)))
    ;; TODO: verify condition flag conformance.
    (setf (bit-at new-bf (1- (width bf))) carry)
    new-bf))
