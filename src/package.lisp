;;;; package.lisp

(defpackage #:arm-iss
  (:use #:cl)
  (:export #:bitfield
           #:width
           #:bits
           #:bit-at
           #:bitfield->unsigned-integer
           #:unsigned-integer->bitfield

           #:register
           #:program-status-register
           #:*psr-flags*
           #:flag-state
           #:mode

           #:logical-shift-left
           #:logical-shift-right
           #:arithmetic-right-shift
           #:rotate-right
           #:rotate-right-extended))
