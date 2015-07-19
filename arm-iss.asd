;;;; arm-iss.asd
;;;; arm-iss: Instruction set simulator for the ARM architecture
;;;; Copyright (C) 2015  Jacob Mitchell

;;;; This program is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version.

;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.

;;;; You should have received a copy of the GNU General Public License
;;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(in-package :asdf-user)

(defsystem #:arm-iss
  :description "Instruction set simulator for the ARM architecture"
  :author "Jacob Mitchell <jacob.d.mitchell@gmail.com>"
  :version (:read-file-form "version.sexp")
  :license "GPLv3"
  :serial t
  :pathname "src/"
  :components ((:file "package")
               (:file "arm-iss")))

(defsystem #:arm-iss/test
  :author "Jacob Mitchell <jacob.d.mitchell@gmail.com>"
  :version (:read-file-form "version.sexp")
  :depends-on (:arm-iss :fiveam)
  :pathname "tests/"
  :components ((:file "test")))

;; TODO(jake): Is this actually doing anything? The tests don't seem
;; to run when either system is loaded into the REPL using
;; QL:QUICKLOAD or ASDF:LOAD-SYSTEM.
(defmethod perform ((o test-op) (c (eql (find-system :arm-iss))))
  (load-system :arm-iss/test :force '(:arm-iss/test))
  (uiop:symbol-call :5am :run! :arm-iss))
