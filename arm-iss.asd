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
  :author "Jacob Mitchell <jmitchell@member.fsf.org>"
  :version (:read-file-form "version.sexp")
  :license "GPLv3"
  :serial t
  :pathname "src/"
  :components ((:file "package")
               (:file "arm-iss")))

(defsystem #:arm-iss/test
  :author "Jacob Mitchell <jmitchell@member.fsf.org>"
  :version (:read-file-form "version.sexp")
  :depends-on (:arm-iss :fiveam)
  :pathname "tests/"
  :components ((:file "test")))
