;;; Copyright (C) 2009 Kenji Nozawa
;;; This file is part of LAPIN.
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License along
;;; with this program; if not, write to the Free Software Foundation, Inc.,
;;; 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

;;(debug-mode)
(let ((*COMPILE-INPUT-DIR* "src")
      (*COMPILE-OUTPUT-DIR* "build/classes")
      (*COMPILE-CLASS-DIR* "build/classes")
      (files '("init.lisp" "init_compiler.lisp")))
  (format t "compile init files: start~%")
  (dolist (file files)
    (format t "compiling file: ~A/~A~%"
	    *COMPILE-INPUT-DIR* file)
    (format t "compile successful: ~A/~A~%"
	    *COMPILE-OUTPUT-DIR*
	    (compile-file file :GENERATE-CLASS-FILE T)))
  (format t "byte code was emitted in ~A/~A~%"
	  *COMPILE-CLASS-DIR* "lapin/function/subrs/gen")
  (format t "compile init files: end~%"))
