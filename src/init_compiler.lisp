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

(defpackage "COMP" (:use "LISP"))
(in-package "COMP")

;;; Compiler macros

(define-compiler-macro max (&whole form &rest args)
  (if (<= (length args) 2) form
    (reduce #'(lambda (x y) `(max ,x ,y)) args)))

(define-compiler-macro min (&whole form &rest args)
  (if (<= (length args) 2) form
    (reduce #'(lambda (x y) `(min ,x ,y)) args)))

;;(define-compiler-macro not (&whole form &rest args)
;;  `(SYS::FALSE ,@args))

(define-compiler-macro funcall (&whole form fun &rest args)
  (if (and (listp fun) (eq (car fun) 'function))
      `(,(cadr fun) ,@args)
    form))

;;(define-compiler-macro and (&rest args)
;;  (cond ((null args)
;;	 t)
;;	((cdr args)
;;	 `(if ,(car args) (and ,@(cdr args))))
;;	(t
;;	 (car args))))
;;
;;(define-compiler-macro or (&optional first &rest rest)
;;  (if (null rest)
;;      first
;;    (let ((g (gensym)))
;;      `(let ((,g ,first))
;;	 (if ,g
;;	     ,g
;;	   (or ,@rest))))))
;;
;;(define-compiler-macro cadr (&whole form &rest args)
;;  `(car (cdr ,@args)))

;;; Special handlers for compiler

(defmacro conv (form) `(sys::convert-form ',form))

(defun process-constant-arg (arg)
  (setq arg (SYS::CONVERT-FORM arg))
  (cond
   ((and (SYS::CONSTANT-FORM-P arg)
	 (numberp (SYS::CONSTANT-VALUE arg)))
    (values t ; const-p
	    (SYS::CONSTANT-VALUE arg) ; num
	    arg)) ; arg (converted)
   (t
    (values nil ; const-p
	    nil ; num
	    arg)))) ; arg (converted)

(defun concat-constant-args
  (args fn identity &aux processed-num processed-args)
  (dolist (arg args)
    (multiple-value-bind (const-p num arg)
	(process-constant-arg arg)
      (cond
       (const-p
	(when (null processed-num)
	  (setq processed-num identity))
	(setq processed-num (funcall fn processed-num num)))
       (t
	(unless (null processed-num)
	  (push `',processed-num processed-args)
	  (setq processed-num nil))
	(push arg processed-args)))))
  ;;(format t "concat: ~S ~S~%" processed-num processed-args)
  (cond
   ((null processed-args)
    (values processed-num
	    nil))
   ((null processed-num)
    (values nil
	    (nreverse processed-args)))
   (t
    (values nil
	    (nreverse (cons `',processed-num
			    processed-args))))))

(defmacro define-c-handler (name type handler)
  (or (eq type 'SYS::C-HANDLER-CONV-FORM) ; type for pass 1
      (eq type 'SYS::C-HANDLER-BUILD-INSTS) ; type for pass 2
      (error "unsupported type specified" type))
  `(progn
     'compile
     (putprop ',name t 'SYS::C-HANDLER)
     (putprop ',name ,handler ',type)))

;;; c-handler defining macro for ADD and MUL operators.
(eval-when (compile eval)
  (defmacro define-c-handler-pass1-a (name op identity)
    `(progn
       'compile
       (defun ,name (form &aux (args (cdr form)))
	 (if (null args)
	     `',(,op)
	   (multiple-value-bind (processed-num processed-args)
	       (concat-constant-args args #',op ,identity)
	     (if (null processed-args)
		 `',processed-num
	       (if (= (length processed-args) 1)
		   `(,',op ,@processed-args)
		 (reduce #'(lambda (x y) `(,',op ,x ,y))
			 processed-args))))))
       (define-c-handler
	 ,op SYS::C-HANDLER-CONV-FORM #',name))))

;;;(defun c-handler-pass1-add (form &aux (args (cdr form)))
;;;  (if (null args)
;;;      `',(plus)
;;;    (multiple-value-bind (processed-num processed-args)
;;;	(concat-constant-args args #'plus 0)
;;;      ;;(format t "plus: ~S ~S~%" processed-num processed-args)
;;;      (if (null processed-args)
;;;	  `',processed-num
;;;	(if (= (length processed-args) 1)
;;;	    `(plus ,@processed-args)
;;;	  (reduce #'(lambda (x y) `(plus ,x ,y))
;;;		  processed-args))))))
;;;(define-c-handler
;;; plus SYS::C-HANDLER-CONV-FORM #'c-handler-pass1-add)
(define-c-handler-pass1-a c-handler-pass1-add plus 0)

;;;(defun c-handler-pass1-add-fixnum (form &aux (args (cdr form)))
;;;  (if (null args)
;;;      `',(+)
;;;    (multiple-value-bind (processed-num processed-args)
;;;	(concat-constant-args args #'+ 0)
;;;      ;;(format t "+: ~S ~S~%" processed-num processed-args)
;;;      (if (null processed-args)
;;;	  `',processed-num
;;;	(if (= (length processed-args) 1)
;;;	    `(+ ,@processed-args)
;;;	  (reduce #'(lambda (x y) `(+ ,x ,y))
;;;		  processed-args))))))
;;;(define-c-handler
;;; + SYS::C-HANDLER-CONV-FORM #'c-handler-pass1-add-fixnum)
(define-c-handler-pass1-a c-handler-pass1-add-fixnum + 0)

;;;(defun c-handler-pass1-add-flonum (form &aux (args (cdr form)))
;;;  (if (null args)
;;;      `',(+$)
;;;    (multiple-value-bind (processed-num processed-args)
;;;	(concat-constant-args args #'+$ 0.0)
;;;      ;;(format t "+$: ~S ~S~%" processed-num processed-args)
;;;      (if (null processed-args)
;;;	  `',processed-num
;;;	(if (= (length processed-args) 1)
;;;	    `(+$ ,@processed-args)
;;;	  (reduce #'(lambda (x y) `(+$ ,x ,y))
;;;		  processed-args))))))
;;;(define-c-handler
;;; +$ SYS::C-HANDLER-CONV-FORM #'c-handler-pass1-add-flonum)
(define-c-handler-pass1-a c-handler-pass1-add-flonum +$ 0.0)

;;;(defun c-handler-pass1-mul (form &aux (args (cdr form)))
;;;  (if (null args)
;;;      `',(times)
;;;    (multiple-value-bind (processed-num processed-args)
;;;	(concat-constant-args args #'times 1)
;;;      ;;(format t "times: ~S ~S~%" processed-num processed-args)
;;;      (if (null processed-args)
;;;	  `',processed-num
;;;	(if (= (length processed-args) 1)
;;;	    `(times ,@processed-args)
;;;	  (reduce #'(lambda (x y) `(times ,x ,y))
;;;		  processed-args))))))
;;;(define-c-handler
;;; times SYS::C-HANDLER-CONV-FORM #'c-handler-pass1-mul)
(define-c-handler-pass1-a c-handler-pass1-mul times 1)

;;;(defun c-handler-pass1-mul-fixnum (form &aux (args (cdr form)))
;;;  (if (null args)
;;;      `',(*)
;;;    (multiple-value-bind (processed-num processed-args)
;;;	(concat-constant-args args #'* 1)
;;;      ;;(format t "*: ~S ~S~%" processed-num processed-args)
;;;      (if (null processed-args)
;;;	  `',processed-num
;;;	(if (= (length processed-args) 1)
;;;	    `(* ,@processed-args)
;;;	  (reduce #'(lambda (x y) `(* ,x ,y))
;;;		  processed-args))))))
;;;(define-c-handler
;;; * SYS::C-HANDLER-CONV-FORM #'c-handler-pass1-mul-fixnum)
(define-c-handler-pass1-a c-handler-pass1-mul-fixnum * 1)

;;;(defun c-handler-pass1-mul-flonum (form &aux (args (cdr form)))
;;;  (if (null args)
;;;      `',(*$)
;;;    (multiple-value-bind (processed-num processed-args)
;;;	(concat-constant-args args #'*$ 1.0)
;;;      ;;(format t "*$: ~S ~S~%" processed-num processed-args)
;;;      (if (null processed-args)
;;;	  `',processed-num
;;;	(if (= (length processed-args) 1)
;;;	    `(*$ ,@processed-args)
;;;	  (reduce #'(lambda (x y) `(*$ ,x ,y))
;;;		  processed-args))))))
;;;(define-c-handler
;;; *$ SYS::C-HANDLER-CONV-FORM #'c-handler-pass1-mul-flonum)
(define-c-handler-pass1-a c-handler-pass1-mul-flonum *$ 1.0)

;;; c-handler defining macro for SUB and DIV operators.
(eval-when (compile eval)
  (defmacro define-c-handler-pass1-b (name op identity op-inverse)
    `(progn
       'compile
       (defun ,name (form &aux (args (cdr form)))
	 (if (null args)
	     `',(,op)
	   (let ((firstarg (car args))
		 (restargs (cdr args)))
	     (multiple-value-bind (const-p num arg)
		 (process-constant-arg firstarg)
	       (if (null restargs)
		   (cond
		    (const-p
		     `',(,op num))
		    (t
		     `(,',op ,arg)))
		 (multiple-value-bind (processed-num processed-args)
		     (concat-constant-args restargs #',op-inverse ,identity)
		   (if (null processed-args)
		       (cond
			(const-p
			 `',(,op num processed-num))
			(t
			 `(,',op ,arg ',processed-num)))
		     (reduce #'(lambda (x y) `(,',op ,x ,y))
			     processed-args :initial-value arg))))))))
       (define-c-handler
	 ,op SYS::C-HANDLER-CONV-FORM #',name))))

;;;(defun c-handler-pass1-sub (form &aux (args (cdr form)))
;;;  (if (null args)
;;;      `',(difference)
;;;    (let ((firstarg (car args))
;;;	  (restargs (cdr args)))
;;;      (multiple-value-bind (const-p num arg)
;;;	  (process-constant-arg firstarg)
;;;	(if (null restargs)
;;;	    (cond
;;;	     (const-p
;;;	      `',(difference num))
;;;	     (t
;;;	      `(difference ,arg)))
;;;	  (multiple-value-bind (processed-num processed-args)
;;;	      (concat-constant-args restargs #'plus 0)
;;;	    ;;(format t "difference: ~S ~S~%" processed-num processed-args)
;;;	    (if (null processed-args)
;;;		(cond
;;;		 (const-p
;;;		  `',(difference num processed-num))
;;;		 (t
;;;		  `(difference ,arg ',processed-num)))
;;;	      (reduce #'(lambda (x y) `(difference ,x ,y))
;;;		      processed-args :initial-value arg))))))))
;;;(define-c-handler
;;; difference SYS::C-HANDLER-CONV-FORM #'c-handler-pass1-sub)
(define-c-handler-pass1-b c-handler-pass1-sub difference 0 plus)

;;;(defun c-handler-pass1-sub-fixnum (form &aux (args (cdr form)))
;;;  (if (null args)
;;;      `',(-)
;;;    (let ((firstarg (car args))
;;;	  (restargs (cdr args)))
;;;      (multiple-value-bind (const-p num arg)
;;;	  (process-constant-arg firstarg)
;;;	(if (null restargs)
;;;	    (cond
;;;	     (const-p
;;;	      `',(- num))
;;;	     (t
;;;	      `(- ,arg)))
;;;	  (multiple-value-bind (processed-num processed-args)
;;;	      (concat-constant-args restargs #'+ 0)
;;;	    ;;(format t "-: ~S ~S~%" processed-num processed-args)
;;;	    (if (null processed-args)
;;;		(cond
;;;		 (const-p
;;;		  `',(- num processed-num))
;;;		 (t
;;;		  `(- ,arg ',processed-num)))
;;;	      (reduce #'(lambda (x y) `(- ,x ,y))
;;;		      processed-args :initial-value arg))))))))
;;;(define-c-handler
;;; - SYS::C-HANDLER-CONV-FORM #'c-handler-pass1-sub-fixnum)
(define-c-handler-pass1-b c-handler-pass1-sub-fixnum - 0 +)

;;;(defun c-handler-pass1-sub-flonum (form &aux (args (cdr form)))
;;;  (if (null args)
;;;      `',(-$)
;;;    (let ((firstarg (car args))
;;;	  (restargs (cdr args)))
;;;      (multiple-value-bind (const-p num arg)
;;;	  (process-constant-arg firstarg)
;;;	(if (null restargs)
;;;	    (cond
;;;	     (const-p
;;;	      `',(-$ num))
;;;	     (t
;;;	      `(-$ ,arg)))
;;;	  (multiple-value-bind (processed-num processed-args)
;;;	      (concat-constant-args restargs #'+$ 0.0)
;;;	    ;;(format t "-$: ~S ~S~%" processed-num processed-args)
;;;	    (if (null processed-args)
;;;		(cond
;;;		 (const-p
;;;		  `',(-$ num processed-num))
;;;		 (t
;;;		  `(-$ ,arg ',processed-num)))
;;;	      (reduce #'(lambda (x y) `(-$ ,x ,y))
;;;		      processed-args :initial-value arg))))))))
;;;(define-c-handler
;;; -$ SYS::C-HANDLER-CONV-FORM #'c-handler-pass1-sub-flonum)
(define-c-handler-pass1-b c-handler-pass1-sub-flonum -$ 0.0 +$)

;;;(defun c-handler-pass1-div (form &aux (args (cdr form)))
;;;  (if (null args)
;;;      `',(quotient)
;;;    (let ((firstarg (car args))
;;;	  (restargs (cdr args)))
;;;      (multiple-value-bind (const-p num arg)
;;;	  (process-constant-arg firstarg)
;;;	(if (null restargs)
;;;	    (cond
;;;	     (const-p
;;;	      `',(quotient num))
;;;	     (t
;;;	      `(quotient ,arg)))
;;;	  (multiple-value-bind (processed-num processed-args)
;;;	      (concat-constant-args restargs #'times 1)
;;;	    ;;(format t "quotient: ~S ~S~%" processed-num processed-args)
;;;	    (if (null processed-args)
;;;		(cond
;;;		 (const-p
;;;		  `',(quotient num processed-num))
;;;		 (t
;;;		  `(quotient ,arg ',processed-num)))
;;;	      (reduce #'(lambda (x y) `(quotient ,x ,y))
;;;		      processed-args :initial-value arg))))))))
;;;(define-c-handler
;;; quotient SYS::C-HANDLER-CONV-FORM #'c-handler-pass1-div)
(define-c-handler-pass1-b c-handler-pass1-div quotient 1 times)

;;;(defun c-handler-pass1-div-fixnum (form &aux (args (cdr form)))
;;;  (if (null args)
;;;      `',(//)
;;;    (let ((firstarg (car args))
;;;	  (restargs (cdr args)))
;;;      (multiple-value-bind (const-p num arg)
;;;	  (process-constant-arg firstarg)
;;;	(if (null restargs)
;;;	    (cond
;;;	     (const-p
;;;	      `',(// num))
;;;	     (t
;;;	      `(// ,arg)))
;;;	  (multiple-value-bind (processed-num processed-args)
;;;	      (concat-constant-args restargs #'* 1)
;;;	    ;;(format t "//: ~S ~S~%" processed-num processed-args)
;;;	    (if (null processed-args)
;;;		(cond
;;;		 (const-p
;;;		  `',(// num processed-num))
;;;		 (t
;;;		  `(// ,arg ',processed-num)))
;;;	      (reduce #'(lambda (x y) `(// ,x ,y))
;;;		      processed-args :initial-value arg))))))))
;;;(define-c-handler
;;; // SYS::C-HANDLER-CONV-FORM #'c-handler-pass1-div-fixnum)
(define-c-handler-pass1-b c-handler-pass1-div-fixnum // 1 *)

;;;(defun c-handler-pass1-div-flonum (form &aux (args (cdr form)))
;;;  (if (null args)
;;;      `',(//$)
;;;    (let ((firstarg (car args))
;;;	  (restargs (cdr args)))
;;;      (multiple-value-bind (const-p num arg)
;;;	  (process-constant-arg firstarg)
;;;	(if (null restargs)
;;;	    (cond
;;;	     (const-p
;;;	      `',(//$ num))
;;;	     (t
;;;	      `(//$ ,arg)))
;;;	  (multiple-value-bind (processed-num processed-args)
;;;	      (concat-constant-args restargs #'*$ 1.0)
;;;	    ;;(format t "//$: ~S ~S~%" processed-num processed-args)
;;;	    (if (null processed-args)
;;;		(cond
;;;		 (const-p
;;;		  `',(//$ num processed-num))
;;;		 (t
;;;		  `(//$ ,arg ',processed-num)))
;;;	      (reduce #'(lambda (x y) `(//$ ,x ,y))
;;;		      processed-args :initial-value arg))))))))
;;;(define-c-handler
;;; //$ SYS::C-HANDLER-CONV-FORM #'c-handler-pass1-div-flonum)
(define-c-handler-pass1-b c-handler-pass1-div-flonum //$ 1.0 *$)

;;; c-handler defining macro for COMP operators.
(eval-when (compile eval)
  (defmacro define-c-handler-pass2-a (name op op-fixnum op-flonum)
    `(progn
       'compile
       (defun ,name (form &aux (args (cdr form)))
	 (let ((arg-types (SYS::CALC-ARG-TYPES args)))
	   (cond ((every #'SYS::FIXNUM-TYPE-P arg-types)
		  (SYS::COMPILE-FORM `(,',op-fixnum ,@args)))
		 ((every #'SYS::FLONUM-TYPE-P arg-types)
		  (SYS::COMPILE-FORM `(,',op-flonum ,@args)))
		 (t
		  ;;(SYS::COMPILE-FORM form))))) ; -> infinite loop!
		  (SYS::COMPILE-CALL ',op args)))))
       (define-c-handler
	 ,op SYS::C-HANDLER-BUILD-INSTS #',name))))

(define-c-handler-pass2-a
  c-handler-pass2-eq = SYS::|EQ:FIXNUM| SYS::|EQ:FLONUM|)
(define-c-handler-pass2-a
  c-handler-pass2-gt > SYS::|GT:FIXNUM| SYS::|GT:FLONUM|)
(define-c-handler-pass2-a
  c-handler-pass2-lt < SYS::|LT:FIXNUM| SYS::|LT:FLONUM|)
(define-c-handler-pass2-a
  c-handler-pass2-ge >= SYS::|GE:FIXNUM| SYS::|GE:FLONUM|)
(define-c-handler-pass2-a
  c-handler-pass2-le <= SYS::|LE:FIXNUM| SYS::|LE:FLONUM|)

;;; c-handler defining macro for NOT and NULL operators.
(eval-when (compile eval)
  (defmacro define-c-handler-pass2-b (name op)
    `(progn
       'compile
       (defun ,name (form &aux (args (cdr form)))
	 (let ((arg-types (SYS::CALC-ARG-TYPES args)))
	   ;; assert that (length args) == (length arg-types) == 1
	   (cond ((SYS::PREDICATE-TYPE-P (car arg-types))
		  (SYS::COMPILE-FORM `(SYS::FALSE ,@args)))
		 (t
		  (SYS::COMPILE-CALL ',op args)))))
       (define-c-handler
	 ,op SYS::C-HANDLER-BUILD-INSTS #',name))))

(define-c-handler-pass2-b c-handler-pass2-not not)
(define-c-handler-pass2-b c-handler-pass2-null null)

;;; c-handler for FIXNUM operators.
(progn
  'compile
  (defun c-handler-pass2-fixnum (form &aux (args (cdr form)))
    (let ((arg-types (SYS::CALC-ARG-TYPES args)))
      ;; assert that (length args) == (length arg-types) == 1
      (cond ((SYS::PREDICATE-TYPE-P (car arg-types))
	     (error "cannot convert predicate to fixnum" form))
	    ((SYS::FIXNUM-TYPE-P (car arg-types))
	     (SYS::COMPILE-FORM `(SYS::|IDENTITY:FIXNUM| ,@args)))
	    ((SYS::FLONUM-TYPE-P (car arg-types))
	     (SYS::COMPILE-FORM `(SYS::FLONUM-TO-FIXNUM ,@args)))
	    ((SYS::CHARACTER-TYPE-P (car arg-types))
	     (SYS::COMPILE-FORM `(SYS::CHARACTER-TO-FIXNUM ,@args)))
	    (t
	     (SYS::COMPILE-CALL 'fixnum args)))))
  (define-c-handler
    fixnum SYS::C-HANDLER-BUILD-INSTS #'c-handler-pass2-fixnum))

;;; c-handler for FLONUM operators.
(progn
  'compile
  (defun c-handler-pass2-flonum (form &aux (args (cdr form)))
    (let ((arg-types (SYS::CALC-ARG-TYPES args)))
      ;; assert that (length args) == (length arg-types) == 1
      (cond ((SYS::PREDICATE-TYPE-P (car arg-types))
	     (error "cannot convert predicate to flonum" form))
	    ((SYS::FIXNUM-TYPE-P (car arg-types))
	     (SYS::COMPILE-FORM `(SYS::FIXNUM-TO-FLONUM ,@args)))
	    ((SYS::FLONUM-TYPE-P (car arg-types))
	     (SYS::COMPILE-FORM `(SYS::|IDENTITY:FLONUM| ,@args)))
	    ((SYS::CHARACTER-TYPE-P (car arg-types))
	     (error "cannot convert character to flonum" form))
	    (t
	     (SYS::COMPILE-CALL 'flonum args)))))
  (define-c-handler
    flonum SYS::C-HANDLER-BUILD-INSTS #'c-handler-pass2-flonum))

;;; c-handler for CHARACTER operators.
(progn
  'compile
  (defun c-handler-pass2-character (form &aux (args (cdr form)))
    (let ((arg-types (SYS::CALC-ARG-TYPES args)))
      ;; assert that (length args) == (length arg-types) == 1
      (cond ((SYS::PREDICATE-TYPE-P (car arg-types))
	     (error "cannot convert predicate to character" form))
	    ((SYS::FIXNUM-TYPE-P (car arg-types))
	     (SYS::COMPILE-FORM `(SYS::FIXNUM-TO-CHARACTER ,@args)))
	    ((SYS::FLONUM-TYPE-P (car arg-types))
	     (error "cannot convert flonum to character" form))
	    ((SYS::CHARACTER-TYPE-P (car arg-types))
	     (SYS::COMPILE-FORM `(SYS::|IDENTITY:CHARACTER| ,@args)))
	    (t
	     (SYS::COMPILE-CALL 'character args)))))
  (define-c-handler
    character SYS::C-HANDLER-BUILD-INSTS #'c-handler-pass2-character))

