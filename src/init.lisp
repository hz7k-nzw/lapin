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

(setq *PACKAGE* (find-package "LISP"))

(export 'backquote)
(defmacro backquote (form)
  (SYS::BQ-EXPAND form))

(export 'in-package)
(defmacro in-package (pkgname)
  `(setq *package*
	 (or (find-package ',pkgname)
	     (error "package not found"))))

(export 'let)
(defmacro let (params &body body)
  `((lambda ,(mapcar #'(lambda (x) (if (atom x) x (car x)))
		     params)
      ,@body)
    ,@(mapcar #'(lambda (x) (if (atom x) nil (cadr x)))
	      params)))

(export 'let*)
(defmacro let* (params &body body)
  (if (null params)
      `(let () ,@body)
    `(let (,(car params))
       (let* ,(cdr params) ,@body))))

(export 'mapcan)
(defun mapcan (fn &rest lsts)
  (apply #'nconc (apply #'mapcar fn lsts)))

(export 'mapcon)
(defun mapcon (fn &rest lsts)
  (apply #'nconc (apply #'maplist fn lsts)))

;;(defun __pair__ (x)
;;  (if (null x) nil
;;    (cons (cons (car x) (cadr x)) (__pair__ (cddr x)))))
;;
;;(export 'psetq)
;;(defmacro psetq (&rest args)
;;  (if (oddp (length args))
;;      (error "odd number of arguments" args))
;;  (let ((pairs (__pair__ args)))
;;    (let ((syms (mapcar #'(lambda (x) (gensym)) pairs)))
;;      `(let ,(mapcar #'list syms (mapcar #'cdr pairs))
;;	 (setq ,@(mapcan #'list (mapcar #'car pairs) syms))))))

(export 'cond)
(defmacro cond (&rest args)
  (if (null args)
      nil
    (let ((clause (car args)))
      (if (cdr clause)
	  `(if ,(car clause)
	       (progn ,@(cdr clause))
	     (cond ,@(cdr args)))
	`(or ,(car clause)
	     (cond ,@(cdr args)))))))

(export 'unless)
(defmacro unless (arg &body body)
  `(if (not ,arg) (progn ,@body)))

(export 'when)
(defmacro when (arg &body body)
  `(if ,arg (progn ,@body)))

(export 'prog1)
(defmacro prog1 (form1 &rest moreforms)
  (let ((g (gensym)))
    `(let ((,g ,form1)) ,@moreforms ,g)))

(export 'prog2)
;;(defmacro prog2 (form1 form2 &rest moreforms)
;;  (let ((g (gensym)))
;;    `(let () (progn ,form1 (let ((,g ,form2)) ,@moreforms ,g)))))
(defmacro prog2 (form1 form2 &rest moreforms)
  (let ((g (gensym)))
    `(progn ,form1 (let ((,g ,form2)) ,@moreforms ,g))))

(export 'do)
(defmacro do (varclauselist exitclause &body body)
  (SYS::DO-EXPAND varclauselist exitclause body 'LET 'PSETQ))

(export 'do*)
(defmacro do* (varclauselist exitclause &body body)
  (SYS::DO-EXPAND varclauselist exitclause body 'LET* 'SETQ))

(export 'dolist)
(defmacro dolist ((var lstform &optional result) &body body)
  (let ((g (gensym)))
    `(do ((,var nil)
	  (,g ,lstform (cdr ,g)))
	 ((endp ,g) (let ((,var nil)) ,result))
       (setq ,var (car ,g))
       ,@body)))

(export 'dotimes)
(defmacro dotimes ((var cntform &optional result) &body body)
  (let ((g (gensym)))
    `(do ((,var 0 (1+ ,var))
	  (,g ,cntform))
	 ((>= ,var ,g) ,result)
       ;; my dotimes supports fixnum var only...
       (declare (fixnum ,var ,g))
       ,@body)))

(export 'while)
(defmacro while (test &body body)
  `(do ()
       ((not ,test))
     ,@body))

(export 'push)
(defmacro push (val arg)
  (or (symbolp arg) (error "arg must be symbol" arg))
  `(setq ,arg (cons ,val ,arg)))

(export 'pop)
(defmacro pop (arg)
  (or (symbolp arg) (error "arg must be symbol" arg))
  `(prog1 (car ,arg) (setq ,arg (cdr ,arg))))

(export 'time)
;;(defmacro time (&body body)
;;  (let ((t1 (gensym))
;;	(t2 (gensym))
;;	(ret (gensym)))
;;    `(let (,t1 ,t2 ,ret)
;;       (setq ,t1 (sys::current-time))
;;       (setq ,ret (progn ,@body))
;;       (setq ,t2 (sys::current-time))
;;       (format t "elapsed time: ~A[msec]~%" (difference ,t2 ,t1))
;;       ,ret)))
(defun SYS::DEFAULT-ELAPSED-TIME-HANDLER (time)
  (format t "elapsed time: ~A[msec]~%" time))

(defvar SYS::*ELAPSED-TIME-HANDLER* #'SYS::DEFAULT-ELAPSED-TIME-HANDLER)

(defmacro time (&body body)
  (let ((time (gensym)))
    `(let ((,time (SYS::CURRENT-TIME)))
       (prog1 (progn ,@body)
	 (setq ,time (difference (SYS::CURRENT-TIME) ,time))
	 (funcall SYS::*ELAPSED-TIME-HANDLER* ,time)))))

(export 'destructuring-bind)
(defmacro destructuring-bind (lambdalist exp &body body)
  ;; my lambda expression can accept any nested lambda list...
  `(apply #'(lambda ,lambdalist ,@body) ,exp))

(export 'subrcall)
(defmacro subrcall (type fun &rest args)
  `(funcall ,fun ,@args))

;;; Supports for maclisp array functions

(export 'array)
(defmacro array (name type &rest args)
  `(*array ',name ',type ,@args))

(export 'arraycall)
;;(defmacro arraycall (type arr &rest args)
;;  `(funcall ,arr ,@args))
(defmacro arraycall (type arr &rest args)
  `(*array-get ,arr ,@args))

(export 'store)
(defmacro store (arrayref value)
  (let ((g (gensym))
	(form (macroexpand arrayref)))
    (or (symbolp (car form))
	(error "illegal arrayref" arrayref))
    (cond ((eq (car form) '*array-get) ; (*array-get array . args)
	   `(let ((,g ,value))
	      (*array-set ,(cadr form)
			  ,g
			  ,@(cddr form))))
	  ((eq (car form) 'funcall) ; (funcall array . args)
	   `(let ((,g ,value))
	      (*array-set ,(cadr form)
			  ,g
			  ,@(cddr form))))
	  ((eq (car form) 'apply) ; (apply array . arg1 arg2 ... restargs)
	   `(let ((,g ,value))
	      (apply #'*array-set
		     ,(cadr form)
		     ,g
		     ,@(cddr form))))
	  (t ; (array . args)
	   `(let ((,g ,value))
	      (*array-set #',(car form)
			  ,g
			  ,@(cdr form)))))))

;;; Supports for some, every, reduce

(export 'some)
(defun some (pred lst &rest lsts)
  (some-helper pred (cons lst lsts)))

(defun some-helper (pred lstofargs)
  (let (ret)
    (do ((lsts lstofargs (lsts-cdr lsts)))
	((lsts-endp lsts) nil)
      ;;(format t "some: lsts=~S~%" lsts)
      (when (setq ret (apply pred (lsts-car lsts)))
	(return ret)))))

(export 'every)
(defun every (pred lst &rest lsts)
  (every-helper pred (cons lst lsts)))

(defun every-helper (pred lstofargs)
  (do ((lsts lstofargs (lsts-cdr lsts)))
      ((lsts-endp lsts) t)
    ;;(format t "every: lsts=~S~%" lsts)
    (when (null (apply pred (lsts-car lsts)))
	(return nil))))

(defun lsts-endp (lsts)
  (dolist (l lsts) (when (endp l) (return t))))

(defun lsts-car (lsts) (mapcar #'car lsts))

(defun lsts-cdr (lsts) (mapcar #'cdr lsts))

(export 'reduce)
(defun reduce (fn lst &key from-end ;start end
		   (initial-value nil initial-value?))
  (cond
   ((and (not initial-value?) (null lst))
    (funcall fn))
   (t
    (if from-end
	(progn
	  (when (not initial-value?)
	    (setq initial-value (car (last lst))
		  lst (butlast lst))) ; lst is not empty!
	  (reduce-helper-right-associative
	   fn initial-value lst))
      (progn
	(when (not initial-value?)
	  (setq initial-value (car lst)
		lst (cdr lst))) ; lst is not empty!
	(reduce-helper-left-associative
	 fn initial-value lst))))))

(defun reduce-helper-left-associative (fn val lst)
  (if (null lst)
      val
    (reduce-helper-left-associative
     fn
     (funcall fn val (car lst))
     (cdr lst))))

(defun reduce-helper-right-associative (fn val lst)
  (if (null lst)
      val
    (funcall
     fn
     (car lst)
     (reduce-helper-right-associative fn val (cdr lst)))))

(export 'with-open-stream)
(defmacro with-open-stream ((var stream) &body body)
  (multiple-value-bind (decl body)
      (SYS::FIND-DECLS body)
    `(let ((,var ,stream))
       ,@decl
       (unwind-protect
	   (progn ,@body)
	 (close ,var)))))

(export 'with-open-file)
(defmacro with-open-file ((stream filename &rest options) &body body)
  `(with-open-stream
    (,stream (open ,filename ,@options))
    ,@body))

(export 'with-input-from-string)
(defmacro with-input-from-string ((var string &key start end) &body body)
  `(with-open-stream
    (,var (make-string-input-stream ,string, start, end))
    ,@body))
;;(defmacro with-input-from-string ((var string &key start end) &body body)
;;  `(let ((,var (make-string-input-stream ,string, start, end)))
;;     ,@body))

(export 'with-output-to-string)
(defmacro with-output-to-string ((var) &body body)
  `(with-open-stream
    (,var (make-string-output-stream))
    ,@body
    (get-output-stream-string ,var)))
;;(defmacro with-output-to-string ((var) &body body)
;;  `(let ((,var (make-string-output-stream)))
;;     ,@body
;;     (get-output-stream-string ,var)))

;;; functions used for debugging

(export 'trace-mode)
(export 'trace)
(defun trace-mode ()
  (setq *log-level* 1
	*print-backtrace-verbose* t
	*print-exception-verbose* t)
  'trace)

(export 'debug-mode)
(export 'debug)
(defun debug-mode ()
  (setq *log-level* 2
	*print-backtrace-verbose* t
	*print-exception-verbose* t)
  'debug)

(export 'normal-mode)
(export 'normal)
(defun normal-mode ()
  (setq *log-level* 3
	*print-backtrace-verbose* nil
	*print-exception-verbose* nil)
  'normal)

(export 'print-memory)
(defun print-memory ()
  (format t "total: ~A[KB], free: ~A[KB]~%"
	  (quotient (SYS::TOTAL-MEMORY) 1000)
	  (quotient (SYS::FREE-MEMORY) 1000)))

(export 'use-bcel)
(export 'bcel)
(defun use-bcel ()
  (setq SYS::*BYTE-CODE-GENERATOR-CLASSNAME*
	"lapin.comp.bcel.BCELByteCodeGenerator")
  'bcel)

(export 'use-asm)
(export 'asm)
(defun use-asm ()
  (setq SYS::*BYTE-CODE-GENERATOR-CLASSNAME*
	"lapin.comp.asm.ASMByteCodeGenerator")
  'asm)

(export 'set-log-out)
(defun set-log-out (&optional (file "lapin.log"))
  (setq *log-out* (open file
			:direction :output
			:if-does-not-exist :create)))

(export 'dump-system-properties)
(defun dump-system-properties (&optional (stream t))
  (dolist (key (SYS::SYSTEM-PROPERTY-KEYS))
    (format stream "~S = ~S~%" key (SYS::GET-SYSTEM-PROPERTY key))))

;;; Imports some useful functions from java API.

(defpackage "JAVA" (:use "LISP"))
(SYS::IMPORT-SUBRS "lapin.function.subrs.JavaSubrs" 'subr "JAVA" t)

;;; Loads resource for compiler.

(SYS::LOAD-RESOURCE "/init_compiler")
