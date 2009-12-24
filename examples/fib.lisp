;;(declare (fixnum (fib fixnum)))
(defun fib (n)
  ;;(declare (fixnum n))
  (declare (fixnum (fib fixnum)))
  (if (< n 2) n
  ;;(if (<= n 2) 1
    (+ (fib (- n 1)) (fib (- n 2)))))

;;(defun test-fib (x &optional (n 5))
;;  (dotimes (i n) (time (fib x))))

(defun test-fib (x &optional (n 5)
		   &aux (*elapsed-times* nil)
		        (*old-handler* sys::*elapsed-time-handler*))
  (declare (special *old-handler* *elapsed-times*))
  (let ((sys::*elapsed-time-handler*
	 #'(lambda (time)
	     (funcall *old-handler* time)
	     (push time *elapsed-times*))))
    (dotimes (i n) (time (fib x)))
    (unless (null *elapsed-times*)
      (format t "avg: ~S[msec]~%"
	      (quotient (apply #'plus *elapsed-times*)
			(length *elapsed-times*))))))

