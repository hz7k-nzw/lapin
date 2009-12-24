;;(defun fact (n)
;;  (declare (fixnum n))
;;  (if (= n 0)
;;      1
;;    (* n (fact (- n 1)))))

(defun fact (n)
  (if (equalp n 0)
      1
    (times n (fact (difference n 1)))))

(defun test-fact (x &optional (n 5))
  (dotimes (i n) (time (fact x))))
