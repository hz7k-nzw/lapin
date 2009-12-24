;;(defun cat (file)
;;  (with-open-file
;;   (stream file)
;;   ;;(format t "stream: ~S~%" stream)
;;   (prog
;;    (line)
;;    next
;;    (setq line (read-line stream nil nil))
;;    (when (null line)
;;      (return 'done))
;;    (format t "~A~%" line)
;;    (go next))))

(defun cat (file)
  (with-open-file
   (stream file)
   ;;(format t "stream: ~S~%" stream)
   (do ((line (read-line stream nil nil)
	      (read-line stream nil nil)))
       ((null line) 'done)
     (format t "~A~%" line))))

;;(defun cat (file &aux stream)
;;  (unwind-protect
;;      (prog (line)
;;	    (setq stream (open file))
;;	    next
;;	    (setq line (read-line stream nil nil))
;;	    (when (null line) (return 'done))
;;	    (format t "~A~%" line)
;;	    (go next))
;;    (close stream)))

;;(defun cat (file &aux stream)
;;  (unwind-protect
;;      (progn
;;	(setq stream (open file))
;;	(do ((line (read-line stream nil nil)
;;		   (read-line stream nil nil)))
;;	    ((null line) 'done)
;;	  (format t "~A~%" line)))
;;    ;;(format t "close stream for file ~S: ~S~%" file stream)
;;    (close stream)))

