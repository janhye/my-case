;;(my-case (var :test #'string=)
;;	(("test" "test2")
;;	 (do-something1))
;;	("test3"
;;	 (do-something2))
;;	(t
;;	 (do-something3)))
;;
;;          ||
;;          V
;;
;;(cond ((or (string= "test" var) (string= "test2" var))
;;       (do-something1))
;;      ((string= "test3" var)
;;       (do-something2))
;;      (t
;;       (do-something3)))
;;
(defmacro my-case ((var &key (test #'eq)) &body body)
  (let ((macro-val (gensym))
	(macro-test-fn (gensym)))
    `(let ((,macro-val ,var)
	   (,macro-test-fn ,test))
       (cond ,@(mapcar #'(lambda (x)
			   (let ((con (car x)))
			     (cond ((consp con)
				    `((or ,@(mapcar #'(lambda (y)
							`(funcall ,macro-test-fn ,macro-val ,y))
						    con)) ,@(cdr x)))
				   ((eq con 't) x)
				   (t
				    `((funcall ,macro-test-fn ,macro-val ,con) ,@(cdr x))))))
		       body)))))
