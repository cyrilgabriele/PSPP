;;ADDED THE NEEDED PRE_GIVEN FUNCTIONS
;; FROM HERE...

;; Decorate function with pre and (optional) post 
;; processing functions; calling a wrapped 
;; function with argument :orig returns the
;; original function
;;
(defun decorate (f pre &optional (post #'identity))
  (lambda (&rest args)
    (cond ((equal args '(:orig)) f)
          (t (funcall post (apply f (apply pre args)))))))
		  
		  
;; Dispatcher: Make a function from a list of functions 
;; that returns the result of the first of these that 
;; returns a non-nil value
;;
(defun dispatch (&rest funcs)
  (lambda (&rest args)
    (if (null funcs) nil
        ;; else
        (let ((result (apply (car funcs) args)))
          (if result result
              ;; else
              (apply (apply #'dispatch (cdr funcs)) args))))))
			  
			  
;; Parse string to float if possible, else return NIL
;;
(defun parse-float (strg)
  (if (stringp strg)
      (with-input-from-string (s strg)
        (let ((res (read s)))
          (if (eq (type-of res) 'single-float) res nil)))))


;; Parse string to int if possible, else return NIL
;; gcy: behaves imo a bit strange => return of (parse-int "24") is:
;;24
;;2 
(defun parse-int (strg)
  (ignore-errors (parse-integer strg)))
  
  
(defmacro setfun (symb fun)
  `(prog1 ',symb (setf (symbol-function ',symb) ,fun)))
;; ... TO HERE
;; FOLLOWING, IS MY CODE

(setfun num (dispatch #'parse-int #'parse-float #'identity))
;;(print(num "8.5"))

(defun num-args (&rest args)
	(mapcar #'num args))
	
;;(print (num-args "3.5" "3" 6 10))
	
;; Does have some issues => Tests do not run 
;;(defun add-without-decorator (&rest args)
	;;(reduce (lambda (a b) (+ a b)) '(#'num-args args) :initial-value 0)
	
(defun addition (a b)
	(+ a b))

(defun add (&rest args)
	(decorate #'+ #'num-args))

;;(print "with reduce")
;;(print(add-without-decorator (num-args "3.5" "3" 6 10)))

;;GEHT NICHT: => WARUM?
;;(setfun add (decorate #'addition #'num-args))
;;GEHT NICHT: => WARUM?
;;(setfun add (decorate (lambda (a b) (+ a b)) #'num-args))

(setfun add (decorate #'+ #'num-args))
(print(add "3.5" "3" 6 10))

;;(print (add-without-decorator "3.5" "3" 6 10))