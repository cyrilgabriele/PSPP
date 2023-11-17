;;
;; Some fragments for the PSPP exercises 
;; Functional Programming (1)
;; (more in more-functional.lisp)
;;

(defmacro setfun (symb fun)
    `(prog1 ',symb (setf (symbol-function ',symb) ,fun)))


(defun repeat (times value)
  (mapcar (lambda (n) value)
          (range times)))
    
          
(defun repeatedly (times fun)
  (mapcar fun (range times)))


(setfun num (dispatch #'parse-int #'parse-float #'identity))


(defun num-args (&rest args)
  (mapcar #'num args))



(defun partial (f &rest args)
  (lambda (&rest more-args)
    (apply f (append args more-args))))
    
    
(defun factorial (n &optional (fact 1))
  (if (<= n 1) fact
      (factorial (- n 1) (* n fact))))


(defun trampoline (fun &rest args)
  (let ((result (apply fun args)))
    (loop while (functionp result) do
      (setq result (funcall result)))
    result))

