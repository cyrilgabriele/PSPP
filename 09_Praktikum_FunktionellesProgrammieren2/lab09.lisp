;; 1. Funktionale Problemlösung
;; helper functions provided from PSPP-Doc
(defun curry-n (f numargs)
  (lambda (&rest args)
  (if (>= (length args) numargs)
    (apply f args)
    (curry-n
      (lambda (&rest restargs) (apply f (append args restargs)))
      (- numargs (length args))))))

(defun partial (f &rest args)
  (lambda (&rest more-args)
    (apply f (append args more-args))))

;; Pipeline: Make a function that composes a 
;; sequence of functions 
;; 
(defun pipeline (&rest funcs)
   (lambda (&rest args)
    (car (reduce
           (lambda (ar f) (list (apply f ar)))
           funcs
           :initial-value args))))

(defmacro setfun (symb fun)
  `(prog1 ',symb (setf (symbol-function ',symb) ,fun)))

;; -----------------------------------------------------------------------------------------------------------------

;;BEGINN OF MY CODE
;; getprop without currying
(defun getprop-fn (key list)
(cdr (assoc key list)))

(print(getprop-fn :result '((:RESULT . "SUCCESS") (:INTERFACE-VERSION . "1.0.3"))))

;; getprop with currying
(setfun getprop (curry-n #'getprop-fn 2))

(setfun result (getprop :result)) ;; hier wird die Zurückgegebene Funktion (lambda function aus curry-n) in result gespeichert
(print (funcall #'result '((:RESULT . "SUCCESS")))) ;; hier wird die curryied function () mit dem 2. fehlenden Param (list) aufgerufen
;; result muss hier via funcall aufgerufen werden, zusätzlich gequoted, da es sonst als Variable interpretiert wird

;; Implementierung von weiteren Funktionen
(defun filter-fn (f seq)
  (remove-if-not f seq))

(defun reject-fn (f seq)
  (remove-if f seq))

(setfun getprop (curry-n #'getprop-fn 2))
(print (getprop :tasks (with-open-file (stream "tasks.lisp" :direction :input) (read stream))))
(print (getprop :tasks))
(print (defvar *tasks* (getprop :tasks (with-open-file (stream "tasks.lisp" :direction :input) (read stream)))))

;; here passed a function to filter-fn which checks if the value is greater than 3 => applied to the list
;; values which are greater than 3 are returned
(print (filter-fn (lambda (x) (< 3 x)) '(1 2 3 4 5)))

;; here passed a function to reject-fn which checks if the value is greater than 3 => applied to the list
;; values which are greater than 3 are rejected
(print (reject-fn (lambda (x) (< 3 x)) '(1 2 3 4 5)))

;; curryied versions of the functionns above
;; filter function => number after #'filter-fn indicates how many parameters has to be passed in total 
(setfun filter (curry-n #'filter-fn 2))
;; passing only the 1. argument/parameter
(setfun return_filter (filter (lambda (x) (< 3 x))))
;; return the curryied function of filter-fn
(print (funcall #'return_filter '(1 2 3 4 5)))

;; reject function
(setfun reject (curry-n #'reject-fn 2))
(setfun return_reject (reject (lambda (x) (< 3 x))))
(print (funcall #'return_reject '(1 2 3 4 5)))

;; prop-eq
(defun prop-eq (prop val)
  (pipeline (getprop prop) (partial #'equal val)))

(print (filter (prop-eq :member "Scott") *tasks*))
(print (filter (prop-eq :member "Scott")))

;; pick functions
(defun pick-fn (attrs obj)
  (remove-if-not #'(lambda (el) (member (car el) attrs)) obj))

(print (format t "~% pick-fn"))
(print (funcall #'pick-fn '(:title) '((:RESULT . "SUCCESS") (:INTERFACE-VERSION . "1.0.3") (:DUE-DATE . "17.11.2023") (:TITLE . "PSPP_Lab09"))))

(setfun pick (curry-n #'pick-fn 2))
(setfun pick-by-duedate (pick '(:due-date)))
(print (format t "~%pick curryied"))
(print (funcall #'pick-by-duedate '((:RESULT . "SUCCESS") (:INTERFACE-VERSION . "1.0.3") (:DUE-DATE . "17.11.2023"))))

;; implementation w/ forall function
(setfun forall (curry-n #'mapcar 2))
(print (format t "~%forall w/ pick"))
(print (forall (pick '(:due-date :title)) *tasks*))
