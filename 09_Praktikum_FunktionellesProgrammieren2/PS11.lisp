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

;; Split string at character
;; 
(defun string-split (c strg)
  (let ((end (position c strg)))
    (cond (end (cons (subseq strg 0 end)
            (string-split c (subseq strg (+ end 1)))))
          (t (list (subseq strg 0))))))

;; Example
;;
;(string-split #\, "eins,zwei,drei")
; --> ("eins" "zwei" "drei")

;; -----------------------------------------------------------------------------------------------------------------

;;BEGINN OF MY CODE
;; getprop without currying
(defun getprop-fn (key list)
(cdr (assoc key list)))

;; getprop with currying
(setfun getprop (curry-n #'getprop-fn 2))

(setfun result (getprop :result)) ;; hier wird die Zurückgegebene Funktion (lambda function aus curry-n) in result gespeichert
;; result muss hier via funcall aufgerufen werden, zusätzlich gequoted, da es sonst als Variable interpretiert wird

;; Implementierung von weiteren Funktionen
(defun filter-fn (f seq)
  (remove-if-not f seq))

(defun reject-fn (f seq)
  (remove-if f seq))

(setfun getprop (curry-n #'getprop-fn 2))

;; curryied versions of the functionns above
;; filter function => number after #'filter-fn indicates how many parameters has to be passed in total 
(setfun filter (curry-n #'filter-fn 2))
;; passing only the 1. argument/parameter
(setfun return_filter (filter (lambda (x) (< 3 x))))
;; return the curryied function of filter-fn

;; reject function
(setfun reject (curry-n #'reject-fn 2))
(setfun return_reject (reject (lambda (x) (< 3 x))))

;; prop-eq
(defun prop-eq (prop val)
  (pipeline (getprop prop) (partial #'equal val)))

;; pick functions
(defun pick-fn (attrs obj)
  (remove-if-not #'(lambda (el) (member (car el) attrs)) obj))

(setfun pick (curry-n #'pick-fn 2))
(setfun pick-by-duedate (pick '(:due-date)))

;; implementation w/ forall function
(setfun forall (curry-n #'mapcar 2))

;; implementation of date-to-universal
;; dates in US format!!!

(defun parse-date-int (items)
  (map 'list #'(lambda (x) (parse-integer x)) items))


(defun date-to-universal (date-string)
  (encode-universal-time 00 00 00
    (second (parse-date-int (string-split #\/ date-string)))
    (first (parse-date-int (string-split #\/ date-string)))
    (third (parse-date-int (string-split #\/ date-string)))))


(defun sort-by-fn (f seq)
  (sort (copy-list seq)
    (lambda (a b) (< (funcall f a) (funcall f b)))))

(setfun sort-by (curry-n #'sort-by-fn 2))

(defun open-tasks (name)
  (pipeline
    (getprop :tasks)
    (filter (prop-eq :member name))
    (reject (prop-eq :complete t))
    (forall (pick '(:id :due-date :title :priority)))
    (sort-by (pipeline (getprop :due-date) #'date-to-universal))))

;; test call => if NOT commented: (print (forall (pick '(:due-date :title)) *tasks*)) must get commented out and vice versa!!!

