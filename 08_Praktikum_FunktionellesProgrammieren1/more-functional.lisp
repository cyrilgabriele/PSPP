;;
;; More Functional Lisp
;; Common Lisp Additions, V 1.3.1
;; Playing around with functional concepts 
;; Gerrit Burkert, 2012...2021
;;
;; Thanks to:
;; Michael Fogus: Functional JavaScript (O'Reilly)
;; ... and numerous other sources
;; ... for many valuable ideas
;;
;; No guarantee that everything works as intended :)
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MACRO SETFUN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; simplifies binding of a function to 
;; a symbol
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro setfun (symb fun)
  `(prog1 ',symb (setf (symbol-function ',symb) ,fun)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LAMBDA READER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; functional programming aficionados:
;; here is a lambda reader macro, now
;; you can use your preferred symbol
;; λ instead of lambda
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HINT: remove this part if your Lisp 
;; loader has problems with this special 
;; UTF-8 character
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Source:
; https://stackoverflow.com/questions/9557469/renaming-lambda-in-common-lisp
(defun lambda-reader (stream char)
  (declare (ignore char stream))
  'LAMBDA)

(set-macro-character #\λ #'lambda-reader)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CURRYING AND PARTIAL APPLICATION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Partial application of a function: return a 
;; function of the remaining arguments
;;
(defun partial (f &rest args)
  (lambda (&rest more-args)
    (apply f (append args more-args))))


;; Curry a function of two arguments
;;
(defun curry2 (f)
  (lambda (a)
    (lambda (b)
      (funcall f a b))))


;; Curry a function of three arguments
;;
(defun curry3 (f)
  (lambda (a)
    (lambda (b)
      (lambda (c)
        (funcall f a b c)))))


;; Curry a function of two arguments (right to left)
;;
(defun curry2r (f)
  (lambda (b)
    (lambda (a)
      (funcall f a b))))


;; Curry a function of three arguments (right to left)
;;
(defun curry3r (f)
  (lambda (c)
    (lambda (b)
      (lambda (a)
        (funcall f a b c)))))


;; Curry a function with numargs parameters: if the resulting 
;; function is called with fewer arguments, a curried function of 
;; the remaining arguments is returned
;;
(defun curry-n (f numargs)
  (lambda (&rest args)
    (if (>= (length args) numargs)
        (apply f args)
        (curry-n 
          (lambda (&rest restargs) (apply f (append args restargs)))
          (- numargs (length args))))))


;; Enhanced version of curry that allows placeholder arguments '_ to
;; be used for arguments not yet available (see examples below)
;;
(defun curry (f numargs)
  (labels 
    ((fill-args (seq values)
      (cond ((null values) seq)
            ((null seq) values)
            ((eq (car seq) '_) (cons (car values) (fill-args (cdr seq) (cdr values))))
            (t (cons (car seq) (fill-args (cdr seq) values)))))
     (take-args (n seq)
       (if (or (null seq) (<= n 0)) nil
           (cons (car seq) (take-args (- n 1) (cdr seq))))))
    (lambda (&rest args)
      (let* ((currargs (take-args numargs args))
             (countargs (- (length currargs) (count '_ args))))
        (if (>= countargs numargs)
            (apply f args)
            (curry
              (lambda (&rest restargs) (apply f (fill-args currargs restargs)))
              (- numargs countargs)))))))

;; Examples for curry
;;
; (setfun exptc (curry #'expt 2))
; (setfun square (exptc '_ 2))
; (square 3)
; (setfun cube (exptc '_ 3))
; (cube 3)
; 
; (defun adds (a b c) (+ a (* b 2) c))
; (setfun adds (curry #'adds 3))
; (funcall (adds) 1 2 3)
; (funcall (adds 1) 2 3)
; (funcall (adds 1 2) 3)
; (funcall (adds 1 '_ 3) 2)
; (funcall (adds '_ '_ 3) 1 2)
; (funcall (funcall (adds '_ '_ 3) '_ 2) 1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HELPER FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Take first n elements from a list
;;
(setfun take
  (curry (lambda (n seq)
           (labels 
               ((take-args (n seq)
                  (if (or (null seq) (<= n 0)) nil
                      (cons (car seq) (take-args (- n 1) (cdr seq))))))
             (take-args n seq))) 2)) 


;; For some special forms we prefer functions
;;
(defun && (m n) (and m n))
(defun || (m n) (or m n))


;; Make a list with a sequence of numbers
;; 
(defun range (start &optional to (step 1))
  (cond ((null to) (range 0 start step))
        ((> step 0) 
         (loop for n from start below to by step collect n))
        ((< step 0) 
         (loop for n from start above to by (- 0 step) collect n))
        (t nil)))


;; Merge two lists to an alist
;;
(defun zip-to-alist (lst1 lst2)
  (cond ((or (null lst1) (null lst2)) nil)
        (t (cons (cons (car lst1) (car lst2)) 
                 (zip-to-alist (cdr lst1) (cdr lst2))))))


;; Flatten list structure
;;
(defun flatten (seq)
  (cond ((null seq) nil)
        ((listp (car seq)) 
           (append (flatten (car seq)) (flatten (cdr seq))))
        (t (cons (car seq) (flatten (cdr seq))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PARAMETER HANDLING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Change function to accept list with parameters
;;
(defun splat (f)
  (lambda (arglist)
    (apply f arglist)))


;; Change function to accept individual parameters 
;; instead of a list
;;
(defun unsplat (f)
  (lambda (&rest args)
    (funcall f args)))


;; Switch first two parameters of the function 
;;
(defun switch-params (f) 
  (lambda (a b &rest args) 
    (apply f (cons b (cons a args)))))


;; Adapt a function with optional or rest parameters to exactly
;; n parameters
;;
(defun args (f n)
  (lambda (&rest args)
    (apply f (take n args))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MAP, REDUCE, FILTER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Mapping over a list (aka mapcar)
;; 
;; Iterative implementation
;  (defun map-list (f seq) 
;    (loop for el in seq collect (funcall f el)))
;;
;; Curried version of mapcar
(setfun map-list 
  (curry (lambda (f lst) (mapcar f lst)) 2))

;; Examples
;;
; (map-list #'numberp)
; (map-list #'numberp '(1 2 NIL t "hi")) 
; (funcall (map-list #'numberp) '(1 2 NIL t "hi"))


;; Reduce list from left
;;
(setfun reduce-list-left
  (curry (lambda (f init seq)
           (reduce f seq :initial-value init)) 3))

;; Examples
;;
; (reduce-list-left #'+)
; (reduce-list-left #'+ 0)
; (funcall (reduce-list-left #'+ 0) '(1 2 3 4))


;; Reduce list from right
;;
(setfun reduce-list-right
  (curry (lambda (f init seq)
           (reduce f seq :initial-value init :from-end t)) 3))


;; Reduce list: aliases
;;
(setfun reduce-list #'reduce-list-right)
(setfun foldl #'reduce-list-left)
(setfun foldr #'reduce-list-right)


;; Filter list with a predicate
;;
(setfun filter-list
  (curry (lambda (pred seq)
           (remove-if-not pred seq)) 2))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ITERATION AND MEMOIZATION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Do something repeatedly
;;
(defun repeatedly (times fun)
  (mapcar fun (range times)))


;; Create list by applying function repeatedly
;;
(defun iterate (fn init count &optional res)
  (let ((res (if (null res) (list init) res)))
    (cond ((<= count 0) (reverse res))
          (t (iterate fn init (- count 1) (cons (funcall fn (car res)) res))))))

;; Examples
;;
;(iterate #'1+ 0 20)
; --> (0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20)
;
;(iterate (lambda (n) (* 2 n)) 1 16)
; --> (1 2 4 8 16 32 64 128 256 512 1024 2048 4096 8192 16384 32768 65536)


;; Return always the same value
;;
(defun always (val)
  (lambda (&rest args)
    (declare (ignore args))
    val))


;; Make memoizing version of a function
;;
(defun memoize (fn)
  (let ((cache (make-hash-table :test #'equal)))
    (lambda (&rest args)
      (multiple-value-bind (val win) (gethash args cache)
        (if win val
            ;; else
            (setf (gethash args cache)
                  (apply fn args)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FUNCTION DECORATION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Decorate function with pre and (optional) post 
;; processing functions; calling a wrapped 
;; function with argument :orig returns the
;; original function
;;
(defun decorate (f pre &optional (post #'identity))
  (lambda (&rest args)
    (cond ((equal args '(:orig)) f)
          (t (funcall post (apply f (apply pre args)))))))


;; Print arguments to console (to be used with wrap-fn)
;;
(defun print-args (&rest args)
  (format t "Called with args ~S~%" args)
  args)


;; Print result to console (to be used with wrap-fn)
;;
(defun print-res (res)
  (format t "Result: ~S~%" res)
  res)

;; Examples
;;
;(decorate #'+ #'print-args #'print-res)
; --> function
;
;(funcall (decorate #'+ #'print-args #'print-res) 1 2)
; --> Called with args (1 2)
; --> Result: 3
; --> 3


;; Make a function with defaults for its arguments
;; 
(defun defaults (&rest defaults)
  (labels
      ((merge-defaults (seq defaults)
         (if (null defaults) seq
             (cons (if (or (null seq) (null (car seq))) (car defaults) (car seq))
                   (merge-defaults (cdr seq) (cdr defaults))))))
    (lambda (&rest args)
      (merge-defaults args defaults))))

;; Examples
;;
;(setfun pow (decorate #'expt (defaults 1 2)))
;(pow 2 5)
; --> 32
;(pow 5)
; --> 25
;(pow)
; --> 1


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FUNCTION COMBINATION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Simple composition of two functions
;;
(defun compose-simple (f g) 
  (lambda (&rest args) 
    (funcall f (apply g args))))


;; Compose two functions and optionally specify the number 
;; of arguments the function called first should consume
;;
(defun compose-funcs (f g &optional npar)
  (lambda (&rest args)
    (let ((nargs (if npar npar (length args))))
      (apply f (cons (apply g (subseq args 0 nargs))
                     (nthcdr nargs args))))))





;; Checks whether all of a sequence of functions
;; (validators) return true
;;
(defun checker (&rest validators)
  (lambda (elem)
    (every (lambda (check) (funcall check elem)) validators)))


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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; STRINGS AND I/O HELPERS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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


;; Parse string to float if possible, else return NIL
;;
(defun parse-float (strg)
  (if (stringp strg)
      (with-input-from-string (s strg)
        (let ((res (read s)))
          (if (eq (type-of res) 'single-float) res nil)))))


;; Parse string to int if possible, else return NIL
;;
(defun parse-int (strg)
  (ignore-errors (parse-integer strg)))


;; Read text file into string
;;
(defun file-to-string (path)
  (with-open-file (stream path)
    (let ((data (make-string (file-length stream))))
      (read-sequence data stream)
      data)))


;; Alternative implementation in case the Common Lisp used does not
;; support the read-sequence function
;;
; (defun file-to-string (path)
;   (with-open-file (stream path)
;     (apply #'concatenate 
;            (cons 'string 
;                  (loop
;                    for line = (read-line stream nil 'eof)
;                    until (eq line 'eof)
;                    collect (format nil "~A~%" line))))))

