(defun range (&rest args)
	(cond 	((= (length args) 1) ;;DONE
				(if (= (first args) 0) nil
				(append (range (- (first args) 1)) (list(- (first args) 1))))
			)
			((= (length args) 2) ;;DONE
				(if (>= (first args) (second args)) nil
				(cons (first args) (range (+ (first args) 1) (second args))))
			)
			((= (length args) 3) ;;DONE
				(if (= (first args) (second args)) nil
				(if (and (< (first args) (second args)) (< (third args) 0)) nil
				(cons (first args) (range (+ (first args) (third args)) (second args) (third args)))))
			)))

;;eingeschränkte Funktion, wleche erweitert werden soll
(defun repeat (times value)
	(mapcar (lambda (n) value)
	(range times)))

;; erweiterte Funktion 
;; fun: Funktion, welche times-mal ausgeführt werden soll als Parameter
;; times: Anzahl Wiederholungen
(defun repeatedly (times fun)
	(mapcar fun (range times)))
	
(defun hello-list (n) 'hello)
;;call: (repeatedly 5 #'hello-list) 
;; needs to be called like this because in Lisp we can not bind a function to a symbol 
;; therefore: #'hello-list
;; could be also passed as a lambda function: 
;; => (repeatedly 5 (lambda (n) 'hello))

;;ID-Aufgabe:
(repeatedly 5 (lambda (n) (concatenate 'string "ID" (write-to-string n))))
;; ("ID0" "ID1" "ID2" "ID3" "ID4")

(defun always (n)
	(lambda (&rest args) n))

