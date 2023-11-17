;; Lab 06 der Vorlesung PSPP - author: Cyril Gabriele
;; Aufgabe 3

;; vorgegebene lower-order function
(defun map-list (f lst)
	(if (null lst) nil
	;;else
	(cons (funcall f (car lst))
		(map-list f (cdr lst)))))

;;QUADRIERUNG
;; quadriert ein Element
(defun sqr (elmnt)
	(* elmnt elmnt))
	
;; quadriert jedes Element einer Liste
(defun list-sqr (lst) (map-list #'sqr lst))

;; VERDOPPELUNG
;; verdoppelt ein Element
(defun double_element (elmnt)
    (* 2 elmnt))
	
;; verdoppelt jedes Element einer Liste
(defun list-double-meineVersion (f lst)
	(map-list f lst))
	
(defun list-double (lst)
	(map-list #'double_element lst))

;; VORZEICHEN
;; gibt 1 für >= 0, sonst -1 zurück => Vorzeichen
(defun sign (elmnt)
    (if (< elmnt 0) -1 
    (if (> elmnt 0) 1 0)))
	
(defun list-sign (lst) (map-list #'sign lst))

;;AUSGABE
;; geht auch so mit lambda Ausdruck 
(print(map-list (lambda (n) (* n n)) '(1 2 3 4 5 6)))
(print (list-double '(1 2 3 4 5 6)))
(print(list-double-meineVersion 'double_element '(1 2 3 4 5 6)))
;; -----------------------------------------------------
(print(list-sqr '(1 2 3 4 5 6)))
;; -----------------------------------------------------
(print(list-sign '(5 2 -3 -1 0 3 -2)))


;; Aufgabe 4
(defun list-sum (lst)
	(if (null lst) 0
	;;else
	(+ (car lst) (list-sum(cdr lst)))))


(defun list-mult (lst)
	(if (null lst) 1
	;;else
	(* (car lst) (list-mult(cdr lst))))) 	

;; geht so leider nicht... 
;;(defun all-true (lst)
	;;(if (null (cdr lst) and ((not) equal lst nil))
	;;(all-true(cdr lst)) nil))
(defun all-true (lst)
    (if (null lst) T
        (and (car lst) (all-true(cdr lst))))
)


(print(list-sum '(10 20 7)))
(print(list-mult '(10 20 7)))
(print(all-true '(34 hallo (7))))
(print(all-true '(34 hallo ())))
