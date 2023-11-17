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
			