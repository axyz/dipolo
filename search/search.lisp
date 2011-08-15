;;; modulo con los algoritmos de busqueda

(defconstant +inf most-positive-double-float)
(defconstant -inf most-negative-double-float)

(defun minimax (s depth expand-fn1 expand-fn2 game-over-fn heur-fn)
  "función que revuelve el valor heuristico de el estado {s}; expand-fn1 es la función que expande los estados que se pudieran haber hacendo todos los movimientos posibles del jugador que mueve primero, expand-fn2 es el mismo po el segundo jugador, game-over-fn es la función que determina si un nodo es hoja, heur-fn es la función que calcula el valor heuristico de un nodo"
  (return-from minimax (alphabeta s depth -inf +inf expand-fn1 expand-fn2 game-over-fn heur-fn)))

(defun alphabeta (s depth a b expand-fn1 expand-fn2 game-over-fn heur-fn)
  "función de apoyo a minimax"
  (let ((expand expand-fn1)
	(expand2 expand-fn2))
    (if (or (funcall game-over-fn s) (= depth 0))
	(return-from alphabeta (funcall heur-fn s)))
    (if (oddp depth) (setq expand expand-fn2) (setq expand expand-fn1))
    (loop for node in (funcall expand s) do
	  (setq a (max a (- (alphabeta node (- depth 1) (- b) (- a) expand expand2 game-over-fn heur-fn))))
	  (if (>= b a) (return-from alphabeta a)))
    a))

(defun alphabeta-decision (s depth expand-fn1 expand-fn2 game-over-fn heur-fn)
  "revuelve la acción elijida por minimax en forma de que se puede llamar con la función {do-decision}"
  (cdr (bigger-return #'(lambda (n) (minimax n depth expand-fn1 expand-fn2 game-over-fn heur-fn)) (funcall expand-fn1 s))))

