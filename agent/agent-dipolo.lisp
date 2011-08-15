;;; modulo con los agentes que juegan

(defun white-ia-play (s d)
  "ejecuta el movimiento elijido como mejor para el jugador blanco por la busqueda con profundidad {d} dado el estado {s}"
  (do-decision (alphabeta-decision s d #'expand-white #'expand-black #'game-over #'heur-dipolo-w)))

(defun black-ia-play (s d)
  "ejecuta el movimiento elijido como mejor para el jugador negro por la busqueda con profundidad {d} dado el estado {s}"
  (do-decision (alphabeta-decision s d #'expand-black #'expand-white #'game-over #'heur-dipolo-b)))
