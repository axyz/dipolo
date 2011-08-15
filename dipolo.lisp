;;; modulo principal con la función "start" que inicia el juego

(load "aux")
(load "./search/heuristic")
(load "./search/search")
(load "./interface/")
(load "./agent/")

(defvar state nil)

(defun start ()
  "función que inicia el juego"
  (let ((choice (menu)))
    (cond 
     ((= choice 1) (play))
     ((= choice 2) (format t "exited")))))

(defun play ()
  "realiza el menu de juego"
  (let ((choice (menu-game)))
    (cond 
     ((= choice 1) (print (human-vs-ia 9998)))
     ((= choice 2) (print (human-vs-ia 2)))
     ((= choice 3) (print (ia-vs-ia))))))

(defun human-vs-ia (d)
  "empieza un juego human vs IA con profundidad de busqueda {d}"
  (initialize-board)
  (loop 
   (if (game-over state) (return (game-over state)))
   (print-board-g state)
   (setq state (cons (eval (human-move state)) nil))
   (if (game-over state) (return (game-over state)))
   (print-board-g state)
   (setq state `(,(black-ia-play state d) . nil))
   (if (game-over state) (return (game-over state)))))
	
(defun ia-vs-ia ()
  "empieza un juego IA vs IA"
  (initialize-board)
  (loop 
   (if (game-over state) (return (game-over state)))
   (setq state `(,(white-ia-play state 9998) . nil))
   (print-board-g state)
   (if (game-over state) (return (game-over state)))
   (setq state `(,(black-ia-play state 9998) . nil))
   (print-board-g state)
   (if (game-over state) (return (game-over state)))))

(defconstant *initial-state* '((((3 1 12)) ((4 8 12))) . nil)) ;es el estado inicial de la tabla

(defun game-over (s)
  "revuelve 'b si en el estado {s} el negro gana, 'w si gana el blanco, 'pb si gana el negro por falta de movimientos blancos, pw si gana el blanco por falta de movimientos negros o nil si ninguno haya todavia ganado"
  (block nil (if (equal (car (car s)) nil) (return 'b))
	 (if (equal (cadr (car s)) nil) (return 'w))
	 (if (equal (expand-black s) nil) (return 'pw))
	 (if (equal (expand-white s) nil) (return 'pb))
	 (return nil)))

(defun initialize-board ()
  "pone la tabla de juego en la configuración inicial"
  (setq state *initial-state*))

;empieza el juego cuando se carga el codigo
(start)



