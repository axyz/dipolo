;;; modulo con las euristicas

(defun heur-dipolo-w (node)
  "revuelve el valor heuristico de {node} (mas grande es en favor del blanco)"
  (let ((s (car node)))
    (cond 
     ((equal (game-over node) 'w) +inf)
     ((equal (game-over node) 'b) -inf)
     ((equal (game-over node) 'pw) +inf)
     ((equal (game-over node) 'pb) -inf)) 
    (+ 
     (* 0.45 (+ (apply #'+ (mapcar #'(lambda (x) (third x)) (car s))) 
	       (apply #'+ (mapcar #'(lambda (x) (third x)) (cadr s))))) ;diferencia de numero entre piezas blancas y negras
     (* 0.25 (/ (apply #'+ (mapcar #'(lambda (x) (third x)) (car s))) (+ (length (car s)) 0.1))) ;media de altitud de las torres blanca
     (* 0.25 (- (/ (apply #'+ (mapcar #'(lambda (x) (second x)) (car s))) (+ (length (car s)) 0.1)))) ;penalizacion de posiciones demasiado adelanteras
     (* 0.05 (random 12))))) ;componente aleatoria

(defun heur-dipolo-b (node)
  "revuelve el valor heuristico de {node} (mas grande es en favor del negro)"
  (let ((s (car node)))
    (cond 
     ((equal (game-over node) 'w) -inf)
     ((equal (game-over node) 'b) +inf)
     ((equal (game-over node) 'pw) -inf)
     ((equal (game-over node) 'pb) +inf)) 
    (+ 
     (* 0.45 (+ (apply #'+ (mapcar #'(lambda (x) (third x)) (cadr s))) 
	       (apply #'+ (mapcar #'(lambda (x) (third x)) (car s))))) ;diferencia de numero entre piezas blancas y negras
     (* 0.25 (/ (apply #'+ (mapcar #'(lambda (x) (third x)) (cadr s))) (+ (length (cadr s)) 0.1))) ;media de altitud de las torres blanca
     (* 0.25 (/ (apply #'+ (mapcar #'(lambda (x) (second x)) (car s))) (+ (length (car s)) 0.1)))
     (* 0.05 (random 12)))))
