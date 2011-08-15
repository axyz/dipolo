;;; colección de funciónes auxiliares

(defun rm (el lst)
  "revuelve una lista como {lst} pero sin los elementos iguales a {el}"                                   
  (let ((ris nil))
    (loop for e in lst do
	  (if (not (equal e el)) (setq ris (append ris (list e)))))
    ris))

(defun add-white-tower (s tower)
  "revuelve un estado de juego como {s} pero con {tower} de mas entre las piezas blancas"
  (append (list (append (car s) (list tower)) (cadr s))))

(defun add-black-tower (s tower)
  "revuelve un estado de juego como {s} pero con {tower} de mas entre las piezas negras"
  (append (list (car s)) (list (append (cadr s) (list tower)))))

(defun remove-white-tower (s tower)
  "revuelve un estado de juego como {s} pero quitando {tower} de las piezas blancas"
  (let ((ris s))
    (loop for to in (car s) do
	  (if (equal to tower) 
	      (setq ris (cons (rm to (car s)) (list (cadr s)))))) 
    ris))

(defun remove-black-tower (s tower)
  "revuelve un estado de juego como {s} pero quitando {tower} de las piezas negras"
  (let ((ris s))
    (loop for to in (cadr s) do
	  (if (equal to tower) 
	      (setq ris (cons (car s) (list (rm to (cadr s)))))))
    ris))

(defun whitep (s tower)
  "revuelve t si en el estado {s} la {tower} es blanca, si es negra o no existe revuelve nil"
  (loop for to in (car s) do
	(if (equal to tower) (return t))))

(defun blackp (s tower)
  "revuelve t si en el estado {s} la {tower} es negra, si es blanca o no existe revuelve nil"
  (loop for to in (cadr s) do
	(if (equal to tower) (return t))))

(defun adversaryp (s tower1 tower2)
  "revuelve {t} si {tower1} y {tower2} son adversarias, {nil} en lo contrario"
  (if (or (and (whitep s tower1) (blackp s tower2))
	  (and (blackp s tower1) (whitep s tower2)))
      (return-from adversaryp t))
  nil)

(defun distance (tower1 tower2)
  "revuelve la distancia entre {tower1} y {tower2}"
  (let ((ris 0)
	(x1 (car tower1))
	(y1 (second tower1))
	(x2 (car tower2))
	(y2 (second tower2)))
    (if (not (= 0 (abs (- x1 x2))))
	(setq ris (abs (- x1 x2)))
      (setq ris (abs (- y1 y2))))
    ris))

(defun move (s tower x y n)
  "mueve la {tower} en la posición {x,y} con {n} piezas"
  (block nil (let ((ris s)
		   (old-tower-x (car tower))
		   (old-tower-y (second tower))
		   (old-tower-n (- (third tower) n)))
	       (if (< old-tower-n 0) (return))
	       (cond 
		((whitep s tower)
		 (setq ris (remove-white-tower ris tower))
		 (setq ris (add-white-tower ris `(,x ,y ,n)))
		 (if (not (= 0 old-tower-n)) (setq ris (add-white-tower ris `(,old-tower-x ,old-tower-y ,old-tower-n))))
		 (return ris))
		((blackp s tower)
		 (setq ris (remove-black-tower ris tower))
		 (setq ris (add-black-tower ris `(,x ,y ,n)))
		 (if (not (= 0 old-tower-n)) (setq ris (add-black-tower ris `(,old-tower-x ,old-tower-y ,old-tower-n))))
		 (return ris)))
	       nil)))

(defun eat (s tower1 tower2)
  "{tower1} come {tower2} y se revuelve un estado como {s} modificado de conseguencia"
  (block nil (let ((ris s))
	       (cond
		((not (adversaryp s tower1 tower2)) (return nil))
		((whitep s tower1)
		 (setq ris (remove-black-tower ris tower2))
		 (setq ris (move ris tower1 (car tower2) (second tower2) (distance tower1 tower2)))
		 (return ris))
		((blackp s tower1)
		 (setq ris (remove-white-tower ris tower2))
		 (setq ris (move ris tower1 (car tower2) (second tower2) (distance tower1 tower2)))
		 (return ris)))
	       nil)))

(defun increase-tower (s tower1 tower2)
  "exegue el movimiento de spostamiento de piezas desde una torre a un otra del mismo color"
  (let ((ris s)
	(tower2-heigth (third tower2)))
    (if (whitep s tower1)
	(progn (setq ris (remove-white-tower ris tower1)) (setq ris (remove-white-tower ris tower2))
	       (setq ris (add-white-tower ris `(,(car tower2) ,(second tower2) ,(+ (third tower2) (distance tower1 tower2)))))
	       (if (not (= 0 (- (third tower1) (distance tower1 tower2)))) (setq ris (add-white-tower ris `(,(car tower1) ,(second tower1) ,(- (third tower1) (distance tower1 tower2))))))
	       (return-from increase-tower ris)))
    (if (blackp s tower2)
	(progn (setq ris (remove-black-tower ris tower1)) (setq ris (remove-black-tower ris tower2))
	       (setq ris (add-black-tower ris `(,(car tower2) ,(second tower2) ,(+ (third tower2) (distance tower1 tower2)))))
	       (if (not (= 0 (- (third tower1) (distance tower1 tower2)))) (setq ris (add-black-tower ris `(,(car tower1) ,(second tower1) ,(- (third tower1) (distance tower1 tower2))))))
	       (return-from increase-tower ris))) 
    nil))

(defun action (s tower x y)
  "revuelve un estado derivado de {s} despues que se mueva la {tower} en la posición {x,y} si puede, y si puede comer... come"
  (let ((ris s)
	(n-tower (third tower))
	(tower-lst (append (car s) (cadr s))))
    (loop for to in tower-lst do
	  (let ((to-x (car to))
		(to-y (second to))
		(to-n (third to)))
	    (if (and (= to-x x) (= to-y y)) 
		(progn 
		  (if (adversaryp s tower to)
		      (progn 
			(if (and (>= (third tower) (distance tower `(,x ,y nil))) 
				 (>= (distance tower `(,x ,y nil)) to-n))
			    (if (setq ris (eat ris tower to)) 
				(return-from action ris)))))
		  (if (and (> (- y (second tower)) 0) 
			   (whitep s tower) 
			   (not (adversaryp s tower to)))
		      (if (setq ris (increase-tower ris tower to)) 
			  (return-from action ris)))
		  (if (and (< (- y (second tower)) 0) 
			   (blackp s tower)
			   (not (adversaryp s tower to)))
		      (if (setq ris (increase-tower ris tower to)) 
			  (return-from action ris)))
		  (return-from action nil)))))
    (if (and (> (- y (second tower)) 0) 
	     (whitep s tower))
	(if (setq ris (move ris tower x y (distance tower `(,x ,y nil)))) 
	    (return-from action ris)))
    (if (and (< (- y (second tower)) 0) 
	     (blackp s tower))
	(if (setq ris (move ris tower x y (distance tower `(,x ,y nil)))) 
	    (return-from action ris))) 
    nil))

;;; todas las acciónes posibles ;;;

(defun move-n (s tower step)
  (block nil (if (oddp step) (return nil))
	 (if (> (+ (second tower) step) 8) (return nil))
	 (if (> step (third tower)) (return nil))
	 (return (action s tower (car tower) (+ (second tower) step)))))

(defun move-s (s tower step)
  (block nil (if (oddp step) (return nil))
	 (if (< (- (second tower) step) 1) (return nil))
	 (if (> step (third tower)) (return nil))
	 (return (action s tower (car tower) (- (second tower) step)))))

(defun move-w (s tower step)
  (block nil (if (oddp step) (return nil))
	 (if (< (- (car tower) step) 1) (return nil))
	 (if (> step (third tower)) (return nil))
	 (return (action s tower (- (car tower) step) (second tower)))))

(defun move-e (s tower step)
  (block nil (if (oddp step) (return nil))
	 (if (> (+ (car tower) step) 8) (return nil))
	 (if (> step (third tower)) (return nil))
	 (return (action s tower (+ (car tower) step) (second tower)))))

(defun move-nw (s tower step)
  (block nil (if (or (< (- (car tower) step) 1) (> (+ (second tower) step) 8)) (return nil))
	 (if (> step (third tower)) (return nil))
	 (return (action s tower (- (car tower) step) (+ (second tower) step)))))

(defun move-ne (s tower step)
  (block nil (if (or (> (+ (car tower) step) 8) (> (+ (second tower) step) 8)) (return nil))
	 (if (> step (third tower)) (return nil))
	 (return (action s tower (+ (car tower) step) (+ (second tower) step)))))

(defun move-sw (s tower step)
  (block nil (if (or (< (- (car tower) step) 1) (< (- (second tower) step) 1)) (return nil))
	 (if (> step (third tower)) (return nil))
	 (return (action s tower (- (car tower) step) (- (second tower) step)))))

(defun move-se (s tower step)
  (block nil (if (or (> (+ (car tower) step) 8) (< (- (second tower) step) 1)) (return nil))
	 (if (> step (third tower)) (return nil))
	 (return (action s tower (+ (car tower) step) (- (second tower) step)))))

;;; fin de las acciónes posibles ;;;

(defun expand-white (node)
  "expande todos los posible nodos hijos de {s} cuando es el turno del blanco"
  (let ((ris nil)
	(s (car node)))
    (loop for tower in (car s) do
	  (loop for step from 1 to (third tower) do
		(push `(,(move-n s tower step) . (,#'move-n ,(car state) ,tower ,step)) ris)
		(if (equal (car (car ris)) nil) (pop ris))
		(push `(,(move-nw s tower step) . (,#'move-nw ,(car state) ,tower ,step)) ris)
		(if (equal (car (car ris)) nil) (pop ris))
		(push `(,(move-ne s tower step) . (,#'move-ne ,(car state) ,tower ,step)) ris)
		(if (equal (car (car ris)) nil) (pop ris))
		(push `(,(move-w s tower step) . (,#'move-w ,(car state) ,tower ,step)) ris)
		(if (equal (car (car ris)) nil) (pop ris))
		(push `(,(move-e s tower step) . (,#'move-e ,(car state) ,tower ,step)) ris)
		(if (equal (car (car ris)) nil) (pop ris))				
		(push `(,(move-s s tower step) . (,#'move-s ,(car state) ,tower ,step)) ris)
		(if (equal (car (car ris)) nil) (pop ris))
		(push `(,(move-sw s tower step) . (,#'move-sw ,(car state) ,tower ,step)) ris)
		(if (equal (car (car ris)) nil) (pop ris))
		(push `(,(move-se s tower step) . (,#'move-se ,(car state) ,tower ,step)) ris)
		(if (equal (car (car ris)) nil) (pop ris))))
    ris))

(defun expand-black (node)
  "expande todos los posible nodos hijos de {s} cuando es el turno del negro"
  (let ((ris nil)
	(s (car node)))
    (loop for tower in (cadr s) do
	  (loop for step from 1 to (third tower) do
		(push `(,(move-n s tower step) . (,#'move-n ,(car state) ,tower ,step)) ris)
		(if (equal (car (car ris)) nil) (pop ris))
		(push `(,(move-nw s tower step) . (,#'move-nw ,(car state) ,tower ,step)) ris)
		(if (equal (car (car ris)) nil) (pop ris))
		(push `(,(move-ne s tower step) . (,#'move-ne ,(car state) ,tower ,step)) ris)
		(if (equal (car (car ris)) nil) (pop ris))
		(push `(,(move-w s tower step) . (,#'move-w ,(car state) ,tower ,step)) ris)
		(if (equal (car (car ris)) nil) (pop ris))
		(push `(,(move-e s tower step) . (,#'move-e ,(car state) ,tower ,step)) ris)
		(if (equal (car (car ris)) nil) (pop ris))				
		(push `(,(move-s s tower step) . (,#'move-s ,(car state) ,tower ,step)) ris)
		(if (equal (car (car ris)) nil) (pop ris))
		(push `(,(move-sw s tower step) . (,#'move-sw ,(car state) ,tower ,step)) ris)
		(if (equal (car (car ris)) nil) (pop ris))
		(push `(,(move-se s tower step) . (,#'move-se ,(car state) ,tower ,step)) ris)
		(if (equal (car (car ris)) nil) (pop ris))))
    ris))

(defun do-decision (dec)
  "exegue la funciión elijida da los algoritmos de busqueda"
  (funcall (first dec) (second dec) (third dec) (fourth dec)))

(defun bigger (lst)
  "revuelve el elemento mas grande de la lista {lst}"
  (let ((ris (car lst)))
    (dolist (el lst) 
      (setq ris (max ris el))) 
    ris))

(defun bigger-return (fn lst)
  "revuelve el elemento de {lst} que aplicandose la función {fn} revuelve el risultado mas grande de todos"
  (let ((tmp (funcall fn (car lst))) 
	(ris (car lst)))
    (dolist (el lst) 
      (cond ((> (funcall fn el) tmp) 
	     (setq ris el) 
	     (setq tmp (funcall fn el))))) 
    ris))
