;;; modulo de jestión de las interfacias

(defconstant *moviments* '(n s e w ne nw se sw))
(defconstant *actions* `((n . ,#'move-n)
			 (s . ,#'move-s)
			 (e . ,#'move-e)
			 (w . ,#'move-w)
			 (ne . ,#'move-ne)
			 (nw . ,#'move-nw)
			 (se . ,#'move-se)
			 (sw . ,#'move-sw)))

(defun menu ()
  "imprime el menu principal"
  (let ((in nil))
    (format t "~%
		   	~t 1 - Juega ~% 
		   	~t 2 - Cerrar ~%")
    (setq in (read))
    in))

(defun menu-game ()
  "imprime el menu de juego"
  (let ((in nil))
    (format t "~%
		   	~t 1 - Human vs IA normal ~% 
                        ~t 2 - Human vs IA sensillo ~%
                        ~t 3 - IA vs IA ~%")
    (setq in (read))
    in))
   
(defun print-board (s)
  "imprime la situación de la tabla en la pantalla"
  (format t "~%
		~t black: ~d ~2%
		~t white: ~d ~%
		-------------------------------~%" (cadr (car s)) (car (car s))))

(defun print-board-g (s)
  "imprime la tabla actual"
  (let ((ar (make-array 64)))
    (dolist (el (cadr (car s)))
      (setf (aref ar (- (+ (* 8 (- 8 (second el))) (car el)) 1)) `(B,(third el))))
    (dolist (el (car (car s)))
      (setf (aref ar (- (+ (* 8 (- 8 (second el))) (car el)) 1)) `(W,(third el))))
    (loop for i from 0 to 63 do
	  (if (equal (aref ar i) nil)
	      (setf (aref ar i) "[   ]")))
    (format t "~%
                 ~3t     1~t    2~t    3~t    4~t    5~t    6~t    7~t    8~%
                 ~t 8 ~d~t~d~t~d~t~d~t~d~t~d~t~d~t~d~t 8 ~2%
                 ~t 7 ~d~t~d~t~d~t~d~t~d~t~d~t~d~t~d~t 7 ~2%
                 ~t 6 ~d~t~d~t~d~t~d~t~d~t~d~t~d~t~d~t 6 ~2%
                 ~t 5 ~d~t~d~t~d~t~d~t~d~t~d~t~d~t~d~t 5 ~2%
                 ~t 4 ~d~t~d~t~d~t~d~t~d~t~d~t~d~t~d~t 4 ~2%
                 ~t 3 ~d~t~d~t~d~t~d~t~d~t~d~t~d~t~d~t 3 ~2%                  
                 ~t 2 ~d~t~d~t~d~t~d~t~d~t~d~t~d~t~d~t 2 ~2%
                 ~t 1 ~d~t~d~t~d~t~d~t~d~t~d~t~d~t~d~t 1 ~2%
                 ~3t     1~t    2~t    3~t    4~t    5~t    6~t    7~t    8~%" 
	    (aref ar 0) (aref ar 1) (aref ar 2) (aref ar 3) (aref ar 4) (aref ar 5) (aref ar 6) (aref ar 7)
	    (aref ar 8) (aref ar 9) (aref ar 10) (aref ar 11) (aref ar 12) (aref ar 13) (aref ar 14) (aref ar 15)
	    (aref ar 16) (aref ar 17) (aref ar 18) (aref ar 19) (aref ar 20) (aref ar 21) (aref ar 22) (aref ar 23)
	    (aref ar 24) (aref ar 25) (aref ar 26) (aref ar 27) (aref ar 28) (aref ar 29) (aref ar 30) (aref ar 31)
	    (aref ar 32) (aref ar 33) (aref ar 34) (aref ar 35) (aref ar 36) (aref ar 37) (aref ar 38) (aref ar 39)
	    (aref ar 40) (aref ar 41) (aref ar 42) (aref ar 43) (aref ar 44) (aref ar 45) (aref ar 46) (aref ar 47)
	    (aref ar 48) (aref ar 49) (aref ar 50) (aref ar 51) (aref ar 52) (aref ar 53) (aref ar 54) (aref ar 55)
	    (aref ar 56) (aref ar 57) (aref ar 58) (aref ar 59) (aref ar 60) (aref ar 61) (aref ar 62) (aref ar 63))  
))

(defun human-move (s)
  "gestiona el input umano"
  (let ((tower nil)
	(move nil))
    (format t "~% elijir torre ")
    (setq tower (read))
    (cond 
     ((whitep (car s) tower)	
      (format t "~% movimiento(n,s,e,w,ne,nw,se,sw)? ")
      (and (not (setq move (find (read) *moviments*))) 
	   (progn (format t "~% el movimento no es valido ") 
		  (return-from human-move (human-move state))))
      (format t "~% de cuantos pasos quieres mover? ")
      (setq move `(funcall ,(cdr (assoc move *actions*)) (car state) ' ,tower ,(read)))
      (and (not (eval move)) 
	   (progn (format t "~% el movimento no es valido ") 
		  (return-from human-move (human-move state)))))
     (t (format t "~% la torre elijida no es blanca ") (human-move state)))
    move))