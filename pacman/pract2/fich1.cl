
(defun leerFich()
   (let ( (aux) (valor) (l)) 
    (with-open-file (f2 "/home/pedro/Escritorio/intellipacman/lab_tests/lab7.txt" 
                        :direction :input )
      (setf tab (make-array '(15 15)))
      (setf vis (make-array '(15 15):initial-element 0))
      
      (loop for i from 0 to 14 do
               (loop for j from 0 to 14 do
                     (setf aux (read-char f2 NIL 'EOF)) 
                     
                     ;;conversion
                     (setf valor (convertirEntrada aux))
                     (setf (aref tab i j) valor)
                     (setf l (list i))
                     (setf l (append l (list j)))
                     (case valor
                       ('a (setf posA l))
                       ('b (setf posB l))
                       ('c (setf posC l))
                       ('f (setf posF l))
                       ('p (setf posP l))
                     )
                     
                     
               )  
            (setf aux (read-char f2 NIL 'EOF)) 
            (setf aux (read-char f2 NIL 'EOF)) 
            )
      
     
    
      )
     tab
   )  
  )


(defun cal (str)   ; Cadena A Lista
   (do* ((stringstream (make-string-input-stream str))
         (result nil (cons next result))
         (next (read stringstream nil 'eos)
               (read stringstream nil 'eos)))
        ((equal next 'eos) (reverse result))))

;terminar


(defun pintar(tablero)
  (let ( (aux) (var) (cadena) ) 
    (setf cadena (string #\Return))

      
      (loop for i from 0 to 14 do
            (loop for j from 0 to 14 do
                  
                  (setf aux (aref tablero i j))
                  (if (equal aux 0)
                      (progn
                      (setf var (string #\Space))
                        (setf var (concatenate 'string var (string #\Space)))
                        )
                      (progn
                      (if (equal aux '*)
                          (progn
                            (setf var (string aux))
                            (setf var (concatenate 'string var (string #\Space)))                           
                            )
                        (progn
                          (setf var (string aux))
                          (setf var (concatenate 'string var (string #\Space)))  
                          )
                        )
                      )
                  )
                   (setf cadena (concatenate 'string cadena var))
                     
                  )
             (setf var (string #\Newline))
                   (setf cadena (concatenate 'string cadena var))
            )
;;;    (setf (value (Find-component :PANELTAB dialog)) cadena)
    

   ;;;;                  (setf (value (Find-component :PANELTAB dialog)) var)
    
    cadena
    )
       
  )



(defun convertirEntrada (ent)
  
  (case ent
    (#\Space 0)
    (#\A 'a)
    (#\B 'b)
    (#\C 'c)
    (#\F 'f)
    (#\P 'p)
    (#\* '*)
    (otherwise NIL)
   )
 
  )

(defun moverFantasmas()
  (let ((i 0) (aux) (ok NIL))
    
    
    (setf aux (random 4))
    
    (loop while (and (< i 4) (not (equal ok T))) do
          
        (setf ok (comprobarMovimiento posA aux))
          (if (equal ok NIL)
             (setf aux (mod (+ aux 1) 4))
          )
        
      (setf i (+ i 1))    
    );;;LOOP A
          
    (if (equal ok T)
         (setf posA (mover posA aux))
    )
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    (setf aux (random 4))
    (setf i 0)
    (setf ok NIL)
    
    (loop while (and (< i 4) (not (equal ok T))) do
          
        (setf ok (comprobarMovimiento posB aux))
          (if (equal ok NIL)
             (setf aux (mod (+ aux 1) 4))
          )
        
      (setf i (+ i 1))    
    );;;LOOP B
          
    (if (equal ok T)
         (setf posB (mover posB aux))
    )
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    (setf aux (random 4))
    (setf i 0)
    (setf ok NIL)
    
    (loop while (and (< i 4) (not (equal ok T))) do
          
        (setf ok (comprobarMovimiento posC aux))
          (if (equal ok NIL)
             (setf aux (mod (+ aux 1) 4))
          )
        
      (setf i (+ i 1))    
    );;;LOOP C
          
    (if (equal ok T)
         (setf posC (mover posC aux))
    )
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   
 )
  t
)


(defun comprobarMovimiento (pos M)
  (let ( (i) (j) (aux) )
    (setf i (nth 0 pos))
    (setf j (nth 1 pos))
    
    (case M
      ;;;NORTE
      (0 (setf i (- i 1)))
      ;;;ESTE
      (1 (setf j (+ j 1)))
      ;;;SUR
      (2 (setf i (+ i 1)))
      ;;;OESTE
      (3 (setf j (- j 1)))
      )
    
    (setf aux (aref tab i j))
    (if (equal aux 0)    ;;;True si coincide con espacio (vacio)
        T
        NIL
     )
    
    
    )
  )


(defun mover (pos M)
    (let ( (i) (j) (aux) )
    (setf i (nth 0 pos))
    (setf j (nth 1 pos))
      (setf aux (aref tab i j))
      
      ;;; Si el que se mueve es PacMan suma 1 a la casilla.
      (if (equal posP pos)
            (setf (aref vis i j) (+ (aref vis i j) 1))
        )
      (setf (aref tab i j) 0)
      
      
      (case M
      ;;;NORTE
      (0 (setf i (- i 1)))
      ;;;ESTE
      (1 (setf j (+ j 1)))
      ;;;SUR
      (2 (setf i (+ i 1)))
      ;;;OESTE
      (3 (setf j (- j 1)))
      )
      (setf (aref tab i j) aux)
      
      
      (setf pos (list i))
      (setf pos (append pos (list j)))
      
      pos
      )
  )



(defun comprobarComidoMuerto()
  (let ((fin 0) )
    
   ;;;Comprobamos pirmero si esta muerto (fin=1)
    (if (estanJuntos posA posP)
        (setf fin 1)
        )
    
    (if (estanJuntos posB posP)
        (setf fin 1)
      )
    
    (if (estanJuntos posC posP)
        (setf fin 1)
      )
    
    ;;; Comprobamos si ha comido en el turno anterior(fin=2)
     (if (equal posF posP)
        (setf fin 2)
      )
   fin
  )
)

(defun estanJuntos (posFan posPac)
  (let ( (iF) (jF) (iP) (jP) (juntos NIL) )
    (setf iF (nth 0 posFan))
    (setf jF (nth 1 posFan))
    (setf iP (nth 0 posPac))
    (setf jP (nth 1 posPac))
    
    (if (equal iF iP)
        (if (equal 1 (abs (- jF jP)))
            (setf juntos T)
            (setf juntos NIL)  
         )
      )
    
     (if (equal jF jP)
        (if (equal 1 (abs (- iF iP)))
            (setf juntos T)
            (setf juntos NIL)  
          )
      )
     juntos     
    )
  )


(defun moverPacman()
  (let ( (movimiento) )
    (setf movimiento (elegirCamino))
    (case movimiento
      ('N (setf posP (mover posP 0)))
      ('E (setf posP (mover posP 1)))
      ('S (setf posP (mover posP 2)))
      ('O (setf posP (mover posP 3)))
   )
 
  
  
  )
)
(defun elegirCamino()
  (let ( (i) (j) (iN) (jN) (iS) (jS) (iE) (jE) (iO) (jO) (resH) (resHaux) (movimiento))
    (setf i (nth 0 posP))
    (setf j (nth 1 posP))
    
    (setf iN (- i 1))
    (setf jN j)
    
    (setf iS (+ i 1))
    (setf jS j)
    
    (setf iE i )
    (setf jE (+ j 1))
    
    (setf iO i )
    (setf jO (- j 1))
    
    
    ;;;; LLAMAMOS A LA FUNCION HEURISTICA PARA LAS 4 COORDENADAS
    (setf resH (heuristica iN jN))
    (setf movimiento 'N)
    
    
    (setf resHaux (heuristica iS jS))
    (if (> resHaux resH)
        (progn
          (setf movimiento 'S)
          (setf resH resHaux)
          )
      )
    
    (setf resHaux (heuristica iE jE))
    (if (> resHaux resH)
        (progn
          (setf movimiento 'E)
          (setf resH resHaux)
          )
      )
    
    (setf resHaux (heuristica iO jO))
    (if (> resHaux resH)
        (progn
          (setf movimiento 'O)
          (setf resH resHaux)
          )
      )
    
    
    ;;;; En MOVIMIENTO tenemos el movimiento mas conveniente
    movimiento

 )   
)

(defun heuristica (iPos jPos)
  
  (let ( (res 0) (distFan) (distFru) (posAux) )
    
    (setf posAux (append (list iPos) (list jPos)))
    
    (if (equal (aref tab iPos jPos) '*)
        (setf res (nth 0 vPesos))
      )
    
    ;;; Si la distancia a un fantasma es menor que 3, no se dara peso a esa casilla
    ;;; Si la distancia es mayor que 4, se da el mismo peso
    (setf distFan (fantasmaCercano iPos jPos))
    (if (and (> distFan 2) (< distFan 5))
        (setf res (+ res (* distFan (nth 2 vPesos))))
        ;;
        (if (> distFan 4)
           (setf res (+ res (* 5 (nth 2 vPesos))))
       
       )
    )
    
    
    ;;; Se resta DistFruta a 30 para que de mas valor cuanto mas cerca.
    (setf distFru (calcularDistancia posF posAux))
    (setf res (+ res (* (- 30 distFru) (nth 1 vPesos))))
    
    ;;; Si es Fruta, sumamos el peso
      (if (equal (aref tab iPos jPos) 'F)
        (setf res (+ res (nth 3 vPesos)))
        )
    
    ;;; Si esta visitada se suma el peso
    (if (equal (aref tab iPos jPos) 0)
       (setf res (+ res (* (nth 4 vPesos) (aref vis iPos jPos))))
    )
      res 
   )

)


(defun iniciarVectorPesos()
  (setf vPesos '(-10000 10 20 10000 -50))
  )

(defun calcularDistancia(pos1 pos2)
  (let ( (distI) (distJ) (dist))
    
    (setf distI (abs (- (nth 0 pos1) (nth 0 pos2))))
    (setf distJ (abs (- (nth 1 pos1) (nth 1 pos2))))
    
    
    (setf dist (+ distI distJ))
    
    dist
    )
  )


(defun fantasmaCercano (iPos jPos)
  (let ((dist) (posAux) (distAux))
    (setf posAux (append (list iPos) (list jPos)))
    
    (setf dist (calcularDistancia posA posAux))
    
    (setf distAux (calcularDistancia posB posAux))
    (if (< distAux dist)
        (setf dist distAux)
      )
    
    (setf distAux (calcularDistancia posC posAux))
    (if (< distAux dist)
        (setf dist distAux)
      )
  
  dist
    )
  )

(defun hacerPartida()
    (let ( (fin 0) )
      (leerfich)

    (loop while (equal fin 0) do
    (moverFantasmas)
    
    (setf fin (comprobarComidoMuerto))
    (if (equal fin 0)
      (moverPacman)
     )
          )
      fin)
  )
