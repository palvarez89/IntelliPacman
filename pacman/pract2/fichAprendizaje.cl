
(defun aprendizaje ()
  (let ( (vAux) (indice 0) (suma) (res) (ganadas 0) (resgan 0) )
    ;;; Calculamos cuantas partidas ganamos con vPesos
      (loop for j from 0 to 9 do
              (setf res (hacerPartida))
              (if (equal res 2)
                  (setf ganadas (+ ganadas 1))
              )
      )
    
    ;;; Echamos a suerte si se resta el peso (0) o se suma (1)
    (setf suma (random 2))
    
    (loop for i from 0 to 9 do
        ;;; Realizamos una copia de vPesos en vAux
          (setf vAux vPesos)   
          
        ;;; Realizamos un cambio en el vector de pesos
          (if (equal suma 0)
                (setf vPesos(cambiarVector vPesos (- (nth indice vPesos) 1) indice))
                (setf vPesos(cambiarVector vPesos (+ (nth indice vPesos) 1) indice))
            )
          
          
          ;;; Calculamos cuantas partidas gana el nuevo vector
          (setf resgan 0)
        (loop for j from 0 to 9 do
              (setf res (hacerPartida))
              (if (equal res 2)
                  (setf resgan (+ resgan 1))
                )
              )
          
          
          ;;; Si es un resultado mejor que el anterior, se actualiza el vector
          ;;; Si no lo es, se vuelve al vector anterior, y se pasa al siguiente
          ;;; valor del vector de pesos para continuar aprendiendo
          
          (if (or (> resgan ganadas) (= resgan ganadas))
              (setf ganadas resgan)
            (progn 
              (setf vPesos vAux)
              (setf indice (mod (+ indice 1) (length vPesos)))
              ;;; Echamos a suerte si se resta el peso (0) o se suma (1)
              (setf suma (random 2))
           )          
          )
      )
  
   )
 )
 
  
(defun cambiarVector (lista valor pos)
  (let ((l '() ))
  (loop for i from 0 to (- (length lista) 1) do
        (if (equal i pos)
           (setf l (append l (list valor)))
          (setf l(append l (list (nth i lista))))
        )
  
        )
    l
    )
  )
