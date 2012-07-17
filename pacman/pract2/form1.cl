
(in-package :common-graphics-user)


(defun dialogoppal-button6-on-double-click (dialog widget)
  (declare (ignorable dialog widget))
  (let ( (cadena) (fin 0) )
    
    (moverFantasmas)
    
    (setf fin (comprobarComidoMuerto))
    (case fin
      (0 (moverPacman))
      (1 (progn 
          (print "MUERTO")
          (setf (available (Find-component :button6 dialog)) nil)
          (setf (value (Find-component :panelinfo dialog)) "MUERTO")
          )) 
      
      (2 (progn 
          (print "COMIDO")
          (setf (available (Find-component :button6 dialog)) nil)
          (setf (value (Find-component :panelinfo dialog)) "COMIDO")
          ))
     )
    
    
    (setf cadena (pintar tab))
    (setf (value (Find-component :PANELTAB dialog)) cadena)
   )
  t)



(defun dialogoppal-button8-on-click (dialog widget)
  (declare (ignorable dialog widget))
  ;; NOTE:  Usually it is better to use an on-change function rather
  ;; than an on-click function.  See the doc pages for those properties.
  (leerfich)
  (iniciarvectorpesos)
  (setf (available (Find-component :Button6 dialog)) t)
  (setf (available (Find-component :Button7 dialog)) t)
  (setf (available (Find-component :Button5 dialog)) t)
  (setf (available (Find-component :Button9 dialog)) t)
  (setf cadena (pintar tab))
  (setf (value (Find-component :PANELTAB dialog)) cadena)
  (setf (value (Find-component :panelinfo dialog)) "INICIADO")
  t)

(defun dialogoppal-button9-on-double-click (dialog widget)
  (declare (ignorable dialog widget))
  (let ( (resgan 0) (res 0))
    (loop for i from 0 to 99 do
          (setf res (hacerPartida))
          (if (equal res 2)
              (setf resgan (+ resgan 1))
            )
          )
    
    (setf (value (Find-component :panelinfo dialog)) (format nil "~A" resgan) )
    )
    ;;; Deshabilitamos el boton Paso
   (setf (available (Find-component :Button6 dialog)) NIL)
  t)

(defun dialogoppal-button5-on-double-click (dialog widget)
  (declare (ignorable dialog widget))
  
  (aprendizaje)
  (setf (value (Find-component :panelinfo dialog)) vPesos)
  
  ;;; Deshabilitamos el boton Paso
   (setf (available (Find-component :Button6 dialog)) NIL)
  t)

(defun dialogoppal-button7-on-double-click (dialog widget)
  (declare (ignorable dialog widget))
  (leerFich)
    (setf cadena (pintar tab))
  (setf (value (Find-component :PANELTAB dialog)) cadena)
  (setf (value (Find-component :panelinfo dialog)) "REINICIADO")
    ;;; Habilitamos el boton Paso
   (setf (available (Find-component :Button6 dialog)) T)
  t)
