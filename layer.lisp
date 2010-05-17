(defgeneric process (neural-object input)
  (:documentation "Process the input vector"))

(defclass layer ()
  ((weights :reader weights)
   (outputs :reader outputs)
   (activation :reader activation :initarg activation :initform (sigmoid))))

(defmethod initialize-instance :after ((instance layer) &key (inputs 1) (neurons 1))
  (setf (slot-value instance 'weights)
        (matrix-create-tabulated (row inputs col neurons) (- 0.5 (random 1.0)))))

(defmethod process ((instance layer) (input matrix))
  (with-slots (activation outputs) instance
    (let ((processed (matrix* input (weights instance))))
      (setf outputs (matrix-collect processed (activation-function activation))))))
