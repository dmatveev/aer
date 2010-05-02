(defgeneric process (neural-object input)
  (:documentation "Process the input vector"))

(defclass layer ()
  ((weights :reader weights)
   (outputs)
   (activation :reader activation :initarg activation :initform (sigmoid))))

(defmethod initialize-instance :after ((instance layer) &key (inputs 1) (neurons 1))
  (setf (slot-value instance 'weights)
        (matrix-create-tabulated (row inputs col neurons) (random 1.0))))

(defmethod process ((instance layer) (input matrix))
  (with-slots (activation outputs) instance
    (setf outputs (matrix-collect (matrix* input (weights instance))
                                  (activation-function activation)))))