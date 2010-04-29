(defgeneric process (neural-object input)
  (:documentation "Process the input vector"))

(defclass layer ()
  ((weights :reader weights)
   (activation :reader activation
			   :initarg activation
			   :initform #'(lambda (x) (/ 1 (+ 1 (exp (- x))))))))

(defmethod initialize-instance :after ((instance layer)
									   &key (inputs 1) (neurons 1))
  (setf (slot-value instance 'weights)
		(make-instance 'matrix :rows inputs :cols neurons)))

(defmethod process ((instance layer) (input matrix))
  (matrix-collect (matrix* input (weights instance)) (activation instance)))