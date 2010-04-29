(defclass network ()
  ((layers :initform (make-array 5 :adjustable t :fill-pointer 0)
		   :reader layers)))

(defmethod initialize-instance :after ((instance network) &key
                                        (inputs 1)
                                        (configuration '(1)))
  (let ((prev-layer-outputs inputs))
    (dolist (neurons configuration)
      (vector-push (make-instance 'layer :inputs prev-layer-outputs :neurons neurons)
                   (slot-value instance 'layers)) 
      (setf prev-layer-outputs neurons))))

(defmethod process ((instance network) (input matrix))
  (let ((prev-layer-output input))
	(loop for layer across (layers instance) do
	  (setf prev-layer-output (process layer prev-layer-output)))
	prev-layer-output))