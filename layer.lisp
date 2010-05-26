(defgeneric process (neural-object input)
  (:documentation "Process the input vector"))

(defgeneric calculate-deltas (layer context)
  (:documentation "Calculate the deltas"))

(defstruct bpcontext target weights deltas errors)

(defclass layer ()
  ((weights :reader weights)
   (outputs :reader outputs)
   (activation :reader activation :initarg :activation :initform (sigmoid))))

(defclass input-layer  (layer) ())
(defclass hidden-layer (layer) ())
(defclass output-layer (layer) ())

(defmethod initialize-instance :after ((instance layer) &key (inputs 1) (neurons 1))
  (setf (slot-value instance 'weights)
        (matrix-create-tabulated (row inputs col neurons) (- 0.5 (random 1.0)))))
  
(defmethod print-object ((object layer) stream)
  (with-slots (weights activation) object
      (format stream
              "<~a inputs: ~a neurons: ~a>"
              (class-of object)
              (matrix-rows weights)
              (matrix-cols weights))))

(defmethod process ((instance layer) (input matrix))
  (with-slots (activation outputs) instance
    (let ((processed (matrix* input (weights instance))))
      (setf outputs (matrix-collect processed (activation-function activation))))))

(defmethod process ((instance input-layer) (input matrix))
  (setf (slot-value instance 'outputs) input))

(defmethod calculate-deltas ((layer hidden-layer) context)
  (with-slots (outputs activation) layer
    (with-slots ((prev-deltas deltas) (prev-weights weights)) context
      (let ((differencial (activation-differencial activation)))
        (matrix-create-tabulated (i 1 j (matrix-cols outputs))
          (* (funcall differencial (matrix-ref outputs 0 j))
             (loop for c from 0 to (1- (matrix-cols prev-weights)) summing
                  (* (matrix-ref prev-deltas  0 c)
                     (matrix-ref prev-weights j c)))))))))

(defmethod calculate-deltas :before ((layer output-layer) context)
  (with-slots (outputs) layer
    (with-slots (target errors) context
      (matrix-tabulate (errors i j)
        (- (aref target j) (matrix-ref outputs 0 j))))))

(defmethod calculate-deltas ((layer output-layer) context)
  (with-slots (outputs activation) layer
    (with-slots (errors) context
      (let ((delta-vector (make-instance 'matrix :cols (matrix-cols outputs)))
            (differencial (activation-differencial activation)))
        (matrix-tabulate (delta-vector i j)
          (* (funcall differencial (matrix-ref outputs 0 j))
             (matrix-ref errors 0 j)))))))
