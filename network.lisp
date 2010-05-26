(defclass network ()
  ((layers :initform (make-array 5 :adjustable t :fill-pointer 0) :reader layers)
   (speed  :initform 0.1 :initarg :speed :reader learning-speed)))

(defmacro add-layer (type initargs container)
  `(vector-push-extend (apply #'make-instance (cons ,type ,initargs)) ,container))

(defmacro lastp (cell)
  `(eql (second ,cell) nil))

(defmethod initialize-instance :after ((instance network)
                                       &key (inputs 1) (config '((:neurons 1))))
  (with-slots (layers) instance
    (add-layer 'input-layer `(:inputs 1 :neurons ,inputs) layers)
    (let ((prev-layer-outputs inputs))
      (flet ((build (args)
               (add-layer (if (lastp args) 'output-layer 'hidden-layer)
                          (append (car args) `(:inputs ,prev-layer-outputs))
                          layers)
               (setf prev-layer-outputs (getf (car args) :neurons))))
        (maplist #'build config)))))

(defmethod process ((instance network) (input matrix))
  (let ((prev-layer-output input))
    (loop for layer across (layers instance) do
      (setf prev-layer-output (process layer prev-layer-output)))
    prev-layer-output))

(defun calculate-corrections (deltas next-outputs nju-param)
  (matrix* (matrix-ebye deltas (matrix-transpose next-outputs) #'*) nju-param))

(defun collect-corrections (network context)
  (loop
     with speed = (learning-speed network) 
     with layers = (reverse (layers network))
     for current-layer across layers 
     for next-layer across (subseq layers 1) collecting
       (with-slots (deltas weights) context
         (setf deltas  (calculate-deltas current-layer context))
         (setf weights (weights current-layer))
         (calculate-corrections deltas (outputs next-layer) speed))))

(defun backprop-learn (network material)
  (process network (material-input material))
  (loop
     with layers = (reverse (layers network))
     with target = (material-output material)
     with context = (make-bpcontext
                     :target target
                     :errors (make-instance 'matrix :cols (length target)))
     with corrections = (collect-corrections network context)
     for layer across layers for correction in corrections do
        (matrix+= (slot-value layer 'weights) correction)
     finally (return (bpcontext-errors context))))
