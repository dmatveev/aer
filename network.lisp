(defclass network ()
  ((layers :initform (make-array 5 :adjustable t :fill-pointer 0) :reader layers)
   (speed  :initform 0.1 :initarg :speed :reader learning-speed)))

(defmacro add-layer (container type &optional (initargs '()))
  `(vector-push-extend (apply #'make-instance (cons ,type ,initargs)) ,container))

(defmacro lastp (cell)
  `(eql (second ,cell) nil))

(defmethod initialize-instance :after ((instance network)
                                       &key (inputs 1) (config '((:neurons 1))))
  (with-slots (layers) instance
    (add-layer layers 'input-layer)
    (let ((prev-layer-outputs inputs))
      (flet ((build (args)
               (add-layer layers
                          (if (lastp args) 'output-layer 'hidden-layer)
                          (append (car args) `(:inputs ,prev-layer-outputs)))
               (setf prev-layer-outputs (getf (car args) :neurons))))
        (maplist #'build config)))))

(defmethod process ((instance network) (input matrix))
  (let ((prev-layer-output input))
    (loop for layer across (layers instance) do
      (setf prev-layer-output (process layer prev-layer-output)))
    prev-layer-output))

(defun collect-corrections (network context)
  (loop
     with speed = (learning-speed network) 
     with layers = (reverse (layers network))
     for current-layer across layers 
     for next-layer across (subseq layers 1) do
       (with-slots ((prev-layer layer)) context
         (calculate-deltas current-layer context)
         (setf prev-layer current-layer)
         (calculate-corrections current-layer (outputs next-layer) speed))))

(defun backprop-learn (network material)
  (process network (material-input material))
  (loop
     with layers = (subseq (layers network) 1)
     with target = (material-output material)
     with context = (make-bpcontext
                     :target target
                     :errors (make-instance 'matrix :cols (length target)))
     initially (collect-corrections network context)
     for layer across layers do (apply-corrections layer)
     finally (return (bpcontext-errors context))))
