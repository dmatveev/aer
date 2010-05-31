(defgeneric backprop-learn (network material scheme)
  (:documentation "Perform a learning iteration"))

(defgeneric process-epoch (network store precision scheme)
  (:documentation "Process a learning epoch"))

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

(defmethod apply-corrections ((instance network) scheme)
  (loop
     with layers = (subseq (layers instance) 1)
     for layer across layers do (apply-corrections layer scheme)))
 
(defun collect-corrections (network context scheme)
  (loop
     with layers = (reverse (layers network))
     for current-layer across layers 
     for next-layer across (subseq layers 1) do
       (with-slots ((prev-layer layer)) context
         (calculate-deltas current-layer context)
         (setf prev-layer current-layer)
         (calculate-corrections network current-layer (outputs next-layer) scheme))))

(defmethod backprop-learn (network material scheme)
  (process network (material-input material))
  (let* ((target (material-output material))
         (context (make-bpcontext
                   :target target
                   :errors (make-instance 'matrix :cols (length target)))))
    (collect-corrections network context scheme)
    (bpcontext-errors context)))

(defmethod backprop-learn :after (network material (scheme backprop))
  (apply-corrections network scheme))

(defmethod process-epoch :before (network store precision (scheme rprop))
  (loop
     with layers = (subseq (layers network) 1)
     for layer across layers do (reset-corrections layer)))

(defmethod process-epoch (network store precision scheme)
  (with-slots (materials) store
    (loop for material across materials summing
           (matrix-inject (backprop-learn network material scheme)
                          #'(lambda (a x) (+ a (* x x))))
         into s
         finally (return (>= precision (/ s (length materials)))))))

(defun calc-update-value (gradient prev-gradient prev-step)
  (let* ((same-sign (* gradient prev-gradient))
         (next-step (if (>= same-sign 0.0)
                        (min (* prev-step 1.2) 10.0)
                        (progn 
                          (max (* prev-step 0.5) 0.0)
                          (setq gradient 0)))))
    (* next-step (if (< gradient 0) -1 1))))

(defmethod process-epoch :after (network store precision (scheme rprop))
  (with-slots ((prev-gradients gradients) (prev-steps steps)) scheme
    (loop
       for layer across (subseq (layers network) 1)
       for prev-gradient-matrix across prev-gradients 
       for prev-step-matrix across prev-steps do
         (let ((cr (slot-value layer 'corrections)))
           (matrix-do (i (matrix-rows cr) j (matrix-cols cr))
             (let* ((current-gradient (matrix-ref cr i j))
                    (current-update (calc-update-value
                                     current-gradient 
                                     (matrix-ref prev-gradient-matrix i j)
                                     (max (matrix-ref prev-step-matrix i j) 0.0001))))
               (setf (matrix-ref cr i j) current-update 
                     (matrix-ref prev-gradient-matrix i j) current-gradient
                     (matrix-ref prev-step-matrix i j) current-update))))))
  (apply-corrections network scheme))

(defun train (network store precision scheme)
  (do ((counter 0 (1+ counter)))
      ((process-epoch network store precision scheme) counter)))

(defun train-times (network store times scheme)
  (do ((counter 0 (1+ counter)))
      ((= counter times) times)
    (process-epoch network store 0.01 scheme)))
