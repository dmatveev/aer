(defclass value-encoder ()
  ((extractor :initarg :extractor)))

(defgeneric encode (value encoder policy)
  (:documentation "encode the value using the specified encoder"))

(defmethod encode (value (encoder value-encoder) (policy value-policy))
  (with-slots (extractor) encoder 
    (with-slots (ranges) policy
      (numeric-encode value (min-value extractor ranges) (max-value extractor ranges) 0.8))))

(defclass diff-encoder ()
  ((base :initform t)))

(defun encode-time-series (from-sequence into-matrix extractor policy)
  (let ((encoder (make-encoder-for policy :extractor extractor)))
    (loop :with i := 0 :for value :in from-sequence :do
       (progn (setf (matrix-ref into-matrix 0 i) (encode value encoder policy))
              (incf i)))))

(defgeneric encode-forecast (fore-sequence into-array extractor policy)
  (:documentation "TBD"))

(defmethod encode-forecast (fore-sequence into-array extractor (policy value-policy))
  (encode-forecast-impl fore-sequence into-array extractor (slot-value policy 'ranges)))

(defun encode-forecast-impl (fore-sequence into-array extractor ranges)
  (loop :with i := -1 :for each :in fore-sequence :do
     (setf
      (aref into-array (incf i))
      (numeric-encode each (min-value extractor ranges) (max-value extractor ranges) 0.8))))
