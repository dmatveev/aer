(defclass value-policy ()
  ((ranges :initform nil :reader ranges)))

(defmethod print-object ((object value-policy) stream)
  (format stream "~a" (slot-value object 'ranges)))

(defgeneric encoder-class (policy)
  (:documentation "factory method"))

(defmethod encoder-class ((policy value-policy))
  'value-encoder)

(defun make-encoder-for (policy &key extractor)
  (make-instance (encoder-class policy) :extractor extractor))
