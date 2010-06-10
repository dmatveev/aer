(defclass time-series ()
  ((series :reader data)))

(defmethod initialize-instance :after ((instance time-series) &key (dimensions 5))
  (setf (slot-value instance 'series) (make-array dimensions :fill-pointer 0)))

(defun time-series-collect (instance value)
  (unless (vector-push value (slot-value instance 'series))
    (format t "Oops!~%")))

(defun time-series-from (instance sequence)
  (loop for value across sequence do (time-series-collect instance value)))

(defun time-series-encode-into (instance matrix min-value max-value
                                &key (offset 0) (compress-factor 0.8) raw)
  (loop
       with range = (- max-value min-value)
       with series = (slot-value instance 'series)
       with len = (length series)
       with top = (+ len offset)
       for value across series
       for index from offset to top
       do (setf (matrix-ref matrix 0 (+ offset index))
                (if raw
                    value 
                    (* compress-factor (/ (- value (+ (/ range 2) min-value))
                                          range))))))

(defun time-series-decode-from (matrix min-value max-value
                                &key (offset 0) (compress-factor 0.8))
  (loop
     with range = (- max-value min-value)
     with len = (matrix-cols matrix)
     with top = (1- (+ len offset))
     with result = (make-instance 'time-series :dimensions len)
     for index from offset upto top
     do (time-series-collect result
                             (+ (/ (* range (matrix-ref matrix 0 index)) compress-factor)
                                (+ min-value (/ range 2))))
     finally (return result)))