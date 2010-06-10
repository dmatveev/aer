(defun tokenize (string token)
  (loop :for start := 0 :then (1+ finish)
        :for finish := (position token string :start start)
        :collecting (subseq string start finish)
        :until (null finish)))

(defun timestamp-to-seazon (timestamp)
  (let ((month (read-from-string (second (tokenize timestamp #\-)))))
	(cond ((or  (<= month 2) (= month 12))  -0.4)
		  ((and (>= month 3) (<= month 5))  -0.2)
		  ((and (>= month 6) (<= month 8))  0.2)
		  ((and (>= month 9) (<= month 11)) 0.4))))

(defclass series-manager (material-manager)
  (ranges))

(defun read-weather-data (filename info-vector seazon-vector)
  (with-open-file (in filename :direction :input)
    (loop
       with tracker = (make-instance 'range-tracker)
       with fallback = nil
       for line = (read-line in nil) while line do
         (with-input-from-string (stream line)
           (let ((seazon (timestamp-to-seazon (read stream)))
                 (info (read-weather-from stream fallback)))
             (track-ranges tracker info)
             (setq fallback info)
             ; now we track temperature only.
             (vector-push-extend (weather-temperature info) info-vector)
             (vector-push-extend seazon seazon-vector)))
       finally (return (ranges tracker)))))

;; rewrite me, please, I'm too ugly
(defmethod initialize-instance :after  ((instance series-manager) &key datafile (depth 5))
  (let* ((series-vector (make-array 5 :adjustable t :fill-pointer 0))
         (seazon-vector (make-array 5 :adjustable t :fill-pointer 0)))
	(with-slots (ranges) instance
	  (setq ranges (read-weather-data datafile series-vector seazon-vector))
      (let ((range (- (weather-ranges-max-temperature ranges)
					  (weather-ranges-min-temperature ranges))))
	  (format t "I've read successfully ~a samples. Now build up a time series~%"
			  (length series-vector))
	  (loop
		 for index from 0 upto (- (length series-vector) depth 1) do
		   (let ((this-data   (subseq series-vector index (+ index depth)))
				 (this-series (make-instance 'time-series :dimensions depth))
				 (this-matrix (make-instance 'matrix :cols (1+ depth))))
			 (time-series-from this-series this-data)
			 (time-series-encode-into this-series this-matrix
									  (weather-ranges-max-temperature ranges)
									  (weather-ranges-min-temperature ranges))
			 (setf (matrix-ref this-matrix 0 depth) (aref seazon-vector (+ depth index)))
			 (add-material
			  instance
			  (make-material
			   :input  this-matrix
			   :output (make-array
						1
						:initial-element (* 0.8 (/ (- (aref series-vector (+ depth index))
													  (+ (/ range 2)
														 (weather-ranges-min-temperature ranges)))
												   range)))))))))))
  ;; (loop for info across info-vector for sitn across sitn-vector do
    ;;      (let ((info-matrix (make-instance 'matrix :cols 6)))
    ;;        (encode-weather-into :matrix info-matrix :offset 0
    ;;                             :weather-info info :ranges ranges)
    ;;        (add-material instance (make-material :input info-matrix
    ;;                                              :output (situation-as-matrix sitn))))


(defun forecast-demo (&key datafile checkfile
					  (depth 5) (times 100) (verb 10)
					  (config '((:neurons 4) (:neurons 1))))
  (let* ((warehouse (make-instance 'series-manager :depth depth :datafile datafile))
		 (verifier  (make-instance 'series-manager  :depth depth :datafile checkfile))
		 (network (make-instance 'network :inputs (1+ depth) :config config))
		 (materials (slot-value verifier 'materials))
		 (scheme (make-instance 'backprop)))
	(format t "Let's go!~%")
	(train-times network warehouse times scheme :verbose-period verb)
	(loop repeat 10 do
		 (let ((demo (aref materials (random (length materials)))))
		   (format t "Demo material: ~a~%Processed value: ~a~%"
				   demo (process network (material-input demo)))))))