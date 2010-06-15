(defun numeric-encode (value min-limit max-limit &optional (compress-factor 1))
  (let ((range (- max-limit min-limit)))
    (* compress-factor (/ (- value (+ (/ range 2) min-limit)) range))))  

(defun numeric-decode (value min-limit max-limit &optional (compress-factor 1))
  (let ((range (- max-limit min-limit)))
    (+ min-limit (/ range 2) (* value (/ range compress-factor)))))

(defgeneric time-series-form (from-sequence policy)
  (:documentation "Form a time series from the sequence"))

(defmethod time-series-form (from-sequence (policy value-policy))
  (coerce from-sequence 'list))

(defun collect-seq-diffs (sequence base-value)
  (loop :with prev := base-value :for value :across sequence :collect
     (let ((v (if (null prev) value (- value prev))))
       (setq prev value)
       v)))

(defgeneric fore-series-form (prev-sequence fore-sequence policy)
  (:documentation "TBD"))

(defmethod fore-series-form (prev-sequence fore-sequence (policy value-policy))
  (declare (ignore prev-sequence policy))
  (coerce fore-sequence 'list))

(defun encode-input-series (past-data depth pos extractor policy)
  (let* ((result (make-instance 'matrix :cols (1+ depth))))
    (encode-time-series (time-series-form past-data policy) result extractor policy)
    (setf (matrix-ref result 0 depth) (encode-position pos))
    result))

(defun make-series-material (data depth pos extractor policy)
  (let* ((past-data (subseq data 0 depth))
         (fore-data (subseq data depth))
         (encoded-past (make-instance 'matrix :cols (1+ depth)))
         (encoded-fore (make-array (length fore-data))))
    (encode-time-series (time-series-form past-data policy) encoded-past extractor policy)
    (setf (matrix-ref encoded-past 0 depth) (encode-position pos))
    (encode-forecast (fore-series-form past-data fore-data policy)
                     encoded-fore extractor policy)
    (make-material :input encoded-past :output encoded-fore)))
