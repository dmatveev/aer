(defun decode-array-impl (array min-limit max-limit &optional compress-factor)
  (let ((result (make-array (length array))))
    (loop :with i := -1 :for value :across array :do
       (setf (aref result (incf i))
             (numeric-decode value min-limit max-limit compress-factor))
       :finally (return result))))

(defun decode-matrix-impl (matrix min-limit max-limit &optional compress-factor)
  (let* ((result-len (matrix-cols matrix))
         (result (make-array result-len)))
    (loop :for i :from 0 :to (1- result-len) :do
       (setf (aref result i)
             (numeric-decode (matrix-ref matrix 0 i) min-limit max-limit compress-factor))
       :finally (return result))))

(defclass decoder ()
  ((extractor :initarg :extractor) (ranges :initarg :ranges)
   (compress :initarg :compress :initform 1) (minv) (maxv)))

(defmethod initialize-instance :after ((instance decoder) &key)
  (with-slots (extractor ranges minv maxv) instance
    (setq minv (min-value extractor ranges)
          maxv (max-value extractor ranges))))

(defun decode-array (decoder array)
  (with-slots (minv maxv compress) decoder
    (decode-array-impl array minv maxv compress)))

(defun decode-matrix (decoder matrix)
  (with-slots (minv maxv compress) decoder
    (decode-matrix-impl matrix minv maxv compress)))

(defclass season-forecaster ()
  ((store) (brain) (extractor) (decoder)))

(defmethod initialize-instance :after ((instance season-forecaster)
                                       &key (depth 7) (ahead 1) (parameter nil))
  (assert parameter)
  (with-slots (store brain extractor) instance
    (setq extractor (make-instance 'data-extractor :parameter parameter)
          store (make-instance 'series-manager :depth depth :ahead ahead :extractor extractor)
          brain (make-instance
                 'network :inputs (1+ depth)
                 :config `((:neurons ,(round (* 0.7 depth)) :activation ,(hypertan))
                           (:neurons ,ahead :activation ,(linear)))))))

(defun forecaster-load-files (forecaster &rest files)
  (loop
     :with store := (slot-value forecaster 'store)
     :for file :in files :do (load-data store :from file)))

(defun forecaster-build (forecaster)
  (with-slots (store extractor decoder) forecaster
    (build-materials (slot-value forecaster 'store))
    (setq decoder (make-instance 'decoder :compress 0.8 :extractor extractor
                                 :ranges (ranges (slot-value store 'policy))))))

(defun forecaster-train (forecaster times)
  (with-slots (store brain) forecaster
    (train-times brain store times (make-instance 'backprop) :verbose-period 50)))

(defun forecaster-perform-with-expected (forecaster input-data expected-data
                                         &key (show-all t))
  (with-slots (store brain extractor decoder) forecaster
    (let* ((result (process brain input-data))
           (expected (decode-array decoder expected-data))
           (processed (decode-matrix decoder result)))
      (when show-all
        (loop
           :with past-data := (decode-matrix decoder input-data)
           :for value :across past-data :do
           (format t "~,1f ~,1f~%" value value)))
      (loop
         :for e :across expected :for p :across processed :do 
         (format t "~,1f ~,1f~%" e p)))))

(defun forecaster-perform (forecaster input-data &key (show-past t) (show-forecast t))
  (with-slots (store brain extractor decoder) forecaster
    (let* ((result (process brain input-data))
           (processed (decode-matrix decoder result)))
      (when show-past
        (loop
           :with past-data := (decode-matrix decoder input-data)
           :for value :across (subseq past-data 0 (1- (length past-data)))  :do
           (format t "~,1f~%" value)))
      (when show-forecast
        (loop :for p :across processed :do (format t "~,1f~%" p)))
      processed)))

(defun forecaster-test (forecaster testfile)
  (with-slots (store brain) forecaster
    (let ((test-store
           (make-instance 'series-manager 
                          :ahead (ahead store) :depth (depth store)
                          :use-range (ranges (slot-value store 'policy)))))
      (load-data test-store :from testfile)
      (build-materials test-store)
      (let* ((materials (slot-value test-store 'materials))
             (the-demo (aref materials (random (1- (length materials))))))
        (forecaster-perform-with-expected forecaster
                                          (material-input the-demo)
                                          (material-output the-demo)
                                          :show-all t)))))
