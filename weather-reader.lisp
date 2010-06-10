;; weather situation classes
;; 1  - osadok seen (14-16)
;; 2  - lightning seen (17)
;; 3  - non-liven moros (20)
;; 4  - non-liven rain (21)
;; 5  - non-liven snow (22)
;; 6  - non-liven rain with snow (23-24)
;; 7  - liven rain (25)
;; 8  - liven rain with snow (26)
;; 9  - grad (27)
;; 10 - fog (28)
;; 11 - lightning here (29)
;; 12 - sand thunder (30-35)
;; 13 - near-ground metel (36-39)
;; 14 - fog (40-47)

(defun read-weather-from (stream fallback)
  (let ((result (make-weather :wind-speed     (read stream)
                              :wind-direction (read stream)
                              :clouds         (read stream)
                              :pressure       (read stream)
                              :humidity       (read stream)
                              :temperature    (read stream))))
    (if fallback (weather-info-fallback-fill result fallback))
    result))

(defclass range-tracker ()
  ((data :reader ranges :initform (make-weather-ranges))
   (first :initform t)))

(defun track-ranges (tracker info)
  (with-slots (data first) tracker
    (unless first (ranges-correct data info))
    (when first
      (ranges-init data info)
      (setq first nil))))

(defun situation-as-matrix (s)
  (let ((result (make-array 10 :initial-element -0.4)))
    (setf (aref result (mod s 10)) 0.4)
    result))

(defun read-situation-data (filename info-vector situation-vector)
  (with-open-file (in filename :direction :input)
    (loop
       with tracker = (make-instance 'range-tracker)
       with fallback = nil
       with prev-sitn = nil
       for line = (read-line in nil) while line do
         (with-input-from-string (stream line)
           (let ((info (read-weather-from stream fallback))
                 (sitn (read stream)))
             (if (null sitn) (setq sitn prev-sitn))
             (track-ranges tracker info)
             (setq fallback info)
             (setq prev-sitn sitn)
             (vector-push-extend info info-vector)
             (vector-push-extend sitn situation-vector)))
       finally (return (ranges tracker)))))


(defclass situation-manager (material-manager)
  ()) 

(defmethod initialize-instance :after ((instance situation-manager) &key datafile)
  (let* ((info-vector (make-array 5 :adjustable t :fill-pointer 0))
         (sitn-vector (make-array 5 :adjustable t :fill-pointer 0))
         (ranges (read-situation-data datafile info-vector sitn-vector)))
    (loop for info across info-vector for sitn across sitn-vector do
         (let ((info-matrix (make-instance 'matrix :cols 6)))
           (encode-weather-into :matrix info-matrix :offset 0
                                :weather-info info :ranges ranges)
           (add-material instance (make-material :input info-matrix
                                                 :output (situation-as-matrix sitn)))))))