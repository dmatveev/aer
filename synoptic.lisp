(defclass synoptic ()
  ((ahead :initform 1 :initarg :ahead)
   (extractor) (winter) (spring) (summer) (autumn)))

(defun worker-thread ()
  (locally (declare (special *worker* *output* *times*))
    (let ((*standard-output* *output*))
      (forecaster-train *worker* *times*))))

(defmethod initialize-instance :after ((instance synoptic) &key parameter
                                       winter-files spring-files
                                       summer-files autumn-files)
  (with-slots (winter spring summer autumn extractor ahead) instance
    (setq extractor (make-instance 'data-extractor :parameter parameter)
          winter (make-instance 'seazon-forecaster :parameter parameter :ahead ahead)
          spring (make-instance 'seazon-forecaster :parameter parameter :ahead ahead)
          summer (make-instance 'seazon-forecaster :parameter parameter :ahead ahead)
          autumn (make-instance 'seazon-forecaster :parameter parameter :ahead ahead))
    (apply #'forecaster-load-files (cons winter winter-files))
    (apply #'forecaster-load-files (cons spring spring-files))
    (apply #'forecaster-load-files (cons summer summer-files))
    (apply #'forecaster-load-files (cons autumn autumn-files))
    (forecaster-build winter)
    (forecaster-build spring)
    (forecaster-build summer)
    (forecaster-build autumn)))

(defun synoptic-train (instance out &optional (times 1500))
  (locally (declare (special *worker* *output* *times*))
    (let* ((slots '(winter spring summer autumn))
           (threads (loop :for worker :in slots :collect
                       (let ((bt:*default-special-bindings*
                              `((*worker* . ,(slot-value instance worker))
                                (*output* . ,out)
                                (*times*  . ,times))))
                         (bt:make-thread #'worker-thread :name "Shannon")))))
      (loop while (some #'bt:thread-alive-p threads)
         do (bt:thread-yield)))))

(defun synoptic-forecaster-for (synoptic date)
  (slot-value synoptic (seazon-of date)))

(defun synoptic-forecast (instance target-date the-past-data &key (messages nil))
  (with-slots (extractor) instance
    (let* ((worker (synoptic-forecaster-for instance target-date))
           (policy (slot-value (slot-value worker 'store) 'policy))
           (input (encode-input-series the-past-data 7 (position-in-seazon target-date)
                                       extractor policy)))
      (forecaster-perform worker input :show-past messages :show-forecast messages))))

(defstruct benchmark-data past month)

(defun make-benchmark-material (data depth pos extractor)
  (let* ((past-data (subseq data 0 depth))
         (fore-data (subseq data depth))
         (month (date-month pos)))
    (make-material :input (make-benchmark-data :past past-data :month month)
                   :output fore-data)))

(defun build-benchmark-series (store data positions ahead depth)
  (with-slots (extractor) store
    (loop :for index :from 0 :upto (- (length data) (+ ahead depth) 1) :do
       (let* ((this-data (subseq data index (+ index (+ ahead depth))))
              (this-date (aref positions (+ depth index)))
              (material (make-benchmark-material this-data depth this-date extractor)))
         (add-material store material)))))

(defun build-benchmark-materials (instance)
  (with-slots (policy series months ahead depth) instance
    (loop :for data :across series :for mts :across months :do
       (build-benchmark-series instance data mts ahead depth))))

(defstruct benchmark-results
  (error-sum 0) (error-count 0) (same-direction-hits 0)
  (extremums-passed 0) (extremums-count 0)
  maximal minimal)

(defun collect-diffs (sequence-a sequence-b)
  (loop :for a :across sequence-a :for b :across sequence-b :collect
     (abs (- a b))))

(defun collect-slopes (sequence)
  (loop
     :with prev := (elt sequence 0)
     :for each :across (subseq sequence 1) :collect
     (let ((slope (- each prev)))
       (setq prev each)
       slope)))

(defun count-extremums (sequence)
  (loop
     :with prev = (elt sequence 0)
     :for each :in (subseq sequence 1) :count
     (let ((changed (< (* prev each) 0.0)))
       (setq prev each)
       changed)))

(defun sequence-last (sequence n)
  (subseq sequence (- (length sequence) n)))

(defun track-benchmark (results past-data cast-data real-data)
  (let ((diffs (collect-diffs cast-data real-data)))
    (with-slots (error-sum error-count) results
      (incf error-count (length diffs))
      (incf error-sum (reduce #'+ diffs)))
    (with-slots (maximal minimal) results
      (loop :for each :in diffs :do
         (progn (if maximal (if (> each maximal) (setq maximal each))
                    (setq maximal each))
                (if minimal (if (< each minimal) (setq minimal each))
                    (setq minimal each)))))
    (let ((cast-slopes
           (collect-slopes (concatenate 'vector (sequence-last past-data 2) cast-data)))
          (real-slopes
           (collect-slopes (concatenate 'vector (sequence-last past-data 2) real-data))))
      (with-slots (same-direction-hits extremums-passed extremums-count) results
        (incf same-direction-hits 
              (1- (loop :for cast-slope :in cast-slopes :for real-slope :in real-slopes
                     :count (>= (* cast-slope real-slope) 0.0))))
        (let ((cast-extremums (count-extremums cast-slopes))
              (real-extremums (count-extremums real-slopes)))
          (incf extremums-count real-extremums)
          (incf extremums-passed (min real-extremums cast-extremums)))))))

(defmethod print-object ((instance benchmark-results) stream)
  (with-slots (error-count) instance
    (with-slots (maximal minimal error-sum) instance
      (format t "Processed ~d samples:~%" error-count)
      (format t "  minimal error: ~,1f~%" minimal)
      (format t "  maximal error: ~,1f~%" maximal)
      (format t "  average error: ~,1f~%~%" (/ error-sum error-count)))
    (with-slots (same-direction-hits extremums-passed extremums-count) instance
      (format t "  direction was predicted in ~,f% of times (~a)~%"
              (* 100.0 (/ same-direction-hits error-count)) same-direction-hits)
      (format t "  extremum was detected in ~,f% of times (~a from ~a)~%"
              (* 100.0 (/ extremums-passed extremums-count)) extremums-passed extremums-count))))

(defun synoptic-benchmark (instance testfile)
  (with-slots (extractor ahead) instance
    (let ((store (make-instance 'series-manager :depth 7 :ahead ahead :extractor extractor)))
      (load-data store :from testfile)
      (build-benchmark-materials store)
      (loop
         :with materials := (slot-value store 'materials)
         :with results := (make-benchmark-results)
         :for test-data :across materials :do
         (let* ((the-data (material-input test-data))
                (the-date (make-date :month (benchmark-data-month the-data)))
                (the-past (benchmark-data-past the-data))
                (the-cast (synoptic-forecast instance the-date the-past))
                (the-real (material-output test-data)))
           (track-benchmark results the-past (coerce the-cast 'vector) the-real))
         :finally (return results)))))