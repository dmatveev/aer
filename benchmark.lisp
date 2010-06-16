(defstruct benchmark-data past month)

(defun make-benchmark-material (data depth pos)
  (let* ((past-data (subseq data 0 depth))
         (fore-data (subseq data depth))
         (month (date-month pos)))
    (make-material :input (make-benchmark-data :past past-data :month month)
                   :output fore-data)))

(defun build-benchmark-series (store data positions ahead depth)
  (loop :for index :from 0 :upto (- (length data) (+ ahead depth)) :do
     (let* ((this-data (subseq data index (+ index (+ ahead depth))))
            (this-date (aref positions (+ depth index)))
              (material (make-benchmark-material this-data depth this-date)))
       (add-material store material))))

(defun build-benchmark-materials (instance)
  (with-slots (policy series months ahead depth) instance
    (loop :for data :across series :for mts :across months :do
       (build-benchmark-series instance data mts ahead depth))))

(defclass benchmark ()
  ((error-sum :initform 0)
   (invokes :initform 0)
   (v-direction-hits)
   (v-extremum-hits)
   (v-extremum-counts)
   (v-minimals)
   (v-maximals)))

(defmethod initialize-instance :after ((instance benchmark) &key (ahead 1))
  (loop :for slot :in (closer-mop:class-slots (class-of instance)) :do
     (let ((slot-name (closer-mop:slot-definition-name slot)))
       (if (equal "v-" (string-downcase (subseq (symbol-name slot-name) 0 2)))
           (setf (slot-value instance slot-name) (make-array ahead))))))

(defmethod print-object ((instance benchmark) stream)
  (with-slots (invokes) instance
    (with-slots (v-minimals v-maximals error-sum) instance
      (format t "Processed ~d samples:~%" invokes)
      (format t "  minimal errors: ~a~%" v-minimals)
      (format t "  maximal errors: ~a~%" v-maximals)
      (format t "  average error: ~a~%" (/ error-sum invokes)))
    (with-slots (v-direction-hits v-extremum-hits v-extremum-counts) instance
      (format t "  direction hits: ~a~%" v-direction-hits)
      (format t "  extremum hits: ~a (of ~a)" v-extremum-hits v-extremum-counts))))

(defun track-minmax (benchmark cast-data real-data)
  (let ((diffs (collect-diffs cast-data real-data)))
    (with-slots (v-minimals v-maximals invokes error-sum) benchmark
      (loop
         :with i := 0 :for each :in diffs
         :for mx :across v-maximals :for mn :across v-minimals
         :do (progn (if (> each mx) (setf (aref v-maximals i) each))
                    (if (is-first invokes) (setf (aref v-minimals i) each)
                        (if (< each mn) (setf (aref v-minimals i) each)))
                    (incf error-sum each)
                    (incf i))))))

(defun track-direction-hits (benchmark cast-slopes real-slopes)
  (with-slots (v-direction-hits) benchmark
    (loop
       :with signs := (mapcar #'* cast-slopes real-slopes)
       :with i := 0 :for s :in (cdr signs)
       :do (progn (if (>= s 0.0) (incf (aref v-direction-hits i)))
                  (incf i)))))

(defun track-extremum-hits (benchmark cast-slopes real-slopes)
  (let ((real-extremums (collect-extremums real-slopes))
        (cast-extremums (collect-extremums cast-slopes)))
    (with-slots (v-extremum-hits v-extremum-counts) benchmark
      (loop
         :with i := 0 :for r :in real-extremums :for c :in cast-extremums
         :do (progn (when (is-extremum r)
                      (incf (aref v-extremum-counts i))
                      (if (eql c r) (incf (aref v-extremum-hits i))))
                    (incf i))))))

(defun track-benchmark (results past-data cast-data real-data)
  (incf (slot-value results 'invokes))
  (track-minmax results cast-data real-data)
  (let ((cast-slopes (collect-slopes (conc (last-2 past-data) cast-data)))
        (real-slopes (collect-slopes (conc (last-2 past-data) real-data))))
    (track-direction-hits results cast-slopes real-slopes) 
    (track-extremum-hits results cast-slopes real-slopes)))

(defun synoptic-benchmark (instance testfile)
  (with-slots (extractor ahead) instance
    (let ((store (make-instance 'series-manager :depth 7 :ahead ahead
                                :extractor extractor)))
      (load-data store :from testfile)
      (build-benchmark-materials store)
      (loop
         :with materials := (slot-value store 'materials)
         :with results := (make-instance 'benchmark :ahead ahead)
         :for test-data :across materials :do
         (let* ((the-data (material-input test-data))
                (the-date (make-date :month (benchmark-data-month the-data)))
                (the-past (benchmark-data-past the-data))
                (the-cast (synoptic-forecast instance the-date the-past))
                (the-real (material-output test-data)))
           (track-benchmark results the-past (coerce the-cast 'vector) the-real))
         :finally (return results)))))