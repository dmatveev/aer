(defclass synoptic ()
  ((extractor) (winter) (spring) (summer) (autumn)))

(defun worker-thread ()
  (locally (declare (special *worker* *output* *times*))
    (let ((*standard-output* *output*))
      (forecaster-train *worker* *times*))))

(defmethod initialize-instance :after ((instance synoptic) &key
                                       parameter
                                       winter-files spring-files
                                       summer-files autumn-files)
  (with-slots (winter spring summer autumn extractor) instance
    (setq extractor (make-instance 'data-extractor :parameter parameter)
          winter (make-instance 'seazon-forecaster :parameter parameter :ahead 2)
          spring (make-instance 'seazon-forecaster :parameter parameter :ahead 2)
          summer (make-instance 'seazon-forecaster :parameter parameter :ahead 2)
          autumn (make-instance 'seazon-forecaster :parameter parameter :ahead 2))
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

(defun synoptic-forecast (instance target-date the-past-data)
  (with-slots (extractor) instance
    (let* ((worker (synoptic-forecaster-for instance target-date))
           (policy (slot-value (slot-value worker 'store) 'policy))
           (input (encode-input-series the-past-data 7 (position-in-seazon target-date)
                                       extractor policy)))
      (forecaster-perform worker input :show-all t))))