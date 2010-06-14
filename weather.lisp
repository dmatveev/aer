(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct weather
	(temperature    0)
	(pressure       0)
	(humidity       0)
	(wind-direction 0)
	(wind-speed     0)
	(clouds         0))

  (defstruct weather-ranges
	(max-temperature    30)   (min-temperature   -30)
	(max-pressure       1200) (min-pressure       1000)
	(max-humidity       100)  (min-humidity       0)
	(max-wind-speed     20)   (min-wind-speed     0)
	(max-wind-direction 360)  (min-wind-direction 0)
	(max-clouds         10)   (min-clouds         0))

  (defun symbol-concat (&rest symbols)
	(intern (apply 'concatenate 'string (mapcar 'symbol-name symbols)))))

(defgeneric encode-meteo (value type ranges)
  (:documentation "Encode the meteorological parameter"))

(defgeneric decode-meteo (value type ranges)
  (:documentation "Decode the meteorological parameter"))

(defmacro ranges-init (ranges info)
  `(progn
     ,@(loop :for slot :in (closer-mop:class-slots (class-of (make-weather))) :collect
            (let* ((slot-name (closer-mop:slot-definition-name slot))
                   (accessor (symbol-concat 'weather- slot-name)))
              `(with-range-slots ,slot-name (max-slot min-slot) ,ranges
                 (setf max-slot (,accessor ,info)
                       min-slot (,accessor ,info)))))
     t))

(defmacro ranges-correct (ranges info)
  (with-gensyms (value)
    `(progn
       ,@(loop :for slot :in (closer-mop:class-slots (class-of (make-weather))) :collect
              (let* ((slot-name (closer-mop:slot-definition-name slot))
                     (accessor (symbol-concat 'weather- slot-name)))
                `(with-range-slots ,slot-name (max-slot min-slot) ,ranges
                 (let ((,value (,accessor ,info)))
                   (cond ((> ,value max-slot) (setf max-slot ,value))
                         ((< ,value min-slot) (setf min-slot ,value)))))))
       t)))

(defmacro weather-diff (from to)
  (let ((slots (closer-mop:class-slots (class-of (make-weather)))))
    `(cond
       ((and (null ,from) (null ,to)) (error "Nothing to diff")) 
       ((or (null ,from) (null ,to)) (make-weather))
       ((make-weather
         ,@(mapcan #'(lambda (slot)
                       (let* ((slot-name (closer-mop:slot-definition-name slot))
                              (slot-arg  (intern (symbol-name slot-name) 'keyword)))
                         `(,slot-arg (- (slot-value ,to   ',slot-name)
                                        (slot-value ,from ',slot-name)))))
                   slots))))))

(defmacro weather-info-fallback-fill (current-data prev-data)
  `(progn
     ,@(loop :for slot :in (closer-mop:class-slots (class-of (make-weather))) :collect
            (let* ((slot-name (closer-mop:slot-definition-name slot))
                   (accessor (symbol-concat 'weather- slot-name)))
              `(with-slots ((value ,slot-name)) ,current-data
                 (if (null value) (setq value (,accessor ,prev-data))))))
     t))

(defun encode-weather-into (&key matrix offset weather-info ranges)
  (loop
     :with slots = (closer-mop:class-slots (class-of weather-info))
     :with len = (length slots)
     :for slot :in slots :for index :upto len :do
       (let ((name (closer-mop:slot-definition-name slot)))
         (setf (matrix-ref matrix 0 (+ offset index))
               (encode-meteo (slot-value weather-info name) name ranges)))))

(defun decode-weather-from (&key matrix offset ranges)
  (loop
     :with result = (make-weather)
     :with slots = (closer-mop:class-slots (class-of result))
     :with len = (length slots)
     :for slot :in slots :for index :upto len :do
       (let ((name (closer-mop:slot-definition-name slot)))
         (setf (slot-value result name)
               (decode-meteo (matrix-ref matrix 0 (+ offset index)) name ranges)))
     :finally (return result)))

(defmacro with-range-slots (type (max-name min-name) range-name &body body)
  (let* ((max-slot-name (symbol-concat 'max- type))
         (min-slot-name (symbol-concat 'min- type)))
    `(with-slots ((,max-name ,max-slot-name) (,min-name ,min-slot-name)) ,range-name
       ,@body)))

(defmacro ranged-encode (type value-name range-name compress-factor)
  `(with-range-slots ,type (max-slot min-slot) ,range-name
     (let ((actual-range (- max-slot min-slot)))
       (* ,compress-factor (/ (- ,value-name (+ (/ actual-range 2) min-slot))
                              actual-range)))))

(defmacro ranged-decode (type value-name range-name compress-factor)
  `(with-range-slots ,type (max-slot min-slot) ,range-name
     (let ((actual-range (- max-slot min-slot)))
       (+ (/ (* actual-range ,value-name) ,compress-factor)
          (+ min-slot (/ actual-range 2))))))

(defmacro defcodec (type &key (compress-factor 0.8))
  `(progn
     (defmethod encode-meteo (value (type (eql ',type)) ranges)
       (ranged-encode ,type value ranges ,compress-factor))
     (defmethod decode-meteo (value (type (eql ',type)) ranges)
       (ranged-decode ,type value ranges ,compress-factor))))

(defmacro register-codecs ()
  `(progn ,@(loop :for slot :in (closer-mop:class-slots (class-of (make-weather)))
               :collect `(defcodec ,(closer-mop:slot-definition-name slot)))))

(register-codecs)

(defclass data-extractor ()
  ((parameter :initarg :parameter)))

(defun min-value (extractor range)
  (with-slots (parameter) extractor
    (funcall (symbol-concat 'weather-ranges-min- parameter) range)))

(defun max-value (extractor range)
  (with-slots (parameter) extractor
    (funcall (symbol-concat 'weather-ranges-max- parameter) range)))

(defun the-value (extractor info)
  (with-slots (parameter) extractor
    (funcall (symbol-concat 'weather- parameter) info)))

(defun read-weather-from (stream fallback)
  (let ((result (make-weather :wind-speed     (read stream)
                              :wind-direction (read stream)
                              :clouds         (read stream)
                              :pressure       (read stream)
                              :humidity       (read stream)
                              :temperature    (read stream))))
    (if fallback (weather-info-fallback-fill result fallback))
    result))