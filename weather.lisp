;; This file is a part of the aer forecasting system
;;
;; Copyright (c) 2010 Dmitry Matveev <dmatveev@inbox.com>

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.


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

(defmacro weather-info-fallback-fill (current-data prev-data)
  `(progn
     ,@(loop :for slot :in (closer-mop:class-slots (class-of (make-weather))) :collect
            (let* ((slot-name (closer-mop:slot-definition-name slot))
                   (accessor (symbol-concat 'weather- slot-name)))
              `(with-slots ((value ,slot-name)) ,current-data
                 (if (null value) (setq value (,accessor ,prev-data))))))
     t))

(defmacro with-range-slots (type (max-name min-name) range-name &body body)
  (let* ((max-slot-name (symbol-concat 'max- type))
         (min-slot-name (symbol-concat 'min- type)))
    `(with-slots ((,max-name ,max-slot-name) (,min-name ,min-slot-name)) ,range-name
       ,@body)))

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