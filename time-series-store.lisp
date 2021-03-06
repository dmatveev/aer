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


(defclass range-tracker ()
  ((data :reader ranges :initform nil :initarg :use)))

(defun track-ranges (tracker info)
  (with-slots (data) tracker
    (if data (ranges-correct data info) (progn (setq data (make-weather-ranges))
                                               (ranges-init data info)))))

(defclass series-manager (material-manager)
  ((policy)
   (extractor :initarg :extractor)
   (frange :initform nil)
   (series :initform (make-array 5 :adjustable t :fill-pointer 0))
   (months :initform (make-array 5 :adjustable t :fill-pointer 0))
   (ahead :initarg :ahead :initform 1 :reader ahead)
   (depth :initarg :depth :initform 7 :reader depth)))

(defmethod initialize-instance :after ((instance series-manager)
                                       &key (series-policy 'value-policy) (use-range nil))
  (with-slots (policy frange) instance
    (setq policy (make-instance series-policy))
    (if use-range (setq frange use-range))))

(defgeneric read-weather-data (filename info-vector season-vector policy)
  (:documentation "Parse and process the weather data from file"))

(defmethod read-weather-data (filename info-vector month-vector (policy value-policy))
  (with-slots (ranges) policy
    (with-open-file (in filename :direction :input)
      (loop
         :with tracker := (make-instance 'range-tracker :use ranges)
         :with fallback := nil
         :for line := (read-line in nil) :while line :do
         (with-input-from-string (stream line)
           (let ((date (date-from (read stream)))
                 (info (read-weather-from stream fallback)))
             (track-ranges tracker info)
             (setq fallback info)
             (vector-push-extend (weather-temperature info) info-vector)
             (vector-push-extend date month-vector)))
         :finally (setq ranges (ranges tracker))))))

(defun build-time-series (store data positions ahead depth policy)
  (with-slots (frange extractor) store
    (if frange (setf (slot-value policy 'ranges) frange)) ; it's ugly, I know
    (loop :for index :from 0 :upto (- (length data) (+ ahead depth)) :do
       (let* ((this-data (subseq data index (+ index (+ ahead depth))))
              (this-date (position-in-season (aref positions (+ depth index))))
              (material (make-series-material this-data depth this-date extractor policy)))
         (add-material store material)))))
                                  
(defun load-data (instance &key from)
  (let* ((series-vector (make-array 5 :adjustable t :fill-pointer 0))
         (months-vector (make-array 5 :adjustable t :fill-pointer 0)))
    (with-slots (policy series months) instance
      (read-weather-data from series-vector months-vector policy)
      (format t "~a: ~d samples were loaded successfully.~%" from (length series-vector))
      (vector-push-extend series-vector series)
      (vector-push-extend months-vector months))))

(defun build-materials (instance)
  (with-slots (policy series months ahead depth) instance
    (loop :for data :across series :for mts :across months :do
       (build-time-series instance data mts ahead depth policy))))
