(defpackage :aer-system (:use :asdf :cl))
(in-package :aer-system)

(defsystem aer
  :name "AER"
  :author "Dmitry Matveev"
  :version "0.0.1"
  :maintainer "Dmitry Matveev"
  :license "MIT"
  :description "Weather prediction system"
  :long-description ""
  :components
  ((:file "utils")
   (:file "matrix")
   (:file "neural-stuff" :depends-on ("matrix"))
   (:file "layer" :depends-on ("matrix"))
   (:file "network" :depends-on ("matrix" "layer" "neural-stuff"))
   (:file "weather" :depends-on ("matrix"))
   (:file "time-series-stuff")
   (:file "time-series" :depends-on ("matrix" "time-series-stuff"))
   (:file "time-series-store" :depends-on ("utils" "time-series" "neural-stuff" "weather"))
   (:file "forecaster" :depends-on ("time-series-store" "network" "weather"))
   (:file "synoptic" :depends-on ("forecaster" "weather"))
   (:file "benchmark" :depends-on ("synoptic" "utils")))
  :depends-on (:closer-mop :bordeaux-threads))