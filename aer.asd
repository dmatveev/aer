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
   (:file "date")
   (:file "activation")
   (:file "material")
   (:file "matrix")
   (:file "material-manager" :depends-on ("matrix" "material"))
   (:file "layer" :depends-on ("matrix"))
   (:file "network" :depends-on ("matrix" "layer" "material"))
   (:file "weather" :depends-on ("matrix"))
   (:file "time-series-policies")
   (:file "time-series" :depends-on ("matrix" "time-series-policies" "time-series-encoders"))
   (:file "time-series-encoders" :depends-on ("time-series-policies"))
   (:file "time-series-store" :depends-on ("date" "time-series" "material-manager" "material"
                                                  "weather"))
   (:file "forecaster" :depends-on ("time-series-store" "network" "weather"))
   (:file "synoptic" :depends-on ("forecaster" "weather"))
   (:file "benchmark" :depends-on ("synoptic" "utils")))
  :depends-on (:closer-mop :bordeaux-threads))