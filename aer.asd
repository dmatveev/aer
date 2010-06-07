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
  ((:file "activation")
   (:file "material")
   (:file "matrix")
   (:file "material-manager" :depends-on ("matrix" "material"))
   (:file "layer" :depends-on ("matrix"))
   (:file "network" :depends-on ("matrix" "material" "layer" "activation")))
  :depends-on (:closer-mop))