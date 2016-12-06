(asdf:defsystem #:cl-neat
  :version "0.8"
  :maintainer "Korobeinikov Dmitrii <kmeatich@gmail.com>"
  :licence "Apache 2.0"
  :description "Implements NeuroEvolution of Augmenting Topologies."
  :depends-on (alexandria) ; for getting Gaussian random numbers
  :serial t
  :components ((:file "package")
               (:file "globals")
               (:file "helpers")
               (:file "printers")
               (:file "genome")
               (:file "network")
               (:file "starter")
               (:file "foreign-evaluator")
               (:file "population")))
