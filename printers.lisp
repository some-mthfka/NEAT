;;;;===========================================================================
;;;; Printers
;;;;===========================================================================

(defun printme2 (population)
  (print "************ POPULATION OFFSPRING *******************")
  (dolist (spr (getf population :offspring))
    (print spr)))

(defun printme (population)
  (print "~~~~~~~~~~~~~~~~~ POPULATION STATS ~~~~~~~~~~~~~~~~~~~")
  (print "total adjusted fitness of the population")
  (prin1 (getf population :total-adjusted-fitness))
  (dolist (species (getf population :species))
    (print "---------------------SPECIES------------------------")
    (print "size")
    (prin1 (length (getf species :organisms)))
    (print "total adjusted fitness")
    (prin1 (getf species :total-adjusted-fitness))
    (print "size-limit")
    (prin1 (getf species :size-limit))
    (print "age")
    (prin1 (getf species :age))
    (dolist (org (getf species :organisms))
      (print "%%%%%%%%%% ORGANISM %%%%%%%%%")
      (print "genome")
      (prin1 (getf org :genome))
      (print "actual fitness")
      (prin1 (getf org :actual-fitness))
      (print "adjusted fitness")
      (prin1 (getf org :adjusted-fitness)))))

;; Prints a neuron to a file to be later read by something that does not
;; understand Lisp. For something that does understand Lisp, a different
;; function should be used.
;; The format of output is: id type <pairs of out-id out-weight>
(defun print-neuron-to-stream (neuron out)
  (let ((tp nil))
    (format out "~a " (getf neuron :id))
    (cond 
      ((equal (getf neuron :type) 'input) (setf tp 0))
      ((equal (getf neuron :type) 'hidden) (setf tp 1))
      (t (setf tp 2)))
    (format out "~a " tp)
    (loop for o-n-w in (getf neuron :out-nodes-and-weights) do
          (format out "~a " (getf o-n-w :id))
          (format out "~a " (coerce (getf o-n-w :weight) 'single-float)))
    (format out "~%")))

;; Saves network of a given organism to file (to be read by the later by
;; simulation, if any). The resulting filename format is:
;; gen_<gen_num>_org_<org-id>.org
;; Note ord-id: it allows to track every beaten record even if the record gets
;; beaten more than once in the same generation.
;; Experiment id will form the name of the folder where to save the organism.
;; The name of the saved file is appended to the end of a file with a given 
;; filename. For saving population, use save-whole-population
(defun save-new-winner (experiment-id generation-n org org-id)
  (let ((dir (list :relative (format nil "exp_~a" experiment-id)))
        (name (format nil "gen-~a-org-~a" generation-n org-id))
        )
    (with-open-file (out (ensure-directories-exist
                           (make-pathname 
                             :directory dir :name name :type "org"))
                         :direction :output
                         :if-exists :supersede)
      ;; First print how many neurons there are
      (format out "~a~%" (length (getf (getf org :network) :neurons)))
      ;; Output organism's neural network (in plain text, non-Lisp).
      (let ((net (getf org :network)))
        (loop for neuron in (getf net :neurons)
              do (print-neuron-to-stream neuron out)))
      ;; Then output some stats: fitness
      (format out "~a" (getf org :actual-fitness))
      ;; Now we save the name of the newly created file in all.orgs
      (with-open-file (all-orgs (make-pathname
                                  :directory dir :name "all" :type "orgs")
                                :direction :output
                                :if-exists :append
                                :if-does-not-exist :create)
        ;(print name all-orgs)
        (format all-orgs "~a~%" name)))))

(defun average-population-actual-fit (population)
  (loop for species in (getf population :species)
        summing (length (getf species :organisms)) into total-orgs
        summing (loop for org in (getf species :organisms)
                      summing (getf org :actual-fitness))
        into total-actual-fitness
        finally (return (/ total-actual-fitness total-orgs))))

(defun print-average-species-stats (population)
  (let ((n-of-sp (length (getf population :species))))
    (loop for species in (getf population :species)
          summing (getf species :age) into total-age
          maximizing (getf species :age) into max-age
          finally (format t 
                          "#sp: ~a avg-age: ~1$ max-age: ~a " 
                          n-of-sp
                          (float (/ total-age n-of-sp))
                          max-age))))

(defun save-population (population experiment-id)
  (with-open-file 
    (out (ensure-directories-exist
           (make-pathname
             :directory (list :relative (format nil "exp-~a" experiment-id))
             :name (format nil "gen-~a" (getf population :generation)) 
             :type "gen"))
         :direction :output
         :if-exists :rename
         :if-does-not-exist :create)
    (print population out)))

(defun print-statistics (population)
  (format t "gen: ~a " (getf population :generation))
  (print-average-species-stats population)
  ;(format t "avg-fit: ~2$ " (average-population-actual-fit population))
  (let ((bb (getf population :best-organism)))
    ;(if (= 0 (rem (getf population :generation) 50))
      ;(print bb))
    (format t "max-fit: ~5$~%" (getf bb :actual-fitness))))
