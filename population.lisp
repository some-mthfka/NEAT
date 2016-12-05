;;;; NeuroEvolution of Augmenting Topologies (NEAT)
;;;;===========================================================================

;;;; Loading assumptions and invariants...
;;;; They are: 
;;;; 1. Neural networks are non-recurrent ()
;;;; 2. In genome, the node-genes are sorted such that all inputs come first
;;;;    and all outputs go last, with hidden nodes in between
;;;; 3. # of inputs and outputs for any given experiment does not change
;;;;    If this could be useful somehow, let me know.
;;;; Terms "neuron" and "neural-node" are used interchangeably
;;;;
;;;; I did not implement biases as part of a neuron or genome itself, but
;;;; I had a good reason to be lazy about it. Quoting NEAT Software DOC:
;;;; "...the sensors include a bias, which is a sensor always set to the same 
;;;; number"
;;;; So, the biases as such are not distinct from inputs on any level except
;;;; starter genome and fitness-evaluation function, which should always supply
;;;; the same numbers to the inputs which are biases.
;;;; Also, since mutation routines do not allow for making a connection from
;;;; input to input (maybe I am wrong and it could be useful), so the
;;;; connections from the bias input to the rest of the inputs could be
;;;; provided only in the starter genome if they are needed.

;;;; NOTE: Inputs do not have weights! The inputs are passed directly to the
;;;; activation function. I did it this way for the convenience of constructing
;;;; neural networks from genomes: each neural node contains 
;;;; weight--output-node-id pairs, and only one variable for the input:
;;;; total-weighted-input. If I were better at LISP when starting out, I would
;;;; have probably included output-node instead output-node-ids and also I
;;;; would probably do a classical model of an neuron. 

;;;; TODOS
;;;; When deciding size-limit, spread the leftover places across evenly
;;;; Competitive coevolution stagnation detection
;;;; mate-multipoint averaging of weights option
;;;; Recurrent networks
;;;; Check for same innovations when reproducing

;;;; Innovation number scheme could be improved

;;;; The mate-multipoint function is dreadfully full of to-do's...

;;;; Probably when a species just appears, it would be wise to let its weights
;;;; change more aggressively. Maybe even model it as a function of age.

;;;; How does a species die?
;;;; A species dies when its size-limit is set to zero. This is the only way
;;;; for it to die. That is, it does not die when eliminiating the worst
;;;; organisms in each species based on *survival-rate*, because at least
;;;; one organism is guaranteed to survive.
;;;; Now, when is size-limit of a species set to zero? This happens when 
;;;; (its total adjusted fitness / total adjusted fitness for the population)
;;;; * *population-size* gets below 1. The total adjusted fitness of a species
;;;; is defined as the sum of the adjusted fitness of its organisms divided by
;;;; the number of those organisms.

;(declaim (optimize (speed 0) (safety 3) (debug 3)))

;;;============================================================================
;;; Organism
;;;============================================================================

;; Compatibility is with respect to another organism, depending on the context:
;; either the organism of its species or a first organism of the population
;; when speciating the initial population
(defun make-organism (genome)
  (list :genome genome
        :network nil ;(make-neural-network genome)
        :actual-fitness nil
        :adjusted-fitness nil))

(defun organism-compatibility (org-one org-two)
  (genome-compatibility (getf org-one :genome) (getf org-two :genome)))

;;;============================================================================
;;; Species
;;;============================================================================

(defun make-species ()
  (list :organisms nil
        :total-adjusted-fitness 0
        :size-limit nil 
        :max-actual-fitness 0
        :age 0))

;;;============================================================================
;;; Population 
;;;============================================================================

(defun make-population (organisms generation)
  (list :organisms organisms
        :species nil
        :offspring nil
        :innovations nil
        :total-adjusted-fitness 0
        :generation generation
        :species-with-best-organism nil
        :best-organism nil))

;; returns the population
;; Initial population consists of organisms with the mutated starter genome
;; each.
(defun create-population-out-of-starter-genome ()
  (make-population 
    (let ((g nil))
      (loop for i from 1 to *population-size*
            do 
            (setf g (copy-tree *starter-genome*))
            (mutate-weights-harshly g)
            collect (make-organism g)))
    0))


;; Expects all organisms to be in :organisms
(defun reset-neural-networks-in-population (population)
  (dolist (org (getf population :organisms))
    (setf (getf org :network) (make-neural-network (getf org :genome)))))
;  (dolist (species (getf population :species))
;    (dolist (org (getf species :organisms))
;      (setf (getf org :network) (make-neural-network (getf org :genome))))))

;; Sets the fitness values of each of the population's organisms.
;; The result of each organism's test run is set to actual-fitness of that 
;; organism.
(defun evaluate-population (population)
  (dolist (org (getf population :organisms))
    (setf (getf org :actual-fitness) 
          (experiment-evaluate-network (getf org :network))))
  population)

;(evaluate-population (create-population-out-of-starter-genome))

(defun speciate-population (population)
  (let ((assigned nil)
        (best-compatibility nil)
        (best-compatible-species nil)
        (current-compatibility nil))
    (dolist (org (getf population :organisms))
      (setf assigned nil)
      (setf best-compatibility nil)
      (setf best-compatible-species nil)
      (setf current-compatibility nil)
      (loop for species in (getf population :species) 
            until assigned ; I _adore_ this construct
            do 
            (setf current-compatibility 
                  (organism-compatibility (first (getf species :organisms)) 
                                          org))
            (if (< current-compatibility *compatibility-threshold*)
              (progn (push org (getf species :organisms)) (setf assigned t))
              (if (or (not best-compatibility) 
                      (< current-compatibility best-compatibility))
                (progn 
                  (setf best-compatibility current-compatibility)
                  (setf best-compatible-species species)))))
      ;; If we the organism did not get assigned, it means it was not
      ;; compatible enough with any existing species. Then we create a
      ;; new species for it. However if the species cap was reached, we
      ;; assign the organism to the best compatible species we found.
      (if (not assigned)
        (if (and *species-cap* 
                 (>= (length (getf population :species)) *species-cap*))
          (push org (getf best-compatible-species :organisms))
          (progn 
            (push (make-species) (getf population :species))
            (push org (getf (car (getf population :species)) :organisms))))))))


;;;============================================================================
;;; Fitness adjustment
;;;============================================================================

(defun age-of-oldest-species (population)
  (loop for org in (getf population :species)
        maximize (getf org :age) into m
        finally (return m)))

;; Explicit sharing means: 
;; Adjusted fitness = actual fitness divided by the number of organisms in the
;; species of the evaluated organism.
;; This is called "explicit fitness sharing".
;; WARNING This function disregards whatever the value was in adjusted-fitness
;; of an organism. It just sets a new value. So, call this function first.
(defun adjust-fitness-through-explicit-sharing (population)
  (let ((n nil)) ; n is # of organisms in a currently processesed species
    (dolist (species (getf population :species))
      (setf n (length (getf species :organisms)))
      (dolist (org (getf species :organisms))
        (setf (getf org :adjusted-fitness) 
              (/ (getf org :actual-fitness) n)))))
  )

;; Age boost. The older the species, the less the boost.
;; Refer to docstring of *age-significance* for more info.
(defun adjust-fitness-through-age-boost (population)
  (let ((oldest-species-age (age-of-oldest-species population)))
    (dolist (species (getf population :species))
      (let ((fitness-booster
              (* (square (- oldest-species-age (getf species :age)))
                 *age-significance*)))
        (dolist (org (getf species :organisms))
          (setf (getf org :adjusted-fitness) 
                (* (getf org :adjusted-fitness) (+ 1 fitness-booster))))))))

(defun set-adjusted-fitness (population)
  (if (not (null (getf population :species)))
    (adjust-fitness-through-explicit-sharing population)
    (adjust-fitness-through-age-boost population)))

;;;============================================================================
;;; Deciding the size limit of each species
;;;============================================================================

;; Calculates total adjusted fitness per species (and saves the value in each)
;; walks over the :species only
(defun species-total-adjusted-fitness (population)
  (dolist (species (getf population :species))
    (loop for org in (getf species :organisms)
          summing (getf org :adjusted-fitness) into s 
          finally (setf (getf species :total-adjusted-fitness) s))))

;; Calculates population total adjusted fitness based on the calculations of
;; species-adjusted-fitness-stats
(defun population-total-adjusted-fitness (population)
  (loop for species in (getf population :species)
        summing (getf species :total-adjusted-fitness) into s 
        finally (setf (getf population :total-adjusted-fitness) s)))

(defun produce-adjusted-fitness-stats (population)
  ;; must call in this order
  (species-total-adjusted-fitness population)
  (population-total-adjusted-fitness population))

(defun count-orgs-in-species (population)
  (loop for x in (getf population :species)
    summing (length (getf x :organisms)) into s
    finally (return s)))

(defun count-size-limits-in-species (population)
  (loop for x in (getf population :species)
    summing (getf x :size-limit) into s
    finally (return s)))

;; Does not assume species to be sorted, because it would not help: best
;; organism is not guaranteed to be in best species (measured by adjusted
;; fitness)
;; Assumes the organisms in population :species to be sorted from worst to best
;; Writes the results to appropriate variables of the given population
(defun detect-species-with-best-organism (population)
  (let* ((best-org-sp (first (getf population :species)))
         (best-org (first (getf best-org-sp :organisms))))
    (dolist (species (getf population :species))
      (if (> (getf (car (last (getf species :organisms))) :actual-fitness) 
             (getf best-org :actual-fitness))
        (progn (setf best-org (car (last (getf species :organisms)))) 
               (setf best-org-sp species))))
    (setf (getf population :best-organism) best-org)
    (setf (getf population :species-with-best-organism) best-org-sp)))

;; So, based on the calculations of adjusted fitness per species and the total
;; adjusted fitness of the population, we decide how worthy is species is and
;; by limiting its size. If the species is larger than this alloted size after
;; butchering, then the species is buthchered to fit it. Then the species can
;; reproduce to fill this size.
(defun set-size-limits-for-species (population)
  (dolist (species (getf population :species))
    (setf (getf species :size-limit)
          (floor (* (/ (getf species :total-adjusted-fitness) 
                       (getf population :total-adjusted-fitness))
                    *population-size*))))
  ;; Calculated allowed size for a species and truncation can remove species
  ;; with the best organism. This is possible when there is stagnation and
  ;; the species with the best org becomes old. But we do not want its removal.
  ;; WARNING In very rare cases, this could lead to an increase in total
  ;; population size by one (but would be fixed in the next generation).
  (let ((best-org-sp (getf population :species-with-best-organism)))
    (if (and (<= (getf best-org-sp :size-limit) 0))
      (setf (getf best-org-sp :size-limit) 1))))

;; Truncation in set-size-limit-for-species makes the total planned size of the
;; population less the allowed size, so we fix that by throwing the free places
;; into the best species we have. The spreading of the free spaces could be
;; done differently though. 
;; TODO Think about how can distribute the free places better
(defun re-adjust-size-limits-for-truncation-error (population)
  (let ((counted 0))
    (loop for species in (getf population :species)
          summing (getf species :size-limit) into s
          finally (setf counted s))
    ;; Probably could check that there is at least one species at this point
    (incf (getf (first (getf population :species)) :size-limit) 
          (- *population-size* counted))))


;;;============================================================================
;;; Butchering
;;;============================================================================

;; Order: from best to worst
;; Sorts by adjusted fitness
(defun sort-species (population)
  ;; I used to just pass the species to sort function. Turns out sort may 
  ;; destruct the list you pass to it, but returns the sorted list.
  ;; It was a bug. Thanks, Obama!
  (setf (getf population :species)
        (sort (getf population :species) 
              (lambda (x y) (> (getf x :total-adjusted-fitness) 
                               (getf y :total-adjusted-fitness))))))

;; Order: from worst to best. This makes it easy to pop the worst when 
;; butchering.
;; Sorts by adjusted fitness
(defun sort-organisms-inside-species (population)
  (dolist (species (getf population :species))
    (setf (getf species :organisms)
          (sort (getf species :organisms) 
                (lambda (x y) (< (getf x :adjusted-fitness) 
                                 (getf y :adjusted-fitness)))))))

;; Organisms inside species are expected to be sorted from worst to best.
;; Every species, if it has one or more organisms, is guaranteed to have at
;; least one organism left after calling this function.
;; :organisms in population is nil when this function is called, so we take
;; care only of the :species. 
(defun butcher-the-worst (population)
  (dolist (species (getf population :species))
    (let ((species-size (length (getf species :organisms))))
      (loop repeat (ceiling (* species-size *death-rate-per-species*))
            until (<= species-size 1)
            do (pop (getf species :organisms))))))


(defun butcher-species-to-their-size-limits (population)
  (dolist (species (getf population :species))
    (loop for i 
          from (+ (getf species :size-limit) 1) 
          to (length (getf species :organisms))
          do (pop (getf species :organisms))))
  (setf (getf population :species) 
        (remove-if (lambda (x) (= 0 (length (getf x :organisms)))) 
                   (getf population :species))))

;;;============================================================================
;;; Statistics
;;;============================================================================

;; Find organism with largest actual-fitness in :organisms
;(defun find-the-fittest (population)
;  (let ((max-org (first (getf population :organisms))))
;    (dolist (org (getf population :organisms))
;      (if (> (getf org :actual-fitness) (getf max-org :actual-fitness))
;        (setf max-org org)))
;    max-org))

;;;============================================================================
;;; Reproduction and mutations
;;;============================================================================

;; TODO Change this to cond
(defun mutate-organism (org population)
  (if (< (/ (random 1000) 1000) *single-weight-mutation-probability*)
    (mutate-random-weight (getf org :genome)))
  (if (< (/ (random 1000) 1000) *harsh-weight-mutation-probability*)
    (mutate-weights-harshly (getf org :genome)))
  ;; nil is placeholder in let below, it is popped later, done for using nconc
  (let ((proposed-genes nil) ;; take care of the same mutations, they
        (duplicate nil))            ;; need to have the same innovation-number
    (if (< (/ (random 1000) 1000) *split-connection-probability*)
      (setf proposed-genes
            (append proposed-genes
                    (split-random-connection-with-node-gene-in-genome 
                      (getf org :genome)))))
    (if (< (/ (random 1000) 1000) *add-connection-probability*)
      (setf proposed-genes
            (append proposed-genes
                    (make-random-connection-gene-in-genome 
                      (getf org :genome)))))
    ;; checking c-gene for nil is necessary because sometimes the mutation will
    ;; be assigned by probability but won't happen somehow and nil will be
    ;; returned and appended to proposed-genes
    (dolist (c-gene proposed-genes)
      ;(if (not (null c-gene)) (format t "NOT NULL: ~a " c-gene))
      (if (not (null c-gene))
        (progn
          (setf duplicate 
                (find-if (where :in-node (getf c-gene :in-node)
                                :out-node (getf c-gene :out-node))
                         (getf population :innovations)))
          (if (not (null duplicate))
            (setf (getf c-gene :innovation-number) 
                  (getf duplicate :innovation-number))
            ;; else, duplicate is not found, add c-gene to innovations
            (push c-gene (getf population :innovations))))))))

;; By this step, all the offspring is in population's :offspring
(defun mutate-offspring (population)
  (dolist (org (getf population :offspring))
    (mutate-organism org population)))

;; How many offspring species will have is based on its size-limit
;; Organisms are sorted from worst to best
;; :organisms of species must not be nil
;; Returns offspring
(defun reproduce-from-species (species population)
  (let* ((allowed-to-reproduce (- (getf species :size-limit) 
                                  (length (getf species :organisms))))
         (offspring nil)
         (orgs (getf species :organisms))
         ;; Want at least one offspring with the winner of the species
         (parent-one (car (last orgs)))
         (parent-two nil)
         (parent-genome-one nil)
         (parent-genome-two nil))
    (loop repeat allowed-to-reproduce
          do
          (setf parent-two (pick-random-item orgs))
          ;; mate-multipoint expects the first genome in the argument to be
          ;; better than the second
          (if (> (getf parent-two :actual-fitness)
                 (getf parent-one :actual-fitness))
            (rotatef parent-one parent-two))
          (setf parent-genome-one (getf parent-one :genome))
          (setf parent-genome-two (getf parent-two :genome))
          (push (make-organism (mate-genome-multipoint parent-genome-one 
                                                       parent-genome-two))
                offspring)
          ;; If two parents were the same organism, mutate the child
          (if (eq parent-genome-one parent-genome-two)
            (mutate-organism (first offspring) population))
          (setf parent-genome-one (getf (pick-random-item orgs) :genome)))
    offspring))

;; All the offspring ends up being in population :offspring
(defun mate-population (population)
  (dolist (species (getf population :species))
    (setf (getf population :offspring) 
          (append (reproduce-from-species species population) 
                  (getf population :offspring)))))

;; Assumes population's best organism to be detected
;; Mutates only organisms in :species of the population, since those are the
;; parents and the offspring is still in :offspring. The :offspring and
;; :species mutations happen separately to allow for possible adjustements.
;; For example, the best organism (population :best-organism)
;; (which is in the :species) does not get modified.
(defun mutate-parents (population)
  (dolist (species (getf population :species))
    (dolist (org (getf species :organisms))
      (if (not (eq org (getf population :best-organism)))
        (mutate-organism org population)))))

;;;============================================================================
;;; Population maintanance
;;;============================================================================

(defun fill-organisms-from-species (population)
    (dolist (species (getf population :species))
      (dolist (org (getf species :organisms))
        (push org (getf population :organisms)))))

;; Assumes organisms to be sorted inside species from worst to best
;; A species ages when it's record fitness is not improving
(defun age-species-and-check-fitness-record (population &key save-folder)
  (let ((orgs-saved 0)) ;; for saving, different orgs have different id's
    (dolist (species (getf population :species))
      (let* ((best-org (car (last (getf species :organisms))))
             (best-org-fitness (getf best-org :actual-fitness)))
        (if (> best-org-fitness (getf species :max-actual-fitness))
          (progn
            (setf (getf species :age) 0)
            (setf (getf species :max-actual-fitness) best-org-fitness)
            (if (and save-folder 
                     (or (null (getf population :best-organism))
                         (> best-org-fitness 
                            (getf (getf population :best-organism) 
                                  :actual-fitness))))
              (progn
                (setf (getf population :best-organism) best-org)
                (save-new-winner save-folder
                                 (getf population :generation) 
                                 best-org
                                 ;; best org is the last in the list inside 
                                 ;; species
                                 (incf orgs-saved)))))
          (incf (getf species :age)))))))

(defun save-state-if-necessary (population exp-id)
  (if (and *save-every-generation*
           exp-id
           (> (getf population :generation) 0)
           (= 0 (rem (getf population :generation) *save-every-generation*)))
    (save-population population exp-id)))

;;;============================================================================
;;; Main
;;;============================================================================

;; Agenda for generate-next is as follows:
;;
;; ----- Preparation -----
;; * Receive a population which has both its :organisms and :species intact 
;;   and coherent, but unevaluated. :offspring should be nil. Neural nets
;;   of the organisms are expected to be constructed. 
;; * Evaluate the population
;;
;;
;; ----- Fitness -----
;; * Adjust fitness for the population
;; * Clear the :organisms of the population. The :species still have the
;;   organisms, but the :organisms will be refilled later. :organisms, after
;;   nil'ing, will be assigned the new offspring for future speciation of
;;   that offspring with function speciate-population. After that is done,
;;   the organisms will be reset with all the organisms from the :species.
;;   the work is done on the species level.
;; * Calculate total adjusted fitness per species and total adjusted 
;;   fitness for the whole population (based on previous calculation)
;; * Sort the species by adjusted fitness
;; * Based on the adjusted fitness stats, decide the limit size of each 
;;   species. For comments, refer to function set-size-limit-for-species
;; * Sort organisms inside species by adjusted fitness
;;
;; ----- Stats -----
;; Good time to print stats.
;;
;; ----- Selection -----
;; * Butcher the organisms per species based on *survival-rate* 
;; * Limit each species to its size-limit and check for nils
;;
;; ----- Reproduction and mutation -----
;; * Reproduce by species. A species gets to reproduce
;;   (- its-size-limit its-length) times.
;;   Collect all the offspring in the :offspring of the population 
;;   (and this list is empty by this step)
;; * Clear the innovations of the population (this is a to-think-about)
;; * Randomly mutate the offspring
;; * Randomly mutate the parents (this should be a separate step as it is)
;;   without modifying the winner of the population (have to traverse every
;;   last organism of every species to find it)
;;
;; ----- Final steps -----
;; * Speciate the new offspring in the :organisms into existing :species.
;;   Since the speciation function specieates organisms from :organisms,
;;   it is first needed to assign :organisms to :offspring.
;; * Clear :offspring of the population
;; * Clear the :organisms from refill from :species
;; * reset-neural-networks-in-population because genomes have changed
;; * Age the population

(defun generate-next (population &key experiment-id)
  ;; ---------------------------- Preparat uion ---------------------------------
  (evaluate-population population)

  ;; ---------------------------- Fitness -------------------------------------
  (set-adjusted-fitness population)
  (setf (getf population :organisms) nil)
  (produce-adjusted-fitness-stats population) ;species / pop adjusted totals
  (sort-species population)
  (sort-organisms-inside-species population)
  (age-species-and-check-fitness-record population :save-folder experiment-id)
  (detect-species-with-best-organism population) ; by actual fitness
  (set-size-limits-for-species population) ;; looks for species with best org
  (re-adjust-size-limits-for-truncation-error population)

  ;; ---------------------------- Stats ---------------------------------------
  (print-statistics population)

  ;; ---------------------------- Save State ----------------------------------
  ;; Yes, the saving part comes here and it works in this place and the output
  ;; file is good in this place.
  (save-state-if-necessary population experiment-id)

  ;; ---------------------------- Selection -----------------------------------
  (butcher-the-worst population)
  (butcher-species-to-their-size-limits population)

  ;; ---------------------------- Reproduction --------------------------------
  (mate-population population)

  ;; ---------------------------- Mutation -------------------------------------
  (setf (getf population :innovations) nil)
  (mutate-offspring population)
  (mutate-parents population)

  ;; ---------------------------- Final Steps ---------------------------------
  (setf (getf population :organisms) (getf population :offspring))
  (speciate-population population)
  (setf (getf population :offspring) nil)
  (setf (getf population :organisms) nil)
  (fill-organisms-from-species population)
  (reset-neural-networks-in-population population)
  (incf (getf population :generation))
  )

(defun initial-step (read-generation-file)
  (let ((init-pop nil)) 
    (if read-generation-file
      (setf init-pop (read-population-at-path read-generation-file))
      (progn 
        (setf init-pop (create-population-out-of-starter-genome))
        (reset-neural-networks-in-population init-pop)
        (evaluate-population init-pop)
        (speciate-population init-pop)))
    init-pop))

;; experiment-id is the id of the experiment which will frame the save-folder's
;; name. Not supplying anything means not saving results. Supplying a number
;; will create a folder named exp-<number> and the winner organisms will be
;; saved there. Necessary for saving state specified with 
;; *save-every-generation*.
;; If experiment was saved to a file, it can be read back from
;; read-generation-file. NOTE: experiment-id has nothing to do with
;; read-generation-file! experiment-id is used only for saving a population.
;; So, read-generation-file needs to contain relative path to the experiment
;; file including all the folders (absolute path will work too).
;; Example calls: 
;; (experiment nil "exp-5/gen-10.gen") will load the generation from the specified
;; file and will save nothing nowhere.
;; (experiment 6) will run a generation from scratch and will save results to
;; folder called "exp-6" every *save-every-generation* times.
;; (experiment 6 "exp-5/gen-10.gen") will load the generation and save results
;; in "exp-6".
;; (experiment 5 "exp-5/gen-10.gen") will load the generation and save results
;; while renaming the population files if they already exists.
(defun experiment (&optional experiment-id read-generation-file)
  (let ((population (initial-step read-generation-file)))
    (loop repeat *total-runs*
        ;; If max-fitness of the population reaches *abort-fitness*, 
        ;; then end the experiment
        until (and (not (null *abort-fitness*))
                   (not (null (getf population :best-organism)))
                   (>= (getf (getf population :best-organism) :actual-fitness) 
                       *abort-fitness*))
        summing 1 into runs-done
        do (generate-next population :experiment-id experiment-id)
        finally (return runs-done))))

;; Returns average number of generations done per experiment, which is
;; meaningful only when *abort-fitness* is set to non-nil.
(defun start (&optional experiment-id read-generation-file)
  (loop repeat *number-of-experiments*
        summing (experiment experiment-id read-generation-file) into total-gens
        finally (return (float (/ total-gens *number-of-experiments*)))))
