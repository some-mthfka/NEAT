;;;;===========================================================================
;;;; Global experiment variables
;;;;===========================================================================

(defparameter *number-of-experiments* 1
  "Defines how many experiment to run. Finally, average amount of generations
   per experiment is returned. Meaningful when *abort-fitness* is non-nil,
   otherwise each experiment will run exactly *total-runs* times.")
(defparameter *total-runs* 1000
  "Defines for how many generations to run the program.")
(defparameter *abort-fitness* nil
  "When fitness reaches this number, it means that a winning network was found
   and the experiment ends. Used when it can be said that a given network 
   behaves correctly (XOR-function for example). Otherwise, set to nil.")
(defparameter *population-size* 500
  "Population size for every generation.")
; used in get-random-weight-change-for-gene
(defparameter *multiplier-for-random-weight* 1.0
  "Specifies how aggressive weight mutations are. When a gaussian random number
   is generated for a weight mutation, it is multiplied by this value.")
(defparameter *disjoint-coefficient* 2.0
  "Used for compatibility computation.")
(defparameter *excess-coefficient* 2.0
  "Used for compatibility computation.")
(defparameter *weight-difference-coefficient* 3.0
  "Used for compatibility computation.")
(defparameter *compatibility-threshold* 1.0
  "An organism ia assigned to a species based on it's compatibility to the
   first member of that species. The less the compatibility value calculated
   for an organism with respect to a species, the more compatible it is to
   that species.")
(defparameter *sigmoid-guard* 50
  "Sigmoid function, when supplied with very large or very slow numbers
   causes floating point underflow or overflow.")
;; TODO change name of *weight-thresh*
(defparameter *weight-thresh* 8.0
  "Making weights very large is not very resonable with sigmoid function and
   floating point arithmetic. So, need to have a threshold.")
(defparameter *species-cap* 70
  "Sets the maximum number of species that can be in one population. So, when
   an organism is not compatible by the measure of *compatibility-threshold* to
   any of the existing species in the population, another species is created.
   When there are as many species as *species-cap* specifies, instead of
   creating a new species, the organism is assigned to a species it is most
   compatible with (in effect, ignoring *compatibility-threshold*).
   Set to nil to have no cap.")
(defparameter *age-significance* 0.6
  "The younger species are given boost in terms of adjusted fitness. The
   older the species, the less boost it gets. The organisms in the oldest 
   species get no boost at all. 
   The value for new (a)djusted (f)itness is
   (* af (+ 1 (* (square (age-of-oldest-species - age-of-current-organism))
                 *age-significance*)))
   This formula is experimental.
   Expect abundancy of hidden nodes if this value is too high (overfitting).")
(defparameter *harsh-max-weight-mutations* 28
  "Maximum times a genome can mutate when constructing initial population.")
(defparameter *death-rate-per-species* 0.4
  "Multiplied with population size is how many organisms die each 
   generation in each species (but leaving at least one organism).")
(defparameter *single-weight-mutation-probability* 0.8
  "Probability that one weight will be mutated in a genome.")
(defparameter *harsh-weight-mutation-probability* 0.1
  "Probability that weights will be mutated in a genome up to 
   *harsh-max-weight-mutations* times.")
(defparameter *split-connection-probability* 0.05
  "Probability that a connection will split when mutating genome.")
(defparameter *add-connection-probability* 0.1
  "Probability that a connection will be added between two nodes when mutating
   a genome.")
(defparameter *save-every-generation* 50
  "If the experiment-id is passed to the function 'start' as the second
   argument, the whole generation will be saved in Lisp format every specified
   interval. The first generation is not printed.")
