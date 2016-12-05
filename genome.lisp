;;;;===========================================================================
;;;; Genome
;;;;===========================================================================

;;;============================================================================
;;; Functions for constructing genome
;;;============================================================================

; type: input, hidden, output
(defun make-node-gene (id type)
  (list :id id :type type))

(defun make-connection-gene (in-node out-node weight innovation-number enabled)
  (list :in-node in-node
        :out-node out-node
        :weight weight
        :innovation-number innovation-number
        :enabled enabled))

(defun find-largest-id (lst)
  (let ((current-max 0))
    (dolist (x lst)
      (setf current-max (max (getf x :id) current-max)))
    current-max))

(defun make-genome (node-genes connection-genes)
  (list :node-genes node-genes
        :connection-genes connection-genes
        :largest-node-gene-id (find-largest-id node-genes)
        :n-of-inputs (count-if (where :type 'input) node-genes)))

;;;============================================================================
;;; Genome mutations
;;;============================================================================

;; First genome is expected to have higher fitness value
(defun matching-and-disjoint-and-excess-genes (g-one g-two)
  (let ((matching-g-one nil) (matching-g-two nil)
        (disjoint-g-one nil) (disjoint-g-two nil)
        (excess-g-one nil) (excess-g-two nil)
        (g-one-c-genes (getf g-one :connection-genes))
        (g-two-c-genes (getf g-two :connection-genes))
        (j 0))
    (flet ((less (x y) (< (getf x :innovation-number) 
                          (getf y :innovation-number))))
      ;; Start traversing the connection genes of first genome one-by-one
      (dolist (c-g-one g-one-c-genes)
        (loop while (and (< j (length g-two-c-genes))
                         (less (nth j g-two-c-genes) c-g-one))
              do (push (nth j g-two-c-genes) disjoint-g-two) (incf j))
        ;; less with respect to innovation number
        (if (< j (length g-two-c-genes))
          (if (less c-g-one (nth j g-two-c-genes))
            (push c-g-one disjoint-g-one)
            ; else we know that c-g-one and jth of c-g-two and are equal with
            ; respect to innovation number
            (progn
              (push c-g-one matching-g-one)
              (push (nth j g-two-c-genes) matching-g-two)
              (incf j)))
          (push c-g-one excess-g-one))))
    (loop while (< j (length g-two-c-genes))
          do (push (nth j g-two-c-genes) excess-g-two) (incf j))
    (copy-tree (list :matching-g-one (reverse matching-g-one)
                     :matching-g-two (reverse matching-g-two)
                     :disjoint-g-one (reverse disjoint-g-one)
                     :disjoint-g-two (reverse disjoint-g-two)
                     :excess-g-one (reverse excess-g-one)
                     :excess-g-two (reverse excess-g-two)))))

;; Takes genome, return a list of all innovation numbers from its
;; connection genes
(defun get-all-innovation-numbers (g)
  (loop for c-gene in (getf g :connection-genes)
        collecting (getf c-gene :innovation-number)))

; innovation of the first element in lst 
;(defun f-inn (lst num)
;  )

;; Does not modify the given genes, just copies them right away
;; Quote from the paper:
;; "In composing the offspring, genes are randmly chosenn from either parent at 
;; matching genes, whereas all excess or disjoint genes are always included 
;; from the more fit parent."
;; In this way, the structure of genome stays as it is in the fittest parent,
;; but the weights are either averaged or taken randomly from either of the two
;; parents, depending on the do-average-weights flag.
;; TODO For interspecies mating however, it is needed to inherit genes from 
;; both genomes. For this to work, get-all-innovation-numbers has to take
;; two genomes and return a sorted list of all innovation numbers contained by
;; them without duplicates. Then, the mate-genome-multipoint will have to be
;; adjusted to include genes from the lesser-fitting g-two. Also, node-genes
;; would have to be picked from both parents somehow.
;; g-one has better fitness by assumption
;; The following code goes after this line in the following function:
;; ((is-inn-next-in matching-g-one (first innovation-numbers))
; TODO The commented code modifies original genome,
; which not good. There is a bug here somewhere too. 
; "NUL is not a number" or some other nonsense. 
; Good stuff. So for now, avg is not supported.
; f-inn stands for first-innovation and takes a list
;(if do-average-weights
; (progn
;  (setf (getf (first matching-g-one) :weight)
;       (/ (+ (f-inn matching-g-one)
;            (f-inn matching-g-two) 2)))
;    (push (first matching-g-one) g-zero))
;  (progn ; else
;; TODO Probably, this is not how its supposed to work, but what happens is
;; sometimes two genes would mate in such a way as to leave all connection
;; genes at nil values. The dolist below marked with a TODO fixes it and is a 
;; hack. UPDATE Actually, if a the genes turn out to be disabled - that's OK,
;; organisms with such genome won't survive anyway. So the fix is commented out
;; for now.
(defun mate-genome-multipoint (g-o g-t)
  (let* ((g-one (copy-tree g-o))
         (g-two (copy-tree g-t))
         (mde (matching-and-disjoint-and-excess-genes g-one g-two))
         (matching-g-one (getf mde :matching-g-one))
         (matching-g-two (getf mde :matching-g-two))
         (disjoint-g-one (getf mde :disjoint-g-one))
         (excess-g-one (getf mde :excess-g-one))
         (g-zero nil) ;connection genes for the resulting genome
         (innovation-numbers (get-all-innovation-numbers g-one)))
    (flet ((is-inn-next-in (lst num) 
             (if (not (null lst))
               (= num (getf (first lst) :innovation-number)))))
      (loop while (not (null innovation-numbers))
            do (progn 
                 (cond 
                   ((is-inn-next-in matching-g-one (first innovation-numbers))
                    (push (pick-random-item 
                            (list (first matching-g-one)
                                  (first matching-g-two))) g-zero)
                    (pop matching-g-one)
                    (pop matching-g-two))
                   ((is-inn-next-in disjoint-g-one (first innovation-numbers))
                    (push (pop disjoint-g-one) g-zero))
                   ((is-inn-next-in excess-g-one (first innovation-numbers))
                    (push (pop excess-g-one) g-zero))
                   (t nil))
                 (pop innovation-numbers))))
    (make-genome (getf g-one :node-genes) (reverse g-zero))))
;(dolist (c-gene g-zero) ; TODO hack
;  (setf (getf c-gene :enabled) t))
;; TODO Is that it? See copy-tree above, I forgot to add it the first time
;; around, maybe I skipped smth else too.
;; [n00b mode on] Damn the LISP variable system [n00b mode off]

(defun modify-connection-gene-weight (c-gene)
  (incf (getf c-gene :weight) 
        (* (alexandria:gaussian-random) *multiplier-for-random-weight*))
  (if (> (getf c-gene :weight) *weight-thresh*)
    (setf (getf c-gene :weight) *weight-thresh*))
  (if (< (getf c-gene :weight) (- *weight-thresh*))
    (setf (getf c-gene :weight) (- *weight-thresh*))))
  
  

;; Mutates only connection genes in genome
;; Does not mutate frozen connection genes
;; Try because if a randomly chosen gene is disabled, we don't modify it
(defun mutate-random-weight (g)
  (let ((enabled-connection-genes 
          (remove-if-not (where :enabled t) (getf g :connection-genes))))
    (if (not (null enabled-connection-genes))
      (modify-connection-gene-weight (pick-random-item 
                                       enabled-connection-genes)))))

(defun mutate-weights-harshly (g)
  (loop for i from 0 to (random *harsh-max-weight-mutations*)
        do (mutate-random-weight g)))

;starter-genome

;; Mutates only connection genes in genome
;; Create new random link such that genome still represents a non-recurrent net 
;; For this condition to be satisfied, the algorithm acts as follows:
;; 1. Get all possible pairs of node-genes for creating a connection link
;;    between them so as to satisfy network's (which is generarated later from
;;    the genome) condition for non-recurrency. For us to pick
;;    such pairs, we will simply allow making only connections between two 
;;    node-genes n-g-1 and n-g-2 such that n-g-1 is always placed before n-g-2
;;    in the list of genome's node genes.
;;    Also, note that making a connection from input-gene to another input-gene
;;    or from an output-gene to another output-gene are useless, so these are
;;    not generated at all.
;; 2. From the pairs generated above remove the ones for which connections 
;;    already exists.
;; 3. Randomly pick a pair from the resulting list and make a connection-gene
;;    based on the it.
;; In this way, there is not chance for creating a loop
;; Warning: suppose you have a connection (6 7) and (7 8), the algorithm can
;;          can make a new connection (6 8). A network constructed with such
;;          genome will still be functional and acting differently.
(defun make-random-connection-gene-in-genome (g)
  (let* ((node-genes (getf g :node-genes))
         (c-genes (getf g :connection-genes))
         (possible-n-g-1s (remove-if (where :type 'output) node-genes))
         (input-node-genes (remove-if-not (where :type 'input) node-genes))
         (all-possible-pairs nil)
         (existing-pairs nil)
         (potential-pairs nil)
         (chosen-pair nil)
         (new-gene nil))
    (loop for i from 0 to (- (length possible-n-g-1s) 1)
          do (loop for j 
                   from (max (+ i 1) (length input-node-genes)) 
                   to (- (length node-genes) 1)
                   do (push (list (getf (nth i node-genes) :id)
                                  (getf (nth j node-genes) :id)) 
                            all-possible-pairs)))
    (setf existing-pairs 
          (loop for c-gene in c-genes
                collecting (list (getf c-gene :in-node)
                                 (getf c-gene :out-node))))
    (setf potential-pairs 
          (set-difference all-possible-pairs 
                          existing-pairs 
                          :test (lambda (x y) (equal x y))))
    (if (not (null potential-pairs))
      (progn 
        (setf chosen-pair (pick-random-item potential-pairs)) 
        (setf new-gene (make-connection-gene 
                         (car chosen-pair) 
                         (car (cdr chosen-pair))
                         0 
                         (draw-next-innovation-number) 
                         t))
        (nconc c-genes (list new-gene))))
    (list new-gene))) ;; return for innovation tracking

;; TODO sometimes there would not be any enabled c-genes whatsoever. Possible
;; reason: while mating, c-genes are chosen randomly and could be disabled.
;; For now I ignore this problem since a network with no connections will do
;; badly and will eventually be discarded. So, probably I won't do anything
;; about it.
;; Adds a node-gene and two connection-genes to genome
;; Sticks a new node-gene (neuron) in an already existing connection:
;; 1. Randomly picks an enabled connection gene
;; 2. Disables it
;; 3. Creates a unique node-gene of :type hidden
;; 4. Creates two new connection genes such that they connect the newly created 
;;    node-gene and connect the nodes of the disabled connection
;; 5. Puts the new neural-node right after the in-node of the disabled gene to
;;    keep the network non-recurrent. Inserting among inputs is not OK because
;;    genome does not keeps a list of its inputs (so what? I dunno but I am
;;    pretty sure some functions relied on that...) (it could be different)
;; Expects that last n-gene in genome will have the largest id
(defun split-random-connection-with-node-gene-in-genome (g)
  (let* ((c-genes (getf g :connection-genes))
         (n-genes (getf g :node-genes))
         (enabled-c-genes (remove-if-not (where :enabled t) c-genes))
         (new-node-gene-id nil)
         (disabled-c-gene nil))
    (if (null enabled-c-genes) ; read the to-do above 
      (return-from split-random-connection-with-node-gene-in-genome nil)) 
    (setf disabled-c-gene (pick-random-item enabled-c-genes))
    (setf (getf disabled-c-gene :enabled) nil)
    (setf new-node-gene-id (incf (getf g :largest-node-gene-id)))
    ;; Insert the new node-gene as a hidden node after the node-gene identified
    ;; by the in-node of the disabled gene.
    (let ((insert-pos (position-if 
                        (where :id (getf disabled-c-gene :in-node)) n-genes)))
      (setf n-genes (insert-after n-genes 
                                  (max insert-pos (- (getf g :n-of-inputs) 1)) 
                                  (make-node-gene new-node-gene-id 'hidden))))
    ;(setf n-genes 
    ;      (insert-after 
    ;        n-genes 
    ;        (position-if (where :id (getf disabled-c-gene :in-node)) n-genes)
    ;        (make-node-gene new-node-gene-id 'hidden)))
    (let* ((c-1 (make-connection-gene (getf disabled-c-gene :in-node)
                                      new-node-gene-id
                                      1.0 ; as specified in the paper 
                                      (draw-next-innovation-number) 
                                      t))
           (c-2 (make-connection-gene new-node-gene-id
                                      (getf disabled-c-gene :out-node)
                                      ; weight - inherit, as said in the paper
                                      (getf disabled-c-gene :weight)
                                      (draw-next-innovation-number) 
                                      t)))
      (nconc c-genes (list c-1))
      (nconc c-genes (list c-2))
      (list c-1 c-2)))) ; return innovations to later find duplicates

;starter-genome
;(split-random-connection-with-node-gene-in-genome starter-genome)

;;;============================================================================
;;; Genome info getters 
;;;============================================================================

(defun get-average-weight-differences (matching-g-one matching-g-two)
  (/
    (loop
      for c-g-one in matching-g-one
      for c-g-two in matching-g-two
      summing (abs (- (getf c-g-one :weight) (getf c-g-two :weight))))
    (length matching-g-one)))

;; Computes compatibility of two genomes
(defun genome-compatibility (g-one g-two)
  (let* ((mde (matching-and-disjoint-and-excess-genes g-one g-two))
         (disjoint (+ (length (getf mde :disjoint-g-one)) 
                      (length (getf mde :disjoint-g-two))))
         (excess (+ (length (getf mde :excess-g-one)) 
                    (length (getf mde :excess-g-two))))
         ;; N is "the number of genes in the larger genome, normalizers for
         ;; genome size (N can be set to 1 if both genomes are small, i.e.,
         ;; consist of fewer than 20 genes)"
         ;; For now, it is only the number of genes in the larger genome
         (n (max (length (getf g-one :connection-genes))
                 (length (getf g-two :connection-genes))))
         (avg-weight-differences (get-average-weight-differences
                                   (getf mde :matching-g-one)
                                   (getf mde :matching-g-two))))
     (+ (/ (+ (* *excess-coefficient* excess) 
               (* *disjoint-coefficient* disjoint)) n) 
        (* *weight-difference-coefficient* avg-weight-differences))))
