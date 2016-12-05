;;;;===========================================================================
;;;; Neural network
;;;;===========================================================================

;;;============================================================================
;;; Neural network functions
;;;============================================================================

;; used for constructing neural network
(defun make-neuron (id type)
  (list :id id
        :type type ; input, hidden, output
        :total-weighted-input 0
        :out-nodes-and-weights nil ; (list node-id weight) semantics
        :output 0))

(defun make-neurons (genome)
  (let ((neurons nil))
    (dolist (node-gene (getf genome :node-genes))
      (push (make-neuron (getf node-gene :id) (getf node-gene :type)) neurons))
    (reverse neurons))) ; reverse to put input and output neurons in front 

;; Use only when constructing a network from scratch!
(defun modify-neurons-according-to-connection-gene (neurons connection-gene)
  (if (getf connection-gene :enabled)
      ;; in-node with respect to connection link
      ;; in-node outputs to the out-node
      (let ((in-node 
              (find-if (where :id (getf connection-gene :in-node)) neurons)))
        (push (list :id (getf connection-gene :out-node) 
                    :weight (getf connection-gene :weight))
              (getf in-node :out-nodes-and-weights)))))

;; Use only when constructing a network from scratch!
(defun modify-neurons-according-to-connection-genes (neurons connection-genes)
  (dolist (connection-gene connection-genes)
    (modify-neurons-according-to-connection-gene neurons connection-gene)))

(defun make-neural-network (genome)
  (let ((neurons (make-neurons genome))
        (inputs nil)
        (outputs nil))
    (modify-neurons-according-to-connection-genes
      neurons (getf genome :connection-genes))
    (setf inputs (remove-if-not (where :type 'input) neurons))
    (setf outputs (remove-if-not (where :type 'output) neurons))
    (list :neurons neurons
          :inputs inputs
          :outputs outputs)))

;;;============================================================================
;;; Functions for running a network
;;;============================================================================

;; Historical note: "TO-DO This looks ugly, so it probably has a bug"
;(defun load-sensors (neural-net inputs)
;  (loop for sensor in (getf neural-net :inputs) do
;        (setf (getf sensor :total-weighted-input) (car inputs))
;        (setf inputs (cdr inputs))))

;; Update: It did have a bug. Fixed here:
(defun load-sensors (neural-net inputs)
  (loop for sensor in (getf neural-net :inputs) do
        (setf (getf sensor :output) (car inputs))
        (setf inputs (cdr inputs))))
;; Still looks ugly...


;; TODO see docstring to *sigmoid-guard* (in experiment-parameters.lisp)
;; This guard could be ommitted by executing certain error suppressors as 
;; part of the script, but these are implementation dependent. So, for now,
;; the guards is the way.
(defun sigmoid (x) 
  (if (> x *sigmoid-guard*)
    (setf x *sigmoid-guard*))
  (if (< x (- *sigmoid-guard*))
    (setf x (- *sigmoid-guard*)))
  (/ 1 (+ 1 (exp (- x)))))

(defun activate-neurons (neurons)
  (loop for neuron in neurons do
        ;; Outputs on input neurons are set with load-sensors, no need to
        ;; activate - wrong to activate them.
        (if (not (equal (getf neuron :type) 'input))
          (setf (getf neuron :output)
                (sigmoid (getf neuron :total-weighted-input))))))

;; Takes a list of neurons to activate and neural net they belong to,
;; activates each neuron, passes the output of each activated neuron
;; to each of its output neurons multiplied by appropriate weight -
;; this info is kept in its :out-nodes-and-weights.
;; The process of finding output nodes could be optimized.
(defun activate-neurons-and-propagate (neural-net neurons-to-activate)
  (activate-neurons neurons-to-activate)
  (let ((neurons-outputted-to nil))
    (dolist (neuron neurons-to-activate)
      ; output neurons do not need to propagate to anywhere
      (if (not (eql (getf neuron :type) 'output)) 
        (dolist (out-node-and-weight (getf neuron :out-nodes-and-weights))
          (let ((out-neuron (find-if (where :id (getf out-node-and-weight :id)) 
                                     (getf neural-net :neurons))))
            (incf (getf out-neuron :total-weighted-input)
                  (* (getf out-node-and-weight :weight) (getf neuron :output)))
            (push out-neuron neurons-outputted-to)))))
    (remove-duplicates neurons-outputted-to)))

;;runs the network till all outputs get activated, returns outputs"
;;don't forget to call load-sensors before calling this function
(defun activate-network-r (neural-net next-nodes-to-activate)
  (if (or (null neural-net) (null next-nodes-to-activate))
    nil
    (activate-network-r 
      neural-net
      (activate-neurons-and-propagate neural-net 
                                      next-nodes-to-activate))))

;; Clears total input of each neuron
(defun flush-network (neural-net)
  (dolist (neuron (getf neural-net :neurons))
    (setf (getf neuron :total-weighted-input) 0)))

(defun activate-network-with-inputs (neural-net inputs)
  (flush-network neural-net)
  (load-sensors neural-net inputs)
  (activate-network-r neural-net (getf neural-net :inputs))
  (getf neural-net :outputs))


