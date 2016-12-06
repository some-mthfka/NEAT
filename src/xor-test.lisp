;;;;===========================================================================
;;;; XOR test
;;;;===========================================================================

(load "xor-experiment-parameters.lisp")

(ql:quickload "cl-neat")

(sb-ext::set-floating-point-modes :current-exceptions nil)

;; Returns fitness of a given neural network
;; Note the generic name, keep it.
;; Since there is no reason to do several experiments in the same run, a 
;; generic name should suffice.
(defun experiment-evaluate-network (network)
  (let* ((out-1 (getf (first (activate-network-with-inputs network '(0 0))) 
                      :output))
         (out-2 (getf (first (activate-network-with-inputs network '(1 0))) 
                      :output))
         (out-3 (getf (first (activate-network-with-inputs network '(0 1))) 
                      :output))
         (out-4 (getf (first (activate-network-with-inputs network '(1 1))) 
                      :output))
         (errorsum (+ (abs out-1) 
                      (abs (- 1 out-2)) 
                      (abs (- 1 out-3)) 
                      (abs out-4))))
    ;; if the network can classify correctly, set errorsum to zero
    ;; which in turn should successfully end the experiment
    (if (and (< out-1 0.5) (>= out-2 0.5) (>= out-3 0.5) (< out-4 0.5))
      (setf errorsum 0))
    (square (- 4 errorsum))))

;; gene: in-node, out-node, weight, innovation-number, enabled
(read-starter-genome
  ;; Format: node-id, type (input, hidden or output)
  ;; Make sure that all inputs are stacked in the beginning and all outputs
  ;; are in the end, with hidden nodes in between, if any.
  (read-node-genes
    '((1 input)
      (2 input)
      (3 output)))
  (auto-read-connection-genes
    '((1 3 0.0)
      (2 3 0.0))))
