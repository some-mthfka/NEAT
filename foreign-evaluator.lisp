;;; Define the used C struct for encoded neuron.
(define-alien-type nil
    (struct encoded-neuron 
            (id int)
            (tp int)   ; tp = type
            (out-nodes (array int 100)) ; TODO Magic #
            (out-weights (array single-float 100)) ; TODO Magic #
            (out-count int))) ; ct = count

;; Takes a neuron, returns encoded-neuron ready for a C function.
(defun convert-neuron-to-encoded-neuron (neuron)
  (with-alien ((enc-n (struct encoded-neuron)))
    ;; Setting id
    (setf (slot enc-n 'id) (getf neuron :id))
    ;; Setting type: 0 - inupt, 1 - hidden, 2 - output
    (let ((tp (getf neuron :type))
          (enc-tp nil))
      (cond 
        ((equal tp 'input) (setf enc-tp 0))
        ((equal tp 'hidden) (setf enc-tp 1))
        ((equal tp 'output) (setf enc-tp 2))
        (t (print "WRONG TYPE IN LISP'S CONVERT-NEURON-TO-ENCODED-NEURON")))
      (setf (slot enc-n 'tp) enc-tp))
    ;; Setting total # of output connections
    (setf (slot enc-n 'out-count) 
          (length (getf neuron :out-nodes-and-weights)))
    ;; Setting out-node-ids and weights
    (loop for o-n-w in (getf neuron :out-nodes-and-weights)
          for i from 0 to 100         ; TODO Magic #
          do
          (setf (deref (slot enc-n 'out-nodes) i) 
                (getf o-n-w :id))
          (setf (deref (slot enc-n 'out-weights) i) 
                (coerce (getf o-n-w :weight) 'single-float))
          finally (return enc-n))))

;; Returns encoded-neurons
(defun encode-network-neurons (network)
    (with-alien ((neurons (array (struct encoded-neuron) 100))) ; TODO Magic #
      (loop for neuron in (getf network :neurons)
            for i from 0 to 100           ; TODO Magic #
            do
            (setf (deref neurons i) (convert-neuron-to-encoded-neuron neuron)))
      (list :neurons neurons :neurons-total (length (getf network :neurons)))))

;(defun get-starter-neurons ()
  ;(let ((init-pop (create-population-out-of-starter-genome)))
    ;(reset-neural-networks-in-population init-pop)
    ;(let ((org (car (getf init-pop :organisms))))
      ;(encode-network-neurons (getf org :network)))))


(declaim (inline c-evalme))
(define-alien-routine c-evalme
    single-float ; return value
    (enc-n (array (struct encoded-neuron) 100)) ; TODO Magic #
    (ct int))

;(sb-int:with-float-traps-masked
                   ;(:invalid)
                   ;c-evalme)

(defun call-evalme (network)
  (let ((enc-ns (encode-network-neurons network)))
    (with-alien ((res (single-float)
                      (c-evalme (getf enc-ns :neurons) 
                                (getf enc-ns :neurons-total)))))))
