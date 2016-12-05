;;;============================================================================
;;; Functions for constructing starter genome
;;;============================================================================

;; used for constructing starter-genome
(defun read-node-genes (data)
  (loop for description in data
        collecting (apply #'make-node-gene description)))

;; used for constructing starter-genome
(defun read-connection-genes (data)
  (loop for description in data
        collecting (apply #'make-connection-gene description)))

;; In starter data, innovation numbers and enabled flags are not specified 
;; So, the format of a starter connection gene is: 
;; in-node-id, out-node-id, weight
(defun auto-read-connection-genes (data)
  (loop for item in data
        collecting (make-connection-gene (car item) 
                                         (cadr item) 
                                         (caddr item)
                                         (draw-next-innovation-number)
                                         t)))

(defun read-starter-genome (nodes genes)
  (setf *starter-genome* (make-genome nodes genes)))

;; Used for reading back a saved-population (in Lisp form).
(defun read-population-at-path (path)
  (with-open-file (in path :direction :input)
    (read in)))

