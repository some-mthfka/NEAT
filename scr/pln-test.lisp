;;;;===========================================================================
;;;; PLN (Polynomial Wars) fitness test
;;;;===========================================================================

(load "pln-experiment-parameters.lisp")
(load "foreign-evaluator.lisp")

(ql:quickload "cl-neat")

(load-shared-object "D:/7_Projects/pln/build_windows/evaluator.dll")

(defun experiment-evaluate-network (network)
  (call-evalme network))

;; gene: in-node, out-node, weight, innovation-number, enabled
(read-starter-genome
  ;; Format: node-id, type (input, hidden or output)
  ;; Make sure that all inputs are stacked in the beginning and all outputs
  ;; are in the end, with hidden nodes in between, if any.
  (read-node-genes
    '((1 input)
      (2 input)
      (3 input)
      (4 input)
      (5 input)
      (6 input)
      (7 input)
      (8 input)
      (9 input)
      (10 output)
      (11 output)
      (12 output)
      (13 output)
      (14 output)
      ))
  (auto-read-connection-genes
    '(
      (1 10 0.0)
      (2 10 0.0)
      (3 10 0.0)
      (4 10 0.0)
      (5 10 0.0)
      (6 10 0.0)
      (7 10 0.0)
      (8 10 0.0)
      (9 10 0.0)

      (1 11 0.0)
      (2 11 0.0)
      (3 11 0.0)
      (4 11 0.0)
      (5 11 0.0)
      (6 11 0.0)
      (7 11 0.0)
      (8 11 0.0)
      (9 11 0.0)

      (1 12 0.0)
      (2 12 0.0)
      (3 12 0.0)
      (4 12 0.0)
      (5 12 0.0)
      (6 12 0.0)
      (7 12 0.0)
      (8 12 0.0)
      (9 12 0.0)

      (1 13 0.0)
      (2 13 0.0)
      (3 13 0.0)
      (4 13 0.0)
      (5 13 0.0)
      (6 13 0.0)
      (7 13 0.0)
      (8 13 0.0)
      (9 13 0.0)

      (1 14 0.0)
      (2 14 0.0)
      (3 14 0.0)
      (4 14 0.0)
      (5 14 0.0)
      (6 14 0.0)
      (7 14 0.0)
      (8 14 0.0)
      (9 14 0.0)
      )))
