;;;;===========================================================================
;;;; Helpers
;;;;===========================================================================

;; All kudos for the "where" macro below go to Peter Seibel, because it was
;; taken from his book "Practical Common Lisp"
(defun make-comparison-expr (field value)
  `(equal (getf cd ,field) ,value))

(defun make-comparisons-list (fields)
  (loop while fields
        collecting (make-comparison-expr (pop fields) (pop fields))))

(defmacro where (&rest clauses)
  `#'(lambda (cd) (and ,@(make-comparisons-list clauses))))

;; Non-destructive, stole from stackoverflow answer, upvoted
(defun insert-after (lst index newelt)
  (push newelt (cdr (nthcdr index lst))) lst)

(defun pick-random-item (lst)
  ;(uf (not (null lst))
    (nth (random (length lst)) lst));)

(defun draw-next-innovation-number()
  (incf *innovation-number* 1)
  (- *innovation-number* 1))

;; How this is not part of lisp is a mystery to me
(defun square (x)
  (* x x))
