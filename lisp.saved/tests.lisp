;;;
;;; Regression tests for the queue implementation
;;;

(define-testq QUEUE-1
    (let ((q (make-empty-queue #'identity)))
      (enqueue q 1)
      (assert (equal (peek-queue q) 1))
      (dequeue q))
   1)

(define-testq QUEUE-2
    (let ((q (make-empty-queue #'identity)))
      (enqueue q 3)
      (enqueue q 2)
      (enqueue q 1)
      (dequeue q))
   1)

(define-testq QUEUE-3
    (let ((q (make-empty-queue #'identity)))
      (enqueue q 3)
      (enqueue q 2)
      (enqueue q 1)
      (assert (equal 1 (dequeue q)))
      (assert (equal 2 (dequeue q)))
      (assert (equal 3 (dequeue q)))
      (assert (empty-queue? q)))
   nil)

(define-testq QUEUE-4
    (let ((q (make-empty-queue #'identity)))
      (enqueue q 1)
      (enqueue q 2)
      (enqueue q 3)
      (assert (equal 1 (dequeue q)))
      (assert (equal 2 (dequeue q)))
      (assert (equal 3 (dequeue q)))
      (assert (empty-queue? q)))
   nil)
