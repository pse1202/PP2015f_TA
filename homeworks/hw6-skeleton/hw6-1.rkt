#lang racket

;;; If these statements are omitted, your submission will be graded 0.
(provide memo-ways)

(define (memoize fun)
  (let ((results (make-hash)))
    (lambda args
      (when (not (hash-has-key? results args))
        (hash-set! results args (apply fun args)))
      (hash-ref results args))))

(define memo-ways ; memo-ways: int * int -> int
  (memoize (lambda (n k)
             (cond ((= n 0) 1)
                   ((= k 0) 1)
                   (else (+ (memo-ways n (- k 1))
                            (memo-ways (- n 1) k)))))))