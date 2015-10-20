#lang racket

(provide iter)

(define (iter n f)

  (if (= n 0) (lambda (x) x)
      (if (> n 0) (lambda (x) (f ((iter (- n 1) f) x)))
        (lambda (x) (f ((iter (+ n 1) f) x))))))
