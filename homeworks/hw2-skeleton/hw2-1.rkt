#lang racket

(provide zipper)

(define (zipper l1 l2)
  (if (null? l1) l2
      (if (null? l2) l1
          (list* (car l1) (car l2) (zipper (cdr l1) (cdr l2))))))

