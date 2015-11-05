#lang racket

(provide empty link case-list)

(define empty
  'empty)

(define (link v l)
  (cons v l))

(define (case-list f1 f2 l)
  (match l
    [(cons x y) (f2 x y)]
    [ 'empty (f1 'unit)]))
