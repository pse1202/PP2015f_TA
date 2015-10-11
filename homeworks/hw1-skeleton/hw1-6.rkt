#lang racket
(require "hw1-4.rkt")
(provide zero one not-c and-c or-c)
(provide is-zero? is-one? is-not? is-and? is-or? sub-circuit)

(define zero
  (leaf #f))
(define one
  (leaf #t))
(define (not-c c)
  (node (list (leaf 'n) c)))
(define (and-c c1 c2)
  (node (list (leaf 'a) c1 c2)))
(define (or-c c1 c2)
  (node (list (leaf 'o) c1 c2)))

(define (is-zero? c)
  (equal? (leaf-val c) #f))
(define (is-one? c)
  (equal? (leaf-val c) #t))
(define (is-not? c)
  (equal? (leaf-val (nth-child c 0)) 'n))
(define (is-and? c)
  (equal? (leaf-val (nth-child c 0)) 'a))
(define (is-or? c)
  (equal? (leaf-val (nth-child c 0)) 'o))

(define (sub-circuit c n)
  (nth-child c (+ 1 n)))

