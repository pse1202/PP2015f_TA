#lang racket

(require "hw5-1.rkt")
(require "hw5-2.rkt")

(provide is-empty? fst rest length nth-elmt map reduce)
(define unit '())

(define (is-empty? l)
  (equal? l empty))

(define (fst l)
  (case-list (lambda () (inr unit)) (lambda (h t) (inl h)) l))

(define (rest l)
  (case-list (lambda () (inr unit)) (lambda (h t) (inl t)) l))

(define (length l)
  (case-list (lambda () 0) (lambda (x y) (+ 1 (length y))) l))

(define (nth-elmt l i)
  (if (> i 0) (case-list (lambda () (inr unit)) (lambda (x y) (nth-elmt y (- i 1))) l) 'TODO))

(define (map f l)
  (case-list (lambda () '()) (lambda (x y) (link (f x) (map f y))) l))

(define (reduce l f s)
  'TODO)
  

  