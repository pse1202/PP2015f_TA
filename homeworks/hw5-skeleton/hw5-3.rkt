#lang racket

(require "hw5-1.rkt")
(require "hw5-2.rkt")

(provide is-empty? fst rest length nth-elmt map reduce)

(define (is-empty? l)
  (equal? l empty))

(define (fst l)
  (case-list (lambda () '()) (lambda (h t) h) l))

(define (rest l)
  (case-list (lambda () 'ERROR) (lambda (h t) t) l))

(define (length l)
  (case-list (lambda () 0) (lambda (x y) (+ 1 (length y))) l))

(define (nth-elmt l i)
  'TODO)

(define (map f l)
  'TODO)

(define (reduce l f s)
  'TODO)
  

  