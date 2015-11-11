#lang racket

(require "hw5-1.rkt")
(require "hw5-2.rkt")

(provide is-empty? fst rest length nth-elmt map reduce)
(define unit '())

(define (is-empty? #| t list -> bool |# l #|t list |#)
  (equal? l empty) #|bool|#)

(define (fst #|t list -> t + unit|# l #|t list|#)
  (case-list #|unit -> t + unit , (t , t list -> t + unit) , t list -> t + unit|# (lambda (u) #|unit -> t + unit|# (inr unit)) (lambda (h t) #|t , t list -> t + unit|# (inl h)) #|t + unit|# l))

(define (rest #|t list -> t list + unit|# l)
  (case-list (lambda (u) (inr unit) #|t list + unit|#) (lambda (h #|t|# t #|t list|#) (inl t) #|t list + unit|#) l) #|t list + unit|#)

(define (length #|t list -> int|# l)
  (case-list (lambda (u) 0) (lambda (x y) (+ 1 (length y))) l))

(define (nth-elmt #|t list , int -> t + unit|# l i)
  (if (> i 0) #|bool|# (case-list (lambda (u) (inr unit) #|t + unit|#) (lambda (x #|t|# y #|t list|#) (nth-elmt y (- i 1)) #|t + unit|#) l)
      (if (= i 0) (fst l) #|t + unit|# (inr unit) #|t + unit|#)) #|t + unit|#)

(define (map #|(t -> a) , t list -> a list|# f #|t -> a|# l #|t list|#)
  (case-list (lambda (u) empty #|a list|#) (lambda (x #|t|# y #|t list|#) (link (f x) #|a|#  (map f y) #|a list|#)) l) #|a list|#)

(define (reduce #|t list , (t , a -> a) , a -> a|# l #|t list|# f #|t , a -> a|# s #|a|#)
  (case-list (lambda (u #|unit|#) s #|a|#) (lambda (x #|t|# y #|t list|#) (reduce y  f (f x s) #|a|#)) l)#|a|#)
  

  