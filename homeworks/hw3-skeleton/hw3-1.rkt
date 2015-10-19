#lang racket
; not for execution
; write a type of each expression

(define (sigma #|int int -> ((int -> int) -> int)|# lower #|int|# upper #|int|#)
  (lambda (f) #|int -> int|#
    (define (loop #|int -> int|# n #|int|#)
      (if (> n upper) #|bool|# 0
          (+ (f n)#|int|# (loop (+ n 1))#|int|#)))#|int|#
    (loop lower)#|int|#
    )#|(int -> int) -> int|#
  )

(define (generic-sum #|'a 'b ('a->'c) ('a 'b -> bool) 'd ('c 'd -> 'd) ('a -> 'a) -> 'd|# lower #|'a|# upper #|'b|# f #|'a -> 'c|# larger #|'a 'b -> bool|# base #|'d|# op #|'c 'd -> 'd|# inc #|'a->'a|#)
  (if (larger lower upper) #|bool|# base 
      (op (f lower) #|'c|#
          (generic-sum (inc lower) #|'a|# upper f larger base op inc) #|'d|#) #|'d|#
      )#|'d|#
  )

(define (map #|('a -> 'b) 'a list -> 'b list|# f #|'a -> 'b|# l #|'a list|#)
  (if (null? l) #|bool|# '() #|'b list|#
      (cons (f (car l)#|'a|#)#|'b|# (map f (cdr l))#|'b list|#)#|'b list|#
      )#|'b list|#
  )

(define (reduce #|'a list ('a 'b -> 'b) 'b -> 'b|# l #|'a list|# op #|'a 'b -> 'b|# init #|'b|#)
  (if (null? l) #|bool|# init #|'b|# 
      (op (car l) #|'a|# (reduce (cdr l) #|'a list|# op init) #|'b|#) #|'b|#
      ) #|'b|#
  )

(define (map-reduce #|('a -> 'b) 'a list ('b 'c -> 'c) 'c -> 'c|# f #|'a -> 'b|# l #|'a list|# op #|'b 'c -> 'c|# init #|'c|#)
  (reduce #|'b list ('b 'c -> 'c) 'c -> 'c|# (map  #|('a -> 'b) 'a list -> 'b list|# f l) op  init) #|'c|#
  )
