#lang racket

(provide zipperN)

(define (zipperN l)
  (if (null? l) null
      (if (null? (car l)) (zipperN (cdr l))
      (list* (car (car l)) (zipperN (reverse (list* (cdr (car l)) (reverse (cdr l)))))))))
      