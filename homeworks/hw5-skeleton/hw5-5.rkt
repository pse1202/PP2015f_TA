#lang racket

;;; If these statements are omitted, your submission will be graded 0.

(provide equal)
(provide size)
(provide beautiful)


; You can use the definitions and functions defined in hw5-4.rkt:
; black, white, glue-array-from-tree, glue-array-from-array,
; rotate-array, neighbor-array, pprint-array, is-array?,
; glue-tree-from-tree, glue-tree-from-array, rotate-tree,
; neighbor-tree, pprint-tree, is-tree?, array-to-tree, tree-to-array,
; glue, rotate, neighbor, pprint
(require "hw5-4.rkt")


;;; interfaces
(define (equal f g) ; equal: form * form -> form
  (equal? (array-to-tree f) (array-to-tree g)))

(define (size f) ; size: form -> int
  (if (and (is-array? f) (is-tree? f)) 0 (rawsize (cdr (array-to-tree f)))))

(define (rawsize f)
  (if (list? f) (+ 1 (rawsize (car f))) 0))

(define (lattice-to-loc lattice f)
  (define s (size f))
  (define (int-to-bin n size)
    (if (= size 0) '() (list* (quotient n (expt 2 (- size 1))) (int-to-bin (modulo n (expt 2 (- size 1))) (- size 1)))))
  (map (lambda (x y) (+ (* y 2) (if (= x y) 0 1))) (int-to-bin (car lattice) s) (int-to-bin (cdr lattice) s))
  )

(define (nthelem l n)
  (if (= 0 n) (car l)
      (nthelem (cdr l) (- n 1))))

(define (beautiful f) ; beautiful: form -> bool
  (define (beautiful-sym f)
    (if (equal f (rotate (rotate f))) #t #f))
  (define (beautiful-neighbor f)
      (if (and (< 1 (minneighbor f)) (> 6 (maxneighbor f))) #t #f))
  (or (beautiful-sym f) (beautiful-neighbor f)))

(define (neighbor-lattice lattice f)
  (neighbor (lattice-to-loc lattice f) f))

(define (maxneighbor f)
  (apply max (for/list ([i (expt 2 (size f))])
    (apply max
     (for/list ([j (expt 2 (size f))])
        (neighbor-lattice (cons i j) f))))))
(define (minneighbor f)
  (apply min (for/list ([i (expt 2 (size f))])
    (apply min
     (for/list ([j (expt 2 (size f))])
        (neighbor-lattice (cons i j) f))))))
